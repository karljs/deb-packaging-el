;;; deb-packaging-dev.el --- LXD dev container for editing upstream source -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/example/deb-packaging
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Persistent LXD container for editing upstream source with real LSP.
;;
;; Writing a quilt patch means editing upstream files, but clangd/pylsp/etc
;; can't resolve anything without the build-deps installed, and those deps
;; aren't on the host.  sbuild chroots have them but are throwaway.
;;
;; Source stays on the host (so magit/git keep working).  We bind-mount it
;; into a persistent LXD container with build-deps installed and run the LSP
;; server inside over TRAMP: `/lxc:NAME:/root/work/PKG/...'.  eglot launches
;; the server on the remote end where headers/modules resolve.
;;
;; One container per package per release (deb-dev-<pkg>-<release>), so the
;; TRAMP path, LSP index and build-deps survive across sessions.
;;
;; Provisioning installs four layers, each independently idempotent:
;;   1. Core build helpers (devscripts, equivs) - installed if missing
;;   2. Build-deps from `mk-build-deps' reading debian/control - marker: control hash
;;   3. Language servers - prompted when needed, picked from
;;      `deb-packaging-dev-language-profiles'.  Selection cached per package
;;      in ~/.cache/deb-packaging-dev/.  Marker: profiles hash.
;;   4. Extra dev tools (`deb-packaging-dev-extra-packages') - marker: tools hash
;;
;; Each layer has its own marker file in the container.  Only the layer
;; whose fingerprint changed re-runs.  Editing debian/control doesn't
;; re-install language servers; adding a dev tool doesn't re-run
;; mk-build-deps.  C-u forces all layers.
;;
;; Ownership: `raw.idmap "both UID 0"' maps container root to the host user,
;; so files written inside are host-owned and host files look root-owned
;; inside.  No shift, no per-user TRAMP method.

;;; Code:

(require 'cl-lib)
(require 'tramp)
(require 'dired)
(require 'json)
(require 'deb-packaging-detect)
(require 'deb-packaging-config)
(require 'deb-packaging-commands)

(declare-function deb-packaging--effective-distro "deb-packaging-config")
(declare-function deb-packaging--find-package-dir "deb-packaging-detect")
(declare-function deb-packaging--parse-changelog "deb-packaging-detect")
(declare-function deb-packaging--run-command "deb-packaging-commands")

;;; Variables

(defvar deb-packaging-dev-image-remote "ubuntu-daily"
  "LXD remote for the dev container source image, e.g. \"ubuntu-daily:noble\".")

(defvar deb-packaging-dev-mount-point "/root/work"
  "Base mount path inside the container.
Package name is appended, e.g. \"/root/work/foo\".")

(defvar deb-packaging-dev-language-profiles
  '((c/c++  "C/C++"  :apt "clangd bear")
    (python "Python" :apt "python3-pylsp")
    (rust   "Rust"   :apt "rust-analyzer")
    (go     "Go"     :setup "apt-get install -y --no-install-recommends golang-go && go install golang.org/x/tools/gopls@latest")
    (bash   "Bash"   :setup "apt-get install -y --no-install-recommends npm && npm install -g bash-language-server")
    (js-ts  "JS/TS"  :setup "apt-get install -y --no-install-recommends npm && npm install -g typescript typescript-language-server"))
  "Registry of language server profiles for dev containers.
Each entry is (KEY LABEL [plist...]) where plist keys are:
  :apt    string of apt packages (best-effort install)
  :setup  /bin/sh command line to install the server
Users can add entries here to extend language support.  KEY is a symbol,
LABEL is shown in the selection prompt.")

(defvar deb-packaging-dev-extra-packages '("git" "gdb" "strace")
  "Extra apt packages installed in dev containers for development.
Things you want in the container that aren't build-deps and aren't
language servers.  Best-effort: missing packages on a release are skipped.
Not part of the provision fingerprint (it's a global setting).")

(defvar deb-packaging-dev-own-remote-path t
  "When non-nil, add `tramp-own-remote-path' to `tramp-remote-path'.
Lets eglot find servers installed in non-default locations inside the container.")

;;; TRAMP method

(defun deb-packaging-dev--ensure-tramp-method ()
  "Register the `lxc' TRAMP method if not already present.
Models the built-in `docker' method on `lxc exec'."
  (unless (assoc "lxc" tramp-methods)
    (add-to-list
     'tramp-methods
     `("lxc"
       (tramp-login-program "lxc")
       (tramp-login-args (("exec") ("%h") ("--") ("%l")))
       (tramp-direct-async ("/bin/sh" "-c"))
       (tramp-remote-shell "/bin/sh")
       (tramp-remote-shell-login ("-l"))
       (tramp-remote-shell-args ("-i" "-c")))))
  (when (and deb-packaging-dev-own-remote-path
             (not (memq 'tramp-own-remote-path tramp-remote-path)))
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)))

;;; Provisioning

(defun deb-packaging-dev--container-name (pkg distro)
  "Container name for PKG in DISTRO: deb-dev-<pkg>-<distro>."
  (format "deb-dev-%s-%s" pkg distro))

(defun deb-packaging-dev--mount-path (pkg)
  "Mount path inside the container for PKG."
  (format "%s/%s" deb-packaging-dev-mount-point pkg))

(defun deb-packaging-dev--profile-lookup (key)
  "Return the profile entry for KEY, or nil."
  (assq key deb-packaging-dev-language-profiles))

(defun deb-packaging-dev--profile-apt (entry)
  "Return the :apt string for profile ENTRY, or nil."
  (plist-get (cddr entry) :apt))

(defun deb-packaging-dev--profile-setup (entry)
  "Return the :setup command for profile ENTRY, or nil."
  (plist-get (cddr entry) :setup))

(defun deb-packaging-dev--langs-cache-file (pkg distro)
  "Return the cache file path for PKG/DISTRO language selection.
Returns nil if the cache directory can't be created."
  (condition-case nil
      (let ((dir (expand-file-name
                  (format "deb-packaging-dev")
                  (or (getenv "XDG_CACHE_HOME")
                      (expand-file-name "~/.cache")))))
        (unless (file-directory-p dir)
          (make-directory dir t))
        (expand-file-name (format "%s-%s-langs" pkg distro) dir))
    (error nil)))

(defun deb-packaging-dev--read-langs-cache (pkg distro)
  "Return cached language profile keys for PKG/DISTRO, or nil.
Never signals; returns nil on any error (missing file, corrupt data, etc)."
  (condition-case nil
      (let ((file (deb-packaging-dev--langs-cache-file pkg distro)))
        (when (and file (file-readable-p file))
          (with-temp-buffer
            (insert-file-contents file)
            (let ((keys nil))
              (dolist (line (split-string (buffer-string) "\n" t))
                (let ((sym (intern-soft (string-trim line))))
                  (when sym (push sym keys))))
              (nreverse keys)))))
    (error nil)))

(defun deb-packaging-dev--write-langs-cache (pkg distro keys)
  "Write language profile KEYS for PKG/DISTRO to the cache file.
Never signals; silently skips on any error."
  (condition-case nil
      (let ((file (deb-packaging-dev--langs-cache-file pkg distro)))
        (when file
          (with-temp-file file
            (dolist (key keys)
              (insert (format "%s\n" key))))))
    (error nil)))

(defun deb-packaging-dev--select-profiles (pkg distro)
  "Prompt for language profiles via `completing-read-multiple'.
Returns a list of profile entry lists.  Pre-selects from the cache file
if one exists for PKG/DISTRO; on any cache error, nothing is pre-selected.
Writes the selection back to the cache."
  (let* ((cached-keys (deb-packaging-dev--read-langs-cache pkg distro))
         (labels (mapcar #'cadr deb-packaging-dev-language-profiles))
         (initial (when cached-keys
                    (mapconcat #'identity
                               (delq nil
                                     (mapcar (lambda (key)
                                               (let ((entry (deb-packaging-dev--profile-lookup key)))
                                                 (when entry (cadr entry))))
                                             cached-keys))
                               ",")))
         (chosen (completing-read-multiple
                  "Language servers (comma-separated, empty for none): "
                  labels nil nil initial))
         (keys (mapcar (lambda (label)
                         (car (cl-find label
                                       deb-packaging-dev-language-profiles
                                       :key #'cadr :test #'equal)))
                       (cl-remove-if #'string-empty-p
                                     (mapcar #'string-trim chosen))))
         (entries (delq nil (mapcar #'deb-packaging-dev--profile-lookup keys))))
    (deb-packaging-dev--write-langs-cache pkg distro keys)
    entries))

(defun deb-packaging-dev--control-fingerprint (pkg-dir)
  "SHA256 of debian/control contents in PKG-DIR."
  (let ((control-file (expand-file-name "debian/control" pkg-dir)))
    (secure-hash
     'sha256
     (if (file-readable-p control-file)
         (with-temp-buffer
           (insert-file-contents control-file)
           (buffer-string))
       ""))))

(defun deb-packaging-dev--langs-fingerprint (profiles)
  "SHA256 of selected PROFILES (full definitions)."
  (secure-hash 'sha256 (format "%S" profiles)))

(defun deb-packaging-dev--tools-fingerprint ()
  "SHA256 of `deb-packaging-dev-extra-packages'."
  (secure-hash 'sha256 (format "%S" deb-packaging-dev-extra-packages)))

(defun deb-packaging-dev--provision-script (name distro pkg-dir mount pkg
                                                control-fp langs-fp tools-fp
                                                force profiles)
  "Build the /bin/sh provision script for container NAME.
Creates from DISTRO if missing, bind-mounts PKG-DIR at MOUNT.
Provisioning is split into three independent layers, each with its own
marker: build-deps (CONTROL-FP), language servers (LANGS-FP), and extra
dev tools (TOOLS-FP).  Only the layer whose fingerprint changed re-runs.
FORCE re-provisions all layers regardless."
  (let ((qname (shell-quote-argument name))
        (qpkg-dir (shell-quote-argument pkg-dir))
        (qmount (shell-quote-argument mount))
        (image (format "%s:%s" deb-packaging-dev-image-remote distro))
        (uid (number-to-string (user-uid)))
        (extra-apt (mapconcat #'shell-quote-argument
                              deb-packaging-dev-extra-packages " "))
        (profile-apts (delq nil
                            (mapcar #'deb-packaging-dev--profile-apt profiles)))
        (profile-setups (delq nil
                              (mapcar #'deb-packaging-dev--profile-setup profiles)))
        (device (format "work-%s" pkg)))
    (string-join
     (append
      (list
       "set -e"
       (format "if ! lxc info %s >/dev/null 2>&1; then" qname)
       (format "  lxc launch %s %s" (shell-quote-argument image) qname)
       (format "  lxc config set %s raw.idmap \"both %s 0\"" qname uid)
       (format "  lxc restart %s" qname)
       "fi"
       (format "lxc start %s >/dev/null 2>&1 || true" qname)
       "i=0"
       "while [ $i -lt 90 ]; do"
       (format "  lxc exec %s -- true >/dev/null 2>&1 && break" qname)
       "  sleep 1; i=$((i+1))"
       "done"
       (format "lxc exec %s -- cloud-init status --wait >/dev/null 2>&1 || true"
               qname)
       (format "lxc config device remove %s %s >/dev/null 2>&1 || true"
               qname (shell-quote-argument device))
       (format "if ! lxc config device add %s %s disk source=%s path=%s 2>/dev/null; then"
               qname (shell-quote-argument device) qpkg-dir qmount)
       (format "  lxc restart %s" qname)
       (format "  lxc config device add %s %s disk source=%s path=%s"
               qname (shell-quote-argument device) qpkg-dir qmount)
       "fi"
       (format "FORCE=%s" (if force "1" "")))
      ;; Core helpers: install if missing (no marker, these are static).
      (list
       (format "if ! lxc exec %s -- dpkg -s devscripts >/dev/null 2>&1; then"
               qname)
       "  echo 'Installing core build helpers...'"
       (format "  lxc exec %s -- sh -c %s"
               qname
               (shell-quote-argument
                "export DEBIAN_FRONTEND=noninteractive; apt-get update && apt-get install -y --no-install-recommends devscripts equivs"))
       "fi")
      ;; Build-deps layer: marker = control fingerprint.
      (list
       (format "FP_CONTROL=%s" (shell-quote-argument control-fp))
       "MARKER_CONTROL=/root/.deb-dev-marker-control"
       "if [ -z \"$FORCE\" ] && [ -f \"$MARKER_CONTROL\" ] && [ \"$(cat \"$MARKER_CONTROL\")\" = \"$FP_CONTROL\" ]; then"
       "  echo 'Build-deps up to date, skipping mk-build-deps'"
       "else"
       "  echo 'Installing build-deps from debian/control...'"
       (format "  lxc exec %s -- sh -c %s || { echo 'mk-build-deps failed: build-deps could not be satisfied' >&2; exit 1; }"
               qname
               (shell-quote-argument
                (format "cd %s && DEBIAN_FRONTEND=noninteractive mk-build-deps -i -t \"apt-get -y --no-install-recommends\" debian/control" mount)))
       (format "  lxc exec %s -- sh -c %s"
               qname
               (shell-quote-argument
                (format "echo %s > /root/.deb-dev-marker-control" control-fp)))
       "fi")
      ;; Language servers layer: marker = langs fingerprint.
      (list
       (format "FP_LANGS=%s" (shell-quote-argument langs-fp))
       "MARKER_LANGS=/root/.deb-dev-marker-langs"
       "if [ -z \"$FORCE\" ] && [ -f \"$MARKER_LANGS\" ] && [ \"$(cat \"$MARKER_LANGS\")\" = \"$FP_LANGS\" ]; then"
       "  echo 'Language servers up to date, skipping'"
       "else"
       (when profile-apts
         (list
          "  echo 'Installing language servers...'"
          (format "  lxc exec %s -- sh -c %s || true"
                  qname
                  (shell-quote-argument
                   (format "export DEBIAN_FRONTEND=noninteractive; apt-get install -y --no-install-recommends %s || echo '  (some unavailable on this release)'"
                           (mapconcat #'shell-quote-argument profile-apts " "))))))
       (mapcar
        (lambda (cmd)
          (format "  lxc exec %s -- sh -c %s"
                  qname
                  (shell-quote-argument
                   (format "export DEBIAN_FRONTEND=noninteractive; %s" cmd))))
        profile-setups)
       (format "  lxc exec %s -- sh -c %s"
               qname
               (shell-quote-argument
                (format "echo %s > /root/.deb-dev-marker-langs" langs-fp)))
       "fi")
      ;; Extra dev tools layer: marker = tools fingerprint.
      (list
       (format "FP_TOOLS=%s" (shell-quote-argument tools-fp))
       "MARKER_TOOLS=/root/.deb-dev-marker-tools"
       "if [ -z \"$FORCE\" ] && [ -f \"$MARKER_TOOLS\" ] && [ \"$(cat \"$MARKER_TOOLS\")\" = \"$FP_TOOLS\" ]; then"
       "  echo 'Dev tools up to date, skipping'"
       "else"
       (when (and deb-packaging-dev-extra-packages
                  (not (string-empty-p extra-apt)))
         (list
          "  echo 'Installing dev tools...'"
          (format "  lxc exec %s -- sh -c %s || true"
                  qname
                  (shell-quote-argument
                   (format "export DEBIAN_FRONTEND=noninteractive; apt-get install -y --no-install-recommends %s || echo '  (some unavailable on this release)'"
                           extra-apt)))))
       (format "  lxc exec %s -- sh -c %s"
               qname
               (shell-quote-argument
                (format "echo %s > /root/.deb-dev-marker-tools" tools-fp)))
       "fi")
      (list (format "echo READY: /lxc:%s:%s" name mount)))
     "\n")))

(defun deb-packaging-dev--open-on-success (proc tramp-path)
  "Open dired at TRAMP-PATH when PROC exits 0.  Chains onto existing sentinels."
  (let ((old (process-sentinel proc)))
    (set-process-sentinel
     proc
     (lambda (p event)
       (when (functionp old)
         (funcall old p event))
       (when (and (eq (process-status p) 'exit)
                  (zerop (process-exit-status p)))
         (dired tramp-path)
         (message "Dev shell ready at %s" tramp-path))))))

;;; Eglot

(defun deb-packaging-dev-eglot ()
  "Start eglot for the current buffer, launching the server in the container.
The buffer must be visiting a file under an `/lxc:' TRAMP path (opened from
the dev shell's dired).  Ensures `tramp-own-remote-path' is in
`tramp-remote-path' so the container-installed server binary is found,
then calls `eglot-ensure'.

No hooks and no auto-start: call this manually after opening a file, or
wire it into your own `prog-mode-hook' if you want it automatic."
  (interactive)
  (unless (and buffer-file-name
              (string-prefix-p "/lxc:" buffer-file-name))
    (user-error "Not visiting a file under /lxc: (open one from the dev shell)"))
  (deb-packaging-dev--ensure-tramp-method)
  (require 'eglot)
  (eglot-ensure))

;;; Compile database

(defun deb-packaging-dev-compile-db ()
  "Generate compile_commands.json in the dev container for the current package.
Runs a bear-wrapped `dpkg-buildpackage -b' inside the container so clangd
can resolve headers and symbols.  Bear captures compiler calls as they
happen, so even a partially-failed build produces a usable database.

Prerequisites: the dev container must already exist (run
`deb-packaging-dev-shell' first) and the C/C++ language profile must have
been selected (it installs bear).

The generated compile_commands.json lands in the package root on the bind
mount, visible on the host.  Add it to .gitignore if you don't want it
tracked.

Re-run after changing build flags or adding new source files."
  (interactive)
  (let* ((pkg-dir (or (deb-packaging--find-package-dir)
                      (user-error "Not in a Debian package directory")))
         (info (deb-packaging--parse-changelog pkg-dir))
         (pkg (nth 0 info))
         (distro (deb-packaging--effective-distro))
         (name (deb-packaging-dev--container-name pkg distro))
         (mount (deb-packaging-dev--mount-path pkg))
         (qname (shell-quote-argument name))
         (qmount (shell-quote-argument mount)))
    (unless (zerop (call-process "lxc" nil nil nil "info" name))
      (user-error
       "Container %s doesn't exist. Run `deb-packaging-dev-shell' first."
       name))
    (call-process "lxc" nil nil nil "start" name)
    (let* ((inner
            (string-join
             (list
              (format "cd %s || exit 1" qmount)
              ;; Check bear is installed before running, so we can give
              ;; a clear message instead of "bear: command not found".
              "if ! command -v bear >/dev/null 2>&1; then"
              "  echo 'bear is not installed in the container.' >&2"
              "  echo 'Re-run dev-shell with C-u and select C/C++ to install it.' >&2"
              "  exit 1"
              "fi"
              ;; Run bear-wrapped build.  Don't let build failure abort
              ;; the script: bear writes partial results as it goes.
              "bear --output compile_commands.json -- dpkg-buildpackage -b -uc -us"
              "BUILD_EXIT=$?"
              "if [ -f compile_commands.json ]; then"
              "  echo \"compile_commands.json generated (build exit: $BUILD_EXIT)\""
              "else"
              "  echo 'Build failed, no compile_commands.json generated.' >&2"
              "  exit 1"
              "fi")
             "\n"))
           (script (format "lxc exec %s -- sh -c %s"
                           qname (shell-quote-argument inner)))
           (buf (deb-packaging--run-command
                 "compile-db" (list "sh" "-c" script) pkg-dir 'compile-db)))
      buf)))

;;; Open existing container

(defun deb-packaging-dev--tramp-path-for-current ()
  "Return the TRAMP path for the current package's dev container.
Starts the container if it's stopped.  Signals `user-error' if the
container doesn't exist or we're not in a package directory."
  (let* ((pkg-dir (or (deb-packaging--find-package-dir)
                      (user-error "Not in a Debian package directory")))
         (info (deb-packaging--parse-changelog pkg-dir))
         (pkg (nth 0 info))
         (distro (deb-packaging--effective-distro))
         (name (deb-packaging-dev--container-name pkg distro))
         (mount (deb-packaging-dev--mount-path pkg)))
    (unless (zerop (call-process "lxc" nil nil nil "info" name))
      (user-error "Container %s doesn't exist. Run `deb-packaging-dev-shell' first."
                  name))
    (call-process "lxc" nil nil nil "start" name)
    (deb-packaging-dev--ensure-tramp-method)
    (format "/lxc:%s:%s" name mount)))

(defun deb-packaging-dev-open ()
  "Open dired at the dev container's TRAMP path.
The container must already exist (run `deb-packaging-dev-shell' first).
Starts it if stopped."
  (interactive)
  (dired (deb-packaging-dev--tramp-path-for-current)))

(defun deb-packaging-dev-exec ()
  "Open an interactive shell in the dev container.
Runs `lxc exec NAME -- bash -l' in a comint buffer.  The container must
already exist; starts it if stopped."
  (interactive)
  (let* ((pkg-dir (or (deb-packaging--find-package-dir)
                      (user-error "Not in a Debian package directory")))
         (info (deb-packaging--parse-changelog pkg-dir))
         (pkg (nth 0 info))
         (distro (deb-packaging--effective-distro))
         (name (deb-packaging-dev--container-name pkg distro)))
    (unless (zerop (call-process "lxc" nil nil nil "info" name))
      (user-error "Container %s doesn't exist. Run `deb-packaging-dev-shell' first."
                  name))
    (call-process "lxc" nil nil nil "start" name)
    (let ((buf (make-comint (format "lxc:%s" name) "lxc" nil
                            "exec" name "--" "bash" "-l")))
      (switch-to-buffer buf))))

(defun deb-packaging-dev-project ()
  "Open the dev container's project for file navigation.
Uses projectile if loaded, otherwise the built-in `project' package.
Falls back to dired if neither is available."
  (interactive)
  (let ((tramp-path (deb-packaging-dev--tramp-path-for-current)))
    (cond
     ((fboundp 'projectile-find-file)
      (let ((default-directory tramp-path))
        (call-interactively #'projectile-find-file)))
     ((fboundp 'project-find-file)
      (let ((default-directory tramp-path))
        (call-interactively #'project-find-file)))
     (t
      (message "Install projectile for project navigation; falling back to dired")
      (dired tramp-path)))))

;;; Container inspection

(defun deb-packaging-dev--list-containers (&optional prefix)
  "Return a list of plists for dev containers matching PREFIX.
PREFIX defaults to \"deb-dev-\".  Each plist has :name, :status
\(e.g. \"RUNNING\"), :source (mount source of the work device, or nil)
and :fingerprint (stored provision marker, or nil).  Returns nil if
`lxc' is unavailable or no containers match."
  (let* ((filter (or prefix "deb-dev-"))
         (output (with-output-to-string
                   (with-current-buffer standard-output
                     (ignore-errors
                      (call-process "lxc" nil t nil "list" filter "--format=json"))))))
    (when (and output (not (string-empty-p output)))
      (let ((entries (ignore-errors (json-read-from-string output))))
        (mapcar
         (lambda (c)
           (let* ((devices (cdr (assoc-string "devices" c)))
                  (work-dev (cl-find-if
                             (lambda (d)
                               (string-prefix-p "work-"
                                                 (symbol-name (car d))))
                             devices))
                  (source (when work-dev
                            (cdr (assoc-string "source" (cdr work-dev))))))
             (list :name (cdr (assoc-string "name" c))
                   :status (cdr (assoc-string "status" c))
                   :source source)))
         (if (vectorp entries) (append entries nil) entries))))))

(defun deb-packaging-dev--container-for-package (pkg distro)
  "Return the container plist for PKG in DISTRO, or nil."
  (cl-find (deb-packaging-dev--container-name pkg distro)
           (deb-packaging-dev--list-containers)
           :key (lambda (c) (plist-get c :name))
           :test #'equal))

;;; Commands

;;;###autoload
(defun deb-packaging-dev-shell (&optional force)
  "Bring up an LXD dev container for the current package and open it.
Creates or reuses a container, bind-mounts the package dir, installs
tooling and build-deps, then opens dired at the container's TRAMP path.

Prompts for language servers when the langs layer needs provisioning
(first run or changed selection).  Pre-selects from the last selection
for this package.  Provisioning is split into independent layers
(build-deps, languages, dev tools); only the layer whose fingerprint
changed re-runs.  C-u (FORCE) re-provisions all layers."
  (interactive "P")
  (deb-packaging-dev--ensure-tramp-method)
  (let* ((pkg-dir (or (deb-packaging--find-package-dir)
                      (user-error "Not in a Debian package directory")))
         (info (deb-packaging--parse-changelog pkg-dir))
         (pkg (nth 0 info))
         (distro (deb-packaging--effective-distro))
         (name (deb-packaging-dev--container-name pkg distro))
         (mount (deb-packaging-dev--mount-path pkg))
         (control-fp (deb-packaging-dev--control-fingerprint pkg-dir))
         (tools-fp (deb-packaging-dev--tools-fingerprint))
         ;; Only prompt for languages if the langs layer will actually run.
         ;; Check the stored marker vs the new langs fingerprint.
         (langs-fp (secure-hash 'sha256 ""))
         (profiles nil))
    ;; Compute langs fingerprint and decide whether to prompt.
    ;; We need profiles for the fingerprint, but we don't want to prompt
    ;; if the marker matches.  So: compute a tentative fingerprint from
    ;; the cache, check the marker, and only prompt if it mismatches.
    (let* ((cached-keys (deb-packaging-dev--read-langs-cache pkg distro))
           (cached-profiles (delq nil
                                  (mapcar #'deb-packaging-dev--profile-lookup
                                          cached-keys)))
           (cached-fp (when cached-profiles
                        (deb-packaging-dev--langs-fingerprint cached-profiles)))
           (marker-val (when (zerop (call-process "lxc" nil nil nil "info" name))
                         (with-output-to-string
                           (with-current-buffer standard-output
                             (ignore-errors
                              (call-process "lxc" nil t nil "exec" name "--"
                                            "cat" "/root/.deb-dev-marker-langs"))))))
           (marker-val (string-trim (or marker-val "")))
           (need-prompt (or force
                            (null cached-fp)
                            (not (string= marker-val cached-fp)))))
      (if need-prompt
          (setq profiles (deb-packaging-dev--select-profiles pkg distro)
                langs-fp (deb-packaging-dev--langs-fingerprint profiles))
        (setq profiles cached-profiles
              langs-fp cached-fp)))
    (let* ((script (deb-packaging-dev--provision-script
                    name distro pkg-dir mount pkg
                    control-fp langs-fp tools-fp force profiles))
           (buf (deb-packaging--run-command
                 "dev-shell" (list "sh" "-c" script) pkg-dir 'dev-shell))
           (tramp-path (format "/lxc:%s:%s" name mount)))
      (when-let ((proc (get-buffer-process buf)))
        (deb-packaging-dev--open-on-success proc tramp-path))
      buf)))

;;;###autoload
(defun deb-packaging-dev-destroy (&optional arg)
  "Delete a dev container.
With no prefix arg and inside a package, deletes that package's container.
With a prefix ARG, or outside a package, prompts with completion over all
existing dev containers."
  (interactive "P")
  (let* ((containers (deb-packaging-dev--list-containers))
         (names (mapcar (lambda (c) (plist-get c :name)) containers))
         (name
          (cond
           ((and (not arg) (deb-packaging--find-package-dir))
            (let* ((pkg-dir (deb-packaging--find-package-dir))
                   (info (deb-packaging--parse-changelog pkg-dir))
                   (pkg (nth 0 info))
                   (distro (deb-packaging--effective-distro)))
              (deb-packaging-dev--container-name pkg distro)))
           ((null names)
            (user-error "No dev containers found"))
           (t
            (completing-read "Delete container: " names nil t)))))
    (when (yes-or-no-p (format "Delete dev container %s? " name))
      (deb-packaging--run-command
       "dev-destroy"
       (list "sh" "-c" (format "lxc delete --force %s"
                               (shell-quote-argument name)))
       nil 'dev-destroy))))

(provide 'deb-packaging-dev)
;;; deb-packaging-dev.el ends here
