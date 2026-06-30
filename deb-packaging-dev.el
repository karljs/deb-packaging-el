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
;; Provisioning is idempotent: a fingerprint of the apt list, extra-setup
;; commands and debian/control is stored in the container; matching fingerprints
;; skip the install block.  C-u on `deb-packaging-dev-shell' forces it.
;;
;; Ownership: `raw.idmap "both UID 0"' maps container root to the host user,
;; so files written inside are host-owned and host files look root-owned
;; inside.  No shift, no per-user TRAMP method.
;;
;; Build-deps come from `mk-build-deps' reading debian/control, not
;; `apt build-dep', since cloud images have no deb-src.

;;; Code:

(require 'cl-lib)
(require 'tramp)
(require 'dired)
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

(defvar deb-packaging-dev-apt-packages
  '("clangd" "python3-pylsp" "rust-analyzer")
  "Apt packages installed for editor tooling (packaged language servers).
eglot picks the server per major mode; this just gets the binary in.
Non-apt servers (gopls, bash-language-server, ...) go in
`deb-packaging-dev-extra-setup'.")

(defvar deb-packaging-dev-extra-setup
  '("apt-get install -y --no-install-recommends npm && npm install -g bash-language-server")
  "Shell commands run in the container after apt setup.
For non-packaged language servers.  Each entry is a /bin/sh command
line.  Don't use single quotes (see the sh -c wrapping in
`deb-packaging-dev--provision-script').
Examples:
  Go: \"... && go install golang.org/x/tools/gopls@latest\"
  TS: \"... && npm install -g typescript typescript-language-server\"")

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

(defun deb-packaging-dev--provision-fingerprint (pkg-dir)
  "SHA256 of the apt list, extra-setup and debian/control contents in PKG-DIR.
Stored in the container; a mismatch triggers re-provisioning."
  (let ((control-file (expand-file-name "debian/control" pkg-dir)))
    (secure-hash
     'sha256
     (concat (format "%S" deb-packaging-dev-apt-packages)
             (format "%S" deb-packaging-dev-extra-setup)
             (if (file-readable-p control-file)
                 (with-temp-buffer
                   (insert-file-contents control-file)
                   (buffer-string))
               "")))))

(defun deb-packaging-dev--provision-script (name distro pkg-dir mount pkg fingerprint force)
  "Build the /bin/sh provision script for container NAME.
Creates from DISTRO if missing, bind-mounts PKG-DIR at MOUNT, and installs
tooling + build-deps unless FINGERPRINT matches the stored marker.
FORCE re-provisions regardless."
  (let ((qname (shell-quote-argument name))
        (qpkg-dir (shell-quote-argument pkg-dir))
        (qmount (shell-quote-argument mount))
        (image (format "%s:%s" deb-packaging-dev-image-remote distro))
        (uid (number-to-string (user-uid)))
        (apt (mapconcat #'shell-quote-argument
                        deb-packaging-dev-apt-packages " "))
        (device (format "work-%s" pkg))
        (marker "/root/.deb-packaging-dev-provisioned"))
    (string-join
     (append
      (list
       "set -e"
       ;; Create on first use; map container root to host user for ownership.
       (format "if ! lxc info %s >/dev/null 2>&1; then" qname)
       (format "  lxc launch %s %s" (shell-quote-argument image) qname)
       (format "  lxc config set %s raw.idmap \"both %s 0\"" qname uid)
       (format "  lxc restart %s" qname)
       "fi"
       (format "lxc start %s >/dev/null 2>&1 || true" qname)
       ;; Wait for the container agent.
       "i=0"
       "while [ $i -lt 90 ]; do"
       (format "  lxc exec %s -- true >/dev/null 2>&1 && break" qname)
       "  sleep 1; i=$((i+1))"
       "done"
       ;; Cloud-init must finish so the archive is reachable.
       (format "lxc exec %s -- cloud-init status --wait >/dev/null 2>&1 || true"
               qname)
       ;; Point the bind mount at the current package.
       (format "lxc config device remove %s %s >/dev/null 2>&1 || true"
               qname (shell-quote-argument device))
       (format "lxc config device add %s %s disk source=%s path=%s"
               qname (shell-quote-argument device) qpkg-dir qmount)
       ;; Skip install when the fingerprint matches, unless forced.
       (format "FP=%s" (shell-quote-argument fingerprint))
       (format "FORCE=%s" (if force "1" ""))
       (format "MARKER=%s" (shell-quote-argument marker))
       "if [ -z \"$FORCE\" ] && [ -f \"$MARKER\" ] && [ \"$(cat \"$MARKER\")\" = \"$FP\" ]; then"
       "  echo \"Already provisioned (fingerprint matches), skipping install\""
       "else"
       "  if [ -n \"$FORCE\" ]; then echo \"Force re-provisioning\"; else echo \"Provisioning (fingerprint changed or first run)\"; fi"
       ;; Language servers + build helpers.
       (format "  lxc exec %s -- sh -c 'export DEBIAN_FRONTEND=noninteractive; apt-get update && apt-get install -y --no-install-recommends devscripts equivs %s'"
               qname apt)
       ;; Build-deps from debian/control.  No -r: keep the package installed
       ;; so re-provisions of an unchanged control are no-ops.
       (format "  lxc exec %s -- sh -c 'cd %s && DEBIAN_FRONTEND=noninteractive mk-build-deps -i -t \"apt-get -y --no-install-recommends\" debian/control'"
               qname qmount))
      ;; Non-apt servers.
      (mapcar
       (lambda (cmd)
         (format "  lxc exec %s -- sh -c 'export DEBIAN_FRONTEND=noninteractive; %s'"
                 qname cmd))
       deb-packaging-dev-extra-setup)
       (list
        ;; Write the fingerprint.  Both values are safe (hex, fixed path)
        ;; so no quoting needed inside the single-quoted sh -c.
        (format "  lxc exec %s -- sh -c 'echo %s > %s'"
                qname fingerprint marker)
        "fi"
        (format "echo READY: /lxc:%s:%s" name mount)))
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

;;; Commands

;;;###autoload
(defun deb-packaging-dev-shell (&optional force)
  "Bring up an LXD dev container for the current package and open it.
Creates or reuses a container, bind-mounts the package dir, installs
tooling and build-deps, then opens dired at the container's TRAMP path.

Provisioning is idempotent: skips the install block when the fingerprint
matches.  C-u (FORCE) re-provisions regardless."
  (interactive "P")
  (deb-packaging-dev--ensure-tramp-method)
  (let* ((pkg-dir (or (deb-packaging--find-package-dir)
                      (user-error "Not in a Debian package directory")))
         (info (deb-packaging--parse-changelog pkg-dir))
         (pkg (nth 0 info))
         (distro (deb-packaging--effective-distro))
         (name (deb-packaging-dev--container-name pkg distro))
         (mount (deb-packaging-dev--mount-path pkg))
         (fingerprint (deb-packaging-dev--provision-fingerprint pkg-dir))
         (script (deb-packaging-dev--provision-script
                  name distro pkg-dir mount pkg fingerprint force))
         (buf (deb-packaging--run-command
               "dev-shell" (list "sh" "-c" script) pkg-dir 'dev-shell))
         (tramp-path (format "/lxc:%s:%s" name mount)))
    (when-let ((proc (get-buffer-process buf)))
      (deb-packaging-dev--open-on-success proc tramp-path))
    buf))

;;;###autoload
(defun deb-packaging-dev-destroy ()
  "Delete the dev container for the current package and target release."
  (interactive)
  (let* ((pkg-dir (or (deb-packaging--find-package-dir)
                      (user-error "Not in a Debian package directory")))
         (info (deb-packaging--parse-changelog pkg-dir))
         (pkg (nth 0 info))
         (distro (deb-packaging--effective-distro))
         (name (deb-packaging-dev--container-name pkg distro)))
    (when (yes-or-no-p (format "Delete dev container %s? " name))
      (deb-packaging--run-command
       "dev-destroy"
       (list "sh" "-c" (format "lxc delete --force %s"
                               (shell-quote-argument name)))
       nil 'dev-destroy))))

(provide 'deb-packaging-dev)
;;; deb-packaging-dev.el ends here
