;;; deb-packaging-dev.el --- LXD dev container for editing upstream source -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/example/deb-packaging
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Stand up a persistent LXD container in which the package's upstream
;; source can be edited with full LSP/editor tooling.
;;
;; The problem this solves: writing a quilt patch means editing upstream
;; files, but an LSP server (clangd, pylsp, ...) is useless unless the
;; build-dependencies — headers, modules — are resolvable.  Those deps
;; are not on the host, and may not even target the right release.  The
;; sbuild chroots have them, but they are throwaway and the wrong
;; lifetime for an editing session.
;;
;; The approach: keep the source on the host (so magit, git and the rest
;; of this package keep operating on it), bind-mount it into a persistent
;; LXD container that has the build-deps installed, and run the LSP server
;; *inside* that container over TRAMP.  Emacs opens the source at a TRAMP
;; path (`/lxc:NAME:/root/work/...'); eglot/lsp-mode then launch the
;; language server on the remote end, where it can resolve everything.
;;
;; Ownership is the usual sharp edge with unprivileged LXD bind mounts.
;; We sidestep it by setting `raw.idmap "both UID 0"' on the container,
;; mapping container root to the host user.  Everything inside runs as
;; root (so apt and mk-build-deps need no sudo), yet every file written
;; to the bind mount is owned by the host user, and the host's own files
;; appear root-owned inside.  No `shift', no per-user TRAMP method.
;;
;; Build-deps are installed with `mk-build-deps' (from equivs) reading
;; debian/control, rather than `apt build-dep', because cloud images do
;; not enable deb-src repositories.
;;
;; Entry point: `deb-packaging-dev-shell'.  Teardown:
;; `deb-packaging-dev-destroy'.  Both are surfaced in
;; `deb-packaging-dispatch'.

;;; Code:

(require 'cl-lib)
(require 'tramp)
(require 'dired)
(require 'deb-packaging-detect)
(require 'deb-packaging-config)
(require 'deb-packaging-commands)

(declare-function deb-packaging--effective-distro "deb-packaging-config")
(declare-function deb-packaging--find-package-dir "deb-packaging-detect")
(declare-function deb-packaging--run-command "deb-packaging-commands")

;;; Customization

(defcustom deb-packaging-dev-image-remote "ubuntu-daily"
  "LXD remote used to launch dev containers.
Combined with the target distro to form the source image, e.g.
\"ubuntu-daily:noble\".  Mirrors the remote used for autopkgtest
images so build-deps resolve against the same archive."
  :type 'string
  :group 'deb-packaging)

(defcustom deb-packaging-dev-container-name-format "deb-dev-%s"
  "Format string for the dev container name, given the target distro.
One persistent container per release; the package dir is re-bind-mounted
on each invocation, so the same container serves whatever package you
visit for that release."
  :type 'string
  :group 'deb-packaging)

(defcustom deb-packaging-dev-mount-point "/root/work"
  "Path inside the dev container where the package directory is mounted.
Lives under root's home because the container runs as root (mapped to
the host user via raw.idmap)."
  :type 'string
  :group 'deb-packaging)

(defcustom deb-packaging-dev-apt-packages
  '("clangd" "python3-pylsp" "rust-analyzer")
  "Apt packages installed in the dev container for editor tooling.
Use this for language servers that Debian/Ubuntu actually package.
eglot already knows which server to launch per major mode (clangd for
C/C++, pylsp for Python, rust-analyzer for Rust, ...), so the only job
is getting the binary into the container.  Servers that are not apt
packages — bash-language-server, gopls, typescript-language-server —
go in `deb-packaging-dev-extra-setup' instead."
  :type '(repeat string)
  :group 'deb-packaging)

(defcustom deb-packaging-dev-extra-setup
  '("apt-get install -y --no-install-recommends npm && npm install -g bash-language-server")
  "Shell commands run (as root) in the dev container after apt setup.
For language servers that are not Debian-packaged and need a toolchain.
Each entry is a single /bin/sh command line; do not use single quotes in
it.  Examples for languages kept manual by default:

  Go:    \"apt-get install -y --no-install-recommends golang-go && \\
          go install golang.org/x/tools/gopls@latest\"
  JS/TS: \"apt-get install -y --no-install-recommends npm && \\
          npm install -g typescript typescript-language-server\""
  :type '(repeat string)
  :group 'deb-packaging)

(defcustom deb-packaging-dev-own-remote-path t
  "When non-nil, let TRAMP use the remote user's login PATH.
This is what lets eglot find language-server binaries installed in
non-default locations inside the container.  Adds
`tramp-own-remote-path' to `tramp-remote-path' globally."
  :type 'boolean
  :group 'deb-packaging)

;;; TRAMP method

(defun deb-packaging-dev--ensure-tramp-method ()
  "Register the `lxc' TRAMP method if it is not already present.
Models the built-in `docker' method on `lxc exec', running as the
container's default user (root)."
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

(defun deb-packaging-dev--container-name (distro)
  "Return the dev container name for DISTRO."
  (format deb-packaging-dev-container-name-format distro))

(defun deb-packaging-dev--provision-script (name distro pkg-dir mount)
  "Return a /bin/sh script that brings up container NAME for editing.
Creates it from DISTRO if missing (mapping container root to the host
user), (re)bind-mounts PKG-DIR at MOUNT, and installs editor tooling
plus the package's build-deps."
  (let ((qname (shell-quote-argument name))
        (qpkg (shell-quote-argument pkg-dir))
        (qmount (shell-quote-argument mount))
        (image (format "%s:%s" deb-packaging-dev-image-remote distro))
        (uid (number-to-string (user-uid)))
        (apt (mapconcat #'shell-quote-argument
                        deb-packaging-dev-apt-packages " ")))
    (string-join
     (append
     (list
      "set -e"
      ;; Create on first use, mapping container root <-> host user so the
      ;; bind mount has correct ownership in both directions.
      (format "if ! lxc info %s >/dev/null 2>&1; then" qname)
      (format "  lxc launch %s %s" (shell-quote-argument image) qname)
      (format "  lxc config set %s raw.idmap \"both %s 0\"" qname uid)
      (format "  lxc restart %s" qname)
      "fi"
      (format "lxc start %s >/dev/null 2>&1 || true" qname)
      ;; Wait for the container agent to answer.
      "i=0"
      "while [ $i -lt 90 ]; do"
      (format "  lxc exec %s -- true >/dev/null 2>&1 && break" qname)
      "  sleep 1; i=$((i+1))"
      "done"
      ;; Let cloud-init finish so the archive is reachable.
      (format "lxc exec %s -- cloud-init status --wait >/dev/null 2>&1 || true"
              qname)
      ;; (Re)point the bind mount at the package we are editing now.
      (format "lxc config device remove %s work >/dev/null 2>&1 || true" qname)
      (format "lxc config device add %s work disk source=%s path=%s"
              qname qpkg qmount)
      ;; Editor tooling (apt-packaged language servers + build helpers).
      (format "lxc exec %s -- sh -c 'export DEBIAN_FRONTEND=noninteractive; apt-get update && apt-get install -y --no-install-recommends devscripts equivs %s'"
              qname apt)
      ;; Build-deps from debian/control (no deb-src needed).
      (format "lxc exec %s -- sh -c 'cd %s && DEBIAN_FRONTEND=noninteractive mk-build-deps -i -r -t \"apt-get -y --no-install-recommends\" debian/control'"
              qname mount))
     ;; Non-apt servers (npm/go/...), one command line each.
     (mapcar
      (lambda (cmd)
        (format "lxc exec %s -- sh -c 'export DEBIAN_FRONTEND=noninteractive; %s'"
                qname cmd))
      deb-packaging-dev-extra-setup)
     (list (format "echo READY: /lxc:%s:%s" name mount)))
     "\n")))

(defun deb-packaging-dev--open-on-success (proc tramp-path)
  "Open dired at TRAMP-PATH when PROC exits successfully.
Chains onto any sentinel already attached to PROC."
  (let ((old (process-sentinel proc)))
    (set-process-sentinel
     proc
     (lambda (p event)
       (when (functionp old)
         (funcall old p event))
       (when (and (eq (process-status p) 'exit)
                  (zerop (process-exit-status p)))
         (dired tramp-path)
         (message "Dev shell ready at %s — open a file and start eglot"
                  tramp-path))))))

;;; Commands

;;;###autoload
(defun deb-packaging-dev-shell ()
  "Bring up an LXD dev container for the current package and open it.
Creates (or reuses) a persistent container for the target release,
bind-mounts the package directory into it, installs editor tooling and
build-deps, then opens a dired buffer at the container's TRAMP path.
Open an upstream file from there and start eglot/lsp-mode: the language
server runs inside the container, where the build-deps resolve."
  (interactive)
  (deb-packaging-dev--ensure-tramp-method)
  (let* ((pkg-dir (or (deb-packaging--find-package-dir)
                      (user-error "Not in a Debian package directory")))
         (distro (deb-packaging--effective-distro))
         (name (deb-packaging-dev--container-name distro))
         (mount deb-packaging-dev-mount-point)
         (script (deb-packaging-dev--provision-script name distro pkg-dir mount))
         (buf (deb-packaging--run-command
               "dev-shell" (list "sh" "-c" script) pkg-dir 'dev-shell))
         (tramp-path (format "/lxc:%s:%s" name mount)))
    (when-let ((proc (get-buffer-process buf)))
      (deb-packaging-dev--open-on-success proc tramp-path))
    buf))

;;;###autoload
(defun deb-packaging-dev-destroy (&optional distro)
  "Delete the dev container for DISTRO (default: the target release)."
  (interactive)
  (let* ((distro (or distro (deb-packaging--effective-distro)))
         (name (deb-packaging-dev--container-name distro)))
    (when (yes-or-no-p (format "Delete dev container %s? " name))
      (deb-packaging--run-command
       "dev-destroy"
       (list "sh" "-c" (format "lxc delete --force %s"
                               (shell-quote-argument name)))
       nil 'dev-destroy))))

(provide 'deb-packaging-dev)
;;; deb-packaging-dev.el ends here
