;;; deb-packaging-presets.el --- Preset system for deb-packaging -*- lexical-binding: t -*-

;; Copyright (C) 2024 Karl Smeltzer
;; Author: Karl Smeltzer
;; Keywords: tools, debian, ubuntu, packaging
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Preset system with two orthogonal dimensions:
;; - Global modes (default, debug, upload) for verbosity/strictness
;; - Tool-specific variants (sbuild variants, test runners)

;;; Code:

(defgroup deb-packaging nil
  "Debian/Ubuntu packaging tools."
  :group 'tools)

;;; Customization variables

(defcustom deb-packaging-global-mode 'default
  "Global mode affecting verbosity and strictness."
  :type '(choice (const :tag "Default" default)
                 (const :tag "Debug" debug)
                 (const :tag "Upload" upload))
  :group 'deb-packaging)

(defcustom deb-packaging-target-distro "noble"
  "Target distribution for builds and tests."
  :type 'string
  :group 'deb-packaging)

(defvar deb-packaging--current-ppa nil
  "Current Launchpad PPA name for the session (e.g. \"ppa:user/name\").
This is session state rather than a customization: a given directory may
target different PPAs at different times (e.g. across branches/versions).")

(defcustom deb-packaging-sbuild-variant 'default
  "Sbuild variant for extra repository configuration."
  :type '(choice (const :tag "Default" default)
                 (const :tag "With Rust PPA" with-rust-ppa)
                 (const :tag "With Proposed" with-proposed))
  :group 'deb-packaging)

(defcustom deb-packaging-test-runner 'lxd
  "Test runner for autopkgtest."
  :type '(choice (const :tag "LXD" lxd)
                 (const :tag "QEMU" qemu))
  :group 'deb-packaging)

;;; Preset data

(defcustom deb-packaging-mode-presets
  '((default
     (dpkg-buildpackage . ("-S" "-d" "-nc" "-sa" "-I" "-i"))
     (lintian . ("-i" "--tag-display-limit=0"))
     (sbuild . ("-A"))
     (autopkgtest . ("--apt-upgrade")))
    (debug
     (dpkg-buildpackage . ("-S" "-d" "-nc" "-sa" "-I" "-i"))
     (lintian . ("-i" "-I" "--pedantic" "--tag-display-limit=0"))
     (sbuild . ("-A" "-v"))
     (autopkgtest . ("--apt-upgrade" "--shell-fail")))
    (upload
     (dpkg-buildpackage . ("-S" "-d" "-sa" "-I" "-i"))
     (lintian . ("-i" "--tag-display-limit=0"))
     (sbuild . ("-A"))
     (autopkgtest . ("--apt-upgrade"))))
  "Mode presets mapping tools to argument lists."
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol
                                   :value-type (repeat string)))
  :group 'deb-packaging)

(defcustom deb-packaging-sbuild-variants
  '((default . nil)
    (with-rust-ppa . ("--extra-repository=deb [trusted=yes] http://ppa.launchpadcontent.net/rust-toolchain/staging/ubuntu/ %s main"))
    (with-proposed . ("--extra-repository=deb http://archive.ubuntu.com/ubuntu/ %s-proposed main")))
  "Sbuild variants with extra repository args. %s is replaced with distro.
Note: \"PPA\" here means an extra apt repository added to the local build
chroot, which is unrelated to the Launchpad PPA workflow (see the `ppa'
snap integration in `deb-packaging-infra' and `deb-packaging-ppa-tests')."
  :type '(alist :key-type symbol
                :value-type (repeat string))
  :group 'deb-packaging)

(defcustom deb-packaging-test-runners
  '((lxd . ((runner . lxd) (image . "autopkgtest/ubuntu/%s/amd64")))
    (qemu . ((runner . qemu) (image . "/var/lib/adt-images/autopkgtest-%s-amd64.img"))))
  "Test runner configurations. %s in image is replaced with distro."
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol :value-type sexp))
  :group 'deb-packaging)

;;; Accessor functions

(defun deb-packaging--get-mode-args (tool)
  "Get args for TOOL from current global mode preset."
  (let ((mode-preset (alist-get deb-packaging-global-mode
                                deb-packaging-mode-presets)))
    (alist-get tool mode-preset)))

(defun deb-packaging--get-sbuild-variant-args (distro)
  "Get extra sbuild args for current variant, formatted with DISTRO."
  (let ((args (alist-get deb-packaging-sbuild-variant
                         deb-packaging-sbuild-variants)))
    (mapcar (lambda (arg) (format arg distro)) args)))

(defun deb-packaging--get-test-runner-config (distro)
  "Get test runner config for current runner, formatted with DISTRO."
  (let ((config (copy-alist (alist-get deb-packaging-test-runner
                                       deb-packaging-test-runners))))
    (when-let ((image (alist-get 'image config)))
      (setf (alist-get 'image config) (format image distro)))
    config))

(defun deb-packaging-set-ppa ()
  "Set the current Launchpad PPA for the session.
Completes against your existing PPAs when `deb-packaging-infra--list-ppas'
is available, but free-text entry is allowed."
  (interactive)
  (let ((candidates (when (fboundp 'deb-packaging-infra--list-ppas)
                      (deb-packaging-infra--list-ppas))))
    (setq deb-packaging--current-ppa
          (completing-read "PPA: " candidates nil nil deb-packaging--current-ppa))
    (message "Current PPA: %s" deb-packaging--current-ppa)))

;;; Cycling functions

(defun deb-packaging--cycle-value (current options)
  "Return next value after CURRENT in OPTIONS list."
  (let* ((keys (mapcar #'car options))
         (pos (cl-position current keys))
         (next-pos (mod (1+ (or pos -1)) (length keys))))
    (nth next-pos keys)))

(defun deb-packaging-cycle-mode ()
  "Cycle through global modes."
  (interactive)
  (setq deb-packaging-global-mode
        (deb-packaging--cycle-value deb-packaging-global-mode
                                    deb-packaging-mode-presets))
  (message "Global mode: %s" deb-packaging-global-mode))

(defun deb-packaging-cycle-sbuild-variant ()
  "Cycle through sbuild variants."
  (interactive)
  (setq deb-packaging-sbuild-variant
        (deb-packaging--cycle-value deb-packaging-sbuild-variant
                                    deb-packaging-sbuild-variants))
  (message "Sbuild variant: %s" deb-packaging-sbuild-variant))

(defun deb-packaging-cycle-test-runner ()
  "Cycle through test runners."
  (interactive)
  (setq deb-packaging-test-runner
        (deb-packaging--cycle-value deb-packaging-test-runner
                                    deb-packaging-test-runners))
  (message "Test runner: %s" deb-packaging-test-runner))

(provide 'deb-packaging-presets)
;;; deb-packaging-presets.el ends here
