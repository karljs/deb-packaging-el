;;; deb-packaging-presets.el --- Preset system for deb-packaging -*- lexical-binding: t -*-

;; Copyright (C) 2024 Karl Smeltzer
;; Author: Karl Smeltzer
;; Keywords: tools, debian, ubuntu, packaging
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Shared customization variables for Debian/Ubuntu packaging.
;;
;; The global "mode" concept (default/debug/upload) and its cross-cutting
;; preset bundles have been removed.  Each tool now carries its own flags
;; directly in its own transient (see deb-packaging-transients.el), with
;; persistence provided by transient's native save mechanism (C-x C-s).
;;
;; What remains here:
;;   - deb-packaging-target-distro  — session distro, seeded once from changelog
;;   - deb-packaging-sbuild-variants — extra-repo URL templates for sbuild
;;   - deb-packaging-test-runners   — runner/image config for autopkgtest

;;; Code:

(defgroup deb-packaging nil
  "Debian/Ubuntu packaging tools."
  :group 'tools)

;;; Target distribution

(defcustom deb-packaging-target-distro "noble"
  "Target distribution for builds and tests.

Seeded once from the changelog the first time a package is visited
\(see `deb-packaging--maybe-seed-distro') and never silently overwritten
thereafter, so a value you choose interactively or via .dir-locals.el is
respected."
  :type 'string
  :group 'deb-packaging)

(defvar deb-packaging--distro-user-set nil
  "Non-nil once `deb-packaging-target-distro' reflects a deliberate choice.
Set when the user picks a distro interactively, or after the one-time seed
from a package's changelog.")

(defun deb-packaging--maybe-seed-distro (distro)
  "Seed `deb-packaging-target-distro' from DISTRO once, if appropriate.
Only takes effect when the user has not already set the distro this session.
Returns the resulting `deb-packaging-target-distro'."
  (when (and distro
             (not (string-empty-p distro))
             (not deb-packaging--distro-user-set))
    (setq deb-packaging-target-distro distro
          deb-packaging--distro-user-set t))
  deb-packaging-target-distro)

;;; sbuild variants
;;
;; Extra apt-repository strings added to the local sbuild chroot.  These are
;; completion candidates in the binary-build transient's --extra-repository
;; option; %s is replaced with the target distro at command-build time.

(defcustom deb-packaging-sbuild-variants
  '(("rust-ppa"
     . "deb [trusted=yes] http://ppa.launchpadcontent.net/rust-toolchain/staging/ubuntu/ %s main")
    ("proposed"
     . "deb http://archive.ubuntu.com/ubuntu/ %s-proposed main"))
  "Alist mapping a short name to an extra-repository string for sbuild.
%s in the value is replaced with the target distro at run time.
These are offered as completion candidates in the binary-build transient."
  :type '(alist :key-type string :value-type string)
  :group 'deb-packaging)

;;; Test runner data
;;
;; Image path templates for each runner; %s replaced with distro at run time.

(defcustom deb-packaging-test-runners
  '(("lxd"  . "autopkgtest/ubuntu/%s/amd64")
    ("qemu" . "/var/lib/adt-images/autopkgtest-%s-amd64.img"))
  "Alist mapping runner name (string) to image path template.
%s is replaced with the target distro at run time.

For Debian, add entries like:
  (\"lxd\" . \"autopkgtest/debian/%s/amd64\")
The tooling (autopkgtest, lxc) is the same; only the image naming
convention differs."
  :type '(alist :key-type string :value-type string)
  :group 'deb-packaging)

;;; Test image build hints
;;
;; Command templates to build a missing test image, keyed by runner name.
;; %s is replaced with the target distro.  Both the command layer's
;; user-error and the status buffer's image-availability hint read from
;; this, so customizing it for Debian (or any other distro family) is
;; the only change needed — no code edits.

(defcustom deb-packaging-test-build-hints
  '(("lxd"  . "autopkgtest-build-lxd ubuntu-daily:%s")
    ("qemu" . "autopkgtest-buildvm-ubuntu-cloud -r %s"))
  "Alist mapping runner name (string) to image-build command template.
%s is replaced with the target distro.  Shown when a test image is
missing, both in the autopkgtest command's error and in the status
buffer's Test section body."
  :type '(alist :key-type string :value-type string)
  :group 'deb-packaging)

(provide 'deb-packaging-presets)
;;; deb-packaging-presets.el ends here
