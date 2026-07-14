;;; deb-packaging-config.el --- Shared configuration -*- lexical-binding: t -*-

;; Copyright (C) 2024 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging

;;; Commentary:

;; Shared distro config.  The target distribution is the one piece of state
;; shared across tools so build/test/upload default to the same value; each
;; tool keeps its own flags in its transient.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'deb-packaging-detect)

;;; Target distribution

(defvar deb-packaging-target-distro "noble"
  "Target distribution for builds and tests.
Seeded from the changelog once; not overwritten silently.")

(defvar deb-packaging--distro-user-set nil
  "Non-nil when `deb-packaging-target-distro' was set deliberately.
Set by interactive choice or one-time changelog seed.")

(defun deb-packaging--maybe-seed-distro (distro)
  "Seed `deb-packaging-target-distro' from DISTRO once, unless user-set.
Returns `deb-packaging-target-distro'."
  (when (and distro
             (not (string-empty-p distro))
             (not deb-packaging--distro-user-set))
  (setq deb-packaging-target-distro distro
        deb-packaging--distro-user-set t)
  deb-packaging-target-distro))

(defun deb-packaging--set-distro (distro)
  "Set `deb-packaging-target-distro' to DISTRO, always overwriting.
Returns DISTRO."
  (setq deb-packaging-target-distro distro
        deb-packaging--distro-user-set t)
  deb-packaging-target-distro)

(defun deb-packaging--effective-distro ()
  "Return the target distro, seeding once from the changelog if unset.
Falls back to \"noble\"."
  (when-let* ((distro (plist-get (deb-packaging--scan-context) :distro)))
    (deb-packaging--maybe-seed-distro distro))
  deb-packaging-target-distro)

;;; Distribution choices

(defconst deb-packaging-ubuntu-distros
  '("focal" "jammy" "noble" "oracular" "plucky" "questing")
  "Known Ubuntu distribution codenames, alphabetical.")

(defconst deb-packaging-debian-distros
  '("sid" "stable" "testing")
  "Known Debian distribution names, alphabetical.")

;;; Propagation

(defvar deb-packaging-propagate-salsa-user nil
  "Your salsa.debian.org username, used to build the personal remote.
When nil, prepared clones get no `personal' remote.")

(defvar deb-packaging-propagate-cache-dir
  (expand-file-name "deb-packaging/propagate"
                    (deb-packaging--cache-dir))
  "Directory for prepared propagate clones.
Under $XDG_CACHE_HOME/deb-packaging/propagate (or ~/.cache).")

(defvar deb-packaging-propagate-clone-mode-lighter " Prop"
  "Lighter for `deb-packaging-propagate-clone-mode'.")

(defun deb-packaging--distro-choices ()
  "Return distro completion list, prepending the changelog distro if unknown."
  (let ((current (deb-packaging--effective-distro))
        (candidates (append deb-packaging-ubuntu-distros
                            deb-packaging-debian-distros)))
    (if (member current candidates)
        candidates
      (cons current candidates))))

(provide 'deb-packaging-config)
;;; deb-packaging-config.el ends here
