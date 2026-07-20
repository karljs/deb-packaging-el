;;; deb-packaging-config.el --- Shared configuration -*- lexical-binding: t -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Shared distro config.  The target distribution is the one piece of state
;; shared across tools so build/test/upload default to the same value; each
;; tool keeps its own flags in its transient.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'deb-packaging-detect)

;;; Target distribution

(defvar deb-packaging-config-target-distro "noble"
  "Target distribution for builds and tests.
Seeded from the changelog once; not overwritten silently.")

(defvar deb-packaging-config--distro-user-set nil
  "Non-nil when `deb-packaging-config-target-distro' was set deliberately.
Set by interactive choice or one-time changelog seed.")

(defun deb-packaging-config--maybe-seed-distro (distro)
  "Seed `deb-packaging-config-target-distro' from DISTRO once, unless user-set.
Returns `deb-packaging-config-target-distro'."
  (when (and distro
             (not (string-empty-p distro))
             (not deb-packaging-config--distro-user-set))
  (setq deb-packaging-config-target-distro distro
        deb-packaging-config--distro-user-set t)
  deb-packaging-config-target-distro))

(defun deb-packaging-config--set-distro (distro)
  "Set `deb-packaging-config-target-distro' to DISTRO, always overwriting.
Returns DISTRO."
  (setq deb-packaging-config-target-distro distro
        deb-packaging-config--distro-user-set t)
  deb-packaging-config-target-distro)

(defun deb-packaging-config--effective-distro ()
  "Return the target distro, seeding once from the changelog if unset.
Falls back to \"noble\"."
  (when-let* ((distro (plist-get (deb-packaging-detect--scan-context) :distro)))
    (deb-packaging-config--maybe-seed-distro distro))
  deb-packaging-config-target-distro)

;;; Distribution choices

(defconst deb-packaging-config-ubuntu-distros
  '("focal" "jammy" "noble" "oracular" "plucky" "questing")
  "Known Ubuntu distribution codenames, alphabetical.")

(defconst deb-packaging-config-debian-distros
  '("sid" "stable" "testing")
  "Known Debian distribution names, alphabetical.")

;;; Propagation

(defvar deb-packaging-config-propagate-salsa-user nil
  "Your salsa.debian.org username, used to build the personal remote.
When nil, prepared clones get no `personal' remote.")

(defvar deb-packaging-config-propagate-cache-dir
  (expand-file-name "deb-packaging/propagate"
                    (deb-packaging-detect--cache-dir))
  "Directory for prepared propagate clones.
Under $XDG_CACHE_HOME/deb-packaging/propagate (or ~/.cache).")

(defvar deb-packaging-config-propagate-clone-mode-lighter " Prop"
  "Lighter for `deb-packaging-propagate-clone-mode'.")

;;; Extra PPA candidates

(defvar deb-packaging-config-extra-ppas nil
  "List of ppa:owner/name strings for binary-build completion candidates.
Merged into the --extra-repository completion list alongside owned PPAs
and sbuild variants.  Defaults to nil; per-package persistence handles
remembering across sessions.  Set in your init file if you want certain
dependency PPAs always available as candidates.")

(defun deb-packaging-config--distro-choices ()
  "Return distro completion list, prepending the changelog distro if unknown."
  (let ((current (deb-packaging-config--effective-distro))
        (candidates (append deb-packaging-config-ubuntu-distros
                            deb-packaging-config-debian-distros)))
    (if (member current candidates)
        candidates
      (cons current candidates))))

(provide 'deb-packaging-config)
;;; deb-packaging-config.el ends here
