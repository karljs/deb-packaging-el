;;; deb-packaging-config.el --- Shared configuration -*- lexical-binding: t -*-

;; Copyright (C) 2024 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging

;;; Commentary:

;; Shared distro configuration for Debian/Ubuntu packaging.
;; The target distribution is the one state shared across tools.  Each tool
;; keeps its own flags in its transient, but the distro propagates so
;; build/test/upload default to the same value.
;;
;; Tool-specific data lives in deb-packaging-commands.el.
;;
;; Here:
;;   - deb-packaging-target-distro   session distro, seeded from changelog
;;   - deb-packaging--distro-choices completion candidates

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
  "Seed `deb-packaging-target-distro' from DISTRO once.
Only when the user has not set it this session.
Returns `deb-packaging-target-distro'."
  (when (and distro
             (not (string-empty-p distro))
             (not deb-packaging--distro-user-set))
  (setq deb-packaging-target-distro distro
        deb-packaging--distro-user-set t)
  deb-packaging-target-distro))

(defun deb-packaging--set-distro (distro)
  "Set `deb-packaging-target-distro' to DISTRO deliberately.
Always overwrites, unlike `deb-packaging--maybe-seed-distro'.
Returns DISTRO."
  (setq deb-packaging-target-distro distro
        deb-packaging--distro-user-set t)
  deb-packaging-target-distro)

(defun deb-packaging--effective-distro ()
  "Return the target distro, seeding from the changelog if needed.
Uses the current value if already chosen; otherwise seeds once from the
changelog, falling back to \"noble\"."
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
When nil, the `personal' remote is not configured in prepared clones;
you'll push to salsa using your own git remote setup.")

(defvar deb-packaging-propagate-cache-dir
  (expand-file-name "deb-packaging/propagate"
                    (deb-packaging--cache-dir))
  "Directory for prepared propagate clones.
Defaults to $XDG_CACHE_HOME/deb-packaging/propagate or
~/.cache/deb-packaging/propagate.")

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
