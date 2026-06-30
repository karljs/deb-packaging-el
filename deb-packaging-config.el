;;; deb-packaging-config.el --- Shared configuration for deb-packaging -*- lexical-binding: t -*-

;; Copyright (C) 2024 Karl Smeltzer
;; Author: Karl Smeltzer
;; Keywords: tools, debian, ubuntu, packaging
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Shared distro configuration for Debian/Ubuntu packaging.
;;
;; This module holds the one piece of state that is genuinely cross-cutting
;; across the per-tool transients: the target distribution.  Each tool carries
;; its own flags in its own transient (see deb-packaging-transients.el), with
;; persistence provided by transient's native save mechanism (C-x C-s), but the
;; distro is propagated across tools so that building for `jammy' makes testing
;; and uploading default to `jammy' too.  It is surfaced globally in the
;; `deb-packaging-dispatch' hub (see deb-packaging.el).
;;
;; Tool-specific data tables (sbuild extra-repo variants, autopkgtest runner
;; image templates and build hints) live with their tools in
;; deb-packaging-commands.el, not here.
;;
;; What lives here:
;;   - deb-packaging-target-distro  — session distro, seeded once from changelog
;;   - deb-packaging--distro-choices — completion candidates for the distro

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'deb-packaging-detect)

;;; Target distribution

(defvar deb-packaging-target-distro "noble"
  "Target distribution for builds and tests.
Seeded once from the changelog (see `deb-packaging--maybe-seed-distro')
and never silently overwritten, so a value set interactively or via
.dir-locals.el is respected.")

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
        deb-packaging--distro-user-set t)
  deb-packaging-target-distro))

(defun deb-packaging--set-distro (distro)
  "Set `deb-packaging-target-distro' to DISTRO as a deliberate choice.
Unlike `deb-packaging--maybe-seed-distro', this always overwrites,
reflecting an explicit selection from a transient.  Returns the
resulting distro."
  (setq deb-packaging-target-distro distro
        deb-packaging--distro-user-set t)
  deb-packaging-target-distro)

(defun deb-packaging--effective-distro ()
  "Return the target distro, seeding from the changelog if needed.
Uses `deb-packaging-target-distro' if the user has already chosen one;
otherwise seeds it once from the current package's changelog and falls
back to \"noble\" when not inside a package tree."
  (when-let ((distro (plist-get (deb-packaging--scan-context) :distro)))
    (deb-packaging--maybe-seed-distro distro))
  deb-packaging-target-distro)

;;; Distribution choices

(defconst deb-packaging-ubuntu-distros
  '("focal" "jammy" "noble" "oracular" "plucky" "questing")
  "Known Ubuntu distribution codenames, alphabetical.")

(defconst deb-packaging-debian-distros
  '("sid" "stable" "testing")
  "Known Debian distribution names, alphabetical.")

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
