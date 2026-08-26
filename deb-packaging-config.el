;;; deb-packaging-config.el --- Shared configuration -*- lexical-binding: t -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Shared configuration.  The distro always comes from the changelog
;; (see `deb-packaging-config--effective-distro'); the variables below
;; are the user-tunable knobs (salsa user, propagate cache, extra PPA
;; candidates).

;;; Code:

(require 'subr-x)
(require 'deb-packaging-detect)

;;; Target distribution

;; One distro, from the changelog: it decides where an upload lands
;; (dpkg-genchanges copies it into the .changes Distribution field) and
;; which chroot/image a build or test uses.  There is deliberately no
;; override surface; declare another series in the changelog instead.

(defvar deb-packaging-config-default-distro "noble"
  "Distro used outside any package tree (no changelog to read).")

(defun deb-packaging-config--effective-distro ()
  "Return the changelog distro of the package in `default-directory'.
Falls back to `deb-packaging-config-default-distro' outside a tree."
  (or (nth 2 (deb-packaging-detect--parse-changelog))
      deb-packaging-config-default-distro))

;;; Propagation

(defvar deb-packaging-config-propagate-salsa-user nil
  "Your salsa.debian.org username, used to build the personal remote.
When nil, prepared clones get no `personal' remote.")

(defvar deb-packaging-config-propagate-cache-dir
  (expand-file-name "deb-packaging/propagate"
                    (deb-packaging-detect--cache-dir))
  "Directory for prepared propagate clones.
Under $XDG_CACHE_HOME/deb-packaging/propagate (or ~/.cache).")

;;; Extra PPA candidates

(defvar deb-packaging-config-extra-ppas nil
  "List of ppa:owner/name strings for binary-build completion candidates.
Merged into the --extra-repository completion list alongside owned PPAs
and sbuild variants.  Defaults to nil; per-package persistence handles
remembering across sessions.  Set in your init file if you want certain
dependency PPAs always available as candidates.")

(provide 'deb-packaging-config)
;;; deb-packaging-config.el ends here
