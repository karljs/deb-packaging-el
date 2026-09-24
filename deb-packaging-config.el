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

(defgroup deb-packaging nil
  "Debian and Ubuntu packaging workflows."
  :group 'tools)

;;; Target distribution

;; One distro, from the changelog: it decides where an upload lands
;; (dpkg-genchanges copies it into the .changes Distribution field) and
;; which chroot/image a build or test uses.  There is deliberately no
;; override surface; declare another series in the changelog instead.

(defcustom deb-packaging-config-default-distro "noble"
  "Distro used outside any package tree (no changelog to read)."
  :type 'string
  :group 'deb-packaging)

(defun deb-packaging-config--effective-distro ()
  "Return the changelog distro of the package in `default-directory'.
Falls back to `deb-packaging-config-default-distro' outside a tree."
  (or (nth 2 (deb-packaging-detect--parse-changelog))
      deb-packaging-config-default-distro))

;;; Target architecture

(defcustom deb-packaging-config-default-architecture nil
  "Default target architecture, or nil to use the host architecture."
  :type '(choice (const :tag "Host architecture" nil) string)
  :group 'deb-packaging)

(defcustom deb-packaging-config-qemu-dir "/var/lib/adt-images/"
  "Directory where autopkgtest QEMU images are stored."
  :type 'directory
  :group 'deb-packaging)

(defun deb-packaging-config--architecture-valid-p (architecture)
  "Return non-nil when ARCHITECTURE is a plausible Debian architecture."
  (and (stringp architecture)
       (string-match-p "\\`[[:alnum:]][[:alnum:]-]*\\'" architecture)))

(defun deb-packaging-config--architecture-file (package distro)
  "Return the target-architecture cache file for PACKAGE and DISTRO."
  (expand-file-name
   (format "%s.%s" package distro)
   (expand-file-name "deb-packaging/architectures"
                     (deb-packaging-detect--cache-dir))))

(defun deb-packaging-config-load-architecture (package distro)
  "Return the saved target architecture for PACKAGE and DISTRO, or nil."
  (let ((file (deb-packaging-config--architecture-file package distro)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((architecture (string-trim (buffer-string))))
          (when (deb-packaging-config--architecture-valid-p architecture)
            architecture))))))

(defun deb-packaging-config-save-architecture (package distro architecture)
  "Persist target ARCHITECTURE for PACKAGE and DISTRO."
  (unless (deb-packaging-config--architecture-valid-p architecture)
    (user-error "Invalid Debian architecture: %s" architecture))
  (let ((file (deb-packaging-config--architecture-file package distro)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert architecture "\n"))))

(defun deb-packaging-config--effective-architecture (&optional context)
  "Return the active target architecture for CONTEXT or the current package."
  (when (and deb-packaging-config-default-architecture
             (not (deb-packaging-config--architecture-valid-p
                   deb-packaging-config-default-architecture)))
    (user-error "Invalid `deb-packaging-config-default-architecture': %s"
                deb-packaging-config-default-architecture))
  (let* ((name (or (plist-get context :name)
                   (deb-packaging-detect--package-name)))
         (distro (or (plist-get context :distro)
                     (deb-packaging-config--effective-distro)))
         (saved (and name distro
                     (deb-packaging-config-load-architecture name distro)))
         (host (or (plist-get context :host-arch)
                   (deb-packaging-detect--call-process-string
                    "dpkg" "--print-architecture"))))
    (or saved deb-packaging-config-default-architecture host "amd64")))

;;; Propagation

(defcustom deb-packaging-config-propagate-salsa-user nil
  "Your salsa.debian.org username, used to build the personal remote.
When nil, prepared clones get no `personal' remote."
  :type '(choice (const :tag "Not configured" nil) string)
  :group 'deb-packaging)

(defcustom deb-packaging-config-propagate-cache-dir
  (expand-file-name "deb-packaging/propagate"
                    (deb-packaging-detect--cache-dir))
  "Directory for prepared propagate clones.
Under $XDG_CACHE_HOME/deb-packaging/propagate (or ~/.cache)."
  :type 'directory
  :group 'deb-packaging)

;;; Extra PPA candidates

(defcustom deb-packaging-config-extra-ppas nil
  "List of ppa:owner/name strings for binary-build completion candidates.
Merged into the --extra-repository completion list alongside owned PPAs
and sbuild variants.  Defaults to nil; per-package persistence handles
remembering across sessions.  Set in your init file if you want certain
dependency PPAs always available as candidates."
  :type '(repeat string)
  :group 'deb-packaging)

(provide 'deb-packaging-config)
;;; deb-packaging-config.el ends here
