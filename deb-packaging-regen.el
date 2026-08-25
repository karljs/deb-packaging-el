;;; deb-packaging-regen.el --- Per-package template regeneration command -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Plain-text store for the shell command that regenerates templated
;; files (e.g. debian/control.in -> debian/control) for a source package
;; and distro.  One file per (package . distro) under the cache dir, one
;; command line.  The command is stored and run verbatim, so env vars,
;; make targets, and multi-step chains all work without configuration.

;;; Code:

(require 'subr-x)
(require 'deb-packaging-detect)

(defun deb-packaging-regen--file (package distro)
  "Return the cache file path for PACKAGE and DISTRO."
  (expand-file-name
   (format "%s.%s" package distro)
   (expand-file-name "deb-packaging/regen"
                     (deb-packaging-detect--cache-dir))))

(defun deb-packaging-regen-load (package distro)
  "Return the saved regen command for PACKAGE and DISTRO, or nil."
  (let ((file (deb-packaging-regen--file package distro)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (unless (string-empty-p line)
            line))))))

(defun deb-packaging-regen-save (package distro command)
  "Write COMMAND for PACKAGE and DISTRO to the cache.
Creates the parent directory if needed."
  (let ((file (deb-packaging-regen--file package distro)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert command "\n"))))

(defun deb-packaging-regen--default-command (pkg-dir)
  "Return a sane default regen command for PKG-DIR, or nil.
Prefills the first prompt only; the stored command wins thereafter."
  (when (file-exists-p (expand-file-name "debian/control.in" pkg-dir))
    "make -f debian/rules control"))

(provide 'deb-packaging-regen)
;;; deb-packaging-regen.el ends here
