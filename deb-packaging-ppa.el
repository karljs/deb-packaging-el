;;; deb-packaging-ppa.el --- Per-package upload PPA persistence -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Plain-text store for the upload PPA last used for a source package and
;; distro.  One file per (package . distro) under the cache dir, one
;; ppa:owner/name line.  Loaded to seed the upload and test transients;
;; saved on upload/test dispatch.

;;; Code:

(require 'subr-x)
(require 'deb-packaging-detect)

(defun deb-packaging-ppa--file (package distro)
  "Return the cache file path for PACKAGE and DISTRO."
  (expand-file-name
   (format "%s.%s" package distro)
   (expand-file-name "deb-packaging/ppas"
                     (deb-packaging-detect--cache-dir))))

(defun deb-packaging-ppa-load (package distro)
  "Return the saved PPA for PACKAGE and DISTRO, or nil."
  (let ((file (deb-packaging-ppa--file package distro)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (unless (string-empty-p line)
            line))))))

(defun deb-packaging-ppa-save (package distro ppa)
  "Write PPA for PACKAGE and DISTRO to the cache.
Creates the parent directory if needed."
  (let ((file (deb-packaging-ppa--file package distro)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert ppa "\n"))))

(provide 'deb-packaging-ppa)
;;; deb-packaging-ppa.el ends here
