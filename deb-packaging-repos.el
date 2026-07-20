;;; deb-packaging-repos.el --- Per-package extra-repository persistence -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Plain-text store for the set of extra-repository entries (variant names,
;; ppa: addresses, raw deb lines) selected for a source package and distro.
;; One file per (package . distro) under the cache dir, one entry per line.
;; Loaded to seed the binary-build transient; saved on build dispatch.

;;; Code:

(require 'subr-x)
(require 'deb-packaging-detect)

(defun deb-packaging-repos--file (package distro)
  "Return the cache file path for PACKAGE and DISTRO."
  (expand-file-name
   (format "%s.%s" package distro)
   (expand-file-name "deb-packaging/extra-repos"
                     (deb-packaging-detect--cache-dir))))

(defun deb-packaging-repos-load (package distro)
  "Return saved extra-repo entries for PACKAGE and DISTRO, or nil.
Entries are pre-expansion values (variant names, ppa: addresses, raw deb
lines).  Returns nil if the file is missing or empty."
  (let ((file (deb-packaging-repos--file package distro)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let (entries)
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
              (unless (string-empty-p line)
                (push line entries)))
            (forward-line 1))
          (nreverse entries))))))

(defun deb-packaging-repos-save (package distro entries)
  "Write ENTRIES (a list of strings) for PACKAGE and DISTRO to the cache.
Empty list writes an empty file so a cleared set sticks.  Creates the
parent directory if needed."
  (let ((file (deb-packaging-repos--file package distro)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (when entries
        (insert (mapconcat #'identity entries "\n"))
        (insert "\n")))))

(provide 'deb-packaging-repos)
;;; deb-packaging-repos.el ends here
