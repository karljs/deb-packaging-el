;;; deb-packaging-detect.el --- Detection for Debian package sources -*- lexical-binding: t; -*-

;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/example/deb-packaging
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Detection utilities for Debian/Ubuntu package maintenance.
;; Finds package source directories, parses debian/changelog,
;; and scans for build artifacts.

;;; Code:

(require 'cl-lib)

;;; Package Directory Detection

(defun deb-packaging--find-package-dir (&optional start-dir)
  "Find directory containing debian/changelog, walking up from START-DIR."
  (let ((dir (locate-dominating-file (or start-dir default-directory)
                                     "debian/changelog")))
    (when dir
      (expand-file-name dir))))

;;; Changelog Parsing

(defun deb-packaging--parse-changelog (&optional dir)
  "Parse debian/changelog in DIR. Return (name version distro)."
  (let* ((pkg-dir (or dir (deb-packaging--find-package-dir)))
         (changelog (when pkg-dir
                      (expand-file-name "debian/changelog" pkg-dir))))
    (when (and changelog (file-readable-p changelog))
      (with-temp-buffer
        (insert-file-contents changelog nil 0 1024)
        (goto-char (point-min))
        (when (looking-at "^\\([^ ]+\\) (\\([^)]+\\)) \\([^;]+\\);")
          (list (match-string 1)
                (match-string 2)
                (string-trim (match-string 3))))))))

;;; Artifact Scanning

(defun deb-packaging--version-to-filename (version)
  "Convert VERSION to filename format. Strips epoch (everything before colon)."
  (replace-regexp-in-string "^[0-9]+:" "" version))

(defun deb-packaging--parse-changes-file (changes-file)
  "Parse CHANGES-FILE and return list of files it references."
  (when (file-readable-p changes-file)
    (with-temp-buffer
      (insert-file-contents changes-file)
      (let ((files nil)
            (in-files nil))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (cond
             ((string-match "^Files:" line)
              (setq in-files t))
             ((and in-files (string-match "^ [a-f0-9]+ [0-9]+ \\S-+ \\S-+ \\(\\S-+\\)$" line))
              (push (match-string 1 line) files))
             ((and in-files (not (string-match "^ " line)))
              (setq in-files nil))))
          (forward-line 1))
        (nreverse files)))))

(defun deb-packaging--scan-artifacts (name version dir)
  "Scan DIR for artifacts matching NAME and VERSION.
Return alist with keys: dsc, source-changes, binary-changes, debs, buildinfo."
  (let* ((file-version (deb-packaging--version-to-filename version))
         (base-pattern (format "^%s_%s" (regexp-quote name) (regexp-quote file-version)))
         (files (directory-files dir nil base-pattern))
         (dsc nil)
         (source-changes nil)
         (binary-changes nil)
         (debs nil)
         (buildinfo nil))
    ;; First pass: find .changes and .dsc files by source package name
    (dolist (file files)
      (cond
       ((string-match "\\.dsc$" file)
        (setq dsc (expand-file-name file dir)))
       ((string-match "_source\\.changes$" file)
        (setq source-changes (expand-file-name file dir)))
       ((string-match "\\.changes$" file)
        (push (expand-file-name file dir) binary-changes))
       ((string-match "_source\\.buildinfo$" file)
        (push (expand-file-name file dir) buildinfo))))
    ;; Second pass: parse binary .changes to find debs (which may have different names)
    (dolist (changes-file binary-changes)
      (dolist (referenced (deb-packaging--parse-changes-file changes-file))
        (let ((full-path (expand-file-name referenced dir)))
          (when (file-exists-p full-path)
            (cond
             ((string-match "\\.deb$" referenced)
              (push full-path debs))
             ((string-match "\\.buildinfo$" referenced)
              (unless (member full-path buildinfo)
                (push full-path buildinfo))))))))
    `((dsc . ,dsc)
      (source-changes . ,source-changes)
      (binary-changes . ,(nreverse binary-changes))
      (debs . ,(nreverse debs))
      (buildinfo . ,(nreverse buildinfo)))))

(provide 'deb-packaging-detect)
;;; deb-packaging-detect.el ends here
