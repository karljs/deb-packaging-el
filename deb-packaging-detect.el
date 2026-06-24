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

(defun deb-packaging--upstream-version (version)
  "Return the upstream version portion of VERSION.
Strips any epoch (everything before the first colon) and the Debian
revision (everything after the last hyphen).  For native packages
\(no hyphen) the whole version is the upstream version.

This is the version embedded in .orig.tar.* filenames, which carry
only the upstream version rather than the full Debian version."
  (let ((file-version (deb-packaging--version-to-filename version)))
    (if (string-match "\\(.*\\)-[^-]+$" file-version)
        (match-string 1 file-version)
      file-version)))

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

(defun deb-packaging--scan-stale-artifacts (name version dir)
  "Scan DIR for artifacts of package NAME from versions other than VERSION.
The build output directory (the parent of the source tree) is typically
shared across packages and across versions of the same package.  This
returns a sorted list of basenames in DIR that belong to NAME (anchored on
\"NAME_\") but do not match the current VERSION, so callers can warn about
leftover artifacts without ever matching sibling packages.

Only well-known packaging extensions are considered \(dsc, changes, deb,
ddeb, udeb, buildinfo, tar.* and their compression suffixes)."
  (when (and name version (file-directory-p dir))
    (let* ((file-version (deb-packaging--version-to-filename version))
           ;; Orig tarballs embed the upstream version, not the full Debian
           ;; version (e.g. foo_1.0.orig.tar.gz for version 1.0-1ubuntu1).
           (upstream-version (deb-packaging--upstream-version version))
           ;; Anchor on "NAME_" so sibling packages can never match.
           (name-pattern (format "^%s_" (regexp-quote name)))
           ;; Current version's filename prefix; anything starting with this
           ;; belongs to the version we are actively working on.
           (current-prefix (format "%s_%s" name file-version))
           ;; Upstream prefix for .orig.tar.* files.
           (upstream-prefix (format "%s_%s" name upstream-version))
           (orig-pattern "\\.orig\\.tar\\.[a-z0-9]+$")
           (ext-pattern
            "\\.\\(dsc\\|changes\\|u?deb\\|ddeb\\|buildinfo\\)$\\|\\.tar\\.[a-z0-9]+$\\|\\.orig\\.tar\\.[a-z0-9]+$")
           (stale nil))
      (dolist (file (directory-files dir nil name-pattern))
        (when (and (string-match-p ext-pattern file)
                   (not (string-prefix-p current-prefix file))
                   ;; Orig tarballs use the upstream version, not the full
                   ;; version, so match them against the upstream prefix.
                   (not (and (string-match-p orig-pattern file)
                             (string-prefix-p upstream-prefix file))))
          (push file stale)))
      (sort (delete-dups stale) #'string<))))

;;; Unified context scan
;;
;; A single, side-effect-free entry point that gathers everything callers need
;; to describe the current package: identity, the two relevant directories, the
;; current artifacts and any stale artifacts from other versions.  This is the
;; one source of truth shared by the status buffer and the dispatch transient;
;; it always re-reads the filesystem and changelog and NEVER mutates user
;; settings (e.g. `deb-packaging-target-distro').  Seeding settings from the
;; changelog is the caller's responsibility (see `deb-packaging--maybe-seed-distro').

(defun deb-packaging--scan-context (&optional start-dir)
  "Return a fresh context plist for the package containing START-DIR.
START-DIR defaults to `default-directory'.  Returns nil when not inside a
Debian package tree.  The plist keys are:

  :name        source package name (from debian/changelog)
  :version     full version string, including any epoch
  :distro      target distribution from the changelog
  :pkg-dir     directory containing debian/changelog (where commands run)
  :parent-dir  parent of PKG-DIR (the shared build-output directory)
  :artifacts   alist from `deb-packaging--scan-artifacts'
  :stale       list of basenames from `deb-packaging--scan-stale-artifacts'

This function performs no caching and has no side effects."
  (when-let* ((pkg-dir (deb-packaging--find-package-dir start-dir))
              (info (deb-packaging--parse-changelog pkg-dir)))
    (let* ((name (nth 0 info))
           (version (nth 1 info))
           (distro (nth 2 info))
           (parent-dir (file-name-directory (directory-file-name pkg-dir)))
           (artifacts (deb-packaging--scan-artifacts name version parent-dir))
           (stale (deb-packaging--scan-stale-artifacts name version parent-dir)))
      (list :name name
            :version version
            :distro distro
            :pkg-dir pkg-dir
            :parent-dir parent-dir
            :artifacts artifacts
            :stale stale))))

(provide 'deb-packaging-detect)
;;; deb-packaging-detect.el ends here
