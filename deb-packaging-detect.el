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

(defun deb-packaging--find-package-dir (&optional start-dir host-only)
  "Find directory containing debian/changelog, walking up from START-DIR.
When HOST-ONLY is non-nil, reject TRAMP paths (e.g. /lxc: container
paths) so host-side commands don't accidentally run inside a dev
container.  Signals `user-error' in that case."
  (let ((dir (locate-dominating-file (or start-dir default-directory)
                                     "debian/changelog")))
    (when dir
      (let ((expanded (expand-file-name dir)))
        (when (and host-only (file-remote-p expanded))
          (user-error
           "This command runs on the host, but the current file is inside a dev container.  Run it from the status buffer (M-x deb-packaging-status) or a host file."))
        expanded))))

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

;;; Source metadata

(defun deb-packaging--source-format (&optional pkg-dir)
  "Return the Debian source format string for the package in PKG-DIR.
Reads `debian/source/format'.  Returns a string like \"3.0 (quilt)\"
or \"3.0 (native)\", or nil when the file is absent or unreadable."
  (let* ((dir (or pkg-dir (deb-packaging--find-package-dir)))
         (format-file (when dir
                        (expand-file-name "debian/source/format" dir))))
    (when (and format-file (file-readable-p format-file))
      (with-temp-buffer
        (insert-file-contents format-file)
        (goto-char (point-min))
        (when (re-search-forward "^[ \t]*\\(.+\\)$" nil t)
          (string-trim (match-string 1)))))))

(defun deb-packaging--orig-tarball (name version parent-dir)
  "Return the path to the .orig.tar.* file for NAME/VERSION in PARENT-DIR.
Uses the upstream version (epoch and Debian revision stripped) to match
`NAME_UPSTREAM.orig.tar.*'.  Returns the expanded path or nil."
  (let* ((upstream (deb-packaging--upstream-version version))
         (prefix (format "%s_%s.orig.tar." name upstream)))
    (when (and name upstream (file-directory-p parent-dir))
      (cl-some
       (lambda (file)
         (when (string-prefix-p prefix file)
           (expand-file-name file parent-dir)))
       (directory-files parent-dir nil (regexp-quote prefix))))))

(defun deb-packaging--control-field (field &optional pkg-dir)
  "Return the value of FIELD from debian/control in PKG-DIR.
FIELD is a string like \"Maintainer\" or \"Architecture\".  Returns
the first matching field's value (trimmed), or nil."
  (let* ((dir (or pkg-dir (deb-packaging--find-package-dir)))
         (control (when dir
                    (expand-file-name "debian/control" dir))))
    (when (and control (file-readable-p control))
      (with-temp-buffer
        (insert-file-contents control)
        (goto-char (point-min))
        (when (re-search-forward
               (format "^%s:\\s-*\\(.+\\)$" (regexp-quote field)) nil t)
          (string-trim (match-string 1)))))))

(defun deb-packaging--architecture ()
  "Return the build architecture string (e.g. \"amd64\")."
  (string-trim
   (with-output-to-string
     (with-current-buffer standard-output
       (call-process "dpkg" nil t nil "--print-architecture")))))

(defun deb-packaging--schroot-exists-p (distro arch)
  "Return non-nil if a schroot for DISTRO-ARCH exists.
Looks for a schroot whose name starts with DISTRO and contains ARCH."
  (when (and distro arch)
    (let ((output (with-output-to-string
                    (with-current-buffer standard-output
                      (call-process "schroot" nil t nil "-l"))))
          (target (format "%s-%s" distro arch)))
      (cl-some
       (lambda (line)
         (when (string-match-p (regexp-quote target) line)
           (string-trim (replace-regexp-in-string ":.*$" "" line))))
       (split-string output "\n" t)))))

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

(defun deb-packaging--binary-package-names (&optional pkg-dir)
  "Return the list of binary package names declared in debian/control.
PKG-DIR is the source package directory (defaults to the detected one).
Returns nil if debian/control is absent or unreadable.  Each entry is the
value of a `Package:' field, with template variables like @LLVM_VERSION@
left as-is (they are irrelevant for prefix matching)."
  (let* ((dir (or pkg-dir (deb-packaging--find-package-dir)))
         (control (when dir
                    (expand-file-name "debian/control" dir))))
    (when (and control (file-readable-p control))
      (with-temp-buffer
        (insert-file-contents control)
        (let (names)
          (goto-char (point-min))
          (while (re-search-forward "^Package:\\s-*\\(.+\\)$" nil t)
            (push (string-trim (match-string 1)) names))
          (nreverse names))))))

(defun deb-packaging--owned-package-prefixes (&optional pkg-dir)
  "Return a list of name prefixes whose artifacts belong to this source.
Includes the source package name, every binary package name from
debian/control, and -dbgsym/-dbg variants of each binary package.
When debian/control is unavailable, falls back to just the source name."
  (let* ((dir (or pkg-dir (deb-packaging--find-package-dir)))
         (info (when dir (deb-packaging--parse-changelog dir)))
         (source-name (nth 0 info))
         (bin-names (deb-packaging--binary-package-names dir))
         (all-names (cons source-name bin-names)))
    (cl-remove-duplicates
     (apply #'nconc
            (mapcar (lambda (n)
                      (list n
                            (concat n "-dbgsym")
                            (concat n "-dbg")))
                    all-names))
     :test #'equal)))

(defun deb-packaging--filename-version (filename)
  "Extract the version field from a packaging FILENAME.
Debian filenames follow NAME_VERSION_ARCH.ext or NAME_VERSION.ext
patterns.  Returns the version string (the second underscore-delimited
field), or nil if it cannot be extracted.  Handles versions containing
dots and hyphens correctly by stripping known packaging suffixes first.
Returns nil for .orig.tar.* files (handled separately by the caller)."
  (cond
   ((string-match "\\.orig\\.tar\\." filename) nil)
   ;; Three-field: NAME_VERSION_ARCH.ext — version is between the two _s
   ((string-match "_\\([^_]+\\)_" filename) (match-string 1 filename))
   ;; Two-field: NAME_VERSION.ext — strip known extensions, then extract
   (t
    (let ((stripped (replace-regexp-in-string
                     "\\.\\(dsc\\|changes\\|u?deb\\|ddeb\\|buildinfo\\|upload\\|debian\\.tar\\.[a-z0-9]+\\|tar\\.[a-z0-9]+\\)$"
                     "" filename)))
      (if (string-match "_\\([^_]+\\)$" stripped)
          (match-string 1 stripped)
        nil)))))

(defun deb-packaging--scan-stale-artifacts (name version dir &optional pkg-dir)
  "Scan DIR for artifacts of package NAME from versions other than VERSION.
The build output directory (the parent of the source tree) is typically
shared across packages and across versions of the same package.  This
returns a sorted list of basenames in DIR that belong to this source
package (including all its binary packages as declared in debian/control)
but do not match the current VERSION, so callers can warn about leftover
artifacts without ever matching sibling source packages.

When PKG-DIR is provided, debian/control is read from it to build the
full list of owned binary package names; otherwise only the source name
is used.  Well-known packaging extensions are matched (dsc, changes,
deb, udeb, ddeb, buildinfo, upload, tar.* and orig.tar.*)."
  (when (and name version (file-directory-p dir))
    (let* ((file-version (deb-packaging--version-to-filename version))
           (upstream-version (deb-packaging--upstream-version version))
           (orig-pattern "\\.orig\\.tar\\.[a-z0-9]+$")
           (ext-pattern
            "\\.\\(dsc\\|changes\\|u?deb\\|ddeb\\|buildinfo\\|upload\\)$\\|\\.tar\\.[a-z0-9]+$\\|\\.orig\\.tar\\.[a-z0-9]+$")
           (prefixes (or (deb-packaging--owned-package-prefixes pkg-dir)
                         (list name)))
           (prefix-regex
            (mapconcat (lambda (p) (concat "^" (regexp-quote p) "_"))
                       prefixes "\\|"))
           (stale nil))
       (dolist (file (directory-files dir nil prefix-regex))
         (when (string-match-p ext-pattern file)
           (if (string-match-p orig-pattern file)
               ;; Orig tarballs use the upstream version, embedded as
               ;; NAME_UPSTREAM.orig.tar.*.  Extract the version between
               ;; the first _ and .orig, and compare to upstream-version.
               (when (string-match "_\\([^_]+\\)\\.orig\\.tar\\." file)
                 (unless (string= (match-string 1 file) upstream-version)
                   (push file stale)))
             ;; Regular artifact: extract version, compare to current.
             (let ((fv (deb-packaging--filename-version file)))
               (when (and fv (not (string= fv file-version)))
                 (push file stale))))))
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

  :name          source package name (from debian/changelog)
  :version       full version string, including any epoch
  :distro        target distribution from the changelog
  :pkg-dir       directory containing debian/changelog (where commands run)
  :parent-dir    parent of PKG-DIR (the shared build-output directory)
  :artifacts     alist from `deb-packaging--scan-artifacts'
  :stale         list of basenames from `deb-packaging--scan-stale-artifacts'
  :source-format source format string (e.g. \"3.0 (quilt)\"), or nil
  :orig-tarball  path to the .orig.tar.* file, or nil
  :arch          build architecture string (e.g. \"amd64\")
  :maintainer    Maintainer field from debian/control, or nil

This function performs no caching and has no side effects."
  (when-let* ((pkg-dir (deb-packaging--find-package-dir start-dir))
              (info (deb-packaging--parse-changelog pkg-dir)))
    (let* ((name (nth 0 info))
           (version (nth 1 info))
           (distro (nth 2 info))
           (parent-dir (file-name-directory (directory-file-name pkg-dir)))
           (artifacts (deb-packaging--scan-artifacts name version parent-dir))
           (stale (deb-packaging--scan-stale-artifacts name version parent-dir pkg-dir)))
      (list :name name
            :version version
            :distro distro
            :pkg-dir pkg-dir
            :parent-dir parent-dir
            :artifacts artifacts
            :stale stale
            :source-format (deb-packaging--source-format pkg-dir)
            :orig-tarball (deb-packaging--orig-tarball name version parent-dir)
            :arch (deb-packaging--architecture)
            :maintainer (deb-packaging--control-field "Maintainer" pkg-dir)))))

(provide 'deb-packaging-detect)
;;; deb-packaging-detect.el ends here
