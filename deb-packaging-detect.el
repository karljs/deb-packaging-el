;;; deb-packaging-detect.el --- Source detection -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging

;;; Commentary:

;; Detection utilities: find the package dir, parse debian/changelog,
;; scan build artifacts.

;;; Code:

(require 'cl-lib)

;;; Package Directory Detection

(defun deb-packaging--find-package-dir (&optional start-dir host-only)
  "Find directory containing debian/changelog, walking up from START-DIR.
With HOST-ONLY, error on TRAMP paths so host commands stay off containers."
  (let ((dir (locate-dominating-file (or start-dir default-directory)
                                     "debian/changelog")))
    (when dir
      (let ((expanded (expand-file-name dir)))
        (when (and host-only (file-remote-p expanded))
          (user-error
           "This command runs on the host, but the current file is inside a dev container.  Run it from the status buffer (M-x deb-packaging-status) or a host file."))
        expanded))))

;;; Shared helpers

(defun deb-packaging--parent-dir (pkg-dir)
  "Return the build-output directory (parent of PKG-DIR)."
  (file-name-directory (directory-file-name pkg-dir)))

(defun deb-packaging--package-info (&optional pkg-dir)
  "Return (NAME VERSION) for PKG-DIR, or nil outside a package tree."
  (when-let* ((info (deb-packaging--parse-changelog pkg-dir)))
    (list (nth 0 info) (nth 1 info))))

(defun deb-packaging--package-name (&optional pkg-dir)
  "Return the source package name for PKG-DIR, or nil."
  (car (deb-packaging--package-info pkg-dir)))

(defun deb-packaging--package-version (&optional pkg-dir)
  "Return the full version string for PKG-DIR, or nil."
  (cadr (deb-packaging--package-info pkg-dir)))

(defun deb-packaging--call-process-string (program &rest args)
  "Run PROGRAM with ARGS, returning trimmed stdout, or nil if empty."
  (let ((output (with-output-to-string
                  (with-current-buffer standard-output
                    (apply #'call-process program nil t nil args)))))
    (unless (string-empty-p output)
      (string-trim output))))

(defun deb-packaging--cache-dir ()
  "Return the base cache directory, honoring $XDG_CACHE_HOME."
  (or (and (getenv "XDG_CACHE_HOME")
           (expand-file-name (getenv "XDG_CACHE_HOME")))
      (expand-file-name "~/.cache")))

;;; Changelog Parsing

(defun deb-packaging--parse-changelog (&optional dir)
  "Parse debian/changelog in DIR. Return (name version distro)."
  (let* ((pkg-dir (or dir (deb-packaging--find-package-dir)))
         (changelog (when pkg-dir
                      (expand-file-name "debian/changelog" pkg-dir))))
    (when (and changelog (file-readable-p changelog))
      (with-temp-buffer
        (insert-file-contents changelog)
        (goto-char (point-min))
        (when (looking-at "^\\([^ ]+\\) (\\([^)]+\\)) \\([^;]+\\);")
          (list (match-string 1)
                (match-string 2)
                (string-trim (match-string 3))))))))

;;; Source metadata

(defun deb-packaging--source-format (&optional pkg-dir)
  "Return the source format string for PKG-DIR from `debian/source/format'.
Returns nil if the file is absent."
  (let* ((dir (or pkg-dir (deb-packaging--find-package-dir)))
         (format-file (when dir
                        (expand-file-name "debian/source/format" dir))))
    (when (and format-file (file-readable-p format-file))
      (with-temp-buffer
        (insert-file-contents format-file)
        (goto-char (point-min))
        (when (re-search-forward "^[ \t]*\\(.+\\)$" nil t)
          (string-trim (match-string 1)))))))

;;; Patches and VCS metadata

(defun deb-packaging--list-patches ()
  "Return an alist of (NAME . ABSOLUTE-PATH) for patches in the series file.
Skips comments, blanks, and quilt options.  Returns nil if series absent."
  (when-let* ((pkg-dir (deb-packaging--find-package-dir))
              (series (expand-file-name "debian/patches/series" pkg-dir)))
    (when (file-readable-p series)
      (with-temp-buffer
        (insert-file-contents series)
        (goto-char (point-min))
        (let (patches)
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
              ;; Strip trailing quilt options.
              (when (string-match "^\\([^# \t][^ \t]*\\)" line)
                (let* ((name (match-string 1 line))
                       (path (expand-file-name
                              (concat "debian/patches/" name) pkg-dir)))
                  (when (file-readable-p path)
                    (push (cons name path) patches)))))
            (forward-line 1))
          (nreverse patches))))))

(defun deb-packaging--vcs-git (&optional pkg-dir)
  "Return the Vcs-Git URL for PKG-DIR, sans trailing `-b BRANCH'.
Returns nil if debian/control has no Vcs-Git field."
  (let ((value (deb-packaging--control-field "Vcs-Git" pkg-dir)))
    (when value
      (string-trim
       (replace-regexp-in-string "\\s-+-b\\s-+\\S-+$" "" value)))))

(defun deb-packaging--upstream-url (&optional pkg-dir)
  "Return a best-effort upstream repo URL for PKG-DIR, or nil.
Prefers Homepage from debian/control; else a GitHub/GitLab URL in
debian/watch."
  (let* ((dir (or pkg-dir (deb-packaging--find-package-dir)))
         (homepage (deb-packaging--control-field "Homepage" dir)))
    (cond
     ((and homepage
           (string-match-p "\\`https?://\\(github\\.com\\|gitlab\\.com\\)/"
                           homepage))
      homepage)
     ((and homepage (not (string-empty-p homepage)))
      homepage)
     (t
      (when-let* ((dir)
                  (watch (expand-file-name "debian/watch" dir)))
        (when (file-readable-p watch)
          (with-temp-buffer
            (insert-file-contents watch)
            (goto-char (point-min))
            (when (re-search-forward
                   "https?://\\(?:gitlab\\.com\\|github\\.com\\)/[^/ \t]+/[^/ \t]+"
                   nil t)
              (match-string 0)))))))))

(defun deb-packaging--orig-tarball (name version parent-dir)
  "Return the .orig.tar.* path matching `NAME_UPSTREAM' in PARENT-DIR, or nil."
  (let* ((upstream (deb-packaging--upstream-version version))
         (prefix (format "%s_%s.orig.tar." name upstream)))
    (when (and name upstream (file-directory-p parent-dir))
      (cl-some
       (lambda (file)
         (when (string-prefix-p prefix file)
           (expand-file-name file parent-dir)))
       (directory-files parent-dir nil (regexp-quote prefix))))))

(defun deb-packaging--control-field (field &optional pkg-dir)
  "Return FIELD's trimmed value from debian/control in PKG-DIR, or nil."
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
  (deb-packaging--call-process-string "dpkg" "--print-architecture"))

(defun deb-packaging--schroot-exists-p (distro arch)
  "Return the schroot name matching DISTRO and ARCH, or nil.
Name (not just a boolean) so callers can reuse it."
  (when (and distro arch)
    (let ((output (deb-packaging--call-process-string "schroot" "-l"))
          (target (format "%s-%s" distro arch)))
      (cl-some
       (lambda (line)
         (when (string-match-p (regexp-quote target) line)
           (string-trim (replace-regexp-in-string ":.*$" "" line))))
       (split-string (or output "") "\n" t)))))

;;; Artifact Scanning

(defun deb-packaging--version-to-filename (version)
  "Convert VERSION to filename form, stripping any epoch prefix."
  (replace-regexp-in-string "^[0-9]+:" "" version))

(defun deb-packaging--upstream-version (version)
  "Return the upstream portion of VERSION, as used in .orig.tar.* names.
Strips epoch and Debian revision.  Native packages return VERSION."
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
    ;; debs are only discoverable via the binary .changes.
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
  "Return binary package names from debian/control in PKG-DIR, or nil.
Template variables are left as-is."
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
  "Return artifact name prefixes owned by this source.
Source name, binary names, and their -dbgsym/-dbg variants.  Falls back
to the source name if debian/control is missing."
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
  "Extract the version field from packaging FILENAME, or nil.
Filenames are NAME_VERSION_ARCH.ext or NAME_VERSION.ext.  Returns nil
for .orig.tar.* files."
  (cond
   ((string-match "\\.orig\\.tar\\." filename) nil)
   ((string-match "_\\([^_]+\\)_" filename) (match-string 1 filename))
   (t
    (let ((stripped (replace-regexp-in-string
                     "\\.\\(dsc\\|changes\\|u?deb\\|ddeb\\|buildinfo\\|upload\\|debian\\.tar\\.[a-z0-9]+\\|tar\\.[a-z0-9]+\\)$"
                     "" filename)))
      (if (string-match "_\\([^_]+\\)$" stripped)
          (match-string 1 stripped)
        nil)))))

(defun deb-packaging--scan-stale-artifacts (name version dir &optional pkg-dir)
  "Return sorted basenames in DIR owned by NAME but not matching VERSION.
PKG-DIR supplies binary package names; otherwise only NAME is used.
Matches packaging extensions (dsc, changes, deb, udeb, ddeb, buildinfo,
upload, tar.*, orig.tar.*)."
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
               ;; Orig tarballs embed the upstream version, not the full one.
               (when (string-match "_\\([^_]+\\)\\.orig\\.tar\\." file)
                 (unless (string= (match-string 1 file) upstream-version)
                   (push file stale)))
             (let ((fv (deb-packaging--filename-version file)))
               (when (and fv (not (string= fv file-version)))
                 (push file stale))))))
      (sort (delete-dups stale) #'string<))))

;;; Unified context scan
;;
;; Single source of truth for the status buffer and dispatch transient.
;; Re-reads the filesystem/changelog on each call, no caching or mutation.

(defun deb-packaging--scan-context (&optional start-dir)
  "Return a fresh context plist for the package containing START-DIR.
START-DIR defaults to `default-directory'.  Returns nil outside a
package tree.  Keys:

  :name          source package name
  :version       full version string
  :distro        target distribution
  :pkg-dir       directory containing debian/changelog
  :parent-dir    build-output directory
  :artifacts     alist from `deb-packaging--scan-artifacts'
  :stale         list from `deb-packaging--scan-stale-artifacts'
  :source-format source format string, or nil
  :orig-tarball  .orig.tar.* path, or nil
  :arch          build architecture string
  :maintainer    Maintainer field, or nil"
  (when-let* ((pkg-dir (deb-packaging--find-package-dir start-dir))
              (info (deb-packaging--parse-changelog pkg-dir)))
    (let* ((name (nth 0 info))
           (version (nth 1 info))
           (distro (nth 2 info))
           (parent-dir (deb-packaging--parent-dir pkg-dir))
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
