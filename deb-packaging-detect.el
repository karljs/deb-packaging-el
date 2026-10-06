;;; deb-packaging-detect.el --- Source detection -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Detection utilities: find the package dir, parse debian/changelog,
;; scan build artifacts.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magit)

;;; Package Directory Detection

(defun deb-packaging-detect--find-package-dir (&optional start-dir host-only)
  "Find directory containing debian/changelog, walking up from START-DIR.
With HOST-ONLY, error on TRAMP paths so host commands stay off containers.
The remote check comes first: `locate-dominating-file' on a TRAMP path
can open a connection before it ever fails."
  (let ((start (or start-dir default-directory)))
    (when (and host-only (file-remote-p start))
      (user-error
       "This command runs on the host, but the current file is inside a dev container.  Run it from the status buffer (M-x deb-packaging-status) or a host file."))
    (let ((dir (locate-dominating-file start "debian/changelog")))
      (when dir
        (expand-file-name dir)))))

(defun deb-packaging-detect--read-package-dir (&optional prompt)
  "Prompt for a package directory, re-prompting until one qualifies.
A directory qualifies when it or an ancestor contains debian/changelog;
the package root is returned, not necessarily the directory picked.
Remote picks are rejected with the host-only message from
`deb-packaging-detect--find-package-dir'.  The rejection reason is
folded into the next prompt: an echo-area message would be overwritten
by the prompt immediately and never seen.  PROMPT defaults to
\"Package directory: \"; C-g aborts as usual."
  (let ((prompt (or prompt "Package directory: "))
        (pkg-dir nil)
        (reason nil))
    (while (not pkg-dir)
      (let* ((dir (read-directory-name
                   (if reason (format "%s[%s] " prompt reason) prompt)
                   nil nil t))
             (rejection nil)
             (found (condition-case err
                        (deb-packaging-detect--find-package-dir dir t)
                      (user-error (setq rejection (cadr err)) nil))))
        (if found
            (setq pkg-dir found)
          ;; Keep the specific rejection (e.g. host-only); the generic
          ;; line would only clobber it.
          (setq reason (or rejection
                           (format "No debian/changelog in %s or any parent"
                                   dir))))))
    pkg-dir))

;;; Shared helpers

(defun deb-packaging-detect--parent-dir (pkg-dir)
  "Return the build-output directory (parent of PKG-DIR)."
  (file-name-directory (directory-file-name pkg-dir)))

(defun deb-packaging-detect--package-info (&optional pkg-dir)
  "Return (NAME VERSION) for PKG-DIR, or nil outside a package tree."
  (when-let* ((info (deb-packaging-detect--parse-changelog pkg-dir)))
    (list (nth 0 info) (nth 1 info))))

(defun deb-packaging-detect--package-name (&optional pkg-dir)
  "Return the source package name for PKG-DIR, or nil."
  (car (deb-packaging-detect--package-info pkg-dir)))

(defun deb-packaging-detect--package-version (&optional pkg-dir)
  "Return the full version string for PKG-DIR, or nil."
  (cadr (deb-packaging-detect--package-info pkg-dir)))

(defvar deb-packaging-detect--memo nil
  "Hash table caching `deb-packaging-detect--call-process-string', or nil.
Bound around one status render, which asks the same questions many times.")

(defun deb-packaging-detect--call-process-string (program &rest args)
  "Run PROGRAM with ARGS, returning trimmed stdout, or nil if empty.
Also nil when PROGRAM is not installed: a missing binary must not
crash callers like the status render, which probes dpkg, schroot, lxc."
  (if deb-packaging-detect--memo
      (let ((key (cons program args)))
        (pcase (gethash key deb-packaging-detect--memo 'unset)
          ('unset (puthash key (deb-packaging-detect--call-process-string-1 program args)
                           deb-packaging-detect--memo))
          (value value)))
    (deb-packaging-detect--call-process-string-1 program args)))

(defun deb-packaging-detect--call-process-string-1 (program args)
  "Run PROGRAM with ARGS uncached; see `deb-packaging-detect--call-process-string'."
  (let ((output (with-output-to-string
                  (with-current-buffer standard-output
                    (condition-case nil
                        (apply #'call-process program nil t nil args)
                      (file-missing nil))))))
    (unless (string-empty-p output)
      (string-trim output))))

(defun deb-packaging-detect--cache-dir ()
  "Return the base cache directory, honoring $XDG_CACHE_HOME."
  (or (and (getenv "XDG_CACHE_HOME")
           (expand-file-name (getenv "XDG_CACHE_HOME")))
      (expand-file-name "~/.cache")))

;;; Changelog Parsing

(defconst deb-packaging-detect--entry-re
  "^\\([^ ]+\\) (\\([^)]+\\)) \\([^;]+\\);"
  "Changelog entry header: name, version, distribution.")

(defun deb-packaging-detect--suite-series (suite)
  "Return SUITE without its pocket (noble-proposed -> noble)."
  (replace-regexp-in-string "-\\(proposed\\|updates\\|security\\|backports\\)\\'"
                            "" suite))

(defun deb-packaging-detect--unreleased-series (version)
  "Return the series an UNRELEASED entry at VERSION builds for.
Point is after the top entry header.  The last released entry's series,
except a new Ubuntu delta on a Debian upload targets Ubuntu devel."
  (let ((previous (save-excursion
                    (cl-loop while (re-search-forward
                                    deb-packaging-detect--entry-re nil t)
                             for d = (string-trim (match-string 3))
                             unless (equal d "UNRELEASED") return d)))
        (ubuntu (split-string (or (deb-packaging-detect--call-process-string
                                   "ubuntu-distro-info" "--all")
                                  ""))))
    (cond ((and ubuntu (string-match-p "ubuntu" version)
                (not (member (and previous (deb-packaging-detect--suite-series previous))
                             ubuntu)))
           (deb-packaging-detect--call-process-string "ubuntu-distro-info" "--devel"))
          (previous (deb-packaging-detect--suite-series previous)))))

(defun deb-packaging-detect--parse-changelog (&optional dir)
  "Parse debian/changelog in DIR.  Return (name version series raw-distro).
SERIES is the release to build for: the distribution without a pocket,
resolved through `deb-packaging-detect--unreleased-series' when UNRELEASED."
  (let* ((pkg-dir (or dir (deb-packaging-detect--find-package-dir)))
         (changelog (when pkg-dir
                      (expand-file-name "debian/changelog" pkg-dir))))
    (when (and changelog (file-readable-p changelog))
      (with-temp-buffer
        (insert-file-contents changelog)
        (goto-char (point-min))
        (when (looking-at deb-packaging-detect--entry-re)
          (let ((name (match-string 1))
                (version (match-string 2))
                (raw (string-trim (match-string 3))))
            (goto-char (match-end 0))
            (list name version
                  (if (equal raw "UNRELEASED")
                      (or (deb-packaging-detect--unreleased-series version) raw)
                    (deb-packaging-detect--suite-series raw))
                  raw)))))))

;;; Change in progress

(defun deb-packaging-detect--changelog-top-text (&optional pkg-dir)
  "Return the text of the top changelog entry in PKG-DIR, or nil."
  (when-let* ((dir (or pkg-dir (deb-packaging-detect--find-package-dir)))
              (file (expand-file-name "debian/changelog" dir))
              ((file-readable-p file)))
    (with-temp-buffer
      ;; ponytail: first 64k; an entry longer than that is truncated.
      (insert-file-contents file nil 0 65536)
      (goto-char (point-min))
      (buffer-substring (point-min)
                        (if (re-search-forward "^ -- " nil t)
                            (line-end-position)
                          (point-max))))))

(defun deb-packaging-detect--launchpad-bug (&optional pkg-dir)
  "Return the Launchpad bug number this change is for, or nil.
From the branch name (lp1234567, lp-1234567, bug/1234567), else the top
changelog entry's LP: #N."
  (let* ((default-directory (or pkg-dir default-directory))
         (branch (ignore-errors (magit-get-current-branch)))
         (case-fold-search t))
    (cond ((and branch (string-match "\\(?:\\`\\|[/_-]\\)\\(?:lp\\|bug\\)[#/_-]?\\([0-9]\\{4,\\}\\)"
                                     branch))
           (match-string 1 branch))
          ((when-let ((text (deb-packaging-detect--changelog-top-text pkg-dir)))
             (and (string-match "LP: *#\\([0-9]+\\)" text)
                  (match-string 1 text)))))))

;;; Source metadata

(defun deb-packaging-detect--source-format (&optional pkg-dir)
  "Return the source format string for PKG-DIR from `debian/source/format'.
Returns nil if the file is absent."
  (let* ((dir (or pkg-dir (deb-packaging-detect--find-package-dir)))
         (format-file (when dir
                        (expand-file-name "debian/source/format" dir))))
    (when (and format-file (file-readable-p format-file))
      (with-temp-buffer
        (insert-file-contents format-file)
        (goto-char (point-min))
        (when (re-search-forward "^[ \t]*\\(.+\\)$" nil t)
          (string-trim (match-string 1)))))))

;;; Patches and VCS metadata

(defun deb-packaging-detect--list-patches ()
  "Return an alist of (NAME . ABSOLUTE-PATH) for patches in the series file.
Skips comments, blanks, and quilt options.  Returns nil if series absent."
  (when-let* ((pkg-dir (deb-packaging-detect--find-package-dir))
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

(defun deb-packaging-detect--vcs-git-info (&optional pkg-dir)
  "Return (URL BRANCH) parsed from Vcs-Git in PKG-DIR."
  (when-let* ((value (deb-packaging-detect--control-field "Vcs-Git" pkg-dir))
              (parts (split-string value)))
    (list (car parts)
          (when-let ((tail (member "-b" parts)))
            (cadr tail)))))

(defun deb-packaging-detect--vcs-git (&optional pkg-dir)
  "Return the Vcs-Git URL for PKG-DIR, or nil."
  (car (deb-packaging-detect--vcs-git-info pkg-dir)))

(defun deb-packaging-detect--orig-tarball (name version parent-dir)
  "Return the .orig.tar.* path matching `NAME_UPSTREAM' in PARENT-DIR, or nil."
  (let* ((upstream (deb-packaging-detect--upstream-version version))
         (prefix (format "%s_%s.orig.tar." name upstream)))
    (when (and name upstream (file-directory-p parent-dir))
      (cl-some
       (lambda (file)
         (when (string-prefix-p prefix file)
           (expand-file-name file parent-dir)))
       (directory-files parent-dir nil (regexp-quote prefix))))))

(defun deb-packaging-detect--control-field (field &optional pkg-dir)
  "Return FIELD's trimmed value from debian/control in PKG-DIR, or nil."
  (let* ((dir (or pkg-dir (deb-packaging-detect--find-package-dir)))
         (control (when dir
                    (expand-file-name "debian/control" dir))))
    (when (and control (file-readable-p control))
      (with-temp-buffer
        (insert-file-contents control)
        (goto-char (point-min))
        (when (re-search-forward
               (format "^%s:\\s-*\\(.+\\)$" (regexp-quote field)) nil t)
          (string-trim (match-string 1)))))))

(defun deb-packaging-detect--schroot-exists-p (distro arch)
  "Return the schroot name matching DISTRO and ARCH, or nil.
Name (not just a boolean) so callers can reuse it."
  (when (and distro arch)
    (let ((output (deb-packaging-detect--call-process-string "schroot" "-l"))
          (target (format "%s-%s" distro arch)))
      (cl-some
       (lambda (line)
         (when (and (string-prefix-p "chroot:" line)
                    (string-match-p (regexp-quote target) line))
           (string-remove-prefix "chroot:" line)))
       (split-string (or output "") "\n" t)))))

;;; Artifact Scanning

(defun deb-packaging-detect--version-to-filename (version)
  "Convert VERSION to filename form, stripping any epoch prefix."
  (replace-regexp-in-string "^[0-9]+:" "" version))

(defun deb-packaging-detect--upstream-version (version)
  "Return the upstream portion of VERSION, as used in .orig.tar.* names.
Strips epoch and Debian revision.  Native packages return VERSION."
  (let ((file-version (deb-packaging-detect--version-to-filename version)))
    (if (string-match "\\(.*\\)-[^-]+$" file-version)
        (match-string 1 file-version)
      file-version)))

(defun deb-packaging-detect--native-version-p (version)
  "Return non-nil when VERSION is native (no Debian revision)."
  (not (string-match-p "-" (deb-packaging-detect--version-to-filename version))))

(defun deb-packaging-detect--parse-changes-file (changes-file)
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

(defun deb-packaging-detect--scan-artifacts (name version dir &optional arch)
  "Scan DIR for artifacts matching NAME and VERSION.
Return alist with keys: dsc, source-changes, binary-changes, debs, buildinfo.
When ARCH is non-nil, include only binary changes for ARCH or `all'."
  (let* ((file-version (deb-packaging-detect--version-to-filename version))
         (base-pattern (format "^%s_" (regexp-quote name)))
         (files (directory-files dir nil base-pattern))
         (dsc nil)
         (source-changes nil)
         (binary-changes nil)
         (debs nil)
         (buildinfo nil))
    (dolist (file files)
      (when (equal (deb-packaging-detect--filename-version file) file-version)
        (cond
         ((string-match "\\.dsc$" file)
          (setq dsc (expand-file-name file dir)))
         ((string-match "_source\\.changes$" file)
          (setq source-changes (expand-file-name file dir)))
         ((string-match "_\\([^_]+\\)\\.changes$" file)
          (when (or (null arch)
                    (member (match-string 1 file) (list arch "all")))
            (push (expand-file-name file dir) binary-changes)))
         ((string-match "_source\\.buildinfo$" file)
          (push (expand-file-name file dir) buildinfo)))))
    ;; debs are only discoverable via the binary .changes.
    (dolist (changes-file binary-changes)
      (dolist (referenced (deb-packaging-detect--parse-changes-file changes-file))
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

(defun deb-packaging-detect--binary-package-names (&optional pkg-dir)
  "Return binary package names from debian/control in PKG-DIR, or nil.
Template variables are left as-is."
  (let* ((dir (or pkg-dir (deb-packaging-detect--find-package-dir)))
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

(defun deb-packaging-detect--owned-package-prefixes (&optional pkg-dir)
  "Return artifact name prefixes owned by this source.
Source name, binary names, and their -dbgsym/-dbg variants.  Falls back
to the source name if debian/control is missing."
  (let* ((dir (or pkg-dir (deb-packaging-detect--find-package-dir)))
         (info (when dir (deb-packaging-detect--parse-changelog dir)))
         (source-name (nth 0 info))
         (bin-names (deb-packaging-detect--binary-package-names dir))
         (all-names (cons source-name bin-names)))
    (delete-dups
     (mapcan (lambda (n)
               (list n
                     (concat n "-dbgsym")
                     (concat n "-dbg")))
             all-names))))

(defun deb-packaging-detect--filename-version (filename)
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

(defun deb-packaging-detect--scan-stale-artifacts (name version dir &optional pkg-dir)
  "Return sorted basenames in DIR owned by NAME but not matching VERSION.
PKG-DIR supplies binary package names; otherwise only NAME is used.
Matches packaging extensions (dsc, changes, deb, udeb, ddeb, buildinfo,
upload, tar.*, orig.tar.*)."
  (when (and name version (file-directory-p dir))
    (let* ((file-version (deb-packaging-detect--version-to-filename version))
           (upstream-version (deb-packaging-detect--upstream-version version))
           (orig-pattern "\\.orig\\.tar\\.[a-z0-9]+$")
           (ext-pattern
            "\\.\\(dsc\\|changes\\|u?deb\\|ddeb\\|buildinfo\\|upload\\)$\\|\\.tar\\.[a-z0-9]+$\\|\\.orig\\.tar\\.[a-z0-9]+$")
           (prefixes (or (deb-packaging-detect--owned-package-prefixes pkg-dir)
                         (list name)))
           (stale nil))
       ;; Not one alternation regexp: gcc-sized control files overflow it.
       (dolist (file (directory-files dir nil "_"))
         (when (and (member (substring file 0 (string-search "_" file)) prefixes)
                    (string-match-p ext-pattern file))
           (if (string-match-p orig-pattern file)
               ;; Orig tarballs embed the upstream version, not the full one.
               (when (string-match "_\\([^_]+\\)\\.orig\\.tar\\." file)
                 (unless (string= (match-string 1 file) upstream-version)
                   (push file stale)))
             (let ((fv (deb-packaging-detect--filename-version file)))
               (when (and fv (not (string= fv file-version)))
                 (push file stale))))))
      (sort (delete-dups stale) #'string<))))

;;; Unified context scan
;;
;; Single source of truth for the status buffer and dispatch transient.
;; Re-reads the filesystem/changelog on each call, no caching or mutation.

(defun deb-packaging-detect--scan-context (&optional start-dir)
  "Return a fresh context plist for the package containing START-DIR.
START-DIR defaults to `default-directory'.  Returns nil outside a
package tree.  Keys:

  :name          source package name
  :version       full version string
  :distro        series to build for (no pocket, UNRELEASED resolved)
  :changelog-distro distribution as written in the top changelog entry
  :pkg-dir       canonical directory containing debian/changelog
  :parent-dir    build-output directory
  :repo-dir      canonical Git top-level, or nil
  :git-p         non-nil inside a Git repository
  :branch        current branch, or nil for detached HEAD/non-Git
  :dirty-p       non-nil for staged, unstaged, untracked, or submodule changes
  :vcs-url       URL from Vcs-Git, or nil
  :vcs-branch    branch from Vcs-Git -b, or nil
  :artifacts     alist from `deb-packaging-detect--scan-artifacts'
  :stale         list from `deb-packaging-detect--scan-stale-artifacts'
  :source-format source format string, or nil
  :orig-tarball  .orig.tar.* path, or nil
  :arch          host architecture string (compatibility key)
  :host-arch     host architecture string"
  (when-let* ((found-dir (deb-packaging-detect--find-package-dir start-dir))
              (pkg-dir (file-name-as-directory (file-truename found-dir)))
              (info (deb-packaging-detect--parse-changelog pkg-dir)))
    (let* ((name (nth 0 info))
           (version (nth 1 info))
           (distro (nth 2 info))
           (changelog-distro (nth 3 info))
           (parent-dir (deb-packaging-detect--parent-dir pkg-dir))
           (default-directory pkg-dir)
           (repo-dir (when-let ((root (ignore-errors (magit-toplevel))))
                       (file-name-as-directory (file-truename root))))
           (vcs-git (deb-packaging-detect--vcs-git-info pkg-dir))
           (host-arch (deb-packaging-detect--call-process-string
                       "dpkg" "--print-architecture"))
           (artifacts (deb-packaging-detect--scan-artifacts name version parent-dir))
           (stale (deb-packaging-detect--scan-stale-artifacts name version parent-dir pkg-dir)))
      (list :name name
            :version version
            :distro distro
            :changelog-distro changelog-distro
            :pkg-dir pkg-dir
            :parent-dir parent-dir
            :repo-dir repo-dir
            :git-p (and repo-dir t)
            :branch (when repo-dir (magit-get-current-branch))
            :dirty-p (and repo-dir
                          (magit-git-lines "status" "--porcelain"
                                           "--untracked-files=normal"
                                           "--ignore-submodules=none")
                          t)
            :vcs-url (car vcs-git)
            :vcs-branch (cadr vcs-git)
            :artifacts artifacts
            :stale stale
            :source-format (deb-packaging-detect--source-format pkg-dir)
            :orig-tarball (deb-packaging-detect--orig-tarball name version parent-dir)
            :arch host-arch
            :host-arch host-arch))))

(provide 'deb-packaging-detect)
;;; deb-packaging-detect.el ends here
