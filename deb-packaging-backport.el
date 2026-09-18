;;; deb-packaging-backport.el --- Backport upstream patches -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Import upstream patches into debian/patches/ as DEP-3 quilt patches.
;;   1. Point at a patch source: a GitHub/GitLab commit or pull-request
;;      page URL (fetched as raw .patch data), any raw patch URL, or a
;;      local file.
;;   2. Mboxes with several commits become one quilt patch per commit.
;;   3. Each patch gets a DEP-3 header and a series entry, then the
;;      series is verified with `quilt push -a'.
;;
;; Entry point: `deb-packaging-backport-patch'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'deb-packaging-detect)
(require 'deb-packaging-commands)
(require 'deb-packaging-propagate)

;;; Preflight

(defun deb-packaging-backport--ensure-quilt (pkg-dir)
  "Signal `user-error' unless PKG-DIR accepts quilt patches.
Only 3.0 (native) is rejected; a missing format file defaults to 1.0,
which still honours debian/patches/series."
  (when (string= (or (deb-packaging-detect--source-format pkg-dir) "")
                 "3.0 (native)")
    (user-error "Source format is 3.0 (native); quilt patches do not apply")))

;;; Source reading and fetching

(defun deb-packaging-backport--read-source ()
  "Read a patch source (URL or file), pre-filling a recent URL kill."
  (let* ((recent (mapcar (lambda (s) (string-trim (substring-no-properties s)))
                         (seq-take kill-ring 5)))
         (url (cl-find-if (lambda (s) (string-prefix-p "http" s)) recent))
         (source (read-string "Patch source (URL or file): " url)))
    (if (string-empty-p source)
        (user-error "No patch source given")
      source)))

(defun deb-packaging-backport--patch-url (url)
  "Return the raw-patch URL for forge page URL.
GitHub pull-request and commit pages and GitLab merge-request pages
get `.patch' appended; fragments and trailing slashes are dropped
first."
  (let* ((clean (string-trim-right
                 (replace-regexp-in-string "#.*\\'" "" url)
                 "/"))
         (page-p (or (string-match-p
                      "//github\\.com/[^/]+/[^/]+/\\(pull\\|commit\\)/[0-9a-f]+" clean)
                     (string-match-p
                      "/-/merge_requests/[0-9]+" clean))))
    (if (and page-p (not (string-match-p "\\.\\(patch\\|diff\\)\\'" clean)))
        (concat clean ".patch")
      clean)))

(defun deb-packaging-backport--fetch (source)
  "Return patch content for SOURCE, a URL or a local file path.
Forge page URLs are rewritten to their raw .patch form and fetched
with curl.  Signals `user-error' when the content is not a patch."
  (let ((content
         (cond
          ((file-readable-p source)
           (with-temp-buffer
             (insert-file-contents source)
             (buffer-string)))
          ((string-prefix-p "http" source)
           (unless (executable-find "curl")
             (user-error "curl not found in `exec-path'"))
           (let ((url (deb-packaging-backport--patch-url source)))
             (with-temp-buffer
               (let ((code (call-process "curl" nil t nil
                                         "-sL" "--max-time" "30" url)))
                 (if (zerop code)
                     (buffer-string)
                   (user-error "Fetching %s failed (curl exit %d)" url code))))))
          (t (user-error "Not a readable file or URL: %s" source)))))
    (cond
     ((string-empty-p (string-trim content))
      (user-error "Patch source %s is empty" source))
     ;; curl exits 0 on HTTP 404 and hands back the HTML error page.
     ((string-prefix-p "<" (string-trim-left content))
      (user-error "Fetched content is HTML, not a patch (bad URL?)"))
     (t content))))

;;; Parsing

(defun deb-packaging-backport--strip-subject (subject)
  "Remove [PATCH ...], Re:, Fwd: prefixes from SUBJECT."
  (string-trim
   (replace-regexp-in-string
    "\\`\\(?:\\(?:\\[[^]]*\\]\\|Re:\\|Fwd:\\)\\s-*\\)+" "" subject)))

(defun deb-packaging-backport--strip-signature (diff)
  "Drop a trailing `-- ' email signature from DIFF."
  (replace-regexp-in-string "\n-- \n\\(.\\|\n\\)*\\'" "\n" diff))

(defun deb-packaging-backport--parse-block (block source)
  "Parse one git-am BLOCK originating from SOURCE into a plist.
Keys: :subject, :author, :body, :diff, :source-url (nil for files)."
  (let* ((diff-pos (or (string-match "^diff --git " block)
                       (string-match "^--- [^ \t\n]" block))))
    (unless diff-pos
      (user-error "No diff found in patch"))
    (let* ((pre (substring block 0 diff-pos))
           (blank (string-match "\n\n" pre))
           (headers (if blank (substring pre 0 blank) pre))
           (msg (if blank (substring pre (+ blank 2)) ""))
           (sep (string-match "^---$" msg))
           (body (string-trim (if sep (substring msg 0 sep) msg)))
           author subject)
      (dolist (line (split-string headers "\n"))
        (cond ((string-match "\\`From: \\(.*\\)" line)
               (setq author (match-string 1 line)))
              ((string-match "\\`Subject: \\(.*\\)" line)
               (setq subject (match-string 1 line)))))
      (list :subject (when subject (deb-packaging-backport--strip-subject subject))
            :author author
            :body body
            :diff (deb-packaging-backport--strip-signature
                   (substring block diff-pos))
            :source-url (when (string-prefix-p "http" source) source)))))

(defun deb-packaging-backport--parse (content source)
  "Parse patch CONTENT from SOURCE into a list of block plists.
git-am content (leading `From ') splits into one block per commit; a
plain diff yields a single block."
  (if (not (string-prefix-p "From " (string-trim-left content)))
      (list (deb-packaging-backport--parse-block content source))
    (let* ((trimmed (string-trim-left content))
           (parts (split-string trimmed "\nFrom "))
           (blocks (cons (car parts)
                         (mapcar (lambda (p) (concat "From " p))
                                 (cdr parts)))))
      (mapcar (lambda (b) (deb-packaging-backport--parse-block b source))
              (seq-filter (lambda (b) (not (string-empty-p (string-trim b))))
                          blocks)))))

(defun deb-packaging-backport--select-blocks (blocks)
  "Return the BLOCKS to import.
A single block passes through; otherwise `completing-read-multiple'
picks subjects, with RET selecting everything."
  (if (null (cdr blocks))
      blocks
    (let* ((subjects (mapcar (lambda (b) (or (plist-get b :subject) "patch"))
                             blocks))
           (sel (completing-read-multiple
                 "Commits to import (RET = all): " subjects nil t))
           (keep (if (or (null sel)
                         (and (= (length sel) 1)
                              (string-empty-p (string-trim (car sel))))
                     subjects sel))))
      (cl-remove-if-not
       (lambda (b) (member (or (plist-get b :subject) "patch") keep))
       blocks))))

;;; DEP-3 patch writing

(defun deb-packaging-backport--dep3-header (block)
  "Return the DEP-3 header text for parsed BLOCK."
  (let* ((subject (or (plist-get block :subject) "backported patch"))
         (body (plist-get block :body))
         (author (plist-get block :author))
         (origin (plist-get block :source-url)))
    (concat
     "Description: " subject "\n"
     (if (string-empty-p body)
         ""
       (concat (mapconcat (lambda (l) (if (string-empty-p l) " ." (concat " " l)))
                          (split-string (string-trim-right body) "\n")
                          "\n")
               "\n"))
     (if origin
         (format "Origin: upstream, %s\n" origin)
       "Origin: upstream\n")
     (when author (format "Author: %s\n" author))
     (format "Last-Update: %s\n" (format-time-string "%Y-%m-%d")))))

(defun deb-packaging-backport--add-to-series (patches-dir name)
  "Append NAME to the series file in PATCHES-DIR, creating it as needed.
Existing entries are preserved; NAME is not added twice."
  (make-directory patches-dir t)
  (let* ((series (expand-file-name "series" patches-dir))
         (existing (if (file-readable-p series)
                       (with-temp-buffer
                         (insert-file-contents series)
                         (buffer-string))
                     "")))
    (unless (member name (split-string existing "\n" t))
      (with-temp-file series
        (insert existing)
        (unless (or (string-empty-p existing)
                    (string-suffix-p "\n" existing))
          (insert "\n"))
        (insert name "\n")))))

(defun deb-packaging-backport--write-block (pkg-dir block name)
  "Write BLOCK as DEP-3 quilt patch NAME in PKG-DIR's debian/patches/.
Also appends NAME to the series file.  Returns the patch path."
  (let* ((patches-dir (expand-file-name "debian/patches" pkg-dir))
         (path (expand-file-name name patches-dir))
         (content (concat (deb-packaging-backport--dep3-header block)
                          "\n"
                          (plist-get block :diff))))
    (make-directory patches-dir t)
    (write-region (if (string-suffix-p "\n" content)
                      content
                    (concat content "\n"))
                  nil path nil 0)
    (deb-packaging-backport--add-to-series patches-dir name)
    path))

(defun deb-packaging-backport--read-patch-name (patches-dir default-name)
  "Read a patch file name under PATCHES-DIR, defaulting to DEFAULT-NAME.
An existing file must be confirmed for overwrite or a new name given."
  (catch 'done
    (let ((name default-name))
      (while t
        (let* ((input (read-file-name
                       "Patch file name: "
                       (file-name-as-directory patches-dir)
                       nil nil name))
               (clean (file-name-nondirectory
                       (directory-file-name input))))
          (cond
           ((string-empty-p clean)
            (user-error "No patch file name given"))
           ((or (not (file-exists-p (expand-file-name clean patches-dir)))
                (y-or-n-p (format "%s already exists; overwrite? " clean)))
            (throw 'done clean))
           (t (setq name clean))))))))

(defun deb-packaging-backport--verify-command (check-only)
  "Return the quilt verification command for CHECK-ONLY."
  (if check-only
      "QUILT_PATCHES=debian/patches quilt push -a --dry-run"
    (concat
     "test -z \"$(QUILT_PATCHES=debian/patches quilt applied 2>/dev/null)\""
     " || { echo 'Quilt patches already applied; refusing to alter existing state'; exit 2; }; "
     "QUILT_PATCHES=debian/patches quilt push -a; rc=$?; "
     "QUILT_PATCHES=debian/patches quilt pop -a; pop_rc=$?; "
     "[ $rc -eq 0 ] && [ $pop_rc -eq 0 ]")))

;;; Command

;;;###autoload
(defun deb-packaging-backport-patch (&optional check-only)
  "Backport an upstream patch into debian/patches/.
Prompt for a patch source: a GitHub/GitLab commit or pull-request page
URL (fetched as raw .patch data), any raw patch URL, or a local file;
the kill ring supplies a URL as the default.  An mbox with several
commits imports as one quilt patch per commit.  Each patch gets a
  DEP-3 header, is appended to debian/patches/series, and is verified by
  `quilt push -a' from the package root; the tree is then popped back to
  its unpatched state (sbuild applies patches itself).  With CHECK-ONLY
  \(prefix arg), verify with --dry-run instead of applying."
  (interactive "P")
  (let* ((pkg-dir (or (deb-packaging-detect--find-package-dir nil t)
                      (user-error "Not in a Debian package directory")))
         (patches-dir (expand-file-name "debian/patches" pkg-dir)))
    (deb-packaging-backport--ensure-quilt pkg-dir)
    (let* ((source (deb-packaging-backport--read-source))
           (content (deb-packaging-backport--fetch source))
           (blocks (deb-packaging-backport--parse content source))
           (selected (deb-packaging-backport--select-blocks blocks))
           (written nil))
      (dolist (block selected)
        (let* ((subject (or (plist-get block :subject)
                            (read-string "Description: ")))
               (block (plist-put block :subject subject))
               (slug (deb-packaging-propagate--slug subject))
               (default-name (concat
                              (if (string-empty-p slug) "backport" slug)
                              ".patch"))
               (name (deb-packaging-backport--read-patch-name
                      patches-dir default-name)))
          (push (deb-packaging-backport--write-block pkg-dir block name)
                written)))
      (if (null written)
          (message "No patches imported")
        (dolist (path (nreverse written))
          (display-buffer (find-file-noselect path)))
        (message "Imported %d patch(es); verifying with quilt..."
                 (length written))
        ;; Verify by applying, then restore the pristine tree: a left-
        ;; applied patch set is a dirty worktree and double-applies on
        ;; the next build.
        (let ((cmd (deb-packaging-backport--verify-command check-only))
              (default-directory pkg-dir))
          (deb-packaging-commands--after-compile
           (deb-packaging-commands--compile cmd)
           (lambda ()
             (deb-packaging-commands--notify-status-refresh)
             (message "Backport complete: %d patch(es) verified, tree unpatched"
                      (length written)))
           (lambda ()
             ;; The patches and series entries were written before
             ;; verification; without this the next sbuild fails on them.
             (message "Verification FAILED; remove from debian/patches/series: %s"
                      (mapconcat #'file-name-nondirectory written ", ")))))))))

(provide 'deb-packaging-backport)
;;; deb-packaging-backport.el ends here
