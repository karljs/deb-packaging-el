;;; deb-packaging-propagate.el --- Propagate fixes across distros -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Propagate packaging fixes from Ubuntu to Debian and upstream.
;;   1. Export quilt patches or git commits as a git-am-friendly .patch.
;;   2. Prepare a salsa.debian.org clone, then apply items one at a time.
;;
;; The clone's git history is the source of truth for what has been
;; applied. The source directory lives in the clone's git config
;; (deb-packaging.source-dir) so apply survives Emacs restarts.
;;
;; Entry point: `deb-packaging-propagate-transient'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magit)
(require 'transient)
(require 'deb-packaging-detect)
(require 'deb-packaging-config)

;;; Slug and description helpers

(defun deb-packaging-propagate--slug (str)
  "Turn STR into a short filesystem-safe slug."
  (let* ((lower (downcase (or str "")))
         (words (split-string lower "[^a-z0-9]+" t))
         (joined (mapconcat #'identity (seq-take words 4) "-")))
    (if (> (length joined) 50)
        (substring joined 0 50)
      joined)))

(defun deb-packaging-propagate--item-slug (item)
  "Return a short slug for fix-source ITEM."
  (deb-packaging-propagate--slug
   (pcase (plist-get item :type)
     ('patch  (plist-get item :name))
     ('commit (plist-get item :subject))
     ('range  (plist-get item :range))
     (_ "fix"))))

(defun deb-packaging-propagate--item-description (item)
  "Return a human-readable description for ITEM."
  (pcase (plist-get item :type)
    ('patch  (format "patch %s" (plist-get item :name)))
    ('commit (format "commit %s (%s)"
                     (plist-get item :ref)
                     (plist-get item :subject)))
    ('range  (format "range %s" (plist-get item :range)))
    (_ "fix")))

;;; Quiet git probes (no *magit-process* spam)

(defun deb-packaging-propagate--git-quiet (dir &rest args)
  "Run git synchronously in DIR with ARGS, returning stdout string.
Read-only probes only; skips Magit process-buffer logging."
  (string-trim
   (with-output-to-string
     (with-current-buffer standard-output
       (apply #'call-process "git" nil t nil
              (append (when dir (list "-C" dir)) args))))))

(defun deb-packaging-propagate--patch-applied-p (patch-path clone-dir)
  "Return non-nil if PATCH-PATH is already applied in CLONE-DIR.
Tests reverse-application with `git apply --check -R'."
  (zerop (call-process "git" nil nil nil
                       "-C" clone-dir "apply" "--check" "-R"
                       patch-path)))

(defun deb-packaging-propagate--commit-applied-p (subject clone-dir)
  "Heuristic: return non-nil if SUBJECT appears in CLONE-DIR's log.
Fooled by rewording; only used for indicators."
  (let ((log-subjects (deb-packaging-propagate--git-quiet
                       clone-dir "log" "--format=%s")))
    (member subject (split-string log-subjects "\n" t))))

(defun deb-packaging-propagate--fork-exists-p (url)
  "Return non-nil if the remote repo at URL is reachable."
  (zerop (call-process "git" nil nil nil "ls-remote" url "HEAD")))

(defun deb-packaging-propagate--default-branch (clone-dir)
  "Return the default branch name for CLONE-DIR, or nil."
  (let ((output (deb-packaging-propagate--git-quiet
                 clone-dir "symbolic-ref" "refs/remotes/origin/HEAD")))
    (when (string-match "refs/remotes/origin/\\(.+\\)$" output)
      (string-trim (match-string 1 output)))))

(defun deb-packaging-propagate--remote-branches (clone-dir)
  "Return a list of remote branch names for CLONE-DIR."
  (split-string
   (deb-packaging-propagate--git-quiet
    clone-dir "for-each-ref" "--format=%(refname:short)"
    "refs/remotes/origin")
   "\n" t))

(defun deb-packaging-propagate--config-get (clone-dir key)
  "Return the git config value for KEY in CLONE-DIR, or nil."
  (let ((output (deb-packaging-propagate--git-quiet
                 clone-dir "config" key)))
    (unless (string-empty-p output)
      output)))

;;; Fix source reading

(defun deb-packaging-propagate--patch-choices (&optional clone-dir)
  "Return an alist of (display-string . item-plist) for quilt patches.
CLONE-DIR, if non-nil, annotates already-applied patches."
  (when-let ((patches (deb-packaging-detect--list-patches)))
    (mapcar
     (lambda (p)
       (let* ((name (car p))
              (path (cdr p))
              (applied (and clone-dir
                             (deb-packaging-propagate--patch-applied-p
                              path clone-dir))))
         (cons (if applied (format "✓ %s" name) name)
               (list :type 'patch :name name :path path))))
     patches)))

(defun deb-packaging-propagate--read-patches-multi (&optional clone-dir)
  "Prompt for one or more quilt patches.  Returns a list of item plists."
  (let ((choices (deb-packaging-propagate--patch-choices clone-dir)))
    (unless choices
      (user-error "No patches found in debian/patches/series"))
    (let* ((selection (completing-read-multiple
                       "Patches (comma-separated): "
                       (mapcar #'car choices) nil t))
           (items (delq nil
                        (mapcar (lambda (s)
                                  (when-let ((entry (assoc (string-trim s) choices)))
                                    (cdr entry)))
                                selection))))
      (if items items
        (user-error "No patches selected")))))

(defun deb-packaging-propagate--read-patch-one (&optional clone-dir)
  "Prompt for a single quilt patch.  Returns one item plist."
  (let ((choices (deb-packaging-propagate--patch-choices clone-dir)))
    (unless choices
      (user-error "No patches found in debian/patches/series"))
    (let* ((selection (completing-read "Patch: "
                                       (mapcar #'car choices) nil t))
           (entry (assoc selection choices)))
      (unless entry
        (user-error "No patch selected"))
      (cdr entry))))

(defun deb-packaging-propagate--read-commit-one (source-dir &optional clone-dir)
  "Prompt for a single git commit from SOURCE-DIR.
CLONE-DIR, if non-nil, annotates already-applied commits."
  (let* ((log-output (deb-packaging-propagate--git-quiet
                      source-dir "log" "--oneline" "-20"))
         (lines (split-string log-output "\n" t))
         (choices
          (delq nil
                (mapcar
                 (lambda (line)
                   (when (string-match "^\\([0-9a-f]+\\) \\(.+\\)$" line)
                     (let* ((ref (match-string 1 line))
                            (subject (match-string 2 line))
                            (applied (and clone-dir
                                          (deb-packaging-propagate--commit-applied-p
                                           subject clone-dir))))
                       (cons (if applied (format "✓ %s" line) line)
                             (list :type 'commit
                                   :ref ref
                                   :subject subject
                                   :source-dir source-dir)))))
                 lines))))
    (unless choices
      (user-error "No commits found in %s" source-dir))
    (let* ((selection (completing-read "Commit: "
                                       (mapcar #'car choices) nil t))
           (entry (assoc selection choices)))
      (unless entry
        (user-error "No commit selected"))
      (cdr entry))))

(defun deb-packaging-propagate--read-range (source-dir)
  "Prompt for a git refspec range from SOURCE-DIR."
  (let ((range (read-string "Range (e.g. HEAD~3..HEAD): ")))
    (when (string-empty-p range)
      (user-error "No range specified"))
    (list :type 'range
          :range range
          :source-dir source-dir)))

(defun deb-packaging-propagate--read-fix-source-multi (&optional clone-dir allow-range)
  "Prompt for a fix source for export (multi-select for patches).
Returns a list of item plists.  CLONE-DIR annotates applied items.
ALLOW-RANGE enables the range type."
  (let* ((pkg-dir (or (deb-packaging-detect--find-package-dir nil t)
                      (user-error "Not in a Debian package directory")))
         (choices (if allow-range
                      '("patch" "commit" "range")
                    '("patch" "commit")))
         (choice (completing-read "Fix source: " choices nil t)))
    (pcase choice
      ("patch"
       (deb-packaging-propagate--read-patches-multi clone-dir))
      ("commit"
       (list (deb-packaging-propagate--read-commit-one pkg-dir clone-dir)))
      ("range"
       (list (deb-packaging-propagate--read-range pkg-dir))))))

(defun deb-packaging-propagate--read-fix-source-one (&optional clone-dir)
  "Prompt for a single fix source for apply.
Returns one item plist.  CLONE-DIR annotates applied items and
provides the source-dir via git config."
  (let* ((source-dir
          (if clone-dir
              (or (deb-packaging-propagate--config-get
                   clone-dir "deb-packaging.source-dir")
                  (user-error "No source-dir stored in clone config"))
            (or (deb-packaging-detect--find-package-dir nil t)
                (user-error "Not in a Debian package directory"))))
         (choice (completing-read "Fix source: "
                                  '("patch" "commit") nil t)))
    (unless (file-directory-p source-dir)
      (user-error "Source directory %s no longer exists" source-dir))
    (pcase choice
      ("patch"
       (deb-packaging-propagate--read-patch-one clone-dir))
      ("commit"
       (deb-packaging-propagate--read-commit-one source-dir clone-dir)))))

;;; Patch normalization (quilt to git-am)

(defun deb-packaging-propagate--parse-quilt-headers (patch-content)
  "Parse quilt patch headers from PATCH-CONTENT.
Return plist: :description, :author.  Description may be multi-line."
  (let (description author
        (in-description nil))
    (dolist (line (split-string patch-content "\n"))
      (cond
       ((string-prefix-p "---" line)
        (setq in-description nil))
       ((string-match "^\\([A-Za-z-]+\\):\\s-*\\(.+\\)$" line)
        (setq in-description nil)
        (let ((field (downcase (match-string 1 line)))
              (value (match-string 2 line)))
          (pcase field
            ("description"
             (setq description value
                   in-description t))
            ("author" (setq author value)))))
       ((and in-description (string-prefix-p " " line))
        (let ((trimmed (string-trim line)))
          (setq description
                (concat description
                        (if (string-empty-p trimmed) "\n"
                          (concat "\n" trimmed))))))))
    (list :description description :author author)))

(defun deb-packaging-propagate--normalize-diff-paths (diff-body)
  "Normalize DIFF-BODY path prefixes to a/ and b/."
  (with-temp-buffer
    (insert diff-body)
    (goto-char (point-min))
    (while (re-search-forward "^\\(---\\|\\+\\+\\+\\) \\(.+\\)$" nil t)
      (let* ((marker (match-string 1))
             (path (string-trim (match-string 2)))
             (clean (replace-regexp-in-string
                     "^\\([ab]\\)/" "" path)))
        (replace-match (format "%s %s/%s"
                               marker
                               (if (string= marker "---") "a" "b")
                               clean)
                       t t)))
    (buffer-string)))

(defun deb-packaging-propagate--quilt-to-git-am-block (patch-path)
  "Convert a quilt patch at PATCH-PATH to a git-am-format block."
  (let* ((content (with-temp-buffer
                    (insert-file-contents patch-path)
                    (buffer-string)))
         (headers (deb-packaging-propagate--parse-quilt-headers content))
         (description (or (plist-get headers :description)
                          (file-name-base patch-path)))
         (author (or (plist-get headers :author)
                     (format "%s <%s>"
                             (or user-full-name "Unknown")
                             (or user-mail-address "unknown@example.com"))))
         (diff-start (string-match "^---" content))
         (diff-body (if diff-start (substring content diff-start) ""))
         (normalized (deb-packaging-propagate--normalize-diff-paths diff-body)))
     (concat
      (format "From %s Mon Sep 17 00:00:00 2001\n"
              (secure-hash 'sha1 patch-path))
      (format "From: %s\n" author)
      (format "Date: %s\n" (format-time-string "%a, %d %b %Y %H:%M:%S %z"))
      (format "Subject: [PATCH] %s\n\n" description)
      normalized
      "\n-- \ndeb-packaging\n\n")))

;;; Clone directory and salsa helpers

(defun deb-packaging-propagate--clone-dir (pkg-name)
  "Return the cache directory for PKG-NAME's Debian clone."
  (expand-file-name (format "debian/%s" pkg-name)
                    deb-packaging-config-propagate-cache-dir))

(defun deb-packaging-propagate--clone-exists-p (dir)
  "Return non-nil if DIR is an existing git repo."
  (and (file-directory-p dir)
       (file-directory-p (expand-file-name ".git" dir))))

(defun deb-packaging-propagate--salsa-project-path (vcs-url)
  "Extract the project path from a salsa VCS-URL."
  (cond
   ((string-match "salsa.debian.org[:/]\\(.+?\\)\\.git" vcs-url)
    (match-string 1 vcs-url))
   ((string-match "salsa.debian.org[:/]\\(.+\\)" vcs-url)
    (match-string 1 vcs-url))
   (t nil)))

(defun deb-packaging-propagate--fork-url (vcs-url)
  "Return the salsa fork-creation web URL for VCS-URL."
  (when-let ((path (deb-packaging-propagate--salsa-project-path vcs-url)))
    (format "https://salsa.debian.org/%s/-/forks/new" path)))

(defun deb-packaging-propagate--salsa-personal-url (pkg-name)
  "Return the salsa personal fork git URL for PKG-NAME, or nil."
  (when deb-packaging-config-propagate-salsa-user
    (format "git@salsa.debian.org:~%s/%s.git"
            deb-packaging-config-propagate-salsa-user pkg-name)))

;;; Patch file production

(defun deb-packaging-propagate--produce-patch-file (item)
  "Produce a patch file path for ITEM suitable for `git apply'.
Quilt patches return their path directly. Commits get format-patch
output written to a temp file and their message pushed to the kill ring."
  (pcase (plist-get item :type)
    ('patch
     (plist-get item :path))
    ('commit
     (let* ((source-dir (plist-get item :source-dir))
            (ref (plist-get item :ref))
            (patch-file (make-temp-file "propagate-" nil ".patch"))
            (output (let ((default-directory source-dir))
                      (magit-git-output "format-patch" "-1" "--stdout" ref))))
       (when (or (null output) (string-empty-p (string-trim output)))
         (user-error "git format-patch produced no output for %s" ref))
       (write-region output nil patch-file nil 'silent)
       (let ((msg (let ((default-directory source-dir))
                    (magit-git-output "log" "--format=%B" "-1" ref))))
         (when msg
           (kill-new msg)
           (message "Commit message saved to kill ring (C-y).")))
       patch-file))
    (_
     (user-error "Cannot produce patch for item type %s"
                 (plist-get item :type)))))

;;; Apply transient (mirrors magit-patch-apply's flags)

(defvar-local deb-packaging-propagate--pending-patch nil
  "Patch file path for the next apply, or nil.
Buffer-local to the clone's Magit status buffer. Set by
`deb-packaging-propagate-apply', consumed by
`deb-packaging-propagate-do-apply'.")

;;;###autoload(autoload 'deb-packaging-propagate-apply-patch "deb-packaging-propagate" nil t)
(transient-define-prefix deb-packaging-propagate-apply-patch ()
  "Apply a propagate patch file with git-apply flags.
The patch file is pre-filled by `deb-packaging-propagate-apply'."
  :value '("--index")
  ["Arguments"
   ("-i" "Apply to index and worktree" "--index")
   ("-c" "Only apply to index"         "--cached")
   ("-3" "Fall back on 3way merge" ("-3" "--3way"))]
  ["Actions"
   ("a" "Apply patch" deb-packaging-propagate-do-apply)])

(defun deb-packaging-propagate-do-apply (&optional args)
  "Apply the pending patch with git-apply ARGS."
  (interactive
   (list (transient-args 'deb-packaging-propagate-apply-patch)))
  (unless deb-packaging-propagate--pending-patch
    (user-error "No pending patch"))
  (let ((file deb-packaging-propagate--pending-patch))
    (setq deb-packaging-propagate--pending-patch nil)
    (magit-run-git "apply" args "--" file)))

;;; Commands

;;;###autoload
(defun deb-packaging-propagate-export-patch (&optional items output-path)
  "Export a .patch file from fix-source ITEMS to OUTPUT-PATH.
Git-am-friendly, one From:/Subject: block per item. Opens the result
in view-mode."
  (interactive
   (let* ((items (deb-packaging-propagate--read-fix-source-multi nil t))
          (pkg-dir (deb-packaging-detect--find-package-dir nil t))
          (name (deb-packaging-detect--package-name pkg-dir))
          (parent (when pkg-dir (deb-packaging-detect--parent-dir pkg-dir)))
          (slug (deb-packaging-propagate--item-slug (car items)))
          (default-output (when (and name parent slug)
                            (expand-file-name
                             (format "%s_%s.patch" name slug) parent)))
          (output (read-file-name "Output patch file: "
                                  (or parent default-directory)
                                  nil nil
                                  (or (file-name-nondirectory default-output)
                                      "fix.patch"))))
     (list items output)))
  (let ((content
         (mapconcat
          (lambda (item)
            (pcase (plist-get item :type)
              ('patch
               (deb-packaging-propagate--quilt-to-git-am-block
                (plist-get item :path)))
              ('commit
               (let ((output (let ((default-directory
                                    (plist-get item :source-dir)))
                                (magit-git-output "format-patch" "-1" "--stdout"
                                                  (plist-get item :ref)))))
                 (or output "")))
              ('range
               (or (let ((default-directory
                          (plist-get item :source-dir)))
                     (magit-git-output "format-patch" "--stdout"
                                       (plist-get item :range)))
                   ""))
              (_ "")))
          items "")))
    (when (string-empty-p (string-trim content))
      (user-error "No patch content produced"))
    (write-region content nil output-path)
    (with-current-buffer (find-file-read-only output-path)
      (view-mode-enter))
    (message "Exported %d bytes to %s" (length content) output-path)
    output-path))

;;;###autoload
(defun deb-packaging-propagate-clone ()
  "Prepare a Debian salsa clone.
Confirms the Vcs-Git URL, clones (or reuses) into the propagate cache,
prompts for base and work branch, sets up the `personal' remote if
configured, and hands off to `magit-status'. Press `P' afterwards to
apply items."
  (interactive)
  (let* ((pkg-dir (deb-packaging-detect--find-package-dir nil t))
         (pkg-name (deb-packaging-detect--package-name pkg-dir))
         (detected-url (or (deb-packaging-detect--vcs-git pkg-dir) ""))
         (vcs-url (read-string "Clone URL: " detected-url))
         (clone-dir (deb-packaging-propagate--clone-dir pkg-name)))
    (when (string-empty-p vcs-url)
      (user-error "No clone URL provided"))
    (let ((parent (file-name-directory clone-dir)))
      (unless (file-directory-p parent)
        (make-directory parent t)))
    (if (deb-packaging-propagate--clone-exists-p clone-dir)        (progn
          (unless (yes-or-no-p
                   (format "Clone exists at %s.  Fetch and reset to origin (discards local work)? "
                           clone-dir))
            (user-error "Aborted"))
          (message "Fetching origin...")
          (let ((default-directory clone-dir))
            (unless (zerop (magit-call-git "fetch" "origin"))
              (user-error "git fetch failed.  See *magit-process* buffer ($ in Magit).")))
          (let ((default-branch (or (deb-packaging-propagate--default-branch clone-dir)
                                    "main")))
            (let ((default-directory clone-dir))
              (magit-call-git "checkout" default-branch)
              (magit-call-git "reset" "--hard"
                               (format "origin/%s" default-branch)))))
      (message "Cloning %s..." vcs-url)
      (let ((default-directory (file-name-directory clone-dir)))
        (unless (zerop (magit-call-git "clone" vcs-url
                                       (file-name-nondirectory
                                        (directory-file-name clone-dir))))
          (user-error "git clone failed.  See *magit-process* buffer ($ in Magit)."))))
    ;; Store source-dir so apply can find it later.
    (let ((default-directory clone-dir))
      (magit-call-git "config" "deb-packaging.source-dir" pkg-dir))
    (let* ((branches (deb-packaging-propagate--remote-branches clone-dir))
           (default-br (or (deb-packaging-propagate--default-branch clone-dir)
                           "main"))
           (base (if branches
                     (magit-completing-read "Base branch: " branches nil t default-br)
                   default-br))
           (branch (read-string "Branch name: "
                                 (format "wip/propagate-%s"
                                         (or pkg-name "fix")))))
      (let ((default-directory clone-dir))
        (magit-call-git "checkout" base))
      ;; Recreate work branch fresh.
      (let ((default-directory clone-dir))
        (when (deb-packaging-propagate--git-quiet clone-dir "rev-parse" "--verify" branch)
          (magit-call-git "branch" "-D" branch))
        (unless (zerop (magit-call-git "checkout" "-b" branch))
          (user-error "Failed to create branch %s" branch))))
    (let ((personal-url (deb-packaging-propagate--salsa-personal-url pkg-name)))
      (when personal-url
        (let ((default-directory clone-dir))
          (when (deb-packaging-propagate--git-quiet clone-dir "config" "remote.personal.url")
            (magit-call-git "remote" "remove" "personal"))
          (if (zerop (magit-call-git "remote" "add" "personal" personal-url))
              (if (deb-packaging-propagate--fork-exists-p personal-url)
                  (message "Personal fork ready at %s" personal-url)
                (let ((fork-url (deb-packaging-propagate--fork-url vcs-url)))
                  (message "Personal fork not found.  Fork at: %s"
                           (or fork-url "salsa.debian.org"))))
            (message "Could not add personal remote (non-fatal)")))))
    (magit-status-setup-buffer clone-dir)
    (when (derived-mode-p 'magit-status-mode)
      (deb-packaging-propagate-clone-mode +1))
    (message "Clone ready at %s.  Press P to apply a fix item."
             clone-dir)))

;;;###autoload
(defun deb-packaging-propagate-apply ()
  "Pick a fix item and open the apply transient.
Reads source-dir from the clone's git config, prompts for a patch or
commit (marking already-applied items), and opens the apply transient."
  (interactive)
  (let* ((clone-dir (or (magit-toplevel)
                        (user-error "Not in a git repository")))
         (item (deb-packaging-propagate--read-fix-source-one clone-dir))
         (patch-file (deb-packaging-propagate--produce-patch-file item)))
    (setq deb-packaging-propagate--pending-patch patch-file)
    (call-interactively #'deb-packaging-propagate-apply-patch)
    (message "Patch ready: %s.  Toggle flags and press `a' to apply."
             (deb-packaging-propagate--item-description item))))

;;; Minor mode

(defvar-keymap deb-packaging-propagate-clone-mode-map
  :doc "Keymap for `deb-packaging-propagate-clone-mode'."
  "P" #'deb-packaging-propagate-apply)

(define-minor-mode deb-packaging-propagate-clone-mode
  "Minor mode for Magit status buffers backed by a propagate clone.
Binds `P' to pick a fix item and open the apply transient."
  :lighter deb-packaging-config-propagate-clone-mode-lighter
  :keymap deb-packaging-propagate-clone-mode-map
  (if deb-packaging-propagate-clone-mode
      (setq header-line-format
            (format "Press [%s] to apply a propagate fix item"
                    (propertize "P" 'face 'bold)))
    (setq header-line-format nil)))

;;; Transient

;;;###autoload(autoload 'deb-packaging-propagate-transient "deb-packaging-propagate" nil t)
(transient-define-prefix deb-packaging-propagate-transient ()
  "Propagate fixes to Debian and upstream."
  [:description "Propagate fixes across distros"]
  ["Actions"
   ("e" "Export .patch (upstream)..."   deb-packaging-propagate-export-patch)
   ("d" "Prepare Debian clone (salsa)..." deb-packaging-propagate-clone)
   ("P" "Apply item to existing clone..." deb-packaging-propagate-apply)]
  ["Navigation"
   ("q" "Quit" transient-quit-one)])

(provide 'deb-packaging-propagate)
;;; deb-packaging-propagate.el ends here
