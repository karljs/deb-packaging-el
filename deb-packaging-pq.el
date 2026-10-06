;;; deb-packaging-pq.el --- Gbp pq patch-queue management -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Maintain Debian quilt patches as git commits via `gbp pq'.
;;
;; import: quilt patches -> patch-queue/<branch> (switches to it).
;; export: patch-queue -> debian/patches/ (commits, drops the branch).
;; Patch application is left to Magit. Requires 3.0 (quilt) format and git.

;;; Code:

(require 'compile)
(require 'magit)
(require 'deb-packaging-detect)
(require 'deb-packaging-commands)

;;; Pre-flight checks

(defun deb-packaging-pq--ensure-quilt-repo ()
  "Signal `user-error' unless in a 3.0 (quilt) git repository."
  (unless (magit-toplevel)
    (user-error "Not in a git repository"))
  (unless (string= (or (deb-packaging-detect--source-format) "") "3.0 (quilt)")
    (user-error "Source format is not 3.0 (quilt); gbp pq requires it")))

;;; Branch state

(defun deb-packaging-pq--patch-queue-branch (&optional branch)
  "Return the patch-queue branch name for BRANCH (default: current).
Returns nil if BRANCH itself is a patch-queue branch."
  (let ((br (or branch (magit-get-current-branch))))
    (when (and br (not (string-prefix-p "patch-queue/" br)))
      (format "patch-queue/%s" br))))

(defun deb-packaging-pq--on-pq-branch-p ()
  "Return non-nil if currently on a patch-queue branch."
  (let ((branch (magit-get-current-branch)))
    (and branch (string-prefix-p "patch-queue/" branch))))

(defun deb-packaging-pq--state ()
  "Return a plist describing the patch-queue state.
Keys: :on-pq-p, :branch, :pq-branch (nil if already on one), :exists-p."
  (let* ((branch (magit-get-current-branch))
         (on-pq (and branch (string-prefix-p "patch-queue/" branch)))
         (pq-branch (unless on-pq
                      (deb-packaging-pq--patch-queue-branch branch)))
         (exists (or on-pq
                     (and pq-branch
                          (magit-ref-p pq-branch)))))
    (list :on-pq-p on-pq
          :branch branch
          :pq-branch (if on-pq branch pq-branch)
          :exists-p exists)))

;;; Commands

;;;###autoload
(defun deb-packaging-pq-import ()
  "Create a patch-queue branch from quilt patches in debian/patches/.
Runs `gbp pq import' (switches to patch-queue/<branch>) and opens
`magit-status' on success."
  (interactive)
  (deb-packaging-pq--ensure-quilt-repo)
  (let ((dir (magit-toplevel)))
    (deb-packaging-commands--after-compile
     (deb-packaging-commands--compile "gbp pq import")
     (lambda ()
       (deb-packaging-commands--notify-status-refresh)
       (when (deb-packaging-pq--on-pq-branch-p)
         (magit-status-setup-buffer dir)
         (message "Each patch is now a commit.  Edit with Magit; finish with a, then x."))))))

;;;###autoload
(defun deb-packaging-pq-switch ()
  "Toggle between the packaging branch and its patch-queue branch."
  (interactive)
  (deb-packaging-pq--ensure-quilt-repo)
  (deb-packaging-commands--after-compile
   (deb-packaging-commands--compile "gbp pq switch")
   (lambda ()
     (deb-packaging-commands--notify-status-refresh)
     (let ((branch (magit-get-current-branch)))
       (message "On branch: %s" (or branch "detached"))))))

;;;###autoload
(defun deb-packaging-pq-rebase ()
  "Rebase the patch-queue branch against the current branch HEAD."
  (interactive)
  (deb-packaging-pq--ensure-quilt-repo)
  (deb-packaging-commands--after-compile
   (deb-packaging-commands--compile "gbp pq rebase")
   #'deb-packaging-commands--notify-status-refresh))

;;;###autoload
(defun deb-packaging-pq-export ()
  "Export the patch-queue branch back to debian/patches/.
Runs `gbp pq export --commit --drop': writes patches, commits on the
packaging branch, deletes the patch-queue branch."
  (interactive)
  (deb-packaging-pq--ensure-quilt-repo)
  (unless (deb-packaging-pq--on-pq-branch-p)
    (user-error "Not on a patch-queue branch; switch first"))
  (deb-packaging-commands--after-compile
   (deb-packaging-commands--compile "gbp pq export --commit --drop")
   (lambda ()
     (deb-packaging-commands--notify-status-refresh)
     (message "Exported patches to debian/patches/"))))

;;;###autoload
(defun deb-packaging-pq-drop ()
  "Delete the patch-queue branch without exporting.
Useful to abort an edit session and start over.  Asks first: commits
on the branch that were never exported are lost."
  (interactive)
  (deb-packaging-pq--ensure-quilt-repo)
  (let* ((state (deb-packaging-pq--state))
         (exists (plist-get state :exists-p))
         (pq-branch (plist-get state :pq-branch)))
    (unless exists
      (user-error "No patch-queue branch to drop"))
    (when (y-or-n-p
           (if pq-branch
               (format "Delete patch-queue branch %s (unexported commits will be lost)? "
                       pq-branch)
             "Delete the patch-queue branch (unexported commits will be lost)? "))
      (deb-packaging-commands--after-compile
       (deb-packaging-commands--compile "gbp pq drop")
       #'deb-packaging-commands--notify-status-refresh))))

(provide 'deb-packaging-pq)
;;; deb-packaging-pq.el ends here
