;;; deb-packaging-pq.el --- gbp pq patch-queue management -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging

;;; Commentary:

;; Maintain Debian quilt patches as git commits via `gbp pq'.
;;
;; import: quilt patches -> patch-queue/<branch> (switches to it).
;; export: patch-queue -> debian/patches/ (commits, drops the branch).
;; Patch application is left to Magit. Requires 3.0 (quilt) format and git.

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'magit)
(require 'transient)
(require 'deb-packaging-detect)
(require 'deb-packaging-commands)

;;; Pre-flight checks

(defun deb-packaging-pq--ensure-quilt-repo ()
  "Signal `user-error' unless in a 3.0 (quilt) git repository."
  (unless (magit-toplevel)
    (user-error "Not in a git repository"))
  (unless (string= (or (deb-packaging--source-format) "") "3.0 (quilt)")
    (user-error "Source format is not 3.0 (quilt); gbp pq requires it")))

;;; Branch state

(defun deb-packaging-pq--current-branch ()
  "Return the current git branch name, or nil if detached."
  (let ((branch (magit-git-string "branch" "--show-current")))
    (and branch (not (string-empty-p branch)) branch)))

(defun deb-packaging-pq--patch-queue-branch (&optional branch)
  "Return the patch-queue branch name for BRANCH (default: current).
Returns nil if BRANCH itself is a patch-queue branch."
  (let ((br (or branch (deb-packaging-pq--current-branch))))
    (when (and br (not (string-prefix-p "patch-queue/" br)))
      (format "patch-queue/%s" br))))

(defun deb-packaging-pq--branch-exists-p (branch)
  "Return non-nil if BRANCH exists in the local repo."
  (and branch
       (string-match-p (regexp-quote branch)
                       (or (magit-git-string "branch" "--list" "--all"
                                             (format "*%s*" branch))
                           ""))))

(defun deb-packaging-pq--on-pq-branch-p ()
  "Return non-nil if currently on a patch-queue branch."
  (let ((branch (deb-packaging-pq--current-branch)))
    (and branch (string-prefix-p "patch-queue/" branch))))

(defun deb-packaging-pq--state ()
  "Return a plist describing the patch-queue state.
Keys: :on-pq-p, :branch, :pq-branch (nil if already on one), :exists-p."
  (let* ((branch (deb-packaging-pq--current-branch))
         (on-pq (and branch (string-prefix-p "patch-queue/" branch)))
         (pq-branch (unless on-pq
                      (deb-packaging-pq--patch-queue-branch branch)))
         (exists (or on-pq
                     (and pq-branch
                          (deb-packaging-pq--branch-exists-p pq-branch)))))
    (list :on-pq-p on-pq
          :branch branch
          :pq-branch (if on-pq branch pq-branch)
          :exists-p exists)))

;;; Compilation follow-up

(defun deb-packaging-pq--after-compile (buf action)
  "Call ACTION (no args) when the compilation in BUF finishes successfully.
One-shot `compilation-finish-functions' hook; skips ACTION on failure
since the buffer already shows the error."
  (letrec ((hook (lambda (finished-buf msg)
                   (when (eq finished-buf buf)
                     (remove-hook 'compilation-finish-functions hook)
                     (when (string-match-p "finished" msg)
                       (funcall action))))))
    (add-hook 'compilation-finish-functions hook)))

;;; Commands

;;;###autoload
(defun deb-packaging-pq-import ()
  "Create a patch-queue branch from quilt patches in debian/patches/.
Runs `gbp pq import' (switches to patch-queue/<branch>) and opens
`magit-status' on success."
  (interactive)
  (deb-packaging-pq--ensure-quilt-repo)
  (let ((dir (magit-toplevel)))
    (deb-packaging-pq--after-compile
     (compile "gbp pq import")
     (lambda ()
       (when (deb-packaging-pq--on-pq-branch-p)
         (magit-status-setup-buffer dir)
         (message "On patch-queue branch.  Edit with Magit, then run export when ready."))))))

;;;###autoload
(defun deb-packaging-pq-switch ()
  "Toggle between the packaging branch and its patch-queue branch."
  (interactive)
  (deb-packaging-pq--ensure-quilt-repo)
  (deb-packaging-pq--after-compile
   (compile "gbp pq switch")
   (lambda ()
     (let ((branch (deb-packaging-pq--current-branch)))
       (message "On branch: %s" (or branch "detached"))))))

;;;###autoload
(defun deb-packaging-pq-rebase ()
  "Rebase the patch-queue branch against the current branch HEAD."
  (interactive)
  (deb-packaging-pq--ensure-quilt-repo)
  (compile "gbp pq rebase"))

;;;###autoload
(defun deb-packaging-pq-export ()
  "Export the patch-queue branch back to debian/patches/.
Runs `gbp pq export --commit --drop': writes patches, commits on the
packaging branch, deletes the patch-queue branch."
  (interactive)
  (deb-packaging-pq--ensure-quilt-repo)
  (unless (deb-packaging-pq--on-pq-branch-p)
    (user-error "Not on a patch-queue branch; switch first"))
  (deb-packaging-pq--after-compile
   (compile "gbp pq export --commit --drop")
   (lambda ()
     (deb-packaging--notify-status-refresh)
     (message "Exported patches to debian/patches/"))))

;;;###autoload
(defun deb-packaging-pq-drop ()
  "Delete the patch-queue branch without exporting.
Useful to abort an edit session and start over."
  (interactive)
  (deb-packaging-pq--ensure-quilt-repo)
  (deb-packaging-pq--after-compile
   (compile "gbp pq drop")
   #'deb-packaging--notify-status-refresh))

;;; Transient

(defun deb-packaging-pq--transient-header ()
  "Return a header string showing the current branch and patch-queue state."
  (let* ((state (deb-packaging-pq--state))
         (branch (plist-get state :branch))
         (on-pq (plist-get state :on-pq-p))
         (exists (plist-get state :exists-p)))
    (format "gbp pq: patch queue\n%s"
            (cond
             (on-pq
              (format "On patch-queue branch: %s\nExport when ready."
                      (or branch "detached")))
             (exists
              (format "Packaging branch: %s\nPatch-queue ready: switch to edit."
                      (or branch "detached")))
             (t
              (format "Packaging branch: %s\nNo patch-queue: import to start."
                      (or branch "detached")))))))

;;;###autoload(autoload 'deb-packaging-pq-transient "deb-packaging-pq" nil t)
(transient-define-prefix deb-packaging-pq-transient ()
  "Manage Debian quilt patches as git commits via gbp pq."
  [:description deb-packaging-pq--transient-header]
  ["Patch queue"
   ("i" "Import (quilt -> patch-queue)"  deb-packaging-pq-import)
   ("s" "Switch (toggle)"                deb-packaging-pq-switch)
   ("r" "Rebase onto HEAD"               deb-packaging-pq-rebase)
   ("e" "Export (-> debian/patches)"     deb-packaging-pq-export)
   ("d" "Drop (delete, no export)"       deb-packaging-pq-drop)]
  ["Navigation"
   ("q" "Quit" transient-quit-one)])

(provide 'deb-packaging-pq)
;;; deb-packaging-pq.el ends here
