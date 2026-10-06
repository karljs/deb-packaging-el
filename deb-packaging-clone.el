;;; deb-packaging-clone.el --- Git-ubuntu clone entry point -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Clone Ubuntu packages with git-ubuntu or packaging repositories with gbp.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit)
(require 'deb-packaging-detect)
(require 'deb-packaging-status)
(require 'deb-packaging-display)

(defvar deb-packaging-clone--last-parent nil
  "Parent directory of the last clone, offered as the next default.")

(defun deb-packaging-clone--read-parent ()
  "Read the directory to clone into, defaulting to the last one used."
  (setq deb-packaging-clone--last-parent
        (read-directory-name "Clone into: "
                             (or deb-packaging-clone--last-parent default-directory)
                             nil t)))

(defun deb-packaging-clone--url-from-kill-ring ()
  "Return the most recent URL-looking kill, or nil."
  (cl-find-if (lambda (k) (string-match-p "\\`\\(https?\\|git\\|ssh\\)://\\|\\`git@" k))
              (mapcar #'substring-no-properties (seq-take kill-ring 5))))

(defun deb-packaging-clone--open-status (dir)
  "Open `deb-packaging-status' with DIR as the package directory."
  (let ((default-directory (file-name-as-directory dir)))
    (deb-packaging-status)))

(defun deb-packaging-clone--sentinel (target)
  "Return a process sentinel that opens the status buffer for TARGET.
Failures are reported by `magit-process-sentinel' in the echo area (with
'$' pointing at *magit-process*); status opens only on exit status 0."
  (lambda (process event)
    (when (memq (process-status process) '(exit signal))
      (magit-process-sentinel process event))
    (when (and (eq (process-status process) 'exit)
               (zerop (process-exit-status process)))
      (deb-packaging-clone--open-status target))))

(defun deb-packaging-clone--async (package target)
  "Clone PACKAGE into TARGET asynchronously via Magit, opening status on success.
Runs `git ubuntu clone'; output goes to *magit-process*."
  (let ((default-directory (file-name-directory (directory-file-name target))))
    (magit-run-git-async "ubuntu" "clone" package target))
  ;; Don't refresh the buffer the command was called from.
  (process-put magit-this-process 'inhibit-refresh t)
  (set-process-sentinel
   magit-this-process
   (deb-packaging-clone--sentinel target)))

;;;###autoload
(defun deb-packaging-clone-git-ubuntu (package parent)
  "Clone Ubuntu source PACKAGE into PARENT with git-ubuntu, then open status.
The clone runs asynchronously through Magit's process machinery.  On
success `deb-packaging-status' opens in the new clone.  If PARENT/PACKAGE
already contains a package tree, skip the clone and open status there."
  (interactive
   (let ((default (thing-at-point 'symbol t)))
     (list (read-string (if default (format "Ubuntu source package [%s]: " default)
                          "Ubuntu source package: ")
                        nil nil default)
           (deb-packaging-clone--read-parent))))
  (when (string-empty-p package)
    (user-error "No package given"))
  (unless (executable-find "git-ubuntu")
    (user-error "git-ubuntu not found in `exec-path'"))
  (let ((target (expand-file-name package parent)))
    (cond
     ((file-exists-p (expand-file-name "debian/changelog" target))
      (message "Already cloned at %s" target)
      (deb-packaging-clone--open-status target))
     ((file-exists-p target)
      (user-error "%s exists and is not a package tree" target))
     (t
      (deb-packaging-clone--async package target)))))

(defun deb-packaging-clone--select-vcs-branch (pkg-dir)
  "Switch PKG-DIR to its declared Vcs-Git branch when it exists remotely."
  (let* ((info (deb-packaging-detect--vcs-git-info pkg-dir))
         (branch (cadr info))
         (default-directory pkg-dir))
    (when (and branch
               (not (equal branch (magit-get-current-branch)))
               (zerop (call-process "git" nil nil nil "check-ref-format"
                                    "--branch" branch))
               (zerop (call-process "git" nil nil nil "show-ref" "--verify"
                                    "--quiet"
                                    (concat "refs/remotes/origin/" branch))))
      (let ((local (zerop (call-process "git" nil nil nil "show-ref" "--verify"
                                        "--quiet"
                                        (concat "refs/heads/" branch)))))
        (unless (zerop (if local
                           (call-process "git" nil nil nil "switch" branch)
                         (call-process "git" nil nil nil "switch" "--track" "-c"
                                       branch (concat "origin/" branch))))
          (message "Could not switch to Vcs-Git branch %s" branch))))))

(defun deb-packaging-clone--gbp-sentinel (target output &optional explicit-branch)
  "Return a sentinel that opens TARGET after a successful gbp clone.
Without EXPLICIT-BRANCH, switch to the Vcs-Git -b branch first."
  (lambda (proc _event)
    (when (memq (process-status proc) '(exit signal))
      (unwind-protect
          (if (and (eq (process-status proc) 'exit)
                   (zerop (process-exit-status proc)))
              (if (file-exists-p (expand-file-name "debian/changelog" target))
                  (progn
                    (unless explicit-branch
                      (deb-packaging-clone--select-vcs-branch target))
                    (deb-packaging-clone--open-status target))
                (message "gbp clone completed, but no debian/changelog was found"))
            (message "gbp clone failed; see %s" (buffer-name output)))
      (when (buffer-live-p output)
        (with-current-buffer output
          (setq deb-packaging-display-category 'output)))))))

;;;###autoload
(defun deb-packaging-clone-gbp (repository target &optional debian-branch)
  "Clone REPOSITORY with gbp into TARGET.
When DEBIAN-BRANCH is non-empty, ask gbp to track that packaging branch.
After cloning, follow Vcs-Git -b when that remote branch exists."
  (interactive
   (let* ((url (read-string "Packaging repository URL: "
                            (deb-packaging-clone--url-from-kill-ring)))
          (name (file-name-base (directory-file-name url))))
     (list url
           (expand-file-name name (deb-packaging-clone--read-parent))
           (let ((branch (read-string "Packaging branch (blank for the repo default): ")))
             (unless (string-empty-p branch) branch)))))
  (when (string-empty-p repository)
    (user-error "No Git repository given"))
  (unless (executable-find "gbp")
    (user-error "gbp not found in `exec-path'"))
  (let ((target (expand-file-name target)))
    (when (file-exists-p target)
      (user-error "%s already exists" target))
    (let* ((default-directory (file-name-directory target))
           (output (generate-new-buffer "*gbp-clone*"))
           (args (append (list "gbp" "clone")
                         (when debian-branch
                           (list (concat "--debian-branch=" debian-branch)))
                         (list repository target))))
      (condition-case err
          (let ((proc (make-process
                       :name "gbp-clone"
                       :buffer output
                       :command args
                       :noquery t
                       :sentinel (deb-packaging-clone--gbp-sentinel
                                  target output debian-branch))))
            (process-put proc 'inhibit-refresh t)
            (deb-packaging-display-buffer output 'output)
            proc)
        (error
         (when (buffer-live-p output)
           (kill-buffer output))
         (signal (car err) (cdr err)))))))

(provide 'deb-packaging-clone)
;;; deb-packaging-clone.el ends here
