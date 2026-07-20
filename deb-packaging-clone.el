;;; deb-packaging-clone.el --- Git-ubuntu clone entry point -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Clone an Ubuntu source package with git-ubuntu and open the packaging
;; status buffer in the result.  The clone runs through Magit's async
;; process machinery: progress lands in *magit-process* and a sentinel
;; opens the status buffer on success.  Usable from any buffer since no
;; package context is needed up front.
;;
;; Entry point: `deb-packaging-clone-git-ubuntu'.

;;; Code:

(require 'subr-x)
(require 'magit)
(require 'deb-packaging-status)

(defun deb-packaging-clone--target-dir (parent package)
  "Return the clone target directory for PACKAGE under PARENT."
  (expand-file-name package parent))

(defun deb-packaging-clone--package-dir-p (dir)
  "Return non-nil if DIR contains debian/changelog."
  (file-exists-p (expand-file-name "debian/changelog" dir)))

(defun deb-packaging-clone--open-status (dir)
  "Open `deb-packaging-status' with DIR as the package directory."
  (let ((default-directory (file-name-as-directory dir)))
    (deb-packaging-status)))

(defun deb-packaging-clone--sentinel (target)
  "Return a process sentinel that opens the status buffer for TARGET.
Failures are delegated to `magit-process-sentinel' with raising enabled;
status opens only on exit status 0."
  (lambda (process event)
    (when (memq (process-status process) '(exit signal))
      (let ((magit-process-raise-error t))
        (magit-process-sentinel process event)))
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
   (list (read-string "Source package: ")
         (read-directory-name "Clone into parent directory: "
                              nil default-directory t)))
  (when (string-empty-p package)
    (user-error "No package given"))
  (unless (executable-find "git-ubuntu")
    (user-error "git-ubuntu not found in `exec-path'"))
  (let ((target (deb-packaging-clone--target-dir parent package)))
    (cond
     ((deb-packaging-clone--package-dir-p target)
      (message "Already cloned at %s" target)
      (deb-packaging-clone--open-status target))
     ((file-exists-p target)
      (user-error "%s exists and is not a package tree" target))
     (t
      (deb-packaging-clone--async package target)))))

(provide 'deb-packaging-clone)
;;; deb-packaging-clone.el ends here
