;;; deb-packaging-test-clone.el --- git-ubuntu clone tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for deb-packaging-clone.el.

;;; Code:

(require 'ert)
(require 'deb-packaging-test)
(require 'deb-packaging-clone)

(defmacro deb-packaging-test-clone--with-temp-dir (&rest body)
  "Create a temp directory, bind it to `root', run BODY, then clean up."
  (declare (indent 0) (debug (body)))
  `(let ((root (make-temp-file "deb-clone-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory root t))))

(defun deb-packaging-test-clone--wait-for-exit (proc)
  "Block until PROC exits."
  (while (memq (process-status proc) '(run stop open listen))
    (accept-process-output nil 0.05)))

;;; Sentinel

(ert-deftest deb-packaging-test-clone/sentinel-opens-status-on-success ()
  (let (opened)
    (cl-letf (((symbol-function 'deb-packaging-status)
               (lambda () (setq opened default-directory)))
              ((symbol-function 'magit-process-sentinel) #'ignore))
      (let ((proc (start-process "deb-clone-test" nil "true")))
        (deb-packaging-test-clone--wait-for-exit proc)
        (funcall (deb-packaging-clone--sentinel "/tmp/xyz") proc "finished\n")
        (should (equal opened "/tmp/xyz/"))))))

(ert-deftest deb-packaging-test-clone/sentinel-ignores-failure ()
  (let (opened)
    (cl-letf (((symbol-function 'deb-packaging-status)
               (lambda () (setq opened t)))
              ((symbol-function 'magit-process-sentinel) #'ignore))
      (let ((proc (start-process "deb-clone-test" nil "false")))
        (deb-packaging-test-clone--wait-for-exit proc)
        (funcall (deb-packaging-clone--sentinel "/tmp/xyz")
                 proc "exited abnormally with code 1\n")
        (should-not opened)))))

;;; Entry point

(ert-deftest deb-packaging-test-clone/empty-package-errors ()
  (should-error (deb-packaging-clone-git-ubuntu "" "/tmp") :type 'user-error))

(ert-deftest deb-packaging-test-clone/missing-git-ubuntu-errors ()
  (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
    (should-error (deb-packaging-clone-git-ubuntu "foo" "/tmp")
                  :type 'user-error)))

(ert-deftest deb-packaging-test-clone/existing-tree-opens-status ()
  (deb-packaging-test--with-package-tree
      '(:name "foo" :version "1.0-1")
    (let (opened)
      (cl-letf (((symbol-function 'executable-find) (lambda (_) "git-ubuntu"))
                ((symbol-function 'deb-packaging-status)
                 (lambda () (setq opened default-directory)))
                ((symbol-function 'magit-run-git-async)
                 (lambda (&rest _) (error "must not clone"))))
        (deb-packaging-clone-git-ubuntu "foo" pkg-parent-dir)
        (should (equal (directory-file-name opened)
                       (directory-file-name pkg-dir)))))))

(ert-deftest deb-packaging-test-clone/existing-non-package-errors ()
  (deb-packaging-test-clone--with-temp-dir
    (make-directory (expand-file-name "foo" root))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "git-ubuntu")))
      (should-error (deb-packaging-clone-git-ubuntu "foo" root)
                    :type 'user-error))))

(ert-deftest deb-packaging-test-clone/fresh-clone-runs-async ()
  (deb-packaging-test-clone--with-temp-dir
    (let (called sentinel)
      (cl-letf (((symbol-function 'executable-find) (lambda (_) "git-ubuntu"))
                ((symbol-function 'magit-run-git-async)
                 (lambda (&rest args)
                   (setq called args)
                   (setq magit-this-process
                         (start-process "deb-clone-test" nil "true"))))
                ((symbol-function 'set-process-sentinel)
                 (lambda (_proc s) (setq sentinel s)))
                ((symbol-function 'deb-packaging-status) #'ignore))
        (deb-packaging-clone-git-ubuntu "foo" root)
        (should (equal called
                       (list "ubuntu" "clone" "foo"
                             (expand-file-name "foo" root))))
         (should (functionp sentinel))))))

(ert-deftest deb-packaging-test-clone/gbp-clone-preserves-requested-branch ()
  (deb-packaging-test-clone--with-temp-dir
    (let* ((target (expand-file-name "foo" root))
           props)
      (cl-letf (((symbol-function 'executable-find) (lambda (_) "gbp"))
                ((symbol-function 'make-process)
                 (lambda (&rest args) (setq props args) 'gbp-process))
                ((symbol-function 'process-put) #'ignore)
                ((symbol-function 'deb-packaging-display-buffer) #'ignore))
        (deb-packaging-clone-gbp "https://example.test/foo.git"
                                 target
                                 "debian/noble"))
      (unwind-protect
          (should (equal (plist-get props :command)
                         (list "gbp" "clone" "--debian-branch=debian/noble"
                               "https://example.test/foo.git" target)))
        (when (buffer-live-p (plist-get props :buffer))
          (kill-buffer (plist-get props :buffer)))))))

(ert-deftest deb-packaging-test-clone/gbp-clone-follows-vcs-git-branch ()
  (deb-packaging-test--with-temp-git-repo
    (deb-packaging-test--build-tree
     repo-dir repo-dir
     '(:name "foo" :version "1.0-1"
       :vcs-git "https://example.test/foo.git -b debian/noble"))
    (deb-packaging-test--git repo-dir "add" "debian")
    (deb-packaging-test--git repo-dir "commit" "-q" "-m" "packaging")
    (deb-packaging-test--git repo-dir "remote" "add" "origin" repo-dir)
    (deb-packaging-test--git repo-dir "update-ref"
                             "refs/remotes/origin/debian/noble" "HEAD")
    (deb-packaging-clone--select-vcs-branch repo-dir)
    (let ((default-directory repo-dir))
      (should (equal (magit-get-current-branch) "debian/noble")))))

(provide 'deb-packaging-test-clone)
;;; deb-packaging-test-clone.el ends here
