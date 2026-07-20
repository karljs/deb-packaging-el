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

;;; Pure helpers

(ert-deftest deb-packaging-test-clone/target-dir ()
  (should (string= (deb-packaging-clone--target-dir "/tmp/x/" "foo")
                   "/tmp/x/foo"))
  (should (string= (deb-packaging-clone--target-dir "/tmp/x" "foo")
                   "/tmp/x/foo")))

(ert-deftest deb-packaging-test-clone/package-dir-p ()
  (deb-packaging-test--with-package-tree
      '(:name "foo" :version "1.0-1")
    (should (deb-packaging-clone--package-dir-p pkg-dir))
    (should-not (deb-packaging-clone--package-dir-p pkg-parent-dir))))

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

(provide 'deb-packaging-test-clone)
;;; deb-packaging-test-clone.el ends here
