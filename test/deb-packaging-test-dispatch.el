;;; deb-packaging-test-dispatch.el --- Dispatch entry-point tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for the `deb-packaging-dispatch' wrapper: outside a package
;; tree it routes through `deb-packaging-status' (which prompts); inside
;; one it opens the transient directly.

;;; Code:

(require 'ert)
(require 'deb-packaging-test)
(require 'deb-packaging)

(ert-deftest deb-packaging-test-dispatch/inside-package-opens-transient ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (let (called)
      (cl-letf (((symbol-function 'deb-packaging-status)
                 (lambda () (push 'status called)))
                ((symbol-function 'deb-packaging-dispatch-transient)
                 (lambda () (push 'transient called))))
        (deb-packaging-dispatch))
      (should (equal called '(transient))))))

(ert-deftest deb-packaging-test-dispatch/outside-package-prompts-via-status ()
  (let ((tmp (make-temp-file "deb-pkg-test-" t)))
    (unwind-protect
        (let ((default-directory tmp)
              (called nil))
          (cl-letf (((symbol-function 'deb-packaging-status)
                     (lambda () (push 'status called)))
                    ((symbol-function 'deb-packaging-dispatch-transient)
                     (lambda () (push 'transient called))))
            (deb-packaging-dispatch))
          (should (equal (nreverse called) '(status transient))))
      (delete-directory tmp t))))

(provide 'deb-packaging-test-dispatch)
;;; deb-packaging-test-dispatch.el ends here
