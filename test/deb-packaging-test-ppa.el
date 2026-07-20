;;; deb-packaging-test-ppa.el --- Upload PPA persistence tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for deb-packaging-ppa.el: round-trip, missing-file, isolation.

;;; Code:

(require 'ert)
(require 'deb-packaging-ppa)

(defmacro deb-packaging-test-ppa--with-cache (&rest body)
  "Run BODY with the cache dir set to a temp directory."
  (declare (indent 0) (debug (body)))
  (let ((tmp (make-symbol "tmp")))
    `(let* ((,tmp (make-temp-file "deb-ppa-test-" t))
            (process-environment (cons (format "XDG_CACHE_HOME=%s" ,tmp)
                                       process-environment)))
       (unwind-protect
           ,@body
         (delete-directory ,tmp t)))))

(ert-deftest deb-packaging-test-ppa/round-trip ()
  "Save then load returns the same PPA."
  (deb-packaging-test-ppa--with-cache
    (deb-packaging-ppa-save "mypkg" "noble" "ppa:me/x")
    (should (equal (deb-packaging-ppa-load "mypkg" "noble") "ppa:me/x"))))

(ert-deftest deb-packaging-test-ppa/missing-file-returns-nil ()
  (deb-packaging-test-ppa--with-cache
    (should (null (deb-packaging-ppa-load "nonsuch" "noble")))))

(ert-deftest deb-packaging-test-ppa/per-distro-isolation ()
  (deb-packaging-test-ppa--with-cache
    (deb-packaging-ppa-save "mypkg" "noble" "ppa:me/x")
    (should (null (deb-packaging-ppa-load "mypkg" "jammy")))))

(ert-deftest deb-packaging-test-ppa/overwrite ()
  (deb-packaging-test-ppa--with-cache
    (deb-packaging-ppa-save "mypkg" "noble" "ppa:me/x")
    (deb-packaging-ppa-save "mypkg" "noble" "ppa:me/y")
    (should (equal (deb-packaging-ppa-load "mypkg" "noble") "ppa:me/y"))))

(provide 'deb-packaging-test-ppa)
;;; deb-packaging-test-ppa.el ends here
