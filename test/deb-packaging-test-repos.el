;;; deb-packaging-test-repos.el --- Extra-repo persistence tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for deb-packaging-repos.el: round-trip, empty-set, missing-file.

;;; Code:

(require 'ert)
(require 'deb-packaging-repos)

(defmacro deb-packaging-test-repos--with-cache (&rest body)
  "Run BODY with the cache dir set to a temp directory."
  (declare (indent 0) (debug (body)))
  (let ((tmp (make-symbol "tmp")))
    `(let* ((,tmp (make-temp-file "deb-repos-test-" t))
            (process-environment (cons (format "XDG_CACHE_HOME=%s" ,tmp)
                                       process-environment)))
       (unwind-protect
           ,@body
         (delete-directory ,tmp t)))))

(ert-deftest deb-packaging-test-repos/round-trip ()
  "Save then load returns the same entries, in order."
  (deb-packaging-test-repos--with-cache
    (let ((entries '("ppa:me/x" "proposed" "deb http://example.com/ubuntu noble main")))
      (deb-packaging-repos-save "mypkg" "noble" entries)
      (should (equal (deb-packaging-repos-load "mypkg" "noble") entries)))))

(ert-deftest deb-packaging-test-repos/empty-set-persists ()
  "Saving an empty list writes a file that loads as nil."
  (deb-packaging-test-repos--with-cache
    (deb-packaging-repos-save "mypkg" "noble" '("ppa:me/x"))
    (deb-packaging-repos-save "mypkg" "noble" nil)
    (should (null (deb-packaging-repos-load "mypkg" "noble")))))

(ert-deftest deb-packaging-test-repos/missing-file-returns-unset ()
  "Loading when no file exists returns `unset', distinct from an empty set."
  (deb-packaging-test-repos--with-cache
    (should (eq (deb-packaging-repos-load "nonsuch" "noble") 'unset))))

(ert-deftest deb-packaging-test-repos/per-distro-isolation ()
  "Saving for noble does not affect jammy."
  (deb-packaging-test-repos--with-cache
    (deb-packaging-repos-save "mypkg" "noble" '("ppa:me/x"))
    (should (eq (deb-packaging-repos-load "mypkg" "jammy") 'unset))
    (should (equal (deb-packaging-repos-load "mypkg" "noble") '("ppa:me/x")))))

(provide 'deb-packaging-test-repos)
;;; deb-packaging-test-repos.el ends here
