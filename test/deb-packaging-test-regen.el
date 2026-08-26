;;; deb-packaging-test-regen.el --- Template regeneration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for deb-packaging-regen.el: store round-trip, isolation,
;; default-command detection, and the regenerate command dispatch.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'deb-packaging-regen)
(require 'deb-packaging-commands)
(require 'deb-packaging-config)
(require 'deb-packaging-test)

(defmacro deb-packaging-test-regen--with-cache (&rest body)
  "Run BODY with the cache dir set to a temp directory."
  (declare (indent 0) (debug (body)))
  (let ((tmp (make-symbol "tmp")))
    `(let* ((,tmp (make-temp-file "deb-regen-test-" t))
            (process-environment (cons (format "XDG_CACHE_HOME=%s" ,tmp)
                                       process-environment)))
       (unwind-protect
           ,@body
         (delete-directory ,tmp t)))))

;;; Store

(ert-deftest deb-packaging-test-regen/round-trip ()
  "Save then load returns the same command, env vars and all."
  (deb-packaging-test-regen--with-cache
    (deb-packaging-regen-save "llvm-toolchain-19" "noble"
                              "LLVM_VERSION=19 make -f debian/rules control")
    (should (equal (deb-packaging-regen-load "llvm-toolchain-19" "noble")
                   "LLVM_VERSION=19 make -f debian/rules control"))))

(ert-deftest deb-packaging-test-regen/missing-file-returns-nil ()
  (deb-packaging-test-regen--with-cache
    (should (null (deb-packaging-regen-load "nonsuch" "noble")))))

(ert-deftest deb-packaging-test-regen/per-distro-isolation ()
  "Different distros get different stored commands."
  (deb-packaging-test-regen--with-cache
    (deb-packaging-regen-save "mypkg" "noble" "make -f debian/rules control")
    (should (null (deb-packaging-regen-load "mypkg" "jammy")))
    (deb-packaging-regen-save "mypkg" "jammy" "OTHER=1 ./debian/regen.sh")
    (should (equal (deb-packaging-regen-load "mypkg" "noble")
                   "make -f debian/rules control"))
    (should (equal (deb-packaging-regen-load "mypkg" "jammy")
                   "OTHER=1 ./debian/regen.sh"))))

(ert-deftest deb-packaging-test-regen/overwrite ()
  (deb-packaging-test-regen--with-cache
    (deb-packaging-regen-save "mypkg" "noble" "cmd-one")
    (deb-packaging-regen-save "mypkg" "noble" "cmd-two")
    (should (equal (deb-packaging-regen-load "mypkg" "noble") "cmd-two"))))

;;; Default command

(ert-deftest deb-packaging-test-regen/default-command-with-control-in ()
  "Prefill make -f debian/rules control when debian/control.in exists."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble")
    (deb-packaging-test--write-file
     (expand-file-name "debian/control.in" pkg-dir) "")
    (should (equal (deb-packaging-regen--default-command pkg-dir)
                   "make -f debian/rules control"))))

(ert-deftest deb-packaging-test-regen/default-command-without-control-in ()
  "No prefill when there is no debian/control.in."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble")
    (should (null (deb-packaging-regen--default-command pkg-dir)))))

;;; Command dispatch

(defun deb-packaging-test-regen--mock-read (_prompt &optional default _hist)
  "Mock `read-shell-command' returning DEFAULT.  _PROMPT, _HIST ignored."
  (or default ""))

(ert-deftest deb-packaging-test-regen/command-dispatch ()
  "Runs the prompt-entered command verbatim via sh -c in the package dir."
  (deb-packaging-test-regen--with-cache
    (deb-packaging-test--with-package-tree
        '(:name "mypkg" :version "1.0-1" :distro "noble")
      (deb-packaging-test--write-file
       (expand-file-name "debian/control.in" pkg-dir) "")
      (let ((captured nil))
        (cl-letf (((symbol-function 'read-shell-command)
                   #'deb-packaging-test-regen--mock-read)
                  ((symbol-function 'deb-packaging-commands--run-command)
                   (lambda (_name args &optional dir key _buffer-dir)
                     (setq captured (list args dir key)))))
          (deb-packaging-commands-regenerate))
        (should (equal (nth 0 captured)
                       '("sh" "-c" "make -f debian/rules control")))
        (should (equal (nth 1 captured) pkg-dir))
        (should (eq (nth 2 captured) 'regen))
        ;; Prompt default was the control.in prefill and the choice stuck.
        (should (equal (deb-packaging-regen-load "mypkg" "noble")
                       "make -f debian/rules control"))))))

(ert-deftest deb-packaging-test-regen/command-prompt-prefills-stored ()
  "The prompt default is the stored command once one exists."
  (deb-packaging-test-regen--with-cache
    (deb-packaging-test--with-package-tree
        '(:name "mypkg" :version "1.0-1" :distro "noble")
      (deb-packaging-regen-save "mypkg" "noble" "OTHER=1 ./debian/regen.sh")
      (let ((captured nil)
            (seen-default nil))
        (cl-letf (((symbol-function 'read-shell-command)
                   (lambda (_prompt &optional default _hist)
                     (setq seen-default default)
                     (or default "")))
                  ((symbol-function 'deb-packaging-commands--run-command)
                   (lambda (_name args &optional dir key _buffer-dir)
                     (setq captured (list args dir key)))))
          (deb-packaging-commands-regenerate))
        (should (equal seen-default "OTHER=1 ./debian/regen.sh"))
        (should (equal (nth 0 captured)
                       '("sh" "-c" "OTHER=1 ./debian/regen.sh")))))))

(ert-deftest deb-packaging-test-regen/command-empty-input-errors ()
  "Empty prompt input signals `user-error' and runs nothing."
  (deb-packaging-test-regen--with-cache
    (deb-packaging-test--with-package-tree
        '(:name "mypkg" :version "1.0-1" :distro "noble")
      (let ((ran nil))
        (cl-letf (((symbol-function 'read-shell-command)
                   (lambda (&rest _) ""))
                  ((symbol-function 'deb-packaging-commands--run-command)
                   (lambda (&rest _) (setq ran t))))
          (should-error (deb-packaging-commands-regenerate)
                        :type 'user-error))
        (should (null ran))))))

(ert-deftest deb-packaging-test-regen/command-outside-package-tree ()
  "Signals `user-error' when not under a debian/changelog."
  (let ((default-directory temporary-file-directory))
    (cl-letf (((symbol-function 'read-shell-command)
               (lambda (&rest _) (error "should not prompt"))))
      (should-error (deb-packaging-commands-regenerate)
                    :type 'user-error))))

(provide 'deb-packaging-test-regen)
;;; deb-packaging-test-regen.el ends here
