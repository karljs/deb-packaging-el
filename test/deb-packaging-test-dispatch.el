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

;;; Mnemonic consistency

(defun deb-packaging-test-dispatch--layout (prefix)
  "Return PREFIX's parsed transient layout as printed text.
Mixes lists and vectors, so shape-independent string matching is used."
  (let ((layout (get prefix 'transient--layout)))
    (should layout)
    (prin1-to-string layout)))

(ert-deftest deb-packaging-test-dispatch/every-transient-binds-quit ()
  "Every package transient binds q to `transient-quit-one'.  q is not
a transient default (only C-g is); a menu without the binding leaves
the learned key dead."
  (dolist (prefix '(deb-packaging-dispatch-transient
                    deb-packaging-commands-source-build-transient
                    deb-packaging-binary-build-transient
                    deb-packaging-lint-transient
                    deb-packaging-test-transient
                    deb-packaging-upload-transient
                    deb-packaging-commands-clean-transient
                    deb-packaging-commands-reset-transient
                    deb-packaging-dev-transient
                    deb-packaging-pq-transient
                    deb-packaging-propagate-transient
                    deb-packaging-infra-dispatch))
    (let ((layout (deb-packaging-test-dispatch--layout prefix)))
      (should (string-match-p ":key \"q\"" layout))
      (should (string-match-p "transient-quit-one" layout)))))

(ert-deftest deb-packaging-test-dispatch/upload-key-matches-status-map ()
  "The dispatch uses U for the upload transient, matching the status
buffer map, where p is section navigation."
  (let ((layout (deb-packaging-test-dispatch--layout
                 'deb-packaging-dispatch-transient)))
    (should (string-match-p ":key \"U\"" layout))
    ;; The U entry must be the upload transient, not something else.
    (should (string-match-p
             ":key \"U\"[^)]*:command deb-packaging-upload-transient"
             layout))
    ;; And the old p binding is gone.
    (should-not (string-match-p
                 ":key \"p\"[^)]*:command deb-packaging-upload-transient"
                 layout))))

(provide 'deb-packaging-test-dispatch)
;;; deb-packaging-test-dispatch.el ends here
