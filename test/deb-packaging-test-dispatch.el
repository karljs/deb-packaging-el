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

(ert-deftest deb-packaging-test-dispatch/header-uses-shared-context ()
  (cl-letf (((symbol-function 'deb-packaging-status--collect-context)
             (lambda ()
               '(:name "foo" :version "1.2-3" :distro "noble"
                 :host-arch "arm64" :target-arch "arm64"
                 :git-p t :branch "ubuntu/noble"
                 :dirty-p t))))
    (let ((header (deb-packaging--dispatch-header)))
      (should (string-match-p "foo 1.2-3 | noble | arm64" header))
      (should (string-match-p "git: ubuntu/noble (modified)" header)))))

(ert-deftest deb-packaging-test-dispatch/header-shows-target-and-host ()
  (cl-letf (((symbol-function 'deb-packaging-status--collect-context)
             (lambda ()
               '(:name "foo" :version "1.2-3" :distro "noble"
                 :host-arch "amd64" :target-arch "arm64"))))
    (should (string-match-p "arm64 (host amd64)"
                            (deb-packaging--dispatch-header)))))

(ert-deftest deb-packaging-test-dispatch/operation-header-shows-context-and-ppa ()
  (cl-letf (((symbol-function 'deb-packaging-transients--context)
             (lambda ()
               '(:name "foo" :version "1.2-3" :distro "noble"
                 :host-arch "arm64" :target-arch "arm64"
                 :git-p t :branch "ubuntu/noble"
                 :default-ppa "ppa:me/foo"))))
    (let ((header (deb-packaging-transients--context-header)))
      (should (string-match-p "foo 1.2-3 | noble | arm64" header))
      (should (string-match-p "PPA: ppa:me/foo" header)))))

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
                    deb-packaging-gbp-build-transient
                    deb-packaging-binary-build-transient
                    deb-packaging-lint-transient
                    deb-packaging-test-transient
                    deb-packaging-upload-transient
                    deb-packaging-commands-clean-transient
                    deb-packaging-commands-reset-transient
                    deb-packaging-dev-transient
                    deb-packaging-pq-transient
                    deb-packaging-propagate-transient
                    deb-packaging-update-transient
                     deb-packaging-infra-dispatch
                     deb-packaging-infra-ppa-config-transient
                     deb-packaging-infra-schroots-dispatch
                    deb-packaging-infra-lxd-dispatch
                    deb-packaging-infra-qemu-dispatch
                    deb-packaging-infra-ppas-dispatch
                    deb-packaging-ppa-tests-dispatch))
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

(ert-deftest deb-packaging-test-dispatch/dispatch-binds-clone ()
  "The git-ubuntu clone entry point is reachable from the dispatch."
  (let ((layout (deb-packaging-test-dispatch--layout
                 'deb-packaging-dispatch-transient)))
    (should (string-match-p
             ":key \"C\".*:command deb-packaging-clone-git-ubuntu"
              layout))))

(ert-deftest deb-packaging-test-dispatch/dispatch-binds-gbp-build ()
  (let ((layout (deb-packaging-test-dispatch--layout
                 'deb-packaging-dispatch-transient)))
    (should (string-match-p
             ":key \"G\"[^)]*:command deb-packaging-gbp-build-transient"
             layout))))

(ert-deftest deb-packaging-test-dispatch/source-transient-binds-gbp-orig ()
  (let ((layout (deb-packaging-test-dispatch--layout
                 'deb-packaging-commands-source-build-transient)))
    (should (string-match-p "deb-packaging-commands-gbp-export-orig" layout))))

(ert-deftest deb-packaging-test-dispatch/autopkgtest-binds-proposed ()
  "The local autopkgtest transient exposes the proposed pocket."
  (let ((layout (deb-packaging-test-dispatch--layout
                 'deb-packaging-test-transient)))
    (should (string-match-p
             ":key \"-P\"[^)]*:argument \"--apt-pocket=proposed\""
             layout))))

(provide 'deb-packaging-test-dispatch)
;;; deb-packaging-test-dispatch.el ends here
