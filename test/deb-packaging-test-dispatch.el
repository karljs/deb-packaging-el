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

(ert-deftest deb-packaging-test-dispatch/doctor-reports-tools-and-opens-customize ()
  (let (displayed customized)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (tool) (and (equal tool "ppa") "/usr/bin/ppa")))
              ((symbol-function 'deb-packaging-display-buffer)
               (lambda (buf _category) (setq displayed buf)))
              ((symbol-function 'customize-group)
               (lambda (group) (setq customized group)))
              ((symbol-function 'message) #'ignore))
      (deb-packaging-doctor))
    (unwind-protect
        (progn
          (should (eq customized 'deb-packaging))
          (with-current-buffer displayed
            (should (string-match-p "ppa +available" (buffer-string)))
            (should (string-match-p "sbuild +missing" (buffer-string)))))
      (when (buffer-live-p displayed)
        (kill-buffer displayed)))))

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

(ert-deftest deb-packaging-test-dispatch/outside-package-offers-get ()
  (let ((tmp (make-temp-file "deb-pkg-test-" t)))
    (unwind-protect
        (let ((default-directory tmp)
              (called nil))
          (cl-letf (((symbol-function 'deb-packaging-get-transient)
                     (lambda () (interactive) (push 'get called)))
                    ((symbol-function 'deb-packaging-dispatch-transient)
                     (lambda () (push 'transient called))))
            (deb-packaging-dispatch))
          (should (equal called '(get))))
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
                    deb-packaging-binary-build-transient
                    deb-packaging-lint-transient
                    deb-packaging-lintian-transient
                    deb-packaging-ubuntu-lint-transient
                    deb-packaging-test-transient
                    deb-packaging-upload-transient
                    deb-packaging-commands-clean-transient
                    deb-packaging-commands-reset-transient
                    deb-packaging-dev-transient
                    deb-packaging-branch-transient
                    deb-packaging-patches-transient
                    deb-packaging-changelog-transient
                    deb-packaging-submit-transient
                    deb-packaging-get-transient
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

(ert-deftest deb-packaging-test-dispatch/dispatch-binds-get-a-package ()
  (should (string-match-p
           ":key \"G\"[^)]*:command deb-packaging-get-transient"
           (deb-packaging-test-dispatch--layout 'deb-packaging-dispatch-transient)))
  (let ((get (deb-packaging-test-dispatch--layout 'deb-packaging-get-transient)))
    (should (string-match-p "deb-packaging-clone-git-ubuntu" get))
    (should (string-match-p "deb-packaging-clone-gbp" get))))

(ert-deftest deb-packaging-test-dispatch/status-and-hub-share-keys ()
  "Every hub key that opens a package menu does the same in status."
  (dolist (pair '(("f" . deb-packaging-branch-transient)
                  ("a" . deb-packaging-patches-transient)
                  ("C" . deb-packaging-changelog-transient)
                  ("N" . deb-packaging-update-transient)
                  ("e" . deb-packaging-dev-transient)
                  ("s" . deb-packaging-commands-source-build-transient)
                  ("b" . deb-packaging-binary-build-transient)
                  ("l" . deb-packaging-lint-transient)
                  ("t" . deb-packaging-test-transient)
                  ("U" . deb-packaging-upload-transient)
                  ("M" . deb-packaging-submit-transient)
                  ("P" . deb-packaging-propagate-transient)
                  ("G" . deb-packaging-get-transient)))
    (should (eq (lookup-key deb-packaging-status-mode-map (car pair)) (cdr pair)))
    (should (string-match-p
             (format ":key \"%s\"[^)]*:command %s" (car pair) (cdr pair))
             (deb-packaging-test-dispatch--layout 'deb-packaging-dispatch-transient)))))

(ert-deftest deb-packaging-test-dispatch/builders-are-options-not-entries ()
  "Builders are a --builder= choice inside Source/Binaries, not hub rows."
  (let ((hub (deb-packaging-test-dispatch--layout
              'deb-packaging-dispatch-transient)))
    (should-not (string-match-p "gbp-build\\|build-binary" hub)))
  (dolist (prefix '(deb-packaging-commands-source-build-transient
                    deb-packaging-binary-build-transient))
    (should (string-match-p ":argument \"--builder=\""
                            (deb-packaging-test-dispatch--layout prefix)))))

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

(ert-deftest deb-packaging-test-dispatch/package-menus-open ()
  "regression: a -n infix shadowed -nc and the source menu failed to open."
  (dolist (prefix '(deb-packaging-dispatch-transient
                    deb-packaging-commands-source-build-transient
                    deb-packaging-binary-build-transient
                    deb-packaging-lint-transient
                    deb-packaging-lintian-transient
                    deb-packaging-ubuntu-lint-transient
                    deb-packaging-test-transient
                    deb-packaging-upload-transient
                    deb-packaging-branch-transient
                    deb-packaging-patches-transient
                    deb-packaging-changelog-transient
                    deb-packaging-submit-transient
                    deb-packaging-propagate-transient
                    deb-packaging-get-transient
                    deb-packaging-infra-dispatch))
    (unwind-protect
        (transient-setup prefix)
      (transient-quit-all))))

(ert-deftest deb-packaging-test-dispatch/unavailable-actions-say-why ()
  (cl-letf (((symbol-function 'deb-packaging-commands--package-context)
             (lambda (&rest _) '(:artifacts ((dsc . "foo.dsc")))))
            ((symbol-function 'executable-find)
             (lambda (tool) (not (equal tool "ubuntu-lint")))))
    (should-not (deb-packaging-transients--why-not '("lintian" dsc)))
    (should (equal (deb-packaging-transients--label "All binaries" '("lintian" debs))
                   "All binaries (needs binaries)"))
    (should (equal (deb-packaging-transients--why-not '("ubuntu-lint"))
                   "ubuntu-lint not installed"))))

(provide 'deb-packaging-test-dispatch)
;;; deb-packaging-test-dispatch.el ends here
