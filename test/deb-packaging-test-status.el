;;; deb-packaging-test-status.el --- Status state-machine tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for the status-buffer phase state machine and fold decisions
;; in deb-packaging-status.el. Rendering is tested only for regressions
;; (e.g. missing tools must not crash the render); the state/decision
;; logic is the main coverage.

;;; Code:

(require 'ert)
(require 'deb-packaging-test)
(require 'deb-packaging-commands)
(require 'deb-packaging-status)

;;; Helpers

(defun deb-packaging-test-status--ctx (arts)
  "Return a context plist with artifacts alist ARTS."
  (list :artifacts arts))

;;; Phase state

(ert-deftest deb-packaging-test-status/phase-state-running-wins-over-done-and-ready ()
  (let ((deb-packaging-commands--run-history nil))
    (deb-packaging-commands--record-run 'source-build 'running nil)
    (should (eq (deb-packaging-status--phase-state 'source-build t t) 'running))
    (should (eq (deb-packaging-status--phase-state 'source-build t nil) 'running))))

(ert-deftest deb-packaging-test-status/phase-state-failure-wins-over-done-and-ready ()
  (let ((deb-packaging-commands--run-history nil))
    (deb-packaging-commands--record-run 'sbuild 'failure nil)
    (should (eq (deb-packaging-status--phase-state 'sbuild t t) 'failed))
    (should (eq (deb-packaging-status--phase-state 'sbuild t nil) 'failed))))

(ert-deftest deb-packaging-test-status/phase-state-done-via-artifacts ()
  (let ((deb-packaging-commands--run-history nil))
    (should (eq (deb-packaging-status--phase-state 'source-build t t) 'done))
    (should (eq (deb-packaging-status--phase-state 'source-build t nil) 'done))))

(ert-deftest deb-packaging-test-status/phase-state-done-via-success-run ()
  (let ((deb-packaging-commands--run-history nil))
    (deb-packaging-commands--record-run 'autopkgtest 'success nil)
    (should (eq (deb-packaging-status--phase-state 'autopkgtest nil t) 'done))))

(ert-deftest deb-packaging-test-status/phase-state-ready-when-not-done ()
  (let ((deb-packaging-commands--run-history nil))
    (should (eq (deb-packaging-status--phase-state 'dput nil t) 'ready))
    (should (eq (deb-packaging-status--phase-state 'sbuild nil t) 'ready))))

(ert-deftest deb-packaging-test-status/phase-state-blocked-when-not-ready ()
  (let ((deb-packaging-commands--run-history nil))
    (should (eq (deb-packaging-status--phase-state 'sbuild nil nil) 'blocked))
    (should (eq (deb-packaging-status--phase-state 'autopkgtest nil nil) 'blocked))))

(ert-deftest deb-packaging-test-status/phase-state-keep-ready-preserves-ready-after-success ()
  (let ((deb-packaging-commands--run-history nil))
    (deb-packaging-commands--record-run 'lintian-source 'success nil)
    (should (eq (deb-packaging-status--phase-state 'lintian-source nil t t) 'ready))
    (should (eq (deb-packaging-status--phase-state 'lintian-source nil nil t) 'blocked))))

;;; Hide phase decision

(ert-deftest deb-packaging-test-status/hide-phase-failed-expand ()
  (should-not (deb-packaging-status--hide-phase-p 'failed 'sbuild 'source-build)))

(ert-deftest deb-packaging-test-status/hide-phase-running-expand ()
  (should-not (deb-packaging-status--hide-phase-p 'running 'sbuild 'source-build)))

(ert-deftest deb-packaging-test-status/hide-phase-next-actionable-expand ()
  (should-not (deb-packaging-status--hide-phase-p 'ready 'sbuild 'sbuild))
  (should-not (deb-packaging-status--hide-phase-p 'blocked 'source-build 'source-build)))

(ert-deftest deb-packaging-test-status/hide-phase-others-collapse ()
  (should (deb-packaging-status--hide-phase-p 'ready 'source-build 'sbuild))
  (should (deb-packaging-status--hide-phase-p 'done 'source-build 'sbuild))
  (should (deb-packaging-status--hide-phase-p 'blocked 'sbuild 'autopkgtest)))

;;; Next actionable key

(ert-deftest deb-packaging-test-status/source-ready-p-native-without-orig ()
  (should (deb-packaging-status--source-ready-p
           (list :version "1.2" :orig-tarball nil))))

(ert-deftest deb-packaging-test-status/source-ready-p-non-native-with-orig ()
  (should (deb-packaging-status--source-ready-p
           (list :version "1.2-3" :orig-tarball "/x/foo_1.2.orig.tar.gz"))))

(ert-deftest deb-packaging-test-status/source-ready-p-non-native-missing-orig ()
  (should-not (deb-packaging-status--source-ready-p
               (list :version "1.2-3" :orig-tarball nil))))

(ert-deftest deb-packaging-test-status/source-ready-p-no-version ()
  ;; A partial context without a version must not block the phase.
  (should (deb-packaging-status--source-ready-p (list :artifacts nil))))

(ert-deftest deb-packaging-test-status/next-actionable-key-source-build-ready ()
  (let ((deb-packaging-commands--run-history nil)
        (ctx (deb-packaging-test-status--ctx
              '((dsc . nil) (source-changes . nil)
                (binary-changes . nil) (debs . nil)))))
    (should (eq (deb-packaging-status--next-actionable-key ctx) 'source-build))))

(ert-deftest deb-packaging-test-status/next-actionable-key-sbuild-ready ()
  (let ((deb-packaging-commands--run-history nil)
        (ctx (deb-packaging-test-status--ctx
              '((dsc . "foo_1.2-3.dsc")
                (source-changes . "foo_1.2-3_source.changes")
                (binary-changes . nil) (debs . nil)))))
    (should (eq (deb-packaging-status--next-actionable-key ctx) 'sbuild))))

(ert-deftest deb-packaging-test-status/next-actionable-key-autopkgtest-ready ()
  (let ((deb-packaging-commands--run-history nil)
        (ctx (deb-packaging-test-status--ctx
              '((dsc . "foo_1.2-3.dsc")
                (source-changes . "foo_1.2-3_source.changes")
                (binary-changes . "foo_1.2-3_amd64.changes")
                (debs . ("foo_1.2-3_amd64.deb"))))))
    (should (eq (deb-packaging-status--next-actionable-key ctx) 'autopkgtest))))

(ert-deftest deb-packaging-test-status/next-actionable-key-dput-when-all-done ()
  (let ((deb-packaging-commands--run-history nil)
        (ctx (deb-packaging-test-status--ctx
              '((dsc . "foo_1.2-3.dsc")
                (source-changes . "foo_1.2-3_source.changes")
                (binary-changes . "foo_1.2-3_amd64.changes")
                (debs . ("foo_1.2-3_amd64.deb"))))))
    ;; Mark autopkgtest complete so dput becomes the first ready phase.
    (deb-packaging-commands--record-run 'autopkgtest 'success nil)
    (should (eq (deb-packaging-status--next-actionable-key ctx) 'dput))))

(ert-deftest deb-packaging-test-status/next-actionable-key-nil-when-all-done ()
  (let ((deb-packaging-commands--run-history nil)
        (ctx (deb-packaging-test-status--ctx
              '((dsc . "foo_1.2-3.dsc")
                (source-changes . "foo_1.2-3_source.changes")
                (binary-changes . "foo_1.2-3_amd64.changes")
                (debs . ("foo_1.2-3_amd64.deb"))))))
    (deb-packaging-commands--record-run 'autopkgtest 'success nil)
    (deb-packaging-commands--record-run 'dput 'success nil)
    (should-not (deb-packaging-status--next-actionable-key ctx))))

(ert-deftest deb-packaging-test-status/next-actionable-key-source-blocked-missing-orig ()
  ;; Non-native with no orig tarball: source is blocked, and with no
  ;; artifacts at all dput is blocked too (nothing to upload), so no
  ;; phase is actionable.
  (let ((deb-packaging-commands--run-history nil)
        (ctx (append (deb-packaging-test-status--ctx
                      '((dsc . nil) (source-changes . nil)
                        (binary-changes . nil) (debs . nil)))
                     (list :version "1.2-3" :orig-tarball nil))))
    (should-not (deb-packaging-status--next-actionable-key ctx))))

(ert-deftest deb-packaging-test-status/next-actionable-key-running-not-ready ()
  ;; source-build is running and nothing else is ready: dput stays
  ;; blocked until a source .changes exists.
  (let ((deb-packaging-commands--run-history nil)
        (ctx (deb-packaging-test-status--ctx
              '((dsc . nil) (source-changes . nil)
                (binary-changes . nil) (debs . nil)))))
    (deb-packaging-commands--record-run 'source-build 'running nil)
    (should-not (deb-packaging-status--next-actionable-key ctx))))

(ert-deftest deb-packaging-test-status/upload-ready-only-with-source-changes ()
  "dput is blocked without a source .changes and ready with one; the
PPA being unset must not gate the phase."
  (let ((deb-packaging-commands--run-history nil)
        (ctx (deb-packaging-test-status--ctx
              '((dsc . "foo_1.2-3.dsc")
                (source-changes . "foo_1.2-3_source.changes")
                (binary-changes . nil) (debs . nil)))))
    (should (eq (alist-get 'dput (deb-packaging-status--phase-states ctx))
                'ready)))
  (let ((deb-packaging-commands--run-history nil)
        (ctx (deb-packaging-test-status--ctx
              '((dsc . nil) (source-changes . nil)
                (binary-changes . nil) (debs . nil)))))
    (should (eq (alist-get 'dput (deb-packaging-status--phase-states ctx))
                'blocked))))

;;; Lint rollup state

(ert-deftest deb-packaging-test-status/lint-rollup-failed-wins ()
  (let ((deb-packaging-commands--run-history nil))
    (deb-packaging-commands--record-run 'lintian-source 'failure nil)
    (let ((ctx (deb-packaging-test-status--ctx
                '((dsc . "foo_1.2-3.dsc") (debs . nil)))))
      (should (eq (deb-packaging-status--lint-rollup-state ctx) 'failed)))))

(ert-deftest deb-packaging-test-status/lint-rollup-running-when-no-failed ()
  (let ((deb-packaging-commands--run-history nil))
    (deb-packaging-commands--record-run 'lintian-binary 'running nil)
    (let ((ctx (deb-packaging-test-status--ctx
                '((dsc . nil) (debs . ("foo_1.2-3_amd64.deb"))))))
      (should (eq (deb-packaging-status--lint-rollup-state ctx) 'running)))))

(ert-deftest deb-packaging-test-status/lint-rollup-ready-by-default ()
  (let ((deb-packaging-commands--run-history nil)
        (ctx (deb-packaging-test-status--ctx
              '((dsc . nil) (debs . nil)))))
    ;; ubuntu-lint is always ready, so rollup is ready, not blocked.
    (should (eq (deb-packaging-status--lint-rollup-state ctx) 'ready))))

(ert-deftest deb-packaging-test-status/lint-rollup-ready-with-success-on-source ()
  (let ((deb-packaging-commands--run-history nil))
    (deb-packaging-commands--record-run 'lintian-source 'success nil)
    (let ((ctx (deb-packaging-test-status--ctx
                '((dsc . "foo_1.2-3.dsc") (debs . nil)))))
      (should (eq (deb-packaging-status--lint-rollup-state ctx) 'ready)))))

;;; Lint hide decision

(ert-deftest deb-packaging-test-status/lint-hide-failed-expand ()
  (let ((deb-packaging-commands--run-history nil))
    (deb-packaging-commands--record-run 'lintian-source 'failure nil)
    (let ((ctx (deb-packaging-test-status--ctx
                '((dsc . "foo_1.2-3.dsc") (debs . nil)))))
      (should-not (deb-packaging-status--lint-hide-p ctx)))))

(ert-deftest deb-packaging-test-status/lint-hide-running-expand ()
  (let ((deb-packaging-commands--run-history nil))
    (deb-packaging-commands--record-run 'ubuntu-lint 'running nil)
    (let ((ctx (deb-packaging-test-status--ctx
                '((dsc . nil) (debs . nil)))))
      (should-not (deb-packaging-status--lint-hide-p ctx)))))

(ert-deftest deb-packaging-test-status/lint-hide-ready-collapse ()
  (let ((deb-packaging-commands--run-history nil)
        (ctx (deb-packaging-test-status--ctx
              '((dsc . "foo_1.2-3.dsc") (debs . nil)))))
    (should (deb-packaging-status--lint-hide-p ctx))))

;;; Stale artifact grouping

(ert-deftest deb-packaging-test-status/group-stale-by-version-sorted ()
  (let ((result (deb-packaging-status--group-stale-by-version
                 '("foo_1.1-1.dsc"
                   "foo_1.1-1_amd64.deb"
                   "foo_1.0-1.dsc"
                   "foo_1.0-1_amd64.deb"))))
    (should (equal (mapcar #'car result)
                   '("1.0-1" "1.1-1")))
    (should (equal (cdr (assoc "1.0-1" result))
                   '("foo_1.0-1.dsc" "foo_1.0-1_amd64.deb")))
    (should (equal (cdr (assoc "1.1-1" result))
                   '("foo_1.1-1.dsc" "foo_1.1-1_amd64.deb")))))

(ert-deftest deb-packaging-test-status/group-stale-orig-tarball-unknown ()
  (let ((result (deb-packaging-status--group-stale-by-version
                 '("foo_1.1-1.dsc"
                   "foo_1.0.orig.tar.gz"))))
    (should (equal (mapcar #'car result) '("1.1-1" "unknown")))
    (should (equal (cdr (assoc "unknown" result))
                   '("foo_1.0.orig.tar.gz")))
    (should (equal (cdr (assoc "1.1-1" result))
                   '("foo_1.1-1.dsc")))))

;;; Lint summary note

(ert-deftest deb-packaging-test-status/lint-summary-note-empty-without-record ()
  (let ((deb-packaging-commands--run-history nil))
    (should (string= (deb-packaging-status--lint-summary-note 'lintian-source)
                     ""))
    (should (string= (deb-packaging-status--lint-summary-note 'ubuntu-lint)
                     ""))))

(ert-deftest deb-packaging-test-status/lint-summary-note-lintian ()
  (let ((deb-packaging-commands--run-history nil))
    (deb-packaging-commands--record-run 'lintian-source
                               'success nil
                               (list :error 2 :warning 5 :info 7))
    (let ((note (deb-packaging-status--lint-summary-note 'lintian-source)))
      (should (> (length note) 0))
      (let ((plain (substring-no-properties note)))
        (should (string-match-p "2" plain))
        (should (string-match-p "5" plain))
        (should (string-match-p "7" plain))))))

(ert-deftest deb-packaging-test-status/lint-summary-note-ubuntu-lint ()
  (let ((deb-packaging-commands--run-history nil))
    (deb-packaging-commands--record-run 'ubuntu-lint
                               'success nil
                               (list :ok 8 :skip 1 :warn 2 :error 3 :fail 4))
    (let ((note (deb-packaging-status--lint-summary-note 'ubuntu-lint)))
      (should (> (length note) 0))
      (let ((plain (substring-no-properties note)))
        (should (string-match-p "4" plain))
        (should (string-match-p "3" plain))
        (should (string-match-p "2" plain))))))

;;; Run time note

(ert-deftest deb-packaging-test-status/run-time-note-empty-without-record ()
  (let ((deb-packaging-commands--run-history nil))
    (should (string= (deb-packaging-status--run-time-note 'source-build) ""))))

(ert-deftest deb-packaging-test-status/run-time-note-non-empty-with-record ()
  (let ((deb-packaging-commands--run-history nil))
    (deb-packaging-commands--record-run 'source-build 'success nil)
    (let ((note (deb-packaging-status--run-time-note 'source-build)))
      (should (> (length note) 0))
      (should (string-match-p ":" (substring-no-properties note))))))

;;; Kept session note

(ert-deftest deb-packaging-test-status/kept-session-note ()
  (let ((deb-packaging-commands--run-history nil))
    (should (null (deb-packaging-status--kept-session-note)))
    (deb-packaging-commands--record-run
     'sbuild 'failure "*buf*" '(:kept-session "sess-1"))
    (should (string-match-p
             "sess-1" (deb-packaging-status--kept-session-note)))))

;;; Mode map

(ert-deftest deb-packaging-test-status/mode-map-keeps-p-for-navigation ()
  "\"p\"/\"n\" stay section navigation; upload lives on \"U\"."
  (should (eq (lookup-key deb-packaging-status-mode-map "p")
              #'magit-section-backward))
  (should (eq (lookup-key deb-packaging-status-mode-map "n")
              #'magit-section-forward))
  (should (eq (lookup-key deb-packaging-status-mode-map "U")
              #'deb-packaging-upload-transient)))

;;; PPA tests summary note

(ert-deftest deb-packaging-test-status/ppa-tests-summary-note ()
  "Counts from the last ppa-tests run summary, empty without one."
  (let ((deb-packaging-commands--run-history nil))
    (should (equal (deb-packaging-status--ppa-tests-summary-note) ""))
    (deb-packaging-commands--record-run
     'ppa-tests 'success nil (list :pass 3 :fail 1 :bad 0))
    (should (string-match-p "3P" (deb-packaging-status--ppa-tests-summary-note)))
    (should (string-match-p "1F" (deb-packaging-status--ppa-tests-summary-note)))
    (should (string-match-p "0B" (deb-packaging-status--ppa-tests-summary-note)))))

;;; Entry point prompting

(ert-deftest deb-packaging-test-status/status-prompts-outside-package ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (let ((default-directory pkg-parent-dir)
          (answers (list pkg-dir))
          (displayed nil))
      (deb-packaging-test--with-mocked-process
          '(("dpkg" . "amd64") ("schroot" . "") ("lxc" . ""))
        (cl-letf (((symbol-function 'read-directory-name)
                   (lambda (&rest _) (pop answers)))
                  ((symbol-function 'deb-packaging-display-buffer)
                   (lambda (buf _category) (setq displayed buf))))
          (deb-packaging-status))
        (should (null answers))
        (should (string= (buffer-name displayed) "*deb-packaging: foo*"))
        (should (file-equal-p (buffer-local-value 'default-directory displayed)
                              pkg-dir))
        (should (equal (plist-get (buffer-local-value
                                   'deb-packaging-status--context displayed)
                                  :name)
                       "foo"))
        (kill-buffer displayed)))))

(ert-deftest deb-packaging-test-status/status-inside-package-does-not-prompt ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (let ((displayed nil))
      (deb-packaging-test--with-mocked-process
          '(("dpkg" . "amd64") ("schroot" . "") ("lxc" . ""))
        (cl-letf (((symbol-function 'read-directory-name)
                   (lambda (&rest _) (error "must not prompt")))
                  ((symbol-function 'deb-packaging-display-buffer)
                   (lambda (buf _category) (setq displayed buf))))
          (deb-packaging-status))
        (should (string= (buffer-name displayed) "*deb-packaging: foo*"))
        (kill-buffer displayed)))))

(ert-deftest deb-packaging-test-status/render-with-missing-tools ()
  "The render must not crash when dpkg/schroot/lxc are all absent."
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (let ((displayed nil))
      (cl-letf (((symbol-function 'call-process)
                 (lambda (&rest _)
                   (signal 'file-missing "No such file or directory")))
                ((symbol-function 'read-directory-name)
                 (lambda (&rest _) (error "must not prompt")))
                ((symbol-function 'deb-packaging-display-buffer)
                 (lambda (buf _category) (setq displayed buf))))
        (deb-packaging-status))
      (unwind-protect
          (with-current-buffer displayed
            (goto-char (point-min))
            (should (search-forward "Source build" nil t))
            ;; Arch row absent (nil arch), not a crash.
            (should (string= (plist-get deb-packaging-status--context :name)
                             "foo")))
        (kill-buffer displayed)))))

(ert-deftest deb-packaging-test-status/render-shows-extra-repos-row ()
  "The Binary section shows what a build would use: the saved set, or
the -proposed default when nothing was saved.  The tree has source
artifacts so Binary is the next actionable phase and renders expanded."
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3" :distro "noble"
            :artifacts '(("foo_1.2-3.dsc" . "")
                         ("foo_1.2-3_source.changes" . "")))
    (let* ((tmp (make-temp-file "deb-repos-render-" t))
           (process-environment (cons (format "XDG_CACHE_HOME=%s" tmp)
                                      process-environment))
           (displayed nil))
      (unwind-protect
          (progn
            (deb-packaging-test--with-mocked-process
                '(("dpkg" . "amd64") ("schroot" . "") ("lxc" . ""))
              (cl-letf (((symbol-function 'read-directory-name)
                         (lambda (&rest _) (error "must not prompt")))
                        ((symbol-function 'deb-packaging-display-buffer)
                         (lambda (buf _category) (setq displayed buf))))
                (deb-packaging-status)))
            (with-current-buffer displayed
              (goto-char (point-min))
              (should (search-forward "Extra repos: proposed" nil t)))
            ;; Saved entries replace the default in the row.
            (deb-packaging-repos-save "foo" "noble" '("ppa:me/x"))
            (deb-packaging-test--with-mocked-process
                '(("dpkg" . "amd64") ("schroot" . "") ("lxc" . ""))
              (cl-letf (((symbol-function 'read-directory-name)
                         (lambda (&rest _) (error "must not prompt")))
                        ((symbol-function 'deb-packaging-display-buffer)
                         (lambda (buf _category) (setq displayed buf))))
                (deb-packaging-status)))
            (with-current-buffer displayed
              (goto-char (point-min))
              (should (search-forward "Extra repos: ppa:me/x" nil t))
              (should-not (search-forward "Extra repos: none" nil t))))
        (when (buffer-live-p displayed) (kill-buffer displayed))
        (delete-directory tmp t)))))

(ert-deftest deb-packaging-test-status/visitable-file-p-excludes-binary-packages ()
  (should (deb-packaging-status--visitable-file-p "foo_1.2-3.dsc"))
  (should (deb-packaging-status--visitable-file-p "foo_1.2-3_source.changes"))
  (should (deb-packaging-status--visitable-file-p "foo_1.2-3_source.buildinfo"))
  (should-not (deb-packaging-status--visitable-file-p "foo_1.2-3_amd64.deb"))
  (should-not (deb-packaging-status--visitable-file-p "foo_1.2-3_amd64.udeb"))
  (should-not (deb-packaging-status--visitable-file-p "foo-dbgsym_1.2-3_amd64.ddeb")))

(ert-deftest deb-packaging-test-status/ret-visits-artifact-file ()
  "RET on a text artifact line opens the file read-only instead of the
phase transient.  Source-build failed so the section renders expanded."
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3" :distro "noble"
            :artifacts '(("foo_1.2-3.dsc" . "")
                         ("foo_1.2-3_source.changes" . "")))
    (let ((displayed nil) (visited nil))
      (deb-packaging-test--with-mocked-process
          '(("dpkg" . "amd64") ("schroot" . "") ("lxc" . ""))
        (cl-letf (((symbol-function 'read-directory-name)
                   (lambda (&rest _) (error "must not prompt")))
                  ((symbol-function 'deb-packaging-display-buffer)
                   (lambda (buf _category) (setq displayed buf))))
          (deb-packaging-commands--record-run 'source-build 'failure nil)
          (deb-packaging-status)))
      (unwind-protect
          (with-current-buffer displayed
            (let ((file-section nil))
              (cl-labels ((walk (s)
                            (when (and s (not file-section))
                              (when (eq (oref s type) 'deb-packaging-file)
                                (setq file-section s))
                              (dolist (c (oref s children)) (walk c)))))
                (walk magit-root-section))
              (should file-section)
              (goto-char (oref file-section start))
              (cl-letf (((symbol-function 'find-file-read-only)
                         (lambda (path) (setq visited path))))
                (deb-packaging-status-visit))
              (should (equal visited
                             (expand-file-name "foo_1.2-3.dsc"
                                               pkg-parent-dir)))))
        (kill-buffer displayed)))))

(ert-deftest deb-packaging-test-status/ret-on-phase-heading-opens-transient ()
  "RET outside a file line keeps opening the phase's transient."
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3" :distro "noble")
    (let ((displayed nil) (called nil))
      (deb-packaging-test--with-mocked-process
          '(("dpkg" . "amd64") ("schroot" . "") ("lxc" . ""))
        (cl-letf (((symbol-function 'read-directory-name)
                   (lambda (&rest _) (error "must not prompt")))
                  ((symbol-function 'deb-packaging-display-buffer)
                   (lambda (buf _category) (setq displayed buf))))
          (deb-packaging-status)))
      (unwind-protect
          (with-current-buffer displayed
            (goto-char (point-min))
            (search-forward "Source build")
            (cl-letf (((symbol-function 'call-interactively)
                       (lambda (cmd &rest _) (setq called cmd))))
              (deb-packaging-status-visit))
            (should (eq called
                        'deb-packaging-commands-source-build-transient)))
        (kill-buffer displayed)))))

(provide 'deb-packaging-test-status)
;;; deb-packaging-test-status.el ends here
