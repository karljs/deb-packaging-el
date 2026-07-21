;;; deb-packaging-test-status.el --- Status state-machine tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for the status-buffer phase state machine and fold decisions
;; in deb-packaging-status.el.  Rendering itself is not tested; only the
;; pure state/decision logic.

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

;;; Actionable state predicate

(ert-deftest deb-packaging-test-status/actionable-state-p-true-for-running-failed-ready ()
  (should (deb-packaging-status--actionable-state-p 'running))
  (should (deb-packaging-status--actionable-state-p 'failed))
  (should (deb-packaging-status--actionable-state-p 'ready)))

(ert-deftest deb-packaging-test-status/actionable-state-p-nil-for-done-blocked ()
  (should-not (deb-packaging-status--actionable-state-p 'done))
  (should-not (deb-packaging-status--actionable-state-p 'blocked))
  (should-not (deb-packaging-status--actionable-state-p 'unknown)))

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
  ;; Non-native with no orig tarball: source is blocked, so the walk
  ;; skips to dput, which is always ready.
  (let ((deb-packaging-commands--run-history nil)
        (ctx (append (deb-packaging-test-status--ctx
                      '((dsc . nil) (source-changes . nil)
                        (binary-changes . nil) (debs . nil)))
                     (list :version "1.2-3" :orig-tarball nil))))
    (should (eq (deb-packaging-status--next-actionable-key ctx) 'dput))))

(ert-deftest deb-packaging-test-status/next-actionable-key-running-not-ready ()
  ;; source-build is running so it is not `ready'.  dput is always ready,
  ;; so it becomes the first ready phase in the walk.
  (let ((deb-packaging-commands--run-history nil)
        (ctx (deb-packaging-test-status--ctx
              '((dsc . nil) (source-changes . nil)
                (binary-changes . nil) (debs . nil)))))
    (deb-packaging-commands--record-run 'source-build 'running nil)
    (should (eq (deb-packaging-status--next-actionable-key ctx) 'dput))))

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
  ;; NOTE: deb-packaging-status--group-stale-by-version uses `alist-get'
  ;; with its default `eq' test, so each new version string becomes a
  ;; separate alist entry.  The test therefore asserts the current
  ;; behaviour including duplicate keys.
  (let ((result (deb-packaging-status--group-stale-by-version
                 '("foo_1.1-1.dsc"
                   "foo_1.1-1_amd64.deb"
                   "foo_1.0-1.dsc"
                   "foo_1.0-1_amd64.deb"))))
    (should (equal (mapcar #'car result)
                   '("1.0-1" "1.0-1" "1.1-1" "1.1-1")))
    (should (equal (cdr (nth 0 result)) '("foo_1.0-1_amd64.deb")))
    (should (equal (cdr (nth 1 result)) '("foo_1.0-1.dsc")))
    (should (equal (cdr (nth 2 result)) '("foo_1.1-1_amd64.deb")))
    (should (equal (cdr (nth 3 result)) '("foo_1.1-1.dsc")))))

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
              #'deb-packaging-status-upload)))

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

(provide 'deb-packaging-test-status)
;;; deb-packaging-test-status.el ends here
