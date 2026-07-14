;;; deb-packaging-test-run.el --- Run-tracking tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for run-outcome tracking and sentinel wrapping in
;; deb-packaging-commands.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'deb-packaging-test)
(require 'deb-packaging-commands)

(cl-defun deb-packaging-test-run--wait (proc &optional (max 100))
  "Wait for PROC to exit, polling up to MAX times."
  (let ((n 0))
    (while (and (process-live-p proc) (< n max))
      (accept-process-output nil 0.05)
      (cl-incf n))
    (accept-process-output nil 0.1)))

(ert-deftest deb-packaging-test-run/record-running-then-retrieve ()
  (let ((deb-packaging--run-history nil))
    (deb-packaging--record-run 'test-run 'running "*buf*")
    (let ((rec (deb-packaging-run-record 'test-run)))
      (should (eq (plist-get rec :status) 'running))
      (should (string= (plist-get rec :buffer) "*buf*"))
      (should (stringp (plist-get rec :time))))))

(ert-deftest deb-packaging-test-run/overwrite-success-keeps-time ()
  (let ((deb-packaging--run-history nil))
    (deb-packaging--record-run 'test-run 'running "*buf*")
    (let ((orig-time (plist-get (deb-packaging-run-record 'test-run) :time)))
      (should orig-time)
      (sleep-for 0.05)
      (deb-packaging--record-run 'test-run 'success "*buf*")
      (should (eq (plist-get (deb-packaging-run-record 'test-run) :status)
                  'success))
      (should (string= (plist-get (deb-packaging-run-record 'test-run) :time)
                       orig-time)))))

(ert-deftest deb-packaging-test-run/nil-key-no-op ()
  (let ((deb-packaging--run-history nil))
    (deb-packaging--record-run nil 'running "*buf*")
    (should (null deb-packaging--run-history))))

(ert-deftest deb-packaging-test-run/run-summary-returns-summary ()
  (let ((deb-packaging--run-history nil)
        (summary (list :error 1 :warning 2 :info 3)))
    (deb-packaging--record-run 'test-run 'success "*buf*" summary)
    (should (equal (deb-packaging--run-summary 'test-run) summary))))

(ert-deftest deb-packaging-test-run/run-summary-nil-when-missing ()
  (let ((deb-packaging--run-history nil))
    (should (null (deb-packaging--run-summary 'no-such-run)))
    (deb-packaging--record-run 'test-run 'success "*buf*")
    (should (null (deb-packaging--run-summary 'test-run)))))

(ert-deftest deb-packaging-test-run/run-summary-parser-lint-keys ()
  (should (eq (deb-packaging--run-summary-parser 'lintian-source)
              #'deb-packaging--parse-lint-summary))
  (should (eq (deb-packaging--run-summary-parser 'lintian-binary)
              #'deb-packaging--parse-lint-summary))
  (should (eq (deb-packaging--run-summary-parser 'ubuntu-lint)
              #'deb-packaging--parse-ubuntu-lint-summary))
  (should (null (deb-packaging--run-summary-parser 'source-build))))

(ert-deftest deb-packaging-test-run/wrap-sentinel-runs-action-on-exit ()
  (let* ((fired nil)
         (proc (make-process :name "deb-test-true"
                             :command '("true")
                             :noquery t)))
    (deb-packaging--wrap-sentinel proc (lambda (_p _e) (setq fired t)))
    (deb-packaging-test-run--wait proc)
    (should fired)))

(ert-deftest deb-packaging-test-run/wrap-sentinel-preserves-existing-sentinel ()
  (let* ((old-fired nil)
         (new-fired nil)
         (proc (make-process :name "deb-test-true-pres"
                             :command '("true")
                             :noquery t)))
    (set-process-sentinel proc (lambda (_p _e) (setq old-fired t)))
    (deb-packaging--wrap-sentinel proc (lambda (_p _e) (setq new-fired t)))
    (deb-packaging-test-run--wait proc)
    (should old-fired)
    (should new-fired)))

(ert-deftest deb-packaging-test-run/attach-run-sentinel-records-success ()
  (let ((deb-packaging--run-history nil))
    (let ((proc (make-process :name "deb-test-ok"
                              :command '("true")
                              :noquery t)))
      (deb-packaging--attach-run-sentinel proc 'test-ok nil)
      (deb-packaging-test-run--wait proc)
      (should (eq (plist-get (deb-packaging-run-record 'test-ok) :status)
                  'success)))))

(ert-deftest deb-packaging-test-run/attach-run-sentinel-records-failure ()
  (let ((deb-packaging--run-history nil))
    (let ((proc (make-process :name "deb-test-bad"
                              :command '("false")
                              :noquery t)))
      (deb-packaging--attach-run-sentinel proc 'test-bad nil)
      (deb-packaging-test-run--wait proc)
      (should (eq (plist-get (deb-packaging-run-record 'test-bad) :status)
                  'failure)))))

(provide 'deb-packaging-test-run)
;;; deb-packaging-test-run.el ends here
