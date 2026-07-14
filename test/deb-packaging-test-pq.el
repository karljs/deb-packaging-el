;;; deb-packaging-test-pq.el --- gbp pq state tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for patch-queue branch-state logic and the compilation
;; follow-up helper in deb-packaging-pq.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'compile)
(require 'deb-packaging-test)
(require 'deb-packaging-pq)

(ert-deftest deb-packaging-test-pq/patch-queue-branch-normal ()
  (should (string= (deb-packaging-pq--patch-queue-branch "main")
                   "patch-queue/main")))

(ert-deftest deb-packaging-test-pq/patch-queue-branch-already-pq ()
  (should (null (deb-packaging-pq--patch-queue-branch "patch-queue/main"))))

(ert-deftest deb-packaging-test-pq/state-on-main ()
  (deb-packaging-test--with-temp-git-repo
    (should (string= (deb-packaging-pq--current-branch) "main"))
    (should (null (deb-packaging-pq--on-pq-branch-p)))
    (let ((state (deb-packaging-pq--state)))
      (should (null (plist-get state :on-pq-p)))
      (should (string= (plist-get state :branch) "main"))
      (should (string= (plist-get state :pq-branch) "patch-queue/main"))
      (should (null (plist-get state :exists-p))))))

(ert-deftest deb-packaging-test-pq/state-exists-after-branch-creation ()
  (deb-packaging-test--with-temp-git-repo
    (deb-packaging-test--git repo-dir "branch" "patch-queue/main")
    (let ((state (deb-packaging-pq--state)))
      (should (string= (plist-get state :branch) "main"))
      (should (string= (plist-get state :pq-branch) "patch-queue/main"))
      (should (plist-get state :exists-p)))))

(ert-deftest deb-packaging-test-pq/state-on-pq-branch ()
  (deb-packaging-test--with-temp-git-repo
    (deb-packaging-test--git repo-dir "branch" "patch-queue/main")
    (deb-packaging-test--git repo-dir "checkout" "-q" "patch-queue/main")
    (should (string= (deb-packaging-pq--current-branch) "patch-queue/main"))
    (should (deb-packaging-pq--on-pq-branch-p))
    (let ((state (deb-packaging-pq--state)))
      (should (plist-get state :on-pq-p))
      (should (string= (plist-get state :branch) "patch-queue/main"))
      (should (string= (plist-get state :pq-branch) "patch-queue/main"))
      (should (plist-get state :exists-p)))))

(ert-deftest deb-packaging-test-pq/after-compile-fires-on-success ()
  (let* ((compilation-finish-functions nil)
         (buf (generate-new-buffer " *fake-compile*"))
         (fired 0))
    (unwind-protect
        (progn
          (deb-packaging-pq--after-compile buf (lambda () (cl-incf fired)))
          ;; Wrong buffer: no fire, hook stays.
          (run-hook-with-args 'compilation-finish-functions
                              (generate-new-buffer " *other*") "finished\n")
          (should (= fired 0))
          (should-not (null compilation-finish-functions))
          ;; Matching buffer, success message: fires once.
          (run-hook-with-args 'compilation-finish-functions buf "finished\n")
          (should (= fired 1))
          ;; Hook removed: second run does nothing.
          (run-hook-with-args 'compilation-finish-functions buf "finished\n")
          (should (= fired 1)))
      (kill-buffer buf))))

(ert-deftest deb-packaging-test-pq/after-compile-skips-on-failure ()
  (let* ((compilation-finish-functions nil)
         (buf (generate-new-buffer " *fake-compile-fail*"))
         (fired 0))
    (unwind-protect
        (progn
          (deb-packaging-pq--after-compile buf (lambda () (cl-incf fired)))
          ;; Matching buffer, failure message: action does not fire, hook removes itself.
          (run-hook-with-args 'compilation-finish-functions
                              buf "exited abnormally with code 1\n")
          (should (= fired 0))
          (should (null compilation-finish-functions))
          ;; Subsequent success on the same buffer must not fire.
          (run-hook-with-args 'compilation-finish-functions buf "finished\n")
          (should (= fired 0)))
      (kill-buffer buf))))

(provide 'deb-packaging-test-pq)
;;; deb-packaging-test-pq.el ends here
