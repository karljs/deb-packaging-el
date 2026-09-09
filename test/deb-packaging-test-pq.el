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
    (should (string= (magit-get-current-branch) "main"))
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
    (should (string= (magit-get-current-branch) "patch-queue/main"))
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
          (deb-packaging-commands--after-compile buf (lambda () (cl-incf fired)))
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
          (deb-packaging-commands--after-compile buf (lambda () (cl-incf fired)))
          ;; Matching buffer, failure message: action does not fire, hook removes itself.
          (run-hook-with-args 'compilation-finish-functions
                              buf "exited abnormally with code 1\n")
          (should (= fired 0))
          (should (null compilation-finish-functions))
          ;; Subsequent success on the same buffer must not fire.
          (run-hook-with-args 'compilation-finish-functions buf "finished\n")
          (should (= fired 0)))
      (kill-buffer buf))))

(ert-deftest deb-packaging-test-pq/after-compile-fires-on-failure-callback ()
  "ON-FAILURE runs on a non-zero exit; ACTION still does not."
  (let* ((compilation-finish-functions nil)
         (buf (generate-new-buffer " *fake-compile-fail2*"))
         (action 0) (failure 0))
    (unwind-protect
        (progn
          (deb-packaging-commands--after-compile
           buf (lambda () (cl-incf action)) (lambda () (cl-incf failure)))
          (run-hook-with-args 'compilation-finish-functions
                              buf "exited abnormally with code 1\n")
          (should (= action 0))
          (should (= failure 1)))
      (kill-buffer buf))))

(ert-deftest deb-packaging-test-pq/after-compile-failure-skips-on-kill ()
  "The killed-buffer event fires neither callback (reuse race)."
  (let* ((compilation-finish-functions nil)
         (buf (generate-new-buffer " *fake-compile-kill*"))
         (failure 0))
    (unwind-protect
        (progn
          (deb-packaging-commands--after-compile
           buf (lambda () nil) (lambda () (cl-incf failure)))
          (run-hook-with-args 'compilation-finish-functions buf "killed\n")
          (should (= failure 0)))
      (kill-buffer buf))))

(ert-deftest deb-packaging-test-pq/drop-asks-before-deleting ()
  "Patch-queue commits are lost; no compile without confirmation."
  (let ((compiled 0))
    (cl-letf (((symbol-function 'deb-packaging-pq--ensure-quilt-repo) #'ignore)
              ((symbol-function 'deb-packaging-pq--state)
               (lambda () (list :on-pq-p t :branch "patch-queue/main"
                                 :pq-branch "patch-queue/main" :exists-p t)))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
              ((symbol-function 'deb-packaging-commands--compile)
               (lambda (&rest _) (cl-incf compiled) nil)))
      (let ((compilation-finish-functions nil))
        (deb-packaging-pq-drop)
        (should (= compiled 0))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (deb-packaging-pq-drop)
          (should (= compiled 1)))))))

(ert-deftest deb-packaging-test-pq/drop-without-branch-errors ()
  (cl-letf (((symbol-function 'deb-packaging-pq--ensure-quilt-repo) #'ignore)
            ((symbol-function 'deb-packaging-pq--state)
             (lambda () (list :on-pq-p nil :branch "main"
                               :pq-branch "patch-queue/main" :exists-p nil))))
    (should-error (deb-packaging-pq-drop) :type 'user-error)))

(provide 'deb-packaging-test-pq)
;;; deb-packaging-test-pq.el ends here
