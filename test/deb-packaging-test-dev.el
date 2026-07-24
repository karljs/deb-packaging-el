;;; deb-packaging-test-dev.el --- Dev-container provision tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for the LXD dev-container provision-script builder in
;; deb-packaging-dev.el.  Each layer is a pure string-list producer, so
;; the tests assert on the generated shell by pattern, and confirm the
;; whole script is a flat, joinable string of lines (the property whose
;; violation previously crashed `string-join').

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'deb-packaging-test)
(require 'deb-packaging-test-run)
(require 'deb-packaging-dev)

(defun deb-packaging-test-dev--join (lines)
  "Join LINES with newlines, asserting each element is a string.
Mirrors what `string-join' does inside the orchestrator, so a nested
list (the historical bug) is caught here rather than at runtime."
  (should (cl-every #'stringp lines))
  (mapconcat #'identity lines "\n"))

;;; Container setup

(ert-deftest deb-packaging-test-dev/container-setup-core-commands ()
  (let ((s (deb-packaging-test-dev--join
            (deb-packaging-dev--script-container-setup
             "ctr" "ubuntu-daily:noble" "1000" "/host/pkg"
             "/root/work/pkg" "work-pkg"))))
    (should (string-match-p "\\`set -e" s))
    ;; The image is shell-quoted, so the colon is escaped.
    (should (string-match-p "lxc launch ubuntu-daily\\\\:noble ctr" s))
    (should (string-match-p "raw.idmap \"both 1000 0\"" s))
    (should (string-match-p "cloud-init status --wait" s))
    (should (string-match-p "lxc config device add ctr work-pkg disk source=/host/pkg path=/root/work/pkg" s))))

(ert-deftest deb-packaging-test-dev/force-line ()
  (should (equal (deb-packaging-dev--script-force-line t) '("FORCE=1")))
  (should (equal (deb-packaging-dev--script-force-line nil) '("FORCE="))))

;;; Core helpers

(ert-deftest deb-packaging-test-dev/core-helpers ()
  (let ((s (deb-packaging-test-dev--join
            (deb-packaging-dev--script-core-helpers "ctr"))))
    (should (string-match-p "dpkg -s devscripts" s))
    (should (string-match-p "devscripts\\\\ equivs" s))))

;;; Build-deps layer

(ert-deftest deb-packaging-test-dev/build-deps-layer-marker-and-run ()
  (let ((s (deb-packaging-test-dev--join
            (deb-packaging-dev--script-build-deps-layer
             "ctr" "/root/work/pkg" "CFP"))))
    ;; Skip branch present (shell decides via $FORCE + marker).
    (should (string-match-p "Build-deps up to date, skipping mk-build-deps" s))
    ;; Run branch present.
    (should (string-match-p "mk-build-deps" s))
    ;; The mk-build-deps command is shell-quoted, so `cd ' escapes its space.
    (should (string-match-p "cd\\\\ /root/work/pkg" s))
    (should (string-match-p "FP_CONTROL=CFP" s))
    (should (string-match-p "/root/.deb-dev-marker-control" s))))

;;; Language servers layer

(ert-deftest deb-packaging-test-dev/langs-layer-with-apts ()
  (let ((s (deb-packaging-test-dev--join
            (deb-packaging-dev--script-langs-layer
             "ctr" "LFP" '("clangd" "bear") nil))))
    (should (string-match-p "Installing language servers" s))
    (should (string-match-p "clangd" s))
    (should (string-match-p "bear" s))
    (should (string-match-p "FP_LANGS=LFP" s))
    (should (string-match-p "/root/.deb-dev-marker-langs" s))))

(ert-deftest deb-packaging-test-dev/langs-layer-no-apts ()
  (let ((s (deb-packaging-test-dev--join
            (deb-packaging-dev--script-langs-layer "ctr" "LFP" nil nil))))
    (should-not (string-match-p "Installing language servers" s))
    ;; Marker is still written even with nothing to install.
    (should (string-match-p "FP_LANGS=LFP" s))
    (should (string-match-p "/root/.deb-dev-marker-langs" s))))

(ert-deftest deb-packaging-test-dev/langs-layer-with-setups ()
  (let ((s (deb-packaging-test-dev--join
            (deb-packaging-dev--script-langs-layer
             "ctr" "LFP" nil '("go install gopls" "npm install -g x")))))
    ;; Setup commands are shell-quoted, so their spaces escape.
    (should (string-match-p "go\\\\ install\\\\ gopls" s))
    (should (string-match-p "npm\\\\ install" s))
    (should (string-match-p "/root/.deb-dev-marker-langs" s))))

;;; Dev tools layer

(ert-deftest deb-packaging-test-dev/tools-layer-with-packages ()
  (let ((s (deb-packaging-test-dev--join
            (deb-packaging-dev--script-tools-layer
             "ctr" "TFP" "git gdb strace"))))
    (should (string-match-p "Installing dev tools" s))
    ;; extra-apt is embedded in a shell-quoted command, so spaces escape.
    (should (string-match-p "git\\\\ gdb\\\\ strace" s))
    (should (string-match-p "FP_TOOLS=TFP" s))
    (should (string-match-p "/root/.deb-dev-marker-tools" s))))

(ert-deftest deb-packaging-test-dev/tools-layer-empty ()
  (let ((s (deb-packaging-test-dev--join
            (deb-packaging-dev--script-tools-layer "ctr" "TFP" ""))))
    (should-not (string-match-p "Installing dev tools" s))
    ;; Marker still written.
    (should (string-match-p "FP_TOOLS=TFP" s))
    (should (string-match-p "/root/.deb-dev-marker-tools" s))))

;;; Full script (regression: must be a flat, joinable string in all cases)

(ert-deftest deb-packaging-test-dev/provision-script-returns-string ()
  "Full script builds a string across profile/extra/force combinations.
Regression for the nested-list bug that made `string-join' crash whenever
the langs or tools layer had packages to install."
  (dolist (extra (list '("git" "gdb" "strace") nil '("gdb")))
    (let ((deb-packaging-dev-extra-packages extra))
      (dolist (profiles
               (list nil
                     (list (assq 'c/c++ deb-packaging-dev-language-profiles))
                     (list (assq 'go deb-packaging-dev-language-profiles)
                           (assq 'python deb-packaging-dev-language-profiles))))
        (dolist (force '(nil t))
          (let ((s (deb-packaging-dev--provision-script
                    "deb-dev-foo-noble" "noble" "/home/u/foo"
                    "/root/work/foo" "foo" "cfp" "lfp" "tfp" force profiles)))
            (should (stringp s))
            (should (> (length s) 0))))))))

(ert-deftest deb-packaging-test-dev/provision-script-layer-order ()
  "Layers appear in the expected order with the READY sentinel last."
  (let* ((deb-packaging-dev-extra-packages '("gdb"))
         (s (deb-packaging-dev--provision-script
             "deb-dev-foo-noble" "noble" "/home/u/foo" "/root/work/foo" "foo"
             "cfp" "lfp" "tfp" t
             (list (assq 'c/c++ deb-packaging-dev-language-profiles))))
         (i-setup (string-match "set -e" s))
         (i-core (string-match "Installing core build helpers" s))
         (i-bd (string-match "FP_CONTROL=" s))
         (i-langs (string-match "FP_LANGS=" s))
         (i-tools (string-match "FP_TOOLS=" s))
         (i-ready (string-match "echo READY: /lxc:deb-dev-foo-noble:/root/work/foo" s)))
    (should (and i-setup i-core i-bd i-langs i-tools i-ready))
    (should (< i-setup i-core i-bd i-langs i-tools i-ready))))

;;; Provisioning sentinel

(ert-deftest deb-packaging-test-dev/open-on-success-displays-without-switching ()
  "The sentinel shows the container dired but never steals the current window."
  (let* ((proc (make-process :name "deb-test-true"
                             :command '("true")
                             :noquery t))
         (dired-buf (get-buffer-create " *deb-test-dired*"))
         (displayed nil)
         (switched nil))
    (unwind-protect
        (cl-letf (((symbol-function 'dired)
                   (lambda (&rest _) (setq switched t)))
                  ((symbol-function 'dired-noselect)
                   (lambda (&rest _) dired-buf))
                  ((symbol-function 'display-buffer)
                   (lambda (buf &rest _) (setq displayed buf))))
          (deb-packaging-dev--open-on-success proc "/lxc:c:/root/work")
          (deb-packaging-test-run--wait proc)
          (should-not switched)
          (should (eq displayed dired-buf)))
      (kill-buffer dired-buf))))

;;; Language selection persistence

(ert-deftest deb-packaging-test-dev/select-profiles-none-persists ()
  "Choosing no languages writes the sentinel so the prompt stops recurring."
  (let ((tmp (make-temp-file "deb-dev-test-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'deb-packaging-detect--cache-dir)
                   (lambda () tmp))
                  ((symbol-function 'completing-read-multiple)
                   (lambda (&rest _) nil)))
          (deb-packaging-dev--select-profiles "foo" "noble")
          (should (equal (deb-packaging-dev--read-langs-cache "foo" "noble")
                         (list deb-packaging-dev--no-langs-key))))
      (delete-directory tmp t))))

(ert-deftest deb-packaging-test-dev/need-langs-prompt-decisions ()
  (let ((fp (deb-packaging-dev--langs-fingerprint nil)))
    (should (deb-packaging-dev--need-langs-prompt-p nil fp "other"))
    (should-not (deb-packaging-dev--need-langs-prompt-p nil fp fp))
    (should-not (deb-packaging-dev--need-langs-prompt-p nil fp ""))
    (should (deb-packaging-dev--need-langs-prompt-p nil nil ""))
    (should (deb-packaging-dev--need-langs-prompt-p t fp fp))))

;;; Project dispatch

(ert-deftest deb-packaging-test-dev/project-uses-projectile-when-root-found ()
  (let (used)
    (cl-letf (((symbol-function 'deb-packaging-dev--tramp-path-for-current)
               (lambda () "/lxc:c:/root/work/foo"))
              ((symbol-function 'projectile-project-root)
               (lambda () "/lxc:c:/root/work/foo/"))
              ((symbol-function 'projectile-find-file)
               (lambda () (interactive) (setq used 'projectile)))
              ((symbol-function 'project-find-file)
               (lambda () (interactive) (setq used 'project)))
              ((symbol-function 'dired)
               (lambda (&rest _) (setq used 'dired))))
      (deb-packaging-dev-project)
      (should (eq used 'projectile)))))

(ert-deftest deb-packaging-test-dev/project-falls-to-project-el-without-root ()
  (let (used)
    (cl-letf (((symbol-function 'deb-packaging-dev--tramp-path-for-current)
               (lambda () "/lxc:c:/root/work/foo"))
              ((symbol-function 'projectile-project-root)
               (lambda () nil))
              ((symbol-function 'projectile-find-file)
               (lambda () (interactive) (setq used 'projectile)))
              ((symbol-function 'project-find-file)
               (lambda () (interactive) (setq used 'project)))
              ((symbol-function 'dired)
               (lambda (&rest _) (setq used 'dired))))
      (deb-packaging-dev-project)
      (should (eq used 'project)))))

(ert-deftest deb-packaging-test-dev/project-falls-to-dired-without-either ()
  (let (used)
    (cl-letf (((symbol-function 'deb-packaging-dev--tramp-path-for-current)
               (lambda () "/lxc:c:/root/work/foo"))
              ((symbol-function 'projectile-find-file) nil)
              ((symbol-function 'project-find-file) nil)
              ((symbol-function 'dired)
               (lambda (&rest _) (setq used 'dired))))
      (deb-packaging-dev-project)
      (should (eq used 'dired)))))

;;; Destroy existence check

(ert-deftest deb-packaging-test-dev/destroy-errors-when-container-missing ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3" :distro "noble")
    (cl-letf (((symbol-function 'deb-packaging-dev--list-containers)
               (lambda (&rest _) nil))
              ((symbol-function 'deb-packaging-dev--container-exists-p)
               (lambda (&rest _) nil))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _) (error "must not confirm"))))
      (should-error (deb-packaging-dev-destroy) :type 'user-error))))

(ert-deftest deb-packaging-test-dev/destroy-wraps-sentinel-for-refresh ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3" :distro "noble")
    (let ((proc nil)
          (original-sentinel nil)
          (buf (generate-new-buffer " *deb-test-destroy*")))
      (unwind-protect
          (cl-letf (((symbol-function 'deb-packaging-dev--container-exists-p)
                     (lambda (&rest _) t))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'deb-packaging-commands--run-command)
                     (lambda (&rest _)
                       (setq proc (make-process :name "deb-test-true"
                                                :buffer buf
                                                :command '("true")
                                                :noquery t))
                       (setq original-sentinel (process-sentinel proc))
                       buf)))
            (deb-packaging-dev-destroy)
            (should proc)
            ;; The default sentinel was replaced by a wrapped one.
            (should (not (eq (process-sentinel proc) original-sentinel))))
        (when proc (delete-process proc))
        (kill-buffer buf)))))

(provide 'deb-packaging-test-dev)
;;; deb-packaging-test-dev.el ends here
