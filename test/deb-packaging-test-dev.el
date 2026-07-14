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

(provide 'deb-packaging-test-dev)
;;; deb-packaging-test-dev.el ends here
