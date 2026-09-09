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

(ert-deftest deb-packaging-test-dev/every-profile-declares-a-server ()
  (dolist (e deb-packaging-dev-language-profiles)
    (should (stringp (plist-get (cddr e) :server)))))

(ert-deftest deb-packaging-test-dev/profile-apts-accepts-legacy-string ()
  "A space-separated :APT string splits into separate packages so it
quotes correctly downstream (the shape that broke the c/c++ install)."
  (should (equal (deb-packaging-dev--profile-apts
                  (list (list 'legacy "Legacy" :apt "clangd bear"
                              :server "clangd")))
                 '("clangd" "bear"))))

(ert-deftest deb-packaging-test-dev/langs-layer-with-apts ()
  (let ((s (deb-packaging-test-dev--join
            (deb-packaging-dev--script-langs-layer
             "ctr" "LFP" '("clangd" "bear") nil '("clangd")))))
    (should (string-match-p "Installing language servers" s))
    ;; Shell-quoted, so the space escapes.
    (should (string-match-p "apt-get\\\\ update" s))
    (should (string-match-p "clangd" s))
    (should (string-match-p "bear" s))
    (should (string-match-p "FP_LANGS=LFP" s))
    ;; The skip honors the self-heal force next to the global one.
    (should (string-match-p "\\[ -z \"\\$FORCE\\$LANGS_FORCE\" \\]" s))
    ;; Install is verified before the marker is written: a failed
    ;; install must not be cached as provisioned.
    (should (string-match-p "Language server install failed" s))
    (should (string-match-p "exit 1" s))
    (should (< (string-match "Language server install failed" s)
               (string-match "echo\\\\ LFP" s)))))

(ert-deftest deb-packaging-test-dev/langs-layer-no-apts ()
  (let ((s (deb-packaging-test-dev--join
            (deb-packaging-dev--script-langs-layer "ctr" "LFP" nil nil nil))))
    (should-not (string-match-p "Installing language servers" s))
    ;; No servers to verify, so no verification block.
    (should-not (string-match-p "Language server install failed" s))
    ;; Marker is still written even with nothing to install.
    (should (string-match-p "FP_LANGS=LFP" s))
    (should (string-match-p "/root/.deb-dev-marker-langs" s))))

(ert-deftest deb-packaging-test-dev/langs-layer-with-setups ()
  (let ((s (deb-packaging-test-dev--join
            (deb-packaging-dev--script-langs-layer
             "ctr" "LFP" nil '("go install gopls" "npm install -g x")
             '("gopls")))))
    ;; Setup commands are shell-quoted, so their spaces escape.
    (should (string-match-p "go\\\\ install\\\\ gopls" s))
    (should (string-match-p "npm\\\\ install" s))
    (should (string-match-p "/root/.deb-dev-marker-langs" s))))

;;; Missing-server probe (self-heal pre-flight)

(ert-deftest deb-packaging-test-dev/missing-servers-parses-probe-output ()
  (let (probe-args)
    (cl-letf (((symbol-function 'deb-packaging-dev--container-exists-p)
               (lambda (&rest _) t))
              ((symbol-function 'deb-packaging-detect--call-process-string)
               (lambda (_program &rest args)
                 (setq probe-args args)
                 "clangd\n")))
      (should (equal (deb-packaging-dev--missing-servers
                      "ctr"
                      (list (assq 'c/c++ deb-packaging-dev-language-profiles)))
                     '("clangd")))
      ;; The probe augments PATH (go installs outside the login PATH)
      ;; and checks each server with command -v.
      (let ((cmd (car (last probe-args))))
        (should (string-match-p "/root/go/bin" cmd))
        (should (string-match-p "command -v" cmd))))))

(ert-deftest deb-packaging-test-dev/missing-servers-empty-probe-none-missing ()
  (cl-letf (((symbol-function 'deb-packaging-dev--container-exists-p)
             (lambda (&rest _) t))
            ((symbol-function 'deb-packaging-detect--call-process-string)
             (lambda (&rest _) "")))
    (should (null (deb-packaging-dev--missing-servers
                   "ctr"
                   (list (assq 'c/c++ deb-packaging-dev-language-profiles)))))))

(ert-deftest deb-packaging-test-dev/missing-servers-nil-when-no-container ()
  (cl-letf (((symbol-function 'deb-packaging-dev--container-exists-p)
             (lambda (&rest _) nil))
            ((symbol-function 'deb-packaging-detect--call-process-string)
             (lambda (&rest _) (error "must not probe"))))
    (should (null (deb-packaging-dev--missing-servers
                   "ctr"
                   (list (assq 'c/c++ deb-packaging-dev-language-profiles)))))))

(ert-deftest deb-packaging-test-dev/provision-script-langs-force ()
  (let ((s (deb-packaging-dev--provision-script
            "deb-dev-foo-noble" "noble" "/home/u/foo" "/root/work/foo" "foo"
            "cfp" "lfp" "tfp" nil
            (list (assq 'c/c++ deb-packaging-dev-language-profiles))
            '("clangd"))))
    ;; Newline anchors: LANGS_FORCE=1 must not match the FORCE=1 probe.
    (should (string-match-p "\nLANGS_FORCE=1" s))
    (should-not (string-match-p "\nFORCE=1" s))))

(ert-deftest deb-packaging-test-dev/provision-script-quotes-profile-apts-correctly ()
  "Regression: the c/c++ profile once had :apt \"clangd bear\" as one
string, which the layer double-escaped into a single apt argument and
failed with \"Unable to locate package clangd bear\" while still writing
the layer marker."
  (let ((s (deb-packaging-dev--provision-script
            "deb-dev-foo-noble" "noble" "/home/u/foo" "/root/work/foo" "foo"
            "cfp" "lfp" "tfp" nil
            (list (assq 'c/c++ deb-packaging-dev-language-profiles)))))
    ;; Single-escaped: apt receives clangd and bear as separate args.
    (should (string-match-p "recommends\\\\ clangd\\\\ bear" s))))

(ert-deftest deb-packaging-test-dev/dev-shell-forces-langs-when-server-missing ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3" :distro "noble")
    (let ((script nil)
          (messages nil))
      (cl-letf (((symbol-function 'deb-packaging-dev--container-exists-p)
                 (lambda (&rest _) t))
                ((symbol-function 'deb-packaging-detect--call-process-string)
                 (lambda (&rest _) ""))
                ((symbol-function 'deb-packaging-dev--read-langs-cache)
                 (lambda (&rest _) '(c/c++)))
                ((symbol-function 'deb-packaging-dev--missing-servers)
                 (lambda (_name _profiles) '("clangd")))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages)))
                ((symbol-function 'deb-packaging-commands--run-command)
                 (lambda (_name args &rest _) (setq script (nth 2 args)) nil)))
        (deb-packaging-dev-shell)
        (should script)
        (should (string-match-p "LANGS_FORCE=1" script))
        (should (cl-some
                (lambda (m) (string-match-p "Re-installing missing language servers" m))
                messages))))))

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

;;; Eglot pre-check

(ert-deftest deb-packaging-test-dev/cached-profiles-parses-container-name ()
  "Hyphenated package names parse correctly: the distro is the last
component, the rest is the package."
  (cl-letf (((symbol-function 'deb-packaging-dev--read-langs-cache)
             (lambda (pkg distro)
               (should (equal pkg "linux-tools"))
               (should (equal distro "noble"))
               '(c/c++))))
    (should (equal (deb-packaging-dev--cached-profiles-for
                    "deb-dev-linux-tools-noble")
                   (list (assq 'c/c++ deb-packaging-dev-language-profiles))))))

(defmacro deb-packaging-test-dev--with-lxc-buffer (path &rest body)
  "Run BODY in a temp buffer visiting PATH, then clean up."
  (declare (indent 1) (debug (form body)))
  `(let ((buf (generate-new-buffer " *deb-test-lxc*")))
     (unwind-protect
         (with-current-buffer buf
           (setq buffer-file-name ,path)
           (setq default-directory (file-name-directory ,path))
           ,@body)
       (kill-buffer buf))))

(ert-deftest deb-packaging-test-dev/eglot-errors-when-all-servers-missing ()
  (deb-packaging-test-dev--with-lxc-buffer
      "/lxc:deb-dev-foo-noble:/root/work/foo/src/foo.c"
    (cl-letf (((symbol-function 'deb-packaging-dev--read-langs-cache)
               (lambda (&rest _) '(c/c++)))
              ((symbol-function 'deb-packaging-dev--missing-servers)
               (lambda (_container _profiles) '("clangd")))
              ((symbol-function 'eglot-ensure)
               (lambda () (error "must not eglot"))))
      (should-error (deb-packaging-dev-eglot) :type 'user-error))))

(ert-deftest deb-packaging-test-dev/eglot-continues-when-some-servers-missing ()
  ;; Pre-load eglot: the `require' inside the command would otherwise
  ;; clobber the eglot-ensure mock by defining the real function.
  (require 'eglot)
  (let ((ran nil)
        (messages nil))
    (deb-packaging-test-dev--with-lxc-buffer
        "/lxc:deb-dev-foo-noble:/root/work/foo/src/foo.c"
      (cl-letf (((symbol-function 'deb-packaging-dev--read-langs-cache)
                 (lambda (&rest _) '(c/c++ python)))
                ((symbol-function 'deb-packaging-dev--missing-servers)
                 (lambda (_container _profiles) '("clangd")))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages)))
                ((symbol-function 'eglot-ensure)
                 (lambda () (setq ran t))))
        (deb-packaging-dev-eglot)
        (should ran)
        (should (cl-some
                (lambda (m) (string-match-p "Missing language servers" m))
                messages))))))

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

;;; Host-only guard

;; A remote default-directory must produce the friendly host-only
;; user-error, not a TRAMP pkg-dir and lxc calls on the wrong host.
;; "/ssh:host:/..." satisfies `file-remote-p' without connecting.

(defmacro deb-packaging-test-dev--with-remote-default-dir (&rest body)
  "Run BODY with `default-directory' on a fake remote host.
`locate-dominating-file' and `call-process' are mocked to fail loudly,
proving neither the TRAMP probe nor any lxc call leaks out."
  (declare (indent 0) (debug (body)))
  `(let ((default-directory "/ssh:host:/tmp/pkg/"))
     (cl-letf (((symbol-function 'locate-dominating-file)
                (lambda (&rest _) (error "must not probe TRAMP")))
               ((symbol-function 'call-process)
                (lambda (&rest _) (error "must not call-process"))))
       ,@body)))

(ert-deftest deb-packaging-test-dev/dev-shell-host-only-errors ()
  (deb-packaging-test-dev--with-remote-default-dir
    (should-error (deb-packaging-dev-shell) :type 'user-error)))

(ert-deftest deb-packaging-test-dev/dev-open-host-only-errors ()
  (deb-packaging-test-dev--with-remote-default-dir
    (should-error (deb-packaging-dev-open) :type 'user-error)))

(ert-deftest deb-packaging-test-dev/dev-exec-host-only-errors ()
  (deb-packaging-test-dev--with-remote-default-dir
    (should-error (deb-packaging-dev-exec) :type 'user-error)))

(ert-deftest deb-packaging-test-dev/dev-compile-db-host-only-errors ()
  (deb-packaging-test-dev--with-remote-default-dir
    (should-error (deb-packaging-dev-compile-db) :type 'user-error)))

(ert-deftest deb-packaging-test-dev/dev-destroy-host-only-errors ()
  (deb-packaging-test-dev--with-remote-default-dir
    (should-error (deb-packaging-dev-destroy) :type 'user-error)))

(provide 'deb-packaging-test-dev)
;;; deb-packaging-test-dev.el ends here
