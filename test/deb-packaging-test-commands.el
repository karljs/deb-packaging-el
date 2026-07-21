;;; deb-packaging-test-commands.el --- Command arg & parse tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for argument filtering, lint-output parsing, and repository
;; expansion in deb-packaging-commands.el.

;;; Code:

(require 'ert)
(require 'deb-packaging-test)
(require 'deb-packaging-commands)
(require 'deb-packaging-infra)
(require 'deb-packaging-transients)
(require 'deb-packaging-repos)
(require 'deb-packaging-ppa)

;;; deb-packaging-commands--filter-args

(ert-deftest deb-packaging-test-commands/filter-keeps-exact-bare-flag ()
  (should (equal (deb-packaging-commands--filter-args
                  '("-i" "-I" "--foo")
                  deb-packaging-commands--lintian-arg-prefixes)
                 '("-i" "-I"))))

(ert-deftest deb-packaging-test-commands/filter-keeps-prefix-flag-with-value ()
  (should (equal (deb-packaging-commands--filter-args
                  '("--color=auto" "--tag-display-limit=5" "--foo")
                  deb-packaging-commands--lintian-arg-prefixes)
                 '("--color=auto" "--tag-display-limit=5"))))

(ert-deftest deb-packaging-test-commands/filter-drops-non-matching ()
  (should (null (deb-packaging-commands--filter-args
                 '("--verbose" "--json" "--foo")
                 deb-packaging-commands--lintian-arg-prefixes))))

(ert-deftest deb-packaging-test-commands/filter-empty-args ()
  (should (null (deb-packaging-commands--filter-args nil deb-packaging-commands--lintian-arg-prefixes)))
  (should (null (deb-packaging-commands--filter-args '() deb-packaging-commands--ubuntu-lint-arg-prefixes))))

(ert-deftest deb-packaging-test-commands/filter-separates-lintian-and-ubuntu-prefixes ()
  (let ((lintian-args '("-i" "--pedantic" "--color=auto" "--verbose" "--json"))
        (ubuntu-args '("--verbose" "--json" "--context=ctx" "--all=yes" "-i" "--color=auto")))
    (should (equal (deb-packaging-commands--filter-args lintian-args deb-packaging-commands--lintian-arg-prefixes)
                   '("-i" "--pedantic" "--color=auto")))
    (should (equal (deb-packaging-commands--filter-args ubuntu-args deb-packaging-commands--ubuntu-lint-arg-prefixes)
                   '("--verbose" "--json" "--context=ctx" "--all=yes")))))

;;; deb-packaging-commands--parse-lint-summary

(ert-deftest deb-packaging-test-commands/parse-lint-summary-counts ()
  (let ((buf (generate-new-buffer " *lint-summary-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "E: foo: bad\nW: foo: meh\nI: foo: note\nE: foo: bad2\n"))
          (should (equal (deb-packaging-commands--parse-lint-summary (buffer-name buf))
                         '(:error 2 :warning 1 :info 1))))
      (kill-buffer buf))))

(ert-deftest deb-packaging-test-commands/parse-lint-summary-zero-findings ()
  (let ((buf (generate-new-buffer " *lint-zero-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "Some unrelated output\nNo errors here\n"))
          (should (equal (deb-packaging-commands--parse-lint-summary (buffer-name buf))
                         '(:error 0 :warning 0 :info 0))))
      (kill-buffer buf))))

(ert-deftest deb-packaging-test-commands/parse-lint-summary-non-live-buffer ()
  (let ((buf (generate-new-buffer " *lint-dead-test*")))
    (kill-buffer buf)
    (should (null (deb-packaging-commands--parse-lint-summary " *lint-dead-test*")))))

;;; deb-packaging-commands--parse-ubuntu-lint-summary

(ert-deftest deb-packaging-test-commands/parse-ubuntu-lint-summary-full-line ()
  (let ((buf (generate-new-buffer " *ubuntu-lint-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "Some output\nSummary: ran 12 lint checks (OK: 10, SKIP: 1, WARN: 1, ERROR: 0, FAIL: 0)\n"))
          (should (equal (deb-packaging-commands--parse-ubuntu-lint-summary (buffer-name buf))
                         '(:ok 10 :skip 1 :warn 1 :error 0 :fail 0))))
      (kill-buffer buf))))

(ert-deftest deb-packaging-test-commands/parse-ubuntu-lint-summary-missing ()
  (let ((buf (generate-new-buffer " *ubuntu-lint-missing-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "Some output without a summary line\n"))
          (should (null (deb-packaging-commands--parse-ubuntu-lint-summary (buffer-name buf)))))
      (kill-buffer buf))))

;;; deb-packaging-commands--parse-sbuild-summary

(ert-deftest deb-packaging-test-commands/parse-sbuild-summary-kept-session ()
  "The sbuild parser extracts a kept session name."
  (with-temp-buffer
    (insert "noise\nKeeping session: stonking-amd64-cb3ccddc-8ae0\nmore\n")
    (should (equal (deb-packaging-commands--parse-sbuild-summary (buffer-name))
                   '(:kept-session "stonking-amd64-cb3ccddc-8ae0")))))

(ert-deftest deb-packaging-test-commands/parse-sbuild-summary-none ()
  "The sbuild parser returns nil when no session was kept."
  (with-temp-buffer
    (insert "Status: successful\n")
    (should (null (deb-packaging-commands--parse-sbuild-summary (buffer-name))))))

;;; deb-packaging-commands--run-summary-parser

(ert-deftest deb-packaging-test-commands/run-summary-parser-lintian-source ()
  (should (eq (deb-packaging-commands--run-summary-parser 'lintian-source)
              #'deb-packaging-commands--parse-lint-summary)))

(ert-deftest deb-packaging-test-commands/run-summary-parser-lintian-binary ()
  (should (eq (deb-packaging-commands--run-summary-parser 'lintian-binary)
              #'deb-packaging-commands--parse-lint-summary)))

(ert-deftest deb-packaging-test-commands/run-summary-parser-ubuntu-lint ()
  (should (eq (deb-packaging-commands--run-summary-parser 'ubuntu-lint)
              #'deb-packaging-commands--parse-ubuntu-lint-summary)))

(ert-deftest deb-packaging-test-commands/run-summary-parser-unknown ()
  (should (null (deb-packaging-commands--run-summary-parser 'something-else)))
  (should (null (deb-packaging-commands--run-summary-parser nil))))

;;; deb-packaging-commands--expand-extra-repo

(ert-deftest deb-packaging-test-commands/expand-extra-repo-variant ()
  (should (string= (deb-packaging-commands--expand-extra-repo "proposed" "noble")
                   "deb http://archive.ubuntu.com/ubuntu/ noble-proposed main")))

(ert-deftest deb-packaging-test-commands/expand-extra-repo-ppa ()
  (should (string= (deb-packaging-commands--expand-extra-repo "ppa:me/x" "noble")
                   "deb [trusted=yes] http://ppa.launchpadcontent.net/me/x/ubuntu/ noble main")))

(ert-deftest deb-packaging-test-commands/expand-extra-repo-raw ()
  (let ((raw "deb http://example.com/ubuntu noble main"))
    (should (string= (deb-packaging-commands--expand-extra-repo raw "noble") raw))))

;;; extra-repo multi-value reader and format

(defun deb-packaging-test-commands--repo-obj (&optional value)
  "Return an extra-repo infix object, its value slot set to VALUE if given."
  (let ((obj (make-instance 'deb-packaging-transients--extra-repo-argument)))
    (when value
      (oset obj value value))
    obj))

(defmacro deb-packaging-test-commands--with-repo-read (choice &rest body)
  "Run BODY with repo candidates mocked and `completing-read' returning CHOICE."
  (declare (indent 1) (debug (form body)))
  `(cl-letf (((symbol-function 'deb-packaging-infra--list-ppas)
              (lambda () nil))
             ((symbol-function 'completing-read)
              (lambda (&rest _) ,choice))
             (deb-packaging-config-extra-ppas nil))
     ,@body))

(ert-deftest deb-packaging-test-commands/extra-repo-reader-adds-first ()
  (deb-packaging-test-commands--with-repo-read "ppa:me/x"
    (should (equal (deb-packaging-transients--extra-repo-read nil)
                   '("ppa:me/x")))))

(ert-deftest deb-packaging-test-commands/extra-repo-reader-accumulates ()
  "A second selection adds to the set rather than replacing it."
  (deb-packaging-test-commands--with-repo-read "proposed"
    (should (equal (deb-packaging-transients--extra-repo-read '("ppa:me/x"))
                   '("ppa:me/x" "proposed")))))

(ert-deftest deb-packaging-test-commands/extra-repo-reader-toggles-off ()
  "Selecting a present entry removes it."
  (deb-packaging-test-commands--with-repo-read "ppa:me/x"
    (should (equal (deb-packaging-transients--extra-repo-read
                    '("ppa:me/x" "proposed"))
                   '("proposed")))))

(ert-deftest deb-packaging-test-commands/extra-repo-reader-empty-keeps-set ()
  (deb-packaging-test-commands--with-repo-read ""
    (should (equal (deb-packaging-transients--extra-repo-read '("ppa:me/x"))
                   '("ppa:me/x")))
    (should (null (deb-packaging-transients--extra-repo-read nil)))))

(ert-deftest deb-packaging-test-commands/extra-repo-reader-legacy-string ()
  "A pre-multi-value string value is treated as a one-entry set."
  (deb-packaging-test-commands--with-repo-read "proposed"
    (should (equal (deb-packaging-transients--extra-repo-read "ppa:me/x")
                   '("ppa:me/x" "proposed")))))

(ert-deftest deb-packaging-test-commands/extra-repo-init-value-roundtrip ()
  "Flat --extra-repository= args in the prefix value restore as entries,
and re-emit without doubling the argument."
  (let ((obj (make-instance 'deb-packaging-transients--extra-repo-argument
                            :argument "--extra-repository="
                            :multi-value 'repeat))
        (transient--prefix (make-instance 'transient-prefix)))
    (oset transient--prefix value
          '("--dist=noble"
            "--extra-repository=ppa:me/x"
            "--extra-repository=proposed"))
    (transient-init-value obj)
    (should (equal (oref obj value) '("ppa:me/x" "proposed")))
    (should (equal (transient-infix-value obj)
                   '("--extra-repository=ppa:me/x"
                     "--extra-repository=proposed")))))

;;; extra-package multi-value reader and init-value

(defmacro deb-packaging-test-commands--with-pkg-read (choice &rest body)
  "Run BODY in a package tree with one .deb, `completing-read' returning CHOICE."
  (declare (indent 1) (debug (form body)))
  `(deb-packaging-test--with-package-tree
       '(:name "mypkg" :version "1.0-1" :distro "noble"
               :artifacts (("mypkg_1.0-1_amd64.deb" . "")))
     (let ((deb-path (expand-file-name "mypkg_1.0-1_amd64.deb"
                                       pkg-parent-dir)))
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (&rest _) ,choice)))
         ,@body))))

(ert-deftest deb-packaging-test-commands/extra-pkg-reader-adds-first ()
  (deb-packaging-test-commands--with-pkg-read deb-path
    (should (equal (deb-packaging-transients--extra-package-read nil)
                   (list deb-path)))))

(ert-deftest deb-packaging-test-commands/extra-pkg-reader-accumulates ()
  "A second selection adds to the set rather than replacing it."
  (deb-packaging-test-commands--with-pkg-read deb-path
    (should (equal (deb-packaging-transients--extra-package-read
                    '("/other/dep_1.0-1_amd64.deb"))
                   (list "/other/dep_1.0-1_amd64.deb" deb-path)))))

(ert-deftest deb-packaging-test-commands/extra-pkg-reader-toggles-off ()
  "Selecting a present entry removes it."
  (deb-packaging-test-commands--with-pkg-read deb-path
    (should (equal (deb-packaging-transients--extra-package-read
                    (list "/other/dep_1.0-1_amd64.deb" deb-path))
                   '("/other/dep_1.0-1_amd64.deb")))))

(ert-deftest deb-packaging-test-commands/extra-pkg-reader-empty-keeps-set ()
  (deb-packaging-test-commands--with-pkg-read ""
    (should (equal (deb-packaging-transients--extra-package-read
                    (list deb-path))
                   (list deb-path)))
    (should (null (deb-packaging-transients--extra-package-read nil)))))

(ert-deftest deb-packaging-test-commands/extra-pkg-init-value-roundtrip ()
  "Flat --extra-package= args in the prefix value restore as paths,
and re-emit without doubling the argument."
  (let ((obj (make-instance 'deb-packaging-transients--extra-package-argument
                            :argument "--extra-package="
                            :multi-value 'repeat))
        (transient--prefix (make-instance 'transient-prefix)))
    (oset transient--prefix value
          '("--dist=noble"
            "--extra-package=/a/dep_1.0-1_amd64.deb"
            "--extra-package=/b/lib_2.0-1_amd64.deb"))
    (transient-init-value obj)
    (should (equal (oref obj value)
                   '("/a/dep_1.0-1_amd64.deb" "/b/lib_2.0-1_amd64.deb")))
    (should (equal (transient-infix-value obj)
                   '("--extra-package=/a/dep_1.0-1_amd64.deb"
                     "--extra-package=/b/lib_2.0-1_amd64.deb")))))

(ert-deftest deb-packaging-test-commands/extra-repo-format-compact ()
  "Formatting shows entries only: no expansion, no full deb line."
  (let* ((obj (deb-packaging-test-commands--repo-obj
               '("ppa:me/x" "proposed")))
         (text (substring-no-properties (transient-format-value obj))))
    (should (string-match-p "ppa:me/x" text))
    (should (string-match-p "proposed" text))
    (should-not (string-match-p "deb " text))
    (should-not (string-match-p "launchpadcontent" text)))
  (should (string-match-p
           "none"
           (transient-format-value (deb-packaging-test-commands--repo-obj)))))

;;; deb-packaging-commands-sbuild multi-value

(ert-deftest deb-packaging-test-commands/sbuild-multiple-extra-repos ()
  "sbuild receives one expanded --extra-repository= flag per entry."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble"
              :artifacts (("mypkg_1.0-1.dsc" . "")))
    (let (captured-args captured-save)
      (cl-letf (((symbol-function 'deb-packaging-commands--run-command)
                 (lambda (_name args &optional _dir _key)
                   (setq captured-args args)))
                ((symbol-function 'deb-packaging-repos-save)
                 (lambda (pkg distro entries)
                   (setq captured-save (list pkg distro entries)))))
        (deb-packaging-commands-sbuild
         '("--dist=noble"
           "--extra-repository=ppa:me/x"
           "--extra-repository=proposed"
           "--extra-repository=deb http://example.com/ubuntu noble main")))
      (should (member "--extra-repository=deb [trusted=yes] http://ppa.launchpadcontent.net/me/x/ubuntu/ noble main"
                      captured-args))
      (should (member "--extra-repository=deb http://archive.ubuntu.com/ubuntu/ noble-proposed main"
                      captured-args))
      (should (member "--extra-repository=deb http://example.com/ubuntu noble main"
                      captured-args))
      (should (equal captured-save
                     '("mypkg" "noble"
                       ("ppa:me/x" "proposed" "deb http://example.com/ubuntu noble main")))))))

(ert-deftest deb-packaging-test-commands/sbuild-no-extra-repos-saves-empty ()
  "sbuild with no --extra-repository saves an empty set."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble"
              :artifacts (("mypkg_1.0-1.dsc" . "")))
    (let (captured-save)
      (cl-letf (((symbol-function 'deb-packaging-commands--run-command)
                 (lambda (_name _args &optional _dir _key)))
                ((symbol-function 'deb-packaging-repos-save)
                 (lambda (pkg distro entries)
                   (setq captured-save (list pkg distro entries)))))
        (deb-packaging-commands-sbuild '("--dist=noble")))
      (should (equal captured-save '("mypkg" "noble" nil))))))

(ert-deftest deb-packaging-test-commands/sbuild-purge-flags-pass-through ()
  "sbuild receives --purge-session= and --purge-build= verbatim."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble"
              :artifacts (("mypkg_1.0-1.dsc" . "")))
    (let (captured-args)
      (cl-letf (((symbol-function 'deb-packaging-commands--run-command)
                 (lambda (_name args &optional _dir _key)
                   (setq captured-args args)))
                ((symbol-function 'deb-packaging-repos-save) #'ignore))
        (deb-packaging-commands-sbuild
         '("--dist=noble" "--purge-session=always" "--purge-build=never")))
      (should (member "--purge-session=always" captured-args))
      (should (member "--purge-build=never" captured-args)))))

;;; deb-packaging-transients--binary-default-value restore

(ert-deftest deb-packaging-test-commands/binary-default-value-seeds-repos ()
  "The binary-build default value includes saved extra-repo entries."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble")
    (let* ((tmp (make-temp-file "deb-repos-test-" t))
           (process-environment (cons (format "XDG_CACHE_HOME=%s" tmp)
                                      process-environment))
           (deb-packaging-config-target-distro "noble")
           (deb-packaging-config--distro-user-set t))
      (unwind-protect
          (progn
            (deb-packaging-repos-save "mypkg" "noble"
                                      '("ppa:me/x" "proposed"))
            (let ((default (deb-packaging-transients--binary-default-value)))
              (should (member "--extra-repository=ppa:me/x" default))
              (should (member "--extra-repository=proposed" default))
              (should (member "--dist=noble" default))))
        (delete-directory tmp t)))))

(ert-deftest deb-packaging-test-commands/binary-default-value-no-saved-repos ()
  "With no saved repos, the default value has no --extra-repository= entries."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble")
    (let* ((tmp (make-temp-file "deb-repos-test-" t))
           (process-environment (cons (format "XDG_CACHE_HOME=%s" tmp)
                                      process-environment))
           (deb-packaging-config-target-distro "noble")
           (deb-packaging-config--distro-user-set t))
      (unwind-protect
          (let ((default (deb-packaging-transients--binary-default-value)))
            (should (member "--dist=noble" default))
            (should-not (cl-some (lambda (a)
                                   (string-prefix-p "--extra-repository=" a))
                                 default)))
        (delete-directory tmp t)))))

;;; deb-packaging-commands--ppa-repo-line

(ert-deftest deb-packaging-test-commands/ppa-repo-line-valid ()
  (should (string= (deb-packaging-commands--ppa-repo-line "ppa:owner/name" "noble")
                   "deb [trusted=yes] http://ppa.launchpadcontent.net/owner/name/ubuntu/ noble main")))

(ert-deftest deb-packaging-test-commands/ppa-repo-line-invalid ()
  (should (null (deb-packaging-commands--ppa-repo-line "not-a-ppa" "noble")))
  (should (null (deb-packaging-commands--ppa-repo-line "http://example.com" "noble"))))

;;; deb-packaging-commands--runner-choices

(ert-deftest deb-packaging-test-commands/runner-choices ()
  (let ((choices (deb-packaging-commands--runner-choices)))
    (should (member "lxd" choices))
    (should (member "qemu" choices))
    (should (equal (length choices)
                   (length deb-packaging-commands-test-runners)))))

;;; deb-packaging-commands--test-image-info

(ert-deftest deb-packaging-test-commands/test-image-info-lxd-exists ()
  (deb-packaging-test--with-mocked-process '(("lxc" . 0))
    (let ((info (deb-packaging-commands--test-image-info "lxd" "noble")))
      (should (equal (plist-get info :runner) "lxd"))
      (should (string= (plist-get info :image)
                       "autopkgtest/ubuntu/noble/amd64"))
      (should (plist-get info :exists)))))

(ert-deftest deb-packaging-test-commands/test-image-info-lxd-missing ()
  (deb-packaging-test--with-mocked-process '(("lxc" . 1))
    (let ((info (deb-packaging-commands--test-image-info "lxd" "noble")))
      (should (equal (plist-get info :runner) "lxd"))
      (should (string= (plist-get info :image)
                       "autopkgtest/ubuntu/noble/amd64"))
      (should (null (plist-get info :exists))))))

(ert-deftest deb-packaging-test-commands/test-image-info-qemu ()
  (let ((info (deb-packaging-commands--test-image-info "qemu" "noble")))
    (should (equal (plist-get info :runner) "qemu"))
    (should (string= (plist-get info :image)
                     "/var/lib/adt-images/autopkgtest-noble-amd64.img"))
    (should (null (plist-get info :exists)))))

;;; deb-packaging-commands--test-image-build-hint

(ert-deftest deb-packaging-test-commands/test-image-build-hint-lxd ()
  (should (string= (deb-packaging-commands--test-image-build-hint "lxd" "noble")
                   "autopkgtest-build-lxd ubuntu-daily:noble")))

(ert-deftest deb-packaging-test-commands/test-image-build-hint-qemu ()
  (should (string= (deb-packaging-commands--test-image-build-hint "qemu" "noble")
                   "autopkgtest-buildvm-ubuntu-cloud -r noble")))

(ert-deftest deb-packaging-test-commands/test-image-build-hint-unknown ()
  (should (null (deb-packaging-commands--test-image-build-hint "docker" "noble"))))

;;; deb-packaging-commands--ubuntu-lint-context-args

(ert-deftest deb-packaging-test-commands/ubuntu-lint-context-args-source-dir ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (should (equal (deb-packaging-commands--ubuntu-lint-context-args "source-dir" pkg-dir)
                   (list "--source-dir" pkg-dir)))))

(ert-deftest deb-packaging-test-commands/ubuntu-lint-context-args-changelog ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (should (equal (deb-packaging-commands--ubuntu-lint-context-args "changelog" pkg-dir)
                   (list "--changelog" (expand-file-name "debian/changelog" pkg-dir))))))

(ert-deftest deb-packaging-test-commands/ubuntu-lint-context-args-changes-with-changes ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :artifacts '(("foo_1.2-3_source.changes" . "")))
    (should (equal (deb-packaging-commands--ubuntu-lint-context-args "changes" pkg-dir)
                   (list "--source-dir" pkg-dir
                         "--changes-file"
                         (expand-file-name "foo_1.2-3_source.changes" pkg-parent-dir))))))

(ert-deftest deb-packaging-test-commands/ubuntu-lint-context-args-changes-without-changes ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (should (equal (deb-packaging-commands--ubuntu-lint-context-args "changes" pkg-dir)
                   (list "--source-dir" pkg-dir)))))

;;; Transient --ppa= seeding

(ert-deftest deb-packaging-test-commands/upload-default-value-seeds-ppa ()
  "The upload default value includes the saved PPA."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble")
    (let* ((tmp (make-temp-file "deb-ppa-test-" t))
           (process-environment (cons (format "XDG_CACHE_HOME=%s" tmp)
                                      process-environment))
           (deb-packaging-config-target-distro "noble")
           (deb-packaging-config--distro-user-set t))
      (unwind-protect
          (progn
            (deb-packaging-ppa-save "mypkg" "noble" "ppa:me/x")
            (let ((default (deb-packaging-transients--upload-default-value)))
              (should (member "--ppa=ppa:me/x" default))
              (should (member "--dist=noble" default))))
        (delete-directory tmp t)))))

(ert-deftest deb-packaging-test-commands/upload-default-value-no-saved-ppa ()
  "With no saved PPA, the upload default value has no --ppa= arg."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble")
    (let* ((tmp (make-temp-file "deb-ppa-test-" t))
           (process-environment (cons (format "XDG_CACHE_HOME=%s" tmp)
                                      process-environment))
           (deb-packaging-config-target-distro "noble")
           (deb-packaging-config--distro-user-set t))
      (unwind-protect
          (let ((default (deb-packaging-transients--upload-default-value)))
            (should (member "--dist=noble" default))
            (should-not (cl-some (lambda (a) (string-prefix-p "--ppa=" a))
                                 default)))
        (delete-directory tmp t)))))

(ert-deftest deb-packaging-test-commands/test-default-value-seeds-ppa ()
  "The test default value includes the saved PPA."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble")
    (let* ((tmp (make-temp-file "deb-ppa-test-" t))
           (process-environment (cons (format "XDG_CACHE_HOME=%s" tmp)
                                      process-environment))
           (deb-packaging-config-target-distro "noble")
           (deb-packaging-config--distro-user-set t))
      (unwind-protect
          (progn
            (deb-packaging-ppa-save "mypkg" "noble" "ppa:me/x")
            (let ((default (deb-packaging-transients--test-default-value)))
              (should (member "--ppa=ppa:me/x" default))
              (should (member "--runner=lxd" default))
              (should (member "--dist=noble" default))))
        (delete-directory tmp t)))))

;;; dput PPA save + auto-prompt

(ert-deftest deb-packaging-test-commands/dput-upload-saves-ppa ()
  "dput-upload runs dput and saves the PPA per package+distro."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble"
              :artifacts (("mypkg_1.0-1_source.changes" . "")))
    (let (captured-args captured-save)
      (cl-letf (((symbol-function 'deb-packaging-commands--run-command)
                 (lambda (_name args &optional _dir _key)
                   (setq captured-args args)))
                ((symbol-function 'deb-packaging-ppa-save)
                 (lambda (pkg distro ppa)
                   (setq captured-save (list pkg distro ppa)))))
        (deb-packaging-commands-dput-upload '("--ppa=ppa:me/x" "--dist=noble")))
      (should (equal (car captured-args) "dput"))
      (should (equal (cadr captured-args) "ppa:me/x"))
      (should (string-suffix-p "_source.changes" (caddr captured-args)))
      (should (equal captured-save '("mypkg" "noble" "ppa:me/x"))))))

(ert-deftest deb-packaging-test-commands/dput-upload-prompts-when-unset ()
  "dput-upload with no --ppa= prompts and uses the answer."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble"
              :artifacts (("mypkg_1.0-1_source.changes" . "")))
    (let (captured-args captured-save)
      (cl-letf (((symbol-function 'deb-packaging-commands--run-command)
                 (lambda (_name args &optional _dir _key)
                   (setq captured-args args)))
                ((symbol-function 'deb-packaging-infra--list-ppas)
                 (lambda () '("ppa:me/x")))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "ppa:me/y"))
                ((symbol-function 'deb-packaging-ppa-save)
                 (lambda (pkg distro ppa)
                   (setq captured-save (list pkg distro ppa)))))
        (deb-packaging-commands-dput-upload '("--dist=noble")))
      (should (equal (cadr captured-args) "ppa:me/y"))
      (should (equal captured-save '("mypkg" "noble" "ppa:me/y"))))))

(ert-deftest deb-packaging-test-commands/dput-upload-empty-prompt-errors ()
  "An empty answer at the PPA prompt is a user-error."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble"
              :artifacts (("mypkg_1.0-1_source.changes" . "")))
    (cl-letf (((symbol-function 'deb-packaging-infra--list-ppas)
               (lambda () nil))
              ((symbol-function 'completing-read)
               (lambda (&rest _) "")))
      (should-error (deb-packaging-commands-dput-upload '("--dist=noble"))
                    :type 'user-error))))

;;; autopkgtest --ppa= filtering

(ert-deftest deb-packaging-test-commands/autopkgtest-filters-ppa-arg ()
  "autopkgtest never receives the transient's --ppa= arg."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble"
              :artifacts
              (("mypkg_1.0-1_amd64.changes"
                . "Format: 1.8\n\nFiles:\n d41d8cd98f00b204e9800998ecf8427e 1234 admin optional mypkg_1.0-1_amd64.deb\n")
                ("mypkg_1.0-1_amd64.deb" . "")))
    (let (captured-args)
      (cl-letf (((symbol-function 'deb-packaging-commands--test-image-info)
                 (lambda (&optional _runner _distro)
                   (list :runner "lxd"
                         :image "autopkgtest/ubuntu/noble/amd64"
                         :exists t)))
                ((symbol-function 'deb-packaging-commands--run-command)
                 (lambda (_name args &optional _dir _key)
                   (setq captured-args args))))
        (deb-packaging-commands-autopkgtest
         '("--apt-upgrade" "--runner=lxd" "--dist=noble" "--ppa=ppa:me/x")))
      (should-not (cl-some (lambda (a) (string-prefix-p "--ppa=" a))
                           captured-args))
      (should (member "--apt-upgrade" captured-args)))))

;;; git ubuntu export-orig

(ert-deftest deb-packaging-test-commands/export-orig-runs-git-ubuntu ()
  "export-orig runs `git ubuntu export-orig' in the package dir."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble")
    (let (captured)
      (cl-letf (((symbol-function 'executable-find) (lambda (_) "git-ubuntu"))
                ((symbol-function 'deb-packaging-commands--run-command)
                 (lambda (name args &optional dir key)
                   (setq captured (list name args dir key)))))
        (deb-packaging-commands-export-orig))
      (should (equal (nth 1 captured) '("git" "ubuntu" "export-orig")))
      (should (string= (nth 2 captured) pkg-dir))
      (should (eq (nth 3 captured) 'export-orig)))))

(ert-deftest deb-packaging-test-commands/export-orig-missing-git-ubuntu-errors ()
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble")
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
      (should-error (deb-packaging-commands-export-orig) :type 'user-error))))

(provide 'deb-packaging-test-commands)
;;; deb-packaging-test-commands.el ends here
