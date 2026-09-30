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
(require 'deb-packaging-ppa-tests)

;;; Builders

(ert-deftest deb-packaging-test-commands/source-build-dpkg-drops-gbp-flags ()
  (deb-packaging-test--with-package-tree
      '(:name "foo" :version "1.0" :distro "noble")
    (let (args)
      (cl-letf (((symbol-function 'deb-packaging-commands--run-command)
                 (lambda (_name command &rest _) (setq args command) nil)))
        (deb-packaging-commands-source-build
         '("--builder=dpkg-buildpackage" "-d" "--git-ignore-new")))
      (should (equal args '("dpkg-buildpackage" "-S" "-d"))))))

(ert-deftest deb-packaging-test-commands/gbp-source-build-uses-repo-and-configured-export-dir ()
  (deb-packaging-test--with-temp-git-repo
    (deb-packaging-test--build-tree
     repo-dir (file-name-directory (directory-file-name repo-dir))
     '(:name "foo" :version "1.0-1"))
    (deb-packaging-test--git repo-dir "add" "debian")
    (deb-packaging-test--git repo-dir "commit" "-q" "-m" "packaging")
    (should (plist-get (deb-packaging-commands--package-context repo-dir)
                       :git-p))
    (let ((real-process-file (symbol-function 'process-file))
          captured args artifact-dir)
      (cl-letf (((symbol-function 'executable-find) (lambda (_) t))
                ((symbol-function 'process-file)
                 (lambda (program &rest args)
                   (if (equal program "gbp")
                       (progn
                         (when (eq (nth 1 args) t)
                           (insert "buildpackage.export-dir=build-area\n"))
                         0)
                     (apply real-process-file program args))))
                ((symbol-function 'deb-packaging-commands--run-command)
                 (lambda (_name command &optional dir key buffer-dir)
                   (setq captured (list dir key buffer-dir)
                         args command)
                   "*gbp-test*"))
                ((symbol-function 'deb-packaging-commands--set-run-artifact-dir)
                 (lambda (_key _buffer path) (setq artifact-dir path))))
        (deb-packaging-commands-source-build
         '("--builder=gbp" "-d" "--git-ignore-new")))
      (should (equal args '("gbp" "buildpackage" "--git-ignore-new" "-S" "-d")))
      (should (equal (car captured) repo-dir))
      (should (eq (cadr captured) 'source-build))
      (should (equal (caddr captured) repo-dir))
      (should (equal artifact-dir (expand-file-name "build-area" repo-dir))))))

(ert-deftest deb-packaging-test-commands/package-context-uses-gbp-output-dir ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.0-1")
    (let ((export-dir (make-temp-file "gbp-export-" t)))
      (unwind-protect
          (progn
            (deb-packaging-test--write-file
             (expand-file-name "foo_1.0-1.dsc" export-dir) "")
            (cl-letf (((symbol-function 'deb-packaging-commands-run-record)
                       (lambda (&rest _) (list :artifact-dir export-dir)))
                      ((symbol-function 'deb-packaging-config--effective-architecture)
                       (lambda (&optional _) "amd64")))
              (let ((ctx (deb-packaging-commands--package-context pkg-dir)))
                (should (equal (plist-get ctx :artifact-dir) export-dir))
                 (should (equal (alist-get 'dsc (plist-get ctx :artifacts))
                                (expand-file-name "foo_1.0-1.dsc" export-dir))))))
        (delete-directory export-dir t)))))

(ert-deftest deb-packaging-test-commands/gbp-orig-uses-gbp-builder ()
  (deb-packaging-test--with-temp-git-repo
    (deb-packaging-test--build-tree
     repo-dir (file-name-directory (directory-file-name repo-dir))
     '(:name "foo" :version "1.0-1"))
    (deb-packaging-test--git repo-dir "add" "debian")
    (deb-packaging-test--git repo-dir "commit" "-q" "-m" "packaging")
    (let ((real-process-file (symbol-function 'process-file))
          args)
      (cl-letf (((symbol-function 'executable-find) (lambda (_) t))
                ((symbol-function 'process-file)
                 (lambda (program &rest call-args)
                   (if (equal program "gbp")
                       2
                     (apply real-process-file program call-args))))
                ((symbol-function 'deb-packaging-commands--run-command)
                 (lambda (_name command &rest _) (setq args command) "*gbp*"))
                ((symbol-function 'deb-packaging-commands--set-run-artifact-dir)
                 #'ignore))
        (deb-packaging-commands-gbp-export-orig))
      (should (equal args '("gbp" "buildpackage"
                            "--git-builder=/bin/true" "--git-no-hooks"))))))

(ert-deftest deb-packaging-test-commands/filter-keeps-exact-bare-flag ()
  (should (equal (deb-packaging-commands--filter-args
                  '("-i" "-I" "--foo")
                  deb-packaging-commands--lintian-arg-prefixes)
                 '("-i" "-I"))))

(ert-deftest deb-packaging-test-commands/filter-keeps-prefix-flag-with-value ()
  (should (equal (deb-packaging-commands--filter-args
                  '("--tag-display-limit=5" "--foo")
                  deb-packaging-commands--lintian-arg-prefixes)
                 '("--tag-display-limit=5"))))

(ert-deftest deb-packaging-test-commands/filter-drops-non-matching ()
  (should (null (deb-packaging-commands--filter-args
                 '("--verbose" "--json" "--foo")
                 deb-packaging-commands--lintian-arg-prefixes))))

(ert-deftest deb-packaging-test-commands/filter-empty-args ()
  (should (null (deb-packaging-commands--filter-args nil deb-packaging-commands--lintian-arg-prefixes)))
  (should (null (deb-packaging-commands--filter-args '() deb-packaging-commands--ubuntu-lint-arg-prefixes))))

(ert-deftest deb-packaging-test-commands/filter-separates-lintian-and-ubuntu-prefixes ()
  (let ((args '("-i" "--pedantic" "--verbose" "--context=ctx" "--all=yes")))
    (should (equal (deb-packaging-commands--filter-args args deb-packaging-commands--lintian-arg-prefixes)
                   '("-i" "--pedantic")))
    (should (equal (deb-packaging-commands--filter-args args deb-packaging-commands--ubuntu-lint-arg-prefixes)
                   '("--verbose" "--all=yes")))))

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

;;; deb-packaging-commands-binary-build multi-value

(ert-deftest deb-packaging-test-commands/sbuild-target-architecture ()
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble"
              :artifacts (("mypkg_1.0-1.dsc" . "")))
    (let (captured-args captured-save)
      (cl-letf (((symbol-function 'deb-packaging-commands--run-command)
                 (lambda (_name args &optional _dir _key _buffer-dir)
                   (setq captured-args args)))
                ((symbol-function 'deb-packaging-repos-save) #'ignore)
                ((symbol-function 'deb-packaging-config-save-architecture)
                 (lambda (package distro arch)
                   (setq captured-save (list package distro arch)))))
        (deb-packaging-commands-binary-build '("--arch=arm64")))
      (should (member "--arch=arm64" captured-args))
      (should (equal captured-save '("mypkg" "noble" "arm64"))))))

(ert-deftest deb-packaging-test-commands/sbuild-multiple-extra-repos ()
  "sbuild receives one expanded --extra-repository= flag per entry."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble"
              :artifacts (("mypkg_1.0-1.dsc" . "")))
    (let (captured-args captured-save)
      (cl-letf (((symbol-function 'deb-packaging-commands--run-command)
                 (lambda (_name args &optional _dir _key _buffer-dir)
                   (setq captured-args args)))
                ((symbol-function 'deb-packaging-repos-save)
                 (lambda (pkg distro entries)
                   (setq captured-save (list pkg distro entries)))))
        (deb-packaging-test--with-mocked-process
            '(("dpkg" . "amd64") ("curl" . "200"))
          (deb-packaging-commands-binary-build
           (deb-packaging-test-commands--sbuild-args
            '("ppa:me/x" "proposed"
              "deb http://example.com/ubuntu noble main")))))
      (should (member "--dist=noble" captured-args))
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
                 (lambda (_name _args &optional _dir _key _buffer-dir)))
                 ((symbol-function 'deb-packaging-repos-save)
                  (lambda (pkg distro entries)
                    (setq captured-save (list pkg distro entries)))))
        (deb-packaging-commands-binary-build nil))
      (should (equal captured-save '("mypkg" "noble" nil))))))

(ert-deftest deb-packaging-test-commands/sbuild-purge-flags-pass-through ()
  "sbuild receives --purge-session= and --purge-build= verbatim."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble"
              :artifacts (("mypkg_1.0-1.dsc" . "")))
    (let (captured-args)
      (cl-letf (((symbol-function 'deb-packaging-commands--run-command)
                 (lambda (_name args &optional _dir _key _buffer-dir)
                   (setq captured-args args)))
                ((symbol-function 'deb-packaging-repos-save) #'ignore))
        (deb-packaging-commands-binary-build
         '("--purge-session=always" "--purge-build=never")))
      (should (member "--dist=noble" captured-args))
      (should (member "--purge-session=always" captured-args))
      (should (member "--purge-build=never" captured-args)))))

(ert-deftest deb-packaging-test-commands/build-binary-uses-working-tree-without-dsc ()
  (deb-packaging-test--with-package-tree
      '(:name "foo" :version "1.2-3" :distro "noble")
    (let (args dir)
      (cl-letf (((symbol-function 'deb-packaging-commands--run-command)
                 (lambda (_name command command-dir &rest _)
                   (setq args command dir command-dir))))
        (deb-packaging-commands-binary-build '("--builder=dpkg-buildpackage"))
        (should (equal args '("dpkg-buildpackage" "-b")))
        (should (equal dir pkg-dir))))))

(ert-deftest deb-packaging-test-commands/build-binary-rejects-cross-architecture ()
  (cl-letf (((symbol-function 'deb-packaging-detect--find-package-dir)
             (lambda (&rest _) "/tmp/package"))
            ((symbol-function 'deb-packaging-commands--package-context)
             (lambda (&rest _) '(:target-arch "arm64" :host-arch "amd64")))
            ((symbol-function 'deb-packaging-commands--run-command)
             (lambda (&rest _) (error "must not run"))))
    (should-error (deb-packaging-commands-binary-build '("--builder=dpkg-buildpackage"))
                  :type 'user-error)
    (should-error (deb-packaging-commands-binary-build '("--builder=gbp"))
                  :type 'user-error)))

(ert-deftest deb-packaging-test-commands/sbuild-buffer-dir-is-pkg-dir ()
  "sbuild runs in the parent dir but its log buffer keeps the package dir."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble"
              :artifacts (("mypkg_1.0-1.dsc" . "")))
    (let (captured-dir captured-buffer-dir)
      (cl-letf (((symbol-function 'deb-packaging-commands--run-command)
                 (lambda (_name _args &optional dir _key buffer-dir)
                   (setq captured-dir dir
                         captured-buffer-dir buffer-dir)))
                 ((symbol-function 'deb-packaging-repos-save) #'ignore))
        (deb-packaging-commands-binary-build nil))
      (should (equal captured-dir pkg-parent-dir))
      (should (equal captured-buffer-dir pkg-dir)))))

;;; deb-packaging-transients--binary-default-value restore

(ert-deftest deb-packaging-test-commands/binary-default-value-seeds-repos ()
  "The binary-build default value includes saved extra-repo entries."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble")
    (let* ((tmp (make-temp-file "deb-repos-test-" t))
           (process-environment (cons (format "XDG_CACHE_HOME=%s" tmp)
                                      process-environment)))
      (unwind-protect
          (progn
            (deb-packaging-repos-save "mypkg" "noble"
                                      '("ppa:me/x" "proposed"))
            (let ((default (deb-packaging-transients--binary-default-value)))
              (should (cl-some (lambda (arg)
                                 (string-prefix-p "--arch=" arg))
                               default))
              (should (member "--extra-repository=ppa:me/x" default))
              (should (member "--extra-repository=proposed" default))))
        (delete-directory tmp t)))))

(ert-deftest deb-packaging-test-commands/binary-default-value-no-saved-repos ()
  "With no saved repos, the default value adds no --extra-repository
args: the chroot's own sources.list provides the distro's pockets."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble")
    (let* ((tmp (make-temp-file "deb-repos-test-" t))
           (process-environment (cons (format "XDG_CACHE_HOME=%s" tmp)
                                      process-environment)))
      (unwind-protect
          (let ((default (deb-packaging-transients--binary-default-value)))
            (should (equal (seq-take default 2) '("--builder=sbuild" "-A")))
            (should (string-prefix-p "--arch=" (nth 2 default)))
            (should-not (cl-some (lambda (a)
                                   (string-prefix-p "--extra-repository=" a))
                                 default)))
        (delete-directory tmp t)))))

(ert-deftest deb-packaging-test-commands/binary-default-value-cleared-repos-stick ()
  "A saved empty set stays empty: opting out of -proposed persists."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble")
    (let* ((tmp (make-temp-file "deb-repos-test-" t))
           (process-environment (cons (format "XDG_CACHE_HOME=%s" tmp)
                                      process-environment)))
      (unwind-protect
          (progn
            (deb-packaging-repos-save "mypkg" "noble" nil)
            (let ((default (deb-packaging-transients--binary-default-value)))
              (should-not (cl-some (lambda (a)
                                     (string-prefix-p "--extra-repository=" a))
                                   default))))
        (delete-directory tmp t)))))

;;; deb-packaging-transients--effective-repos

(ert-deftest deb-packaging-test-commands/effective-repos-no-default ()
  "No saved set means no extra repos: the build chroot's own
sources.list already provides the distro's pockets, and a `proposed'
default would duplicate them (or break Debian builds)."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble")
    (let* ((tmp (make-temp-file "deb-repos-test-" t))
           (process-environment (cons (format "XDG_CACHE_HOME=%s" tmp)
                                      process-environment)))
      (unwind-protect
          (should (null (deb-packaging-transients--effective-repos)))
        (delete-directory tmp t)))))

(ert-deftest deb-packaging-test-commands/effective-repos-returns-saved ()
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble")
    (let* ((tmp (make-temp-file "deb-repos-test-" t))
           (process-environment (cons (format "XDG_CACHE_HOME=%s" tmp)
                                      process-environment)))
      (unwind-protect
          (progn
            (deb-packaging-repos-save "mypkg" "noble" '("ppa:me/x"))
            (should (equal (deb-packaging-transients--effective-repos)
                           '("ppa:me/x"))))
        (delete-directory tmp t)))))

(ert-deftest deb-packaging-test-commands/effective-repos-cleared-stays-empty ()
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble")
    (let* ((tmp (make-temp-file "deb-repos-test-" t))
           (process-environment (cons (format "XDG_CACHE_HOME=%s" tmp)
                                      process-environment)))
      (unwind-protect
          (progn
            (deb-packaging-repos-save "mypkg" "noble" nil)
            (should (null (deb-packaging-transients--effective-repos))))
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
    (let ((info (deb-packaging-commands--test-image-info "lxd" "noble" "amd64")))
      (should (equal (plist-get info :runner) "lxd"))
      (should (string= (plist-get info :image)
                       "autopkgtest/ubuntu/noble/amd64"))
      (should (plist-get info :exists)))))

(ert-deftest deb-packaging-test-commands/test-image-info-lxd-missing ()
  (deb-packaging-test--with-mocked-process '(("lxc" . 1))
    (let ((info (deb-packaging-commands--test-image-info "lxd" "noble" "amd64")))
      (should (equal (plist-get info :runner) "lxd"))
      (should (string= (plist-get info :image)
                       "autopkgtest/ubuntu/noble/amd64"))
      (should (null (plist-get info :exists))))))

(ert-deftest deb-packaging-test-commands/test-image-info-qemu ()
  (let ((info (deb-packaging-commands--test-image-info "qemu" "noble" "amd64")))
    (should (equal (plist-get info :runner) "qemu"))
    (should (string= (plist-get info :image)
                     "/var/lib/adt-images/autopkgtest-noble-amd64.img"))
    (should (null (plist-get info :exists)))))

;;; deb-packaging-commands--test-image-build-hint

(ert-deftest deb-packaging-test-commands/test-image-build-hint-lxd ()
  (should (string= (deb-packaging-commands--test-image-build-hint
                    "lxd" "noble" "amd64")
                   "autopkgtest-build-lxd ubuntu-daily:noble/amd64")))

(ert-deftest deb-packaging-test-commands/test-image-build-hint-qemu ()
  (should (string= (deb-packaging-commands--test-image-build-hint
                    "qemu" "noble" "amd64")
                   "autopkgtest-buildvm-ubuntu-cloud -r noble -a amd64")))

(ert-deftest deb-packaging-test-commands/test-image-build-hint-unknown ()
  (should (null (deb-packaging-commands--test-image-build-hint "docker" "noble"))))

(ert-deftest deb-packaging-test-commands/test-image-paths-use-target-architecture ()
  (should (equal (plist-get
                  (deb-packaging-commands--test-image-info
                   "lxd" "noble" "arm64")
                  :image)
                 "autopkgtest/ubuntu/noble/arm64"))
  (should (equal (plist-get
                  (deb-packaging-commands--test-image-info
                   "qemu" "noble" "arm64")
                  :image)
                 "/var/lib/adt-images/autopkgtest-noble-arm64.img")))

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
                                      process-environment)))
      (unwind-protect
          (progn
            (deb-packaging-ppa-save "mypkg" "noble" "ppa:me/x")
            (let ((default (deb-packaging-transients--upload-default-value)))
              (should (member "--ppa=ppa:me/x" default))))
        (delete-directory tmp t)))))

(ert-deftest deb-packaging-test-commands/upload-default-value-no-saved-ppa ()
  "With no saved PPA, the upload default value has no --ppa= arg."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble")
    (let* ((tmp (make-temp-file "deb-ppa-test-" t))
           (process-environment (cons (format "XDG_CACHE_HOME=%s" tmp)
                                      process-environment)))
      (unwind-protect
          (let ((default (deb-packaging-transients--upload-default-value)))
            (should-not (cl-some (lambda (a) (string-prefix-p "--ppa=" a))
                                 default)))
        (delete-directory tmp t)))))

(ert-deftest deb-packaging-test-commands/ppa-tests-show-uses-saved-ppa ()
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble")
    (let* ((tmp (make-temp-file "deb-ppa-test-" t))
           (process-environment (cons (format "XDG_CACHE_HOME=%s" tmp)
                                      process-environment))
           fetched)
      (unwind-protect
          (progn
            (deb-packaging-ppa-save "mypkg" "noble" "ppa:me/x")
            (cl-letf (((symbol-function 'deb-packaging-ppa-tests--fetch)
                       (lambda (ppa &rest _) (setq fetched ppa)))
                      ((symbol-function 'deb-packaging-display-buffer) #'ignore)
                      ((symbol-function 'completing-read)
                       (lambda (&rest _) (error "must not prompt"))))
              (deb-packaging-ppa-tests-show))
            (should (equal fetched "ppa:me/x")))
        (delete-directory tmp t)))))

;;; dput PPA save + auto-prompt

(ert-deftest deb-packaging-test-commands/resolve-ppa-rejects-malformed-address ()
  (should-error (deb-packaging-commands--resolve-ppa '("--ppa=not-a-ppa"))
                :type 'user-error))

(ert-deftest deb-packaging-test-commands/dput-upload-saves-ppa ()
  "dput-upload runs dput and saves the PPA per package+distro."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble"
              :artifacts (("mypkg_1.0-1_source.changes" . "")))
    (let (captured-args captured-save)
      (cl-letf (((symbol-function 'deb-packaging-commands--run-command)
                 (lambda (_name args &optional _dir _key _buffer-dir)
                   (setq captured-args args)))
                ((symbol-function 'deb-packaging-ppa-save)
                 (lambda (pkg distro ppa)
                   (setq captured-save (list pkg distro ppa)))))
        (deb-packaging-commands-dput-upload '("--ppa=ppa:me/x")))
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
                 (lambda (_name args &optional _dir _key _buffer-dir)
                   (setq captured-args args)))
                ((symbol-function 'deb-packaging-infra--list-ppas)
                 (lambda () '("ppa:me/x")))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "ppa:me/y"))
                 ((symbol-function 'deb-packaging-ppa-save)
                  (lambda (pkg distro ppa)
                    (setq captured-save (list pkg distro ppa)))))
        (deb-packaging-commands-dput-upload nil))
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
      (should-error (deb-packaging-commands-dput-upload nil)
                    :type 'user-error))))

;;; autopkgtest --ppa= filtering

(ert-deftest deb-packaging-test-commands/autopkgtest-filters-control-args ()
  "autopkgtest receives local options but not transient control arguments."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble"
              :artifacts
              (("mypkg_1.0-1_amd64.changes"
                . "Format: 1.8\n\nFiles:\n d41d8cd98f00b204e9800998ecf8427e 1234 admin optional mypkg_1.0-1_amd64.deb\n")
                ("mypkg_1.0-1_amd64.deb" . "")))
    (let (captured-args)
      (cl-letf (((symbol-function 'deb-packaging-commands--test-image-info)
                  (lambda (&optional _runner _distro _architecture)
                   (list :runner "lxd"
                         :image "autopkgtest/ubuntu/noble/amd64"
                         :exists t)))
                ((symbol-function 'deb-packaging-commands--run-command)
                 (lambda (_name args &optional _dir _key)
                   (setq captured-args args))))
        (deb-packaging-commands-autopkgtest
         '("--apt-upgrade" "--apt-pocket=proposed"
           "--runner=lxd" "--ppa=ppa:me/x")))
      (should-not (cl-some (lambda (a) (string-prefix-p "--ppa=" a))
                           captured-args))
      (should-not (cl-some (lambda (a) (string-prefix-p "--runner=" a))
                           captured-args))
      (should (equal (cl-subseq captured-args 0 3)
                     '("autopkgtest" "--apt-upgrade"
                       "--apt-pocket=proposed"))))))

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

;;; Compilation wrapper

(ert-deftest deb-packaging-test-commands/compile-wrapper-sets-conventions ()
  (let (captured)
    (cl-letf (((symbol-function 'compile)
               (lambda (cmd &rest _)
                 (setq captured
                       (list cmd
                             compilation-ask-about-save
                             compilation-always-kill
                             display-buffer-overriding-action))
                 (get-buffer-create "*deb-test-compile*"))))
      (let ((buf (deb-packaging-commands--compile "make foo")))
        (should (equal captured
                        (list "make foo" nil nil
                              (deb-packaging-display--action 'output))))
        (should (eq (buffer-local-value 'deb-packaging-display-category buf)
                    'output))
        (kill-buffer buf)))))

(ert-deftest deb-packaging-test-commands/compile-wrapper-tolerates-nil-buffer ()
  (cl-letf (((symbol-function 'compile) (lambda (&rest _) nil)))
    (should (null (deb-packaging-commands--compile "make foo")))))

(ert-deftest deb-packaging-test-commands/compile-wrapper-captures-context ()
  (let ((context '(:name "foo" :distro "noble")))
    (cl-letf (((symbol-function 'deb-packaging-detect--scan-context)
               (lambda (&rest _) context))
              ((symbol-function 'compile)
               (lambda (&rest _) (get-buffer-create "*deb-context-compile*"))))
      (let ((buf (deb-packaging-commands--compile "make foo")))
        (unwind-protect
            (should (eq
                     (buffer-local-value 'deb-packaging-commands--context buf)
                     context))
          (kill-buffer buf))))))

(ert-deftest deb-packaging-test-commands/compile-wrapper-uses-unique-buffers ()
  (let (buffers)
    (unwind-protect
        (cl-letf (((symbol-function 'compile)
                   (lambda (&rest _)
                     (let ((buf (get-buffer-create
                                 (funcall compilation-buffer-name-function
                                          "compilation"))))
                       (push buf buffers)
                       buf))))
          (deb-packaging-commands--compile "make one")
          (deb-packaging-commands--compile "make two")
          (should (= (length (delete-dups (mapcar #'buffer-name buffers))) 2)))
      (mapc #'kill-buffer buffers))))

;;; sbuild shell-on-failure flag: single source of truth

(ert-deftest deb-packaging-test-commands/sbuild-shell-flag-matches-suffix ()
  "The defconst the status buffer matches against must equal the
argument of the transient's -F suffix.  They were once two literals
that could drift apart silently."
  (let ((proto (get 'deb-packaging-transients--sbuild-shell
                    'transient--suffix)))
    (should proto)
    (should (equal (slot-value proto 'argument)
                   deb-packaging-transients-sbuild-shell-flag))))

(ert-deftest deb-packaging-test-commands/binary-transient-contains-shell-suffix ()
  "The binary-build transient layout must reference the -F suffix
command; otherwise the flag exists but is unreachable.
Matches on the printed form: the layout mixes lists and vectors, which
`flatten-tree' will not descend into."
  (let ((layout (get 'deb-packaging-binary-build-transient 'transient--layout)))
    (should layout)
    (should (string-match-p
             "\\<deb-packaging-transients--sbuild-shell\\>"
             (prin1-to-string layout)))))

;;; PPA extra-repo pre-flight

(defun deb-packaging-test-commands--sbuild-args (repos)
  "Build a transient ARGS list with the given extra REPOS entries."
  (mapcar (lambda (r) (concat "--extra-repository=" r)) repos))

(defmacro deb-packaging-test-commands--with-sbuild-tree (&rest body)
  "Run BODY inside a package tree with a .dsc, run-command mocked.
Binds `captured-args' to whatever sbuild would run."
  (declare (indent 0) (debug (body)))
  `(deb-packaging-test--with-package-tree
       '(:name "mypkg" :version "1.0-1" :distro "noble"
               :artifacts (("mypkg_1.0-1.dsc" . "")))
     (let ((captured-args nil))
       (cl-letf (((symbol-function 'deb-packaging-commands--run-command)
                  (lambda (_name args &optional _dir _key _buffer-dir)
                    (setq captured-args args)))
                 ((symbol-function 'deb-packaging-repos-save) #'ignore))
         ,@body))))

(ert-deftest deb-packaging-test-commands/sbuild-errors-on-unpublished-ppa ()
  "A ppa: entry with no series for the distro errors at dispatch,
before sbuild runs."
  (deb-packaging-test-commands--with-sbuild-tree
    (deb-packaging-test--with-mocked-process
        '(("dpkg" . "amd64") ("curl" . "403"))
      (should-error (deb-packaging-commands-binary-build
                     (deb-packaging-test-commands--sbuild-args
                      '("ppa:karljs/empty")))
                    :type 'user-error)
      (should (null captured-args)))))

(ert-deftest deb-packaging-test-commands/sbuild-errors-on-missing-series ()
  (deb-packaging-test-commands--with-sbuild-tree
    (deb-packaging-test--with-mocked-process
        '(("dpkg" . "amd64") ("curl" . "404"))
      (should-error (deb-packaging-commands-binary-build
                     (deb-packaging-test-commands--sbuild-args
                      '("ppa:karljs/only-noble")))
                    :type 'user-error)
      (should (null captured-args)))))

(ert-deftest deb-packaging-test-commands/sbuild-proceeds-when-published ()
  (deb-packaging-test-commands--with-sbuild-tree
    (deb-packaging-test--with-mocked-process
        '(("dpkg" . "amd64") ("curl" . "200"))
      (deb-packaging-commands-binary-build
       (deb-packaging-test-commands--sbuild-args '("ppa:karljs/good")))
      (should (cl-some (lambda (a) (string-prefix-p
                                    "--extra-repository=deb [trusted=yes]"
                                    a))
                       captured-args)))))

(ert-deftest deb-packaging-test-commands/sbuild-probe-fails-open ()
  "An unanswerable probe (timeout, missing curl, 5xx) must not block
the build."
  (deb-packaging-test-commands--with-sbuild-tree
    (deb-packaging-test--with-mocked-process
        '(("dpkg" . "amd64") ("curl" . "000"))
      (deb-packaging-commands-binary-build
       (deb-packaging-test-commands--sbuild-args '("ppa:karljs/flaky")))
      (should captured-args))))

(ert-deftest deb-packaging-test-commands/sbuild-non-ppa-entries-unprobed ()
  "Variant names and raw deb lines skip the probe entirely."
  (deb-packaging-test-commands--with-sbuild-tree
    (deb-packaging-test--with-mocked-process
        '(("dpkg" . "amd64") ("curl" . (error . "must not probe")))
      (deb-packaging-commands-binary-build
       (deb-packaging-test-commands--sbuild-args
        '("proposed" "deb http://example.com/ubuntu noble main")))
      (should (cl-some (lambda (a) (string-prefix-p
                                    "--extra-repository=deb http://example.com"
                                    a))
                       captured-args)))))

(ert-deftest deb-packaging-test-commands/ppa-series-published-p-mapping ()
  "200 -> t, 403/404 -> nil, anything else -> unknown."
  (dolist (cell '(("200" . t) ("403" . nil) ("404" . nil)
                  ("500" . unknown) ("000" . unknown)))
    (cl-letf (((symbol-function 'deb-packaging-commands--probe-http-code)
               (lambda (_url) (car cell))))
      (should (eq (deb-packaging-commands--ppa-series-published-p
                   "ppa:owner/name" "noble")
                  (cdr cell)))))
  ;; Not a ppa: address: unanswerable, fail open.
  (should (eq (deb-packaging-commands--ppa-series-published-p
               "not-a-ppa" "noble")
              'unknown)))

(ert-deftest deb-packaging-test-commands/probe-http-code-parses-curl ()
  (deb-packaging-test--with-mocked-process
      '(("curl" . "200"))
    (should (string= (deb-packaging-commands--probe-http-code
                      "http://example.com/Release")
                     "200"))))

(ert-deftest deb-packaging-test-commands/probe-http-code-nil-on-garbage ()
  (deb-packaging-test--with-mocked-process
      '(("curl" . "curl: (7) couldn't connect"))
    (should (null (deb-packaging-commands--probe-http-code
                   "http://example.com/Release")))))

(provide 'deb-packaging-test-commands)
;;; deb-packaging-test-commands.el ends here
