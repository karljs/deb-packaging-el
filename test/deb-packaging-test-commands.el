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

(provide 'deb-packaging-test-commands)
;;; deb-packaging-test-commands.el ends here
