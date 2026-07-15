;;; deb-packaging-test-detect.el --- Detection & parsing tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for changelog/control/patch/artifact parsing in
;; deb-packaging-detect.el.

;;; Code:

(require 'ert)
(require 'deb-packaging-test)
(require 'deb-packaging-detect)

;;; Changelog parsing

(ert-deftest deb-packaging-test-detect/parse-changelog-standard ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3" :distro "noble")
    (should (equal (deb-packaging-detect--parse-changelog pkg-dir)
                   (list "foo" "1.2-3" "noble")))))

(ert-deftest deb-packaging-test-detect/parse-changelog-native ()
  (deb-packaging-test--with-package-tree
      (list :name "bar" :version "2.0" :distro "unstable")
    (should (equal (deb-packaging-detect--parse-changelog pkg-dir)
                   (list "bar" "2.0" "unstable")))))

(ert-deftest deb-packaging-test-detect/parse-changelog-with-epoch ()
  (deb-packaging-test--with-package-tree
      (list :name "baz" :version "1:3.4-5" :distro "jammy")
    (should (equal (deb-packaging-detect--parse-changelog pkg-dir)
                   (list "baz" "1:3.4-5" "jammy")))))

(ert-deftest deb-packaging-test-detect/parse-changelog-missing-file ()
  (let ((tmp (make-temp-file "deb-pkg-test-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "debian" tmp) t)
          (should (null (deb-packaging-detect--parse-changelog tmp))))
      (delete-directory tmp t))))

;;; Package directory detection

(ert-deftest deb-packaging-test-detect/find-package-dir-from-root ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (should (file-equal-p (deb-packaging-detect--find-package-dir pkg-dir)
                          pkg-dir))))

(ert-deftest deb-packaging-test-detect/find-package-dir-from-subdir ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (let ((subdir (expand-file-name "debian/foo" pkg-dir)))
      (make-directory subdir t)
      (should (file-equal-p (deb-packaging-detect--find-package-dir subdir)
                            pkg-dir)))))

(ert-deftest deb-packaging-test-detect/find-package-dir-outside-package ()
  (let ((tmp (make-temp-file "deb-pkg-test-" t)))
    (unwind-protect
        (should (null (deb-packaging-detect--find-package-dir tmp)))
      (delete-directory tmp t))))

(ert-deftest deb-packaging-test-detect/find-package-dir-host-only-tramp-errors ()
  (cl-letf (((symbol-function 'locate-dominating-file)
             (lambda (_dir _name) "/ssh:host:/tmp/pkg/")))
    (should-error (deb-packaging-detect--find-package-dir "/ssh:host:/tmp/foo" t)
                  :type 'user-error)))

;;; Control field extraction

(ert-deftest deb-packaging-test-detect/control-field-existing ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :maintainer "Karl Smeltzer <karl@example.com>")
    (should (string= (deb-packaging-detect--control-field "Maintainer" pkg-dir)
                     "Karl Smeltzer <karl@example.com>"))))

(ert-deftest deb-packaging-test-detect/control-field-missing ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (should (null (deb-packaging-detect--control-field "Uploaders" pkg-dir)))))

(ert-deftest deb-packaging-test-detect/control-field-homepage ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :homepage "https://github.com/example/foo")
    (should (string= (deb-packaging-detect--control-field "Homepage" pkg-dir)
                     "https://github.com/example/foo"))))

(ert-deftest deb-packaging-test-detect/control-field-vcs-git ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :vcs-git "https://github.com/example/foo.git")
    (should (string= (deb-packaging-detect--control-field "Vcs-Git" pkg-dir)
                     "https://github.com/example/foo.git"))))

;;; Binary package names

(ert-deftest deb-packaging-test-detect/binary-package-names-single ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3" :bin-names '("foo"))
    (should (equal (deb-packaging-detect--binary-package-names pkg-dir)
                   '("foo")))))

(ert-deftest deb-packaging-test-detect/binary-package-names-multiple ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :bin-names '("foo" "foo-utils" "foo-dev"))
    (should (equal (deb-packaging-detect--binary-package-names pkg-dir)
                   '("foo" "foo-utils" "foo-dev")))))

;;; Source format

(ert-deftest deb-packaging-test-detect/source-format-quilt ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :source-format "3.0 (quilt)")
    (should (string= (deb-packaging-detect--source-format pkg-dir)
                     "3.0 (quilt)"))))

(ert-deftest deb-packaging-test-detect/source-format-native ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :source-format "3.0 (native)")
    (should (string= (deb-packaging-detect--source-format pkg-dir)
                     "3.0 (native)"))))

(ert-deftest deb-packaging-test-detect/source-format-missing ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (should (null (deb-packaging-detect--source-format pkg-dir)))))

;;; Patches

(ert-deftest deb-packaging-test-detect/list-patches-normal ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :patches '(("01-fix-foo.patch" . "fix")
                       ("02-add-bar.patch" . "bar")))
    (let ((patches (deb-packaging-detect--list-patches)))
      (should (= (length patches) 2))
      (should (equal (mapcar #'car patches)
                     '("01-fix-foo.patch" "02-add-bar.patch")))
      (should (cl-every (lambda (p)
                          (file-readable-p (cdr p)))
                        patches)))))

(ert-deftest deb-packaging-test-detect/list-patches-skips-comments-and-blanks ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :patches '(("real.patch" . "real"))
            :series '("# comment" "" "   " "real.patch" "# another"))
    (let ((patches (deb-packaging-detect--list-patches)))
      (should (= (length patches) 1))
      (should (string= (caar patches) "real.patch")))))

(ert-deftest deb-packaging-test-detect/list-patches-strips-quilt-options ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :patches '(("fix.patch" . "fix"))
            :series '("fix.patch -p1"))
    (let ((patches (deb-packaging-detect--list-patches)))
      (should (= (length patches) 1))
      (should (string= (caar patches) "fix.patch")))))

(ert-deftest deb-packaging-test-detect/list-patches-missing-series ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (should (null (deb-packaging-detect--list-patches)))))

(ert-deftest deb-packaging-test-detect/list-patches-unreadable-ignored ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :patches '(("present.patch" . "present"))
            :series '("present.patch" "missing.patch"))
    (let ((patches (deb-packaging-detect--list-patches)))
      (should (= (length patches) 1))
      (should (string= (caar patches) "present.patch")))))

;;; VCS Git

(ert-deftest deb-packaging-test-detect/vcs-git-plain-url ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :vcs-git "https://github.com/example/foo.git")
    (should (string= (deb-packaging-detect--vcs-git pkg-dir)
                     "https://github.com/example/foo.git"))))

(ert-deftest deb-packaging-test-detect/vcs-git-strips-branch ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :vcs-git "https://github.com/example/foo.git -b nightly")
    (should (string= (deb-packaging-detect--vcs-git pkg-dir)
                     "https://github.com/example/foo.git"))))

(ert-deftest deb-packaging-test-detect/vcs-git-missing ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (should (null (deb-packaging-detect--vcs-git pkg-dir)))))

;;; Upstream URL

(ert-deftest deb-packaging-test-detect/upstream-url-github-homepage ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :homepage "https://github.com/example/foo")
    (should (string= (deb-packaging-detect--upstream-url pkg-dir)
                     "https://github.com/example/foo"))))

(ert-deftest deb-packaging-test-detect/upstream-url-gitlab-homepage ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :homepage "https://gitlab.com/example/foo")
    (should (string= (deb-packaging-detect--upstream-url pkg-dir)
                     "https://gitlab.com/example/foo"))))

(ert-deftest deb-packaging-test-detect/upstream-url-non-git-homepage ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :homepage "https://foo.example.org")
    (should (string= (deb-packaging-detect--upstream-url pkg-dir)
                     "https://foo.example.org"))))

(ert-deftest deb-packaging-test-detect/upstream-url-from-watch ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :watch "version=4\nopts=... https://github.com/example/foo/tags .../archive/@ANY_VERSION@.tar.gz\n")
    (should (string= (deb-packaging-detect--upstream-url pkg-dir)
                     "https://github.com/example/foo"))))

(ert-deftest deb-packaging-test-detect/upstream-url-from-watch-gitlab ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :watch "version=4\nhttps://gitlab.com/example/foo/-/archive/v@ANY_VERSION@/foo-@ANY_VERSION@.tar.gz\n")
    (should (string= (deb-packaging-detect--upstream-url pkg-dir)
                     "https://gitlab.com/example/foo"))))

(ert-deftest deb-packaging-test-detect/upstream-url-nothing ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (should (null (deb-packaging-detect--upstream-url pkg-dir)))))

;;; Changes file parsing

(ert-deftest deb-packaging-test-detect/parse-changes-file-single-file ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (let ((changes (expand-file-name "foo_1.2-3_amd64.changes" pkg-parent-dir)))
      (write-region "Files:\n abc123 100 devel optional foo_1.2-3_amd64.deb\n" nil changes)
      (should (equal (deb-packaging-detect--parse-changes-file changes)
                     '("foo_1.2-3_amd64.deb"))))))

(ert-deftest deb-packaging-test-detect/parse-changes-file-multiple-files ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (let ((changes (expand-file-name "foo_1.2-3_amd64.changes" pkg-parent-dir)))
      (write-region
       "Format: 1.8\nFiles:\n abc1 1 devel optional foo_1.2-3_amd64.deb\n def2 2 doc optional foo-doc_1.2-3_all.deb\nChecksums-Sha256:\n 000\n" nil changes)
      (should (equal (deb-packaging-detect--parse-changes-file changes)
                     '("foo_1.2-3_amd64.deb" "foo-doc_1.2-3_all.deb"))))))

(ert-deftest deb-packaging-test-detect/parse-changes-file-no-files-section ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (let ((changes (expand-file-name "foo_1.2-3_amd64.changes" pkg-parent-dir)))
      (write-region "Format: 1.8\n" nil changes)
      (should (null (deb-packaging-detect--parse-changes-file changes))))))

;;; Artifact scanning

(ert-deftest deb-packaging-test-detect/scan-artifacts-full-set ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3" :bin-names '("foo")
            :artifacts
            '(("foo_1.2-3.dsc" . "")
              ("foo_1.2-3_source.changes" . "")
              ("foo_1.2-3_amd64.changes" . "Files:\n abc123 100 devel optional foo_1.2-3_amd64.deb\n def456 1 devel optional foo_1.2-3_amd64.buildinfo\n")
              ("foo_1.2-3_amd64.deb" . "")
              ("foo_1.2-3_amd64.buildinfo" . "")))
    (let ((arts (deb-packaging-detect--scan-artifacts "foo" "1.2-3" pkg-parent-dir)))
      (should (alist-get 'dsc arts))
      (should (alist-get 'source-changes arts))
      (should (= (length (alist-get 'binary-changes arts)) 1))
      (should (member (expand-file-name "foo_1.2-3_amd64.deb" pkg-parent-dir)
                      (alist-get 'debs arts)))
      (should (member (expand-file-name "foo_1.2-3_amd64.buildinfo" pkg-parent-dir)
                      (alist-get 'buildinfo arts))))))

(ert-deftest deb-packaging-test-detect/scan-artifacts-dsc-only ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :artifacts '(("foo_1.2-3.dsc" . "")))
    (let ((arts (deb-packaging-detect--scan-artifacts "foo" "1.2-3" pkg-parent-dir)))
      (should (alist-get 'dsc arts))
      (should (null (alist-get 'source-changes arts)))
      (should (null (alist-get 'binary-changes arts)))
      (should (null (alist-get 'debs arts)))
      (should (null (alist-get 'buildinfo arts))))))

(ert-deftest deb-packaging-test-detect/scan-artifacts-empty-parent ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (let ((arts (deb-packaging-detect--scan-artifacts "foo" "1.2-3" pkg-parent-dir)))
      (should (null (alist-get 'dsc arts)))
      (should (null (alist-get 'source-changes arts)))
      (should (null (alist-get 'binary-changes arts)))
      (should (null (alist-get 'debs arts))))))

;;; Stale artifacts

(ert-deftest deb-packaging-test-detect/scan-stale-artifacts-old-versions ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3" :bin-names '("foo")
            :artifacts
            '(("foo_1.2-3.dsc" . "")
              ("foo_1.2-3_source.changes" . "")
              ("foo_1.2-3_amd64.deb" . "")
              ("foo_1.1-1.dsc" . "")
              ("foo_1.1-1_source.changes" . "")
              ("foo_1.1-1_amd64.deb" . "")))
    (let ((stale (deb-packaging-detect--scan-stale-artifacts "foo" "1.2-3" pkg-parent-dir pkg-dir)))
      (should (= (length stale) 3))
      (should (member "foo_1.1-1.dsc" stale))
      (should (member "foo_1.1-1_source.changes" stale))
      (should (member "foo_1.1-1_amd64.deb" stale))
      (should-not (member "foo_1.2-3.dsc" stale)))))

(ert-deftest deb-packaging-test-detect/scan-stale-artifacts-old-orig-tarball ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3" :bin-names '("foo")
            :artifacts
            '(("foo_1.2.orig.tar.gz" . "")
              ("foo_1.1.orig.tar.gz" . "")
              ("foo_1.2-3.dsc" . "")))
    (let ((stale (deb-packaging-detect--scan-stale-artifacts "foo" "1.2-3" pkg-parent-dir pkg-dir)))
      (should (member "foo_1.1.orig.tar.gz" stale))
      (should-not (member "foo_1.2.orig.tar.gz" stale)))))

(ert-deftest deb-packaging-test-detect/scan-stale-artifacts-dbgsym-and-dbg ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3" :bin-names '("foo")
            :artifacts
            '(("foo_1.2-3_amd64.deb" . "")
              ("foo-dbgsym_1.1-1_amd64.deb" . "")
              ("foo-dbg_1.1-1_amd64.ddeb" . "")))
    (let ((stale (deb-packaging-detect--scan-stale-artifacts "foo" "1.2-3" pkg-parent-dir pkg-dir)))
      (should (= (length stale) 2))
      (should (member "foo-dbgsym_1.1-1_amd64.deb" stale))
      (should (member "foo-dbg_1.1-1_amd64.ddeb" stale)))))

(ert-deftest deb-packaging-test-detect/scan-stale-artifacts-none ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (should (null (deb-packaging-detect--scan-stale-artifacts "foo" "1.2-3" pkg-parent-dir pkg-dir)))))

(ert-deftest deb-packaging-test-detect/scan-stale-artifacts-with-epoch ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1:2.0-1" :bin-names '("foo")
            :artifacts
            '(("foo_2.0-1.dsc" . "")
              ("foo_2.0.orig.tar.gz" . "")
              ("foo_1.5-1.dsc" . "")
              ("foo_1.5.orig.tar.gz" . "")))
    (let ((stale (deb-packaging-detect--scan-stale-artifacts "foo" "1:2.0-1" pkg-parent-dir pkg-dir)))
      (should (member "foo_1.5-1.dsc" stale))
      (should (member "foo_1.5.orig.tar.gz" stale))
      (should-not (member "foo_2.0-1.dsc" stale))
      (should-not (member "foo_2.0.orig.tar.gz" stale)))))

;;; Owned package prefixes

(ert-deftest deb-packaging-test-detect/owned-package-prefixes-with-binaries ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :bin-names '("foo" "foo-dev"))
    (let ((prefixes (deb-packaging-detect--owned-package-prefixes pkg-dir)))
      (should (member "foo" prefixes))
      (should (member "foo-dev" prefixes))
      (should (member "foo-dbgsym" prefixes))
      (should (member "foo-dbg" prefixes))
      (should (member "foo-dev-dbgsym" prefixes))
      (should (member "foo-dev-dbg" prefixes))
      ;; No duplicates.
      (should (= (length prefixes)
                 (length (delete-dups prefixes)))))))

(ert-deftest deb-packaging-test-detect/owned-package-prefixes-fallback-source ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (let ((tmp (make-temp-file "deb-pkg-test-" t)))
      (unwind-protect
          (progn
            ;; Copy just the changelog so source name is available.
            (make-directory (expand-file-name "debian" tmp) t)
            (copy-file (expand-file-name "debian/changelog" pkg-dir)
                       (expand-file-name "debian/changelog" tmp) t)
            (should (equal (deb-packaging-detect--owned-package-prefixes tmp)
                           '("foo" "foo-dbgsym" "foo-dbg"))))
        (delete-directory tmp t)))))

;;; Unified context scan

(ert-deftest deb-packaging-test-detect/scan-context-full-fixture ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3" :distro "noble"
            :bin-names '("foo")
            :maintainer "Karl Smeltzer <karl@example.com>"
            :source-format "3.0 (quilt)"
            :artifacts
            '(("foo_1.2-3.dsc" . "")
              ("foo_1.2-3_source.changes" . "")))
    (deb-packaging-test--with-mocked-process
        '(("dpkg" . "amd64"))
      (let ((ctx (deb-packaging-detect--scan-context pkg-dir)))
        (should ctx)
        (should (string= (plist-get ctx :name) "foo"))
        (should (string= (plist-get ctx :version) "1.2-3"))
        (should (string= (plist-get ctx :distro) "noble"))
        (should (file-equal-p (plist-get ctx :pkg-dir) pkg-dir))
        (should (file-equal-p (plist-get ctx :parent-dir) pkg-parent-dir))
        (should (string= (plist-get ctx :source-format) "3.0 (quilt)"))
        (should (string= (plist-get ctx :maintainer)
                         "Karl Smeltzer <karl@example.com>"))
        (should (alist-get 'dsc (plist-get ctx :artifacts)))
        (should (string= (plist-get ctx :arch) "amd64"))))))

(ert-deftest deb-packaging-test-detect/scan-context-outside-package ()
  (let ((tmp (make-temp-file "deb-pkg-test-" t)))
    (unwind-protect
        (should (null (deb-packaging-detect--scan-context tmp)))
      (delete-directory tmp t))))

(provide 'deb-packaging-test-detect)
;;; deb-packaging-test-detect.el ends here
