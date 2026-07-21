;;; deb-packaging-test-version.el --- Version pipeline tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for version-string manipulation in deb-packaging-detect.el.

;;; Code:

(require 'ert)
(require 'deb-packaging-test)
(require 'deb-packaging-detect)

(ert-deftest deb-packaging-test-version/version-to-filename-strips-epoch ()
  (should (string= (deb-packaging-detect--version-to-filename "1:2.3-4") "2.3-4")))

(ert-deftest deb-packaging-test-version/version-to-filename-no-epoch ()
  (should (string= (deb-packaging-detect--version-to-filename "2.3-4") "2.3-4")))

(ert-deftest deb-packaging-test-version/version-to-filename-native ()
  (should (string= (deb-packaging-detect--version-to-filename "1.2") "1.2")))

(ert-deftest deb-packaging-test-version/upstream-version-with-revision ()
  (should (string= (deb-packaging-detect--upstream-version "1.2-3") "1.2")))

(ert-deftest deb-packaging-test-version/upstream-version-native ()
  (should (string= (deb-packaging-detect--upstream-version "1.2") "1.2")))

(ert-deftest deb-packaging-test-version/upstream-version-with-epoch ()
  (should (string= (deb-packaging-detect--upstream-version "1:1.2-3") "1.2")))

(ert-deftest deb-packaging-test-version/upstream-version-ubuntu-revision ()
  (should (string= (deb-packaging-detect--upstream-version "1.2-3ubuntu1") "1.2")))

(ert-deftest deb-packaging-test-version/upstream-version-multiple-dashes ()
  (should (string= (deb-packaging-detect--upstream-version "2.0-1-2") "2.0-1")))

(ert-deftest deb-packaging-test-version/native-version-p-native ()
  (should (deb-packaging-detect--native-version-p "1.2"))
  (should (deb-packaging-detect--native-version-p "1:1.2")))

(ert-deftest deb-packaging-test-version/native-version-p-non-native ()
  (should-not (deb-packaging-detect--native-version-p "1.2-3"))
  (should-not (deb-packaging-detect--native-version-p "1:1.2-3ubuntu1")))

(ert-deftest deb-packaging-test-version/filename-version-three-field-deb ()
  (should (string= (deb-packaging-detect--filename-version "foo_1.2-3_amd64.deb") "1.2-3")))

(ert-deftest deb-packaging-test-version/filename-version-dsc ()
  (should (string= (deb-packaging-detect--filename-version "foo_1.2-3.dsc") "1.2-3")))

(ert-deftest deb-packaging-test-version/filename-version-source-changes ()
  (should (string= (deb-packaging-detect--filename-version "foo_1.2-3_source.changes") "1.2-3")))

(ert-deftest deb-packaging-test-version/filename-version-orig-tar-gz-nil ()
  (should (null (deb-packaging-detect--filename-version "foo_1.2.orig.tar.gz"))))

(ert-deftest deb-packaging-test-version/filename-version-orig-tar-xz-nil ()
  (should (null (deb-packaging-detect--filename-version "foo_1.2.orig.tar.xz"))))

(ert-deftest deb-packaging-test-version/filename-version-debian-tar ()
  (should (string= (deb-packaging-detect--filename-version "foo_1.2-3.debian.tar.xz") "1.2-3")))

(ert-deftest deb-packaging-test-version/filename-version-buildinfo ()
  (should (string= (deb-packaging-detect--filename-version "foo_1.2-3_amd64.buildinfo") "1.2-3")))

(ert-deftest deb-packaging-test-version/filename-version-plain-tar ()
  (should (string= (deb-packaging-detect--filename-version "foo_1.2-3.tar.gz") "1.2-3")))

(ert-deftest deb-packaging-test-version/orig-tarball-found-gz ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :artifacts '(("foo_1.2.orig.tar.gz" . "")))
    (let ((path (deb-packaging-detect--orig-tarball "foo" "1.2-3" pkg-parent-dir)))
      (should path)
      (should (file-name-absolute-p path))
      (should (string-suffix-p "foo_1.2.orig.tar.gz" path)))))

(ert-deftest deb-packaging-test-version/orig-tarball-found-xz ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :artifacts '(("foo_1.2.orig.tar.xz" . "")))
    (let ((path (deb-packaging-detect--orig-tarball "foo" "1.2-3" pkg-parent-dir)))
      (should path)
      (should (file-name-absolute-p path))
      (should (string-suffix-p "foo_1.2.orig.tar.xz" path)))))

(ert-deftest deb-packaging-test-version/orig-tarball-missing ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (should (null (deb-packaging-detect--orig-tarball "foo" "1.2-3" pkg-parent-dir)))))

(ert-deftest deb-packaging-test-version/orig-tarball-wrong-version ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :artifacts '(("foo_1.0.orig.tar.gz" . "")))
    (should (null (deb-packaging-detect--orig-tarball "foo" "1.2-3" pkg-parent-dir)))))

(provide 'deb-packaging-test-version)
;;; deb-packaging-test-version.el ends here
