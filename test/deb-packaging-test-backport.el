;;; deb-packaging-test-backport.el --- Backport tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Keywords: tools, debian, ubuntu, packaging

;;; Commentary:

;; ERT tests for upstream patch backporting in deb-packaging-backport.el.

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'deb-packaging-test)
(require 'deb-packaging-backport)

(defconst deb-packaging-test-backport--mbox
  "From 1111111111111111111111111111111111111111 Mon Sep 17 00:00:00 2001
From: Jane Doe <jane@example.com>
Date: Mon, 25 Aug 2025 10:00:00 +0200
Subject: [PATCH] Fix the frobnicator

The frobnicator crashed when the widget
was missing.

---
 src/frob.c | 2 +-
 1 file changed, 1 insertion(+), 1 deletion(-)

diff --git a/src/frob.c b/src/frob.c
index 1111111..2222222 100644
--- a/src/frob.c
+++ b/src/frob.c
@@ -1,3 +1,3 @@
 int main(void) {
-    return 1;
+    return 0;
 }
-- 
2.43.0
")

(defconst deb-packaging-test-backport--mbox-2
  (concat deb-packaging-test-backport--mbox
          "From 2222222222222222222222222222222222222222 Mon Sep 17 00:00:00 2001
From: Bob Roe <bob@example.com>
Date: Tue, 26 Aug 2025 11:00:00 +0200
Subject: [PATCH] Add tests

---
 tests/frob | 1 +
 1 file changed, 1 insertion(+)

diff --git a/tests/frob b/tests/frob
new file mode 100644
index 0000000..3333333
--- /dev/null
+++ b/tests/frob
@@ -0,0 +1 @@
+ok
-- 
2.43.0
"))

;;; URL rewriting

(ert-deftest deb-packaging-test-backport/patch-url-forge-pages ()
  (should (string= (deb-packaging-backport--patch-url
                    "https://github.com/foo/bar/pull/123")
                   "https://github.com/foo/bar/pull/123.patch"))
  (should (string= (deb-packaging-backport--patch-url
                    "https://github.com/foo/bar/commit/abc123def")
                   "https://github.com/foo/bar/commit/abc123def.patch"))
  (should (string= (deb-packaging-backport--patch-url
                    "https://gitlab.com/foo/bar/-/merge_requests/7")
                   "https://gitlab.com/foo/bar/-/merge_requests/7.patch")))

(ert-deftest deb-packaging-test-backport/patch-url-passthrough ()
  (should (string= (deb-packaging-backport--patch-url
                    "https://github.com/foo/bar/pull/123.patch")
                   "https://github.com/foo/bar/pull/123.patch"))
  (should (string= (deb-packaging-backport--patch-url
                    "https://example.com/x.diff")
                   "https://example.com/x.diff"))
  (should (string= (deb-packaging-backport--patch-url
                    "https://example.com/anything")
                   "https://example.com/anything")))

(ert-deftest deb-packaging-test-backport/patch-url-cleans ()
  (should (string= (deb-packaging-backport--patch-url
                    "https://github.com/foo/bar/pull/123#discussion_r999")
                   "https://github.com/foo/bar/pull/123.patch"))
  (should (string= (deb-packaging-backport--patch-url
                    "https://github.com/foo/bar/pull/123/")
                   "https://github.com/foo/bar/pull/123.patch")))

;;; Fetching

(ert-deftest deb-packaging-test-backport/fetch-local-file ()
  (let* ((dir (make-temp-file "bp-test-" t))
         (f (expand-file-name "p.patch" dir)))
    (unwind-protect
        (progn
          (write-region "diff --git a/x b/x\n" nil f nil 0)
          (should (string= (deb-packaging-backport--fetch f)
                           "diff --git a/x b/x\n")))
      (delete-directory dir t))))

(ert-deftest deb-packaging-test-backport/fetch-url ()
  (deb-packaging-test--with-mocked-process
      '(("curl" . "diff --git a/x b/x\n"))
    (should (string= (deb-packaging-backport--fetch "https://example.com/x.patch")
                     "diff --git a/x b/x\n"))))

(ert-deftest deb-packaging-test-backport/fetch-curl-failure ()
  (deb-packaging-test--with-mocked-process
      '(("curl" . 22))
    (should-error (deb-packaging-backport--fetch "https://example.com/x.patch")
                  :type 'user-error)))

(ert-deftest deb-packaging-test-backport/fetch-empty-content ()
  (deb-packaging-test--with-mocked-process
      '(("curl" . ""))
    (should-error (deb-packaging-backport--fetch "https://example.com/x.patch")
                  :type 'user-error)))

(ert-deftest deb-packaging-test-backport/fetch-html-content ()
  (deb-packaging-test--with-mocked-process
      '(("curl" . "<!DOCTYPE html>\n<html></html>\n"))
    (should-error (deb-packaging-backport--fetch "https://example.com/bad")
                  :type 'user-error)))

(ert-deftest deb-packaging-test-backport/fetch-unreadable-source ()
  (should-error (deb-packaging-backport--fetch "/nonexistent/zzz.patch")
                :type 'user-error))

;;; Subject and signature handling

(ert-deftest deb-packaging-test-backport/strip-subject ()
  (should (string= (deb-packaging-backport--strip-subject "[PATCH] Fix") "Fix"))
  (should (string= (deb-packaging-backport--strip-subject
                    "[PATCH 2/3] Re: [v2] Fix")
                   "Fix"))
  (should (string= (deb-packaging-backport--strip-subject "Re: Fix") "Fix"))
  (should (string= (deb-packaging-backport--strip-subject "Fix") "Fix")))

(ert-deftest deb-packaging-test-backport/strip-signature ()
  (should (string= (deb-packaging-backport--strip-signature
                    "diff --git a/x b/x\n+1\n-- \n2.43.0\n")
                   "diff --git a/x b/x\n+1\n"))
  (should (string= (deb-packaging-backport--strip-signature
                    "diff --git a/x b/x\n+1\n")
                   "diff --git a/x b/x\n+1\n")))

;;; Parsing

(ert-deftest deb-packaging-test-backport/parse-single-commit ()
  (let* ((blocks (deb-packaging-backport--parse
                  deb-packaging-test-backport--mbox
                  "https://github.com/foo/bar/pull/123"))
         (b (car blocks)))
    (should (= (length blocks) 1))
    (should (string= (plist-get b :subject) "Fix the frobnicator"))
    (should (string= (plist-get b :author) "Jane Doe <jane@example.com>"))
    (should (string= (plist-get b :body)
                     "The frobnicator crashed when the widget\nwas missing."))
    (should (string-prefix-p "diff --git a/src/frob.c b/src/frob.c"
                             (plist-get b :diff)))
    (should (string-suffix-p " }\n" (plist-get b :diff)))
    (should-not (string-match-p "\n-- \n" (plist-get b :diff)))
    (should-not (string-match-p "2\\.43\\.0" (plist-get b :diff)))
    (should (string= (plist-get b :source-url)
                     "https://github.com/foo/bar/pull/123"))))

(ert-deftest deb-packaging-test-backport/parse-multi-commit ()
  (let* ((blocks (deb-packaging-backport--parse
                  deb-packaging-test-backport--mbox-2
                  "https://github.com/foo/bar/pull/123")))
    (should (= (length blocks) 2))
    (should (string= (plist-get (nth 0 blocks) :subject)
                     "Fix the frobnicator"))
    (should (string= (plist-get (nth 1 blocks) :subject) "Add tests"))
    (should (string= (plist-get (nth 1 blocks) :author)
                     "Bob Roe <bob@example.com>"))
    (should (string-empty-p (plist-get (nth 1 blocks) :body)))))

(ert-deftest deb-packaging-test-backport/parse-plain-diff ()
  (let* ((content "--- a/f.txt\n+++ b/f.txt\n@@ -1 +1 @@\n-x\n+y\n")
         (blocks (deb-packaging-backport--parse content "/tmp/local.patch"))
         (b (car blocks)))
    (should (= (length blocks) 1))
    (should (null (plist-get b :subject)))
    (should (null (plist-get b :author)))
    (should (string-empty-p (plist-get b :body)))
    (should (string= (plist-get b :diff) content))
    (should (null (plist-get b :source-url)))))

(ert-deftest deb-packaging-test-backport/parse-no-diff ()
  (should-error (deb-packaging-backport--parse
                 "hello world\nnot a patch\n" "https://example.com/x")
                :type 'user-error))

;;; DEP-3 header

(ert-deftest deb-packaging-test-backport/dep3-header-fields ()
  (let* ((blocks (deb-packaging-backport--parse
                  deb-packaging-test-backport--mbox
                  "https://github.com/foo/bar/pull/123"))
         (header (deb-packaging-backport--dep3-header (car blocks)))
         (lines (split-string header "\n")))
    (should (member "Description: Fix the frobnicator" lines))
    (should (member " The frobnicator crashed when the widget" lines))
    (should (member " was missing." lines))
    (should (member
             "Origin: upstream, https://github.com/foo/bar/pull/123" lines))
    (should (member "Author: Jane Doe <jane@example.com>" lines))
    (should (string-match-p
             "^Last-Update: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" header))))

(ert-deftest deb-packaging-test-backport/dep3-header-plain-diff ()
  (let* ((blocks (deb-packaging-backport--parse
                  "--- a/f.txt\n+++ b/f.txt\n@@ -1 +1 @@\n-x\n+y\n"
                  "/tmp/local.patch"))
         (header (deb-packaging-backport--dep3-header (car blocks))))
    (should (string-prefix-p "Description: backported patch\n" header))
    (should (string-match-p "^Origin: upstream$" header))
    (should-not (string-match-p "^Author: " header))
    (should (string-match-p "^Last-Update: " header))))

;;; Preflight

(ert-deftest deb-packaging-test-backport/ensure-quilt-native-rejected ()
  (deb-packaging-test--with-package-tree
      '(:name "demo" :version "1.0-1" :source-format "3.0 (native)")
    (should-error (deb-packaging-backport--ensure-quilt pkg-dir)
                  :type 'user-error)))

(ert-deftest deb-packaging-test-backport/ensure-quilt-accepts-quilt ()
  (deb-packaging-test--with-package-tree
      '(:name "demo" :version "1.0-1" :source-format "3.0 (quilt)")
    (should (null (deb-packaging-backport--ensure-quilt pkg-dir)))))

(ert-deftest deb-packaging-test-backport/ensure-quilt-accepts-missing-format ()
  (deb-packaging-test--with-package-tree
      '(:name "demo" :version "1.0-1")
    (should (null (deb-packaging-backport--ensure-quilt pkg-dir)))))

;;; Writing

(ert-deftest deb-packaging-test-backport/write-block-creates-series ()
  (deb-packaging-test--with-package-tree
      '(:name "demo" :version "1.0-1" :source-format "3.0 (quilt)")
    (let* ((blocks (deb-packaging-backport--parse
                    deb-packaging-test-backport--mbox
                    "https://github.com/foo/bar/pull/123"))
           (path (deb-packaging-backport--write-block
                  pkg-dir (car blocks) "fix-the-frobnicator.patch"))
           (content (with-temp-buffer
                      (insert-file-contents path)
                      (buffer-string)))
           (series (with-temp-buffer
                     (insert-file-contents
                      (expand-file-name "debian/patches/series" pkg-dir))
                     (buffer-string))))
      (should (file-readable-p path))
      (should (string-prefix-p "Description: Fix the frobnicator\n" content))
      (should (string-match-p
               "^Last-Update: .*\n\ndiff --git a/src/frob\\.c b/src/frob\\.c$"
               content))
      (should (string-suffix-p " }\n" content))
      (should (string= series "fix-the-frobnicator.patch\n")))))

(ert-deftest deb-packaging-test-backport/write-block-appends-to-series ()
  (deb-packaging-test--with-package-tree
      '(:name "demo" :version "1.0-1" :source-format "3.0 (quilt)"
              :patches (("existing.patch" . "Description: old\n\n--- a/x\n+++ b/x\n")))
    (let* ((blocks (deb-packaging-backport--parse
                    deb-packaging-test-backport--mbox
                    "https://github.com/foo/bar/pull/123"))
           (series (progn
                     (deb-packaging-backport--write-block
                      pkg-dir (car blocks) "new.patch")
                     (with-temp-buffer
                       (insert-file-contents
                        (expand-file-name "debian/patches/series" pkg-dir))
                       (buffer-string)))))
      (should (string= series "existing.patch\nnew.patch\n")))))

(ert-deftest deb-packaging-test-backport/add-to-series-dedup ()
  (deb-packaging-test--with-package-tree
      '(:name "demo" :version "1.0-1")
    (let ((dir (expand-file-name "debian/patches" pkg-dir)))
      (deb-packaging-backport--add-to-series dir "a.patch")
      (deb-packaging-backport--add-to-series dir "a.patch")
      (let ((series (with-temp-buffer
                      (insert-file-contents (expand-file-name "series" dir))
                      (buffer-string))))
        (should (string= series "a.patch\n"))))))

(provide 'deb-packaging-test-backport)
;;; deb-packaging-test-backport.el ends here
