;;; deb-packaging-test-propagate.el --- Propagate tests -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for quilt->git-am normalization and salsa URL handling in
;; deb-packaging-propagate.el.

;;; Code:

(require 'ert)
(require 'deb-packaging-test)
(require 'deb-packaging-propagate)

;;; Slug and description helpers

(ert-deftest deb-packaging-test-propagate/slug-nil-or-empty ()
  (should (string= (deb-packaging-propagate--slug nil) ""))
  (should (string= (deb-packaging-propagate--slug "") "")))

(ert-deftest deb-packaging-test-propagate/slug-basic ()
  (should (string= (deb-packaging-propagate--slug "Fix the Thing Now Please")
                   "fix-the-thing-now")))

(ert-deftest deb-packaging-test-propagate/slug-special-chars ()
  (should (string= (deb-packaging-propagate--slug "Hello, World! Today is great")
                   "hello-world-today-is")))

(ert-deftest deb-packaging-test-propagate/slug-truncate ()
  (let ((long "abcdefghijklmno abcdefghijklmno abcdefghijklmno abcdefghijklmno"))
    (should (= (length (deb-packaging-propagate--slug long)) 50))
    (should (string-prefix-p "abcdefghijklmno-abcdefghijklmno-abcdefghijklmno-ab" 
                              (deb-packaging-propagate--slug long)))))

(ert-deftest deb-packaging-test-propagate/item-slug-dispatch ()
  (should (string= (deb-packaging-propagate--item-slug
                    '(:type patch :name "Fix the Bug"))
                   "fix-the-bug"))
  (should (string= (deb-packaging-propagate--item-slug
                    '(:type commit :subject "Do the thing"))
                   "do-the-thing"))
  (should (string= (deb-packaging-propagate--item-slug
                    '(:type range :range "HEAD~2..HEAD"))
                   "head-2-head"))
  (should (string= (deb-packaging-propagate--item-slug
                    '(:type unknown :foo "x"))
                   "fix")))

(ert-deftest deb-packaging-test-propagate/item-description-dispatch ()
  (should (string= (deb-packaging-propagate--item-description
                    '(:type patch :name "fix.diff"))
                   "patch fix.diff"))
  (should (string= (deb-packaging-propagate--item-description
                    '(:type commit :ref "abc123" :subject "Do it"))
                   "commit abc123 (Do it)"))
  (should (string= (deb-packaging-propagate--item-description
                    '(:type range :range "HEAD~2..HEAD"))
                   "range HEAD~2..HEAD"))
  (should (string= (deb-packaging-propagate--item-description
                    '(:type weird))
                   "fix")))

;;; Quilt header parsing

(ert-deftest deb-packaging-test-propagate/parse-quilt-headers ()
  (let* ((patch "Description: Short desc
 More text

 More after blank
Author: Jane Doe <jane@example.com>
--- a/foo
+++ b/foo
")
         (result (deb-packaging-propagate--parse-quilt-headers patch)))
    (should (string= (plist-get result :description)
                     "Short desc\nMore text\nMore after blank"))
    (should (string= (plist-get result :author)
                     "Jane Doe <jane@example.com>"))))

(ert-deftest deb-packaging-test-propagate/parse-quilt-headers-terminator ()
  (let* ((patch "Description: Desc
---
Author: Me <me@example.com>")
         (result (deb-packaging-propagate--parse-quilt-headers patch)))
    (should (string= (plist-get result :description) "Desc"))
    (should (string= (plist-get result :author) "Me <me@example.com>"))))

(ert-deftest deb-packaging-test-propagate/parse-quilt-headers-missing ()
  (let* ((patch "--- a/foo
+++ b/foo
")
         (result (deb-packaging-propagate--parse-quilt-headers patch)))
    (should (null (plist-get result :description)))
    (should (null (plist-get result :author)))))

;;; Diff path normalization

(ert-deftest deb-packaging-test-propagate/normalize-diff-paths ()
  "`---' lines get a/ prefix; `+++' lines get b/ prefix."
  (let* ((input "--- a/src/foo.c
+++ b/src/foo.c
--- README
+++ README
--- a/.gitlab-ci.yml
+++ b/.gitlab-ci.yml
")
         (expected "--- a/src/foo.c\n+++ b/src/foo.c\n--- a/README\n+++ b/README\n--- a/.gitlab-ci.yml\n+++ b/.gitlab-ci.yml\n")
         (got (deb-packaging-propagate--normalize-diff-paths input)))
    (should (string= got expected))))

(ert-deftest deb-packaging-test-propagate/normalize-diff-paths-swapped-prefix ()
  (let* ((input "--- b/foo.c\n+++ a/foo.c\n")
         (expected "--- a/foo.c\n+++ b/foo.c\n")
         (got (deb-packaging-propagate--normalize-diff-paths input)))
    (should (string= got expected))))

;;; quilt -> git-am conversion

(ert-deftest deb-packaging-test-propagate/quilt-to-git-am-block ()
  (deb-packaging-test--with-package-tree
      '(:name "demo"
        :version "1.0-1"
        :patches (("my-fix.patch" . "Description: Fix the bug
Author: A U Thor <author@example.com>

--- a/f.txt
+++ b/f.txt
@@ -1 +1 @@
-line
+fixed line
")))
    (let* ((patches (deb-packaging--list-patches))
           (path (cdar patches))
           (block (deb-packaging-propagate--quilt-to-git-am-block path)))
      (should (string-match-p "^From " block))
      (should (string-match-p "^From: A U Thor <author@example.com>$" block))
      (should (string-match-p "^Date: " block))
      (should (string-match-p "^Subject: \\[PATCH\\] Fix the bug$" block))
      (should (string-match-p "^--- a/f\\.txt$" block))
      (should (string-match-p "^\\+\\+\\+ b/f\\.txt$" block)))))

(ert-deftest deb-packaging-test-propagate/quilt-to-git-am-block-fallback ()
  (deb-packaging-test--with-package-tree
      '(:name "demo2"
        :version "1.0-1"
        :patches (("fallback.patch" . "--- a/f.txt
+++ b/f.txt
@@ -1 +1 @@
-x
+y
")))
    (let* ((patches (deb-packaging--list-patches))
           (path (cdar patches))
           (block (let ((user-full-name "Test User")
                        (user-mail-address "test@example.com"))
                    (deb-packaging-propagate--quilt-to-git-am-block path))))
      (should (string-match-p "^From: Test User <test@example.com>$" block))
      (should (string-match-p "^Subject: \\[PATCH\\] fallback$" block)))))

;;; Salsa URL helpers

(ert-deftest deb-packaging-test-propagate/salsa-project-path ()
  (should (string= (deb-packaging-propagate--salsa-project-path
                    "https://salsa.debian.org/foo/bar.git")
                   "foo/bar"))
  (should (string= (deb-packaging-propagate--salsa-project-path
                    "git@salsa.debian.org:foo/bar.git")
                   "foo/bar"))
  (should (string= (deb-packaging-propagate--salsa-project-path
                    "https://salsa.debian.org/foo/bar")
                   "foo/bar"))
  (should (null (deb-packaging-propagate--salsa-project-path
                 "https://github.com/foo/bar.git"))))

(ert-deftest deb-packaging-test-propagate/fork-url ()
  (should (string= (deb-packaging-propagate--fork-url
                    "https://salsa.debian.org/foo/bar.git")
                   "https://salsa.debian.org/foo/bar/-/forks/new"))
  (should (null (deb-packaging-propagate--fork-url
                 "https://github.com/foo/bar.git"))))

(ert-deftest deb-packaging-test-propagate/salsa-personal-url ()
  (should (null (deb-packaging-propagate--salsa-personal-url "pkg")))
  (let ((deb-packaging-propagate-salsa-user "jdoe"))
    (should (string= (deb-packaging-propagate--salsa-personal-url "pkg")
                     "git@salsa.debian.org:~jdoe/pkg.git"))))

;;; Clone helpers

(ert-deftest deb-packaging-test-propagate/clone-dir ()
  (let ((deb-packaging-propagate-cache-dir (make-temp-file "prop-cache-" t)))
    (unwind-protect
        (let ((dir (deb-packaging-propagate--clone-dir "foo")))
          (should (file-name-absolute-p dir))
          (should (string-suffix-p "debian/foo" dir)))
      (delete-directory deb-packaging-propagate-cache-dir t))))

(ert-deftest deb-packaging-test-propagate/clone-exists-p ()
  (let ((root (make-temp-file "clone-test-" t)))
    (unwind-protect
        (progn
          (should-not (deb-packaging-propagate--clone-exists-p root))
          (make-directory (expand-file-name ".git" root) t)
          (should (deb-packaging-propagate--clone-exists-p root))
          (should-not (deb-packaging-propagate--clone-exists-p
                       (expand-file-name "nonexistent" root))))
      (delete-directory root t))))

;;; Git probes

(ert-deftest deb-packaging-test-propagate/git-quiet ()
  (deb-packaging-test--with-temp-git-repo
    (should (string= (deb-packaging-propagate--git-quiet
                      repo-dir "branch" "--show-current")
                     "main"))
    (should (string= (deb-packaging-propagate--git-quiet
                      repo-dir "rev-list" "--count" "HEAD")
                     "1"))))

(ert-deftest deb-packaging-test-propagate/commit-applied-p ()
  (deb-packaging-test--with-temp-git-repo
    (deb-packaging-test--write-file (expand-file-name "f.txt" repo-dir) "x\n")
    (deb-packaging-test--git repo-dir "add" "-A")
    (deb-packaging-test--git repo-dir "commit" "-q" "-m" "My fix")
    (should (deb-packaging-propagate--commit-applied-p "My fix" repo-dir))
    (should-not (deb-packaging-propagate--commit-applied-p "Nope" repo-dir))))

(ert-deftest deb-packaging-test-propagate/patch-applied-p ()
  (deb-packaging-test--with-temp-git-repo
    (deb-packaging-test--write-file (expand-file-name "f.txt" repo-dir) "line1\n")
    (deb-packaging-test--git repo-dir "add" "f.txt")
    (deb-packaging-test--git repo-dir "commit" "-q" "-m" "initial file")
    (let ((patch-file (make-temp-file "patch-" nil ".patch")))
      (unwind-protect
          (progn
            (with-temp-file patch-file
              (insert "--- a/f.txt\n+++ b/f.txt\n@@ -1 +1 @@\n-line1\n+line2\n"))
            (should-not (deb-packaging-propagate--patch-applied-p patch-file repo-dir))
            (deb-packaging-test--git repo-dir "apply" patch-file)
            (deb-packaging-test--git repo-dir "add" "f.txt")
            (deb-packaging-test--git repo-dir "commit" "-q" "-m" "apply fix")
            (should (deb-packaging-propagate--patch-applied-p patch-file repo-dir)))
        (delete-file patch-file)))))

(ert-deftest deb-packaging-test-propagate/default-branch ()
  (deb-packaging-test--with-temp-git-repo
    (let* ((clone-root (make-temp-file "clone-"))
           (clone-dir (file-name-as-directory clone-root)))
      (delete-file clone-root)
      (unwind-protect
          (progn
            (deb-packaging-test--git repo-dir "clone" "-q" repo-dir clone-dir)
            (let ((branch (deb-packaging-propagate--default-branch clone-dir)))
              (should (member branch '("main" "master")))))
        (when (file-directory-p clone-root)
          (delete-directory clone-root t))))))

(provide 'deb-packaging-test-propagate)
;;; deb-packaging-test-propagate.el ends here
