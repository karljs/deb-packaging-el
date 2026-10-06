;;; deb-packaging-test-resume.el --- Kept-build resume tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'deb-packaging-test)
(require 'deb-packaging-resume)
(require 'deb-packaging-commands)

(ert-deftest deb-packaging-test-resume/parses-what-sbuild-prints ()
  (with-temp-buffer
    (insert "DEB_BUILD_OPTIONS=parallel=16\n"
            "dpkg-source: info: extracting kbtest in /build/kbtest-LNs78U/kbtest-1.0\n"
            "Command: dpkg-buildpackage --sanitize-env -Pnocheck -us -uc -B\n"
            "Keeping session: stonking-amd64-b003\n")
    (should (equal (deb-packaging-resume--parse-sbuild (current-buffer))
                   '(:session "stonking-amd64-b003"
                     :tree "/build/kbtest-LNs78U/kbtest-1.0"
                     :command "dpkg-buildpackage --sanitize-env -Pnocheck -us -uc -B"
                     :dbo "parallel=16")))))

(ert-deftest deb-packaging-test-resume/nothing-kept-without-session ()
  (with-temp-buffer
    (insert "dpkg-source: info: extracting kbtest in /build/x/kbtest-1.0\n")
    (should-not (deb-packaging-resume--parse-sbuild (current-buffer)))))

(ert-deftest deb-packaging-test-resume/command-continues-without-clean ()
  (should (equal (deb-packaging-resume--command
                  '(:command "dpkg-buildpackage --sanitize-env -us -uc -B"
                    :dbo "parallel=16 nocheck"))
                 "export LC_ALL=C.UTF-8 DEB_BUILD_OPTIONS=parallel\\=16\\ nocheck; dpkg-buildpackage --sanitize-env -us -uc -B -nc")))

(defmacro deb-packaging-test-resume--with-trees (&rest body)
  "Run BODY with CHECKOUT and TREE dirs holding debian/patches."
  (declare (indent 0))
  `(let* ((root (make-temp-file "deb-resume-" t))
          (checkout (expand-file-name "checkout/" root))
          (tree (expand-file-name "tree/" root)))
     (unwind-protect
         (cl-flet ((put (dir rel text)
                     (deb-packaging-test--write-file (expand-file-name rel dir) text)))
           (ignore checkout tree)
           ,@body)
       (delete-directory root t))))

(ert-deftest deb-packaging-test-resume/only-changed-patches-and-later-unapply ()
  (deb-packaging-test-resume--with-trees
    (dolist (d (list checkout tree))
      (put d "debian/patches/a.patch" "a")
      (put d "debian/patches/c.patch" "c"))
    (put checkout "debian/patches/b.patch" "b-new")
    (put tree "debian/patches/b.patch" "b-old")
    (let ((stale (deb-packaging-resume--stale-patches
                  '("a.patch" "b.patch" "c.patch") '("a.patch" "b.patch" "c.patch")
                  (lambda (p) (expand-file-name (concat "debian/patches/" p) checkout))
                  (lambda (p) (expand-file-name (concat "debian/patches/" p) tree)))))
      (should (equal stale '("b.patch" "c.patch"))))
    (should-not (deb-packaging-resume--stale-patches
                 '("a.patch") '("a.patch" "new.patch")
                 (lambda (p) (expand-file-name (concat "debian/patches/" p) checkout))
                 (lambda (p) (expand-file-name (concat "debian/patches/" p) tree))))))

(ert-deftest deb-packaging-test-resume/sync-copies-only-differences-with-fresh-mtime ()
  (deb-packaging-test-resume--with-trees
    (put checkout "debian/rules" "same\n")
    (put tree "debian/rules" "same\n")
    (put checkout "debian/patches/series" "fix.patch\n")
    (put checkout "debian/patches/fix.patch" "fix\n")
    (put tree "debian/patches/series" "")
    (set-file-times (expand-file-name "debian/patches/fix.patch" checkout)
                    (encode-time 0 0 0 1 1 2001))
    (let ((rules-time (file-attribute-modification-time
                       (file-attributes (expand-file-name "debian/rules" tree)))))
      (should (equal (plist-get (deb-packaging-resume--sync checkout tree) :copied)
                     '("debian/patches/fix.patch" "debian/patches/series")))
      (should (equal rules-time (file-attribute-modification-time
                                 (file-attributes (expand-file-name "debian/rules" tree)))))
      ;; Old checkout mtime must not leak in, or make would skip the rebuild.
      (should (time-less-p (encode-time 0 0 0 1 1 2020)
                           (file-attribute-modification-time
                            (file-attributes
                             (expand-file-name "debian/patches/fix.patch" tree))))))))

(ert-deftest deb-packaging-test-resume/series-options-and-comments ()
  (deb-packaging-test-resume--with-trees
    (put checkout "series" "# note\nfix.patch -p1\n\nother.patch\n")
    (should (equal (deb-packaging-resume--read-lines (expand-file-name "series" checkout))
                   '("fix.patch" "other.patch")))))

(ert-deftest deb-packaging-test-resume/changed-applied-patch-is-reapplied ()
  (skip-unless (executable-find "patch"))
  (deb-packaging-test-resume--with-trees
    (let ((old "--- a/f\n+++ b/f\n@@ -1 +1 @@\n-one\n+two\n")
          (new "--- a/f\n+++ b/f\n@@ -1 +1 @@\n-one\n+three\n"))
      (put tree "f" "two\n")
      (put tree "debian/patches/series" "p.patch\n")
      (put tree "debian/patches/p.patch" old)
      (put tree ".pc/applied-patches" "p.patch\n")
      (make-directory (expand-file-name ".pc/p.patch" tree) t)
      (put checkout "debian/patches/series" "p.patch\n")
      (put checkout "debian/patches/p.patch" new)
      (should (equal (plist-get (deb-packaging-resume--sync checkout tree) :reapplied)
                     '("p.patch")))
      ;; Unapplied with the old text; dpkg-buildpackage re-applies the new one.
      (should (equal (with-temp-buffer
                       (insert-file-contents (expand-file-name "f" tree)) (buffer-string))
                     "one\n"))
      (should (equal (deb-packaging-resume--read-lines
                      (expand-file-name ".pc/applied-patches" tree))
                     nil))
      (should-not (file-exists-p (expand-file-name ".pc/p.patch" tree))))))

(provide 'deb-packaging-test-resume)
;;; deb-packaging-test-resume.el ends here
