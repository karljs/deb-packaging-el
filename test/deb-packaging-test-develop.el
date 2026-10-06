;;; deb-packaging-test-develop.el --- Change-workflow tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'deb-packaging-test)
(require 'deb-packaging-develop)
(require 'deb-packaging-status)

(defmacro deb-packaging-test-develop--with-clone (&rest body)
  "Run BODY in a git-ubuntu-like repo: ubuntu/devel tracking pkg/ubuntu/devel."
  (declare (indent 0))
  `(deb-packaging-test--with-temp-git-repo
     (deb-packaging-test--build-tree
      repo-dir (file-name-directory (directory-file-name repo-dir))
      '(:name "foo" :version "1.0-1" :distro "noble" :source-format "3.0 (quilt)"))
     (deb-packaging-test--git repo-dir "add" "debian")
     (deb-packaging-test--git repo-dir "commit" "-q" "-m" "packaging")
     (deb-packaging-test--git repo-dir "branch" "-q" "-m" "ubuntu/devel")
     (deb-packaging-test--git repo-dir "remote" "add" "pkg"
                              "https://git.launchpad.net/ubuntu/+source/foo")
     (deb-packaging-test--git repo-dir "update-ref" "refs/remotes/pkg/ubuntu/devel" "HEAD")
     (deb-packaging-test--git repo-dir "branch" "-q" "--set-upstream-to=pkg/ubuntu/devel")
     ,@body))

(defun deb-packaging-test-develop--facts ()
  (deb-packaging-develop--facts (deb-packaging-commands--package-context)))

(ert-deftest deb-packaging-test-develop/fix-branch-counts-commits-against-archive ()
  (deb-packaging-test-develop--with-clone
    (should (deb-packaging-develop--git-ubuntu-p))
    (should (deb-packaging-status--on-base-branch-p (deb-packaging-test-develop--facts)))
    (deb-packaging-develop-new-branch "lp2071234")
    (let ((facts (deb-packaging-test-develop--facts)))
      (should (equal (plist-get facts :branch) "lp2071234"))
      (should (equal (plist-get facts :upstream) "pkg/ubuntu/devel"))
      (should (equal (plist-get facts :bug) "2071234"))
      (should-not (deb-packaging-status--on-base-branch-p facts)))
    (deb-packaging-test--write-file (expand-file-name "fix.c" repo-dir) "x\n")
    (deb-packaging-test--git repo-dir "add" "fix.c")
    (deb-packaging-test--git repo-dir "commit" "-q" "-m" "fix")
    (let ((facts (deb-packaging-test-develop--facts)))
      (should (= (plist-get facts :ahead) 1))
      (should-not (plist-get facts :changelog-changed))
      (should (car (deb-packaging-status--changelog-warnings facts))))))

(ert-deftest deb-packaging-test-develop/changelog-add-appends-only-while-unreleased ()
  (deb-packaging-test--with-package-tree
      '(:name "foo" :version "1.0-1" :distro "noble")
    (let (calls)
      (cl-letf (((symbol-function 'deb-packaging-develop--dch)
                 (lambda (_dir &rest args) (push args calls))))
        (deb-packaging-develop-changelog-add "Fix it" "123")
        (should (equal (pop calls) '("--increment" "Fix it (LP: #123)")))
        (deb-packaging-test--write-file
         (expand-file-name "debian/changelog" pkg-dir)
         "foo (1.0-1ubuntu1) UNRELEASED; urgency=medium\n\n  * x\n\n -- A <a@b.c>  Mon, 01 Jan 2024 00:00:00 +0000\n")
        (deb-packaging-develop-changelog-add "More (LP: #9)" "123")
        (should (equal (pop calls) '("--append" "More (LP: #9)")))))))

(ert-deftest deb-packaging-test-develop/release-uses-the-series-status-shows ()
  (deb-packaging-test--with-package-tree
      '(:name "foo" :version "1.0-1ubuntu1" :distro "noble-proposed")
    (let (args)
      (cl-letf (((symbol-function 'deb-packaging-develop--dch)
                 (lambda (_dir &rest a) (setq args a))))
        (deb-packaging-develop-changelog-release))
      (should (equal args '("--release" "--distribution" "noble" ""))))))

(ert-deftest deb-packaging-test-develop/bug-from-branch-or-changelog ()
  (cl-letf (((symbol-function 'magit-get-current-branch) (lambda () "fix/lp-1234567-crash"))
            ((symbol-function 'deb-packaging-detect--changelog-top-text) #'ignore))
    (should (equal (deb-packaging-detect--launchpad-bug "/tmp/") "1234567")))
  (cl-letf (((symbol-function 'magit-get-current-branch) (lambda () "ubuntu/devel"))
            ((symbol-function 'deb-packaging-detect--changelog-top-text)
             (lambda (&rest _) "foo (1) UNRELEASED;\n\n  * Fix (LP: #42)\n")))
    (should (equal (deb-packaging-detect--launchpad-bug "/tmp/") "42"))))

(ert-deftest deb-packaging-test-develop/maintainer-stale-only-for-ubuntu-versions ()
  (deb-packaging-test--with-package-tree
      '(:name "foo" :version "1.0-1" :maintainer "A B <a@debian.org>")
    (should (deb-packaging-develop--maintainer-stale-p pkg-dir "1.0-1ubuntu1"))
    (should-not (deb-packaging-develop--maintainer-stale-p pkg-dir "1.0-1"))))

(ert-deftest deb-packaging-test-develop/upload-blocked-while-unreleased ()
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) t)))
    (should (string-match-p
             "UNRELEASED"
             (alist-get 'dput (deb-packaging-status--blockers
                               '(:changelog-distro "UNRELEASED"
                                 :artifacts ((source-changes . "x.changes")))))))))

(ert-deftest deb-packaging-test-develop/status-shows-the-whole-flow ()
  (deb-packaging-test-develop--with-clone
    (cl-letf (((symbol-function 'deb-packaging-dev--list-containers) #'ignore))
      (with-temp-buffer
        (deb-packaging-status-mode)
        (setq default-directory repo-dir)
        (deb-packaging-status--render)
        (let ((text (buffer-string)))
          (dolist (label '("Develop" "Branch" "Patches" "Changelog" "Dev shell"
                           "Local" "Launchpad" "Submit" "Merge proposal" "Forward"))
            (should (string-match-p (regexp-quote label) text)))
          (should (string-match-p "Start a fix branch" text)))))))

(ert-deftest deb-packaging-test-develop/uscan-summary ()
  (with-temp-buffer
    (insert "foo: Newest version of foo on remote site is 2.0, local version is 1.0\n"
            "foo:  => Newer package available from:\n")
    (should (equal (deb-packaging-commands--parse-uscan-summary (buffer-name))
                   '(:newest "2.0" :newer t)))))

(provide 'deb-packaging-test-develop)
;;; deb-packaging-test-develop.el ends here
