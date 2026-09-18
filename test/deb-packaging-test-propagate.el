;;; deb-packaging-test-propagate.el --- Propagate tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

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

(ert-deftest deb-packaging-test-propagate/normalize-diff-paths-keeps-dev-null ()
  (let* ((input "--- /dev/null\n+++ new.txt\n--- old.txt\n+++ /dev/null\n")
         (expected "--- /dev/null\n+++ b/new.txt\n--- a/old.txt\n+++ /dev/null\n"))
    (should (string= (deb-packaging-propagate--normalize-diff-paths input)
                     expected))))

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
    (let* ((patches (deb-packaging-detect--list-patches))
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
    (let* ((patches (deb-packaging-detect--list-patches))
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
  (let ((deb-packaging-config-propagate-salsa-user "jdoe"))
    (should (string= (deb-packaging-propagate--salsa-personal-url "pkg")
                     "git@salsa.debian.org:~jdoe/pkg.git"))))

;;; Clone helpers

(ert-deftest deb-packaging-test-propagate/clone-dir ()
  (let ((deb-packaging-config-propagate-cache-dir (make-temp-file "prop-cache-" t)))
    (unwind-protect
        (let ((dir (deb-packaging-propagate--clone-dir "foo")))
          (should (file-name-absolute-p dir))
          (should (string-suffix-p "debian/foo" dir)))
      (delete-directory deb-packaging-config-propagate-cache-dir t))))

(ert-deftest deb-packaging-test-propagate/clone-exists-p ()
  (deb-packaging-test--with-temp-git-repo
    (should (deb-packaging-propagate--clone-exists-p repo-dir))
    (should-not (deb-packaging-propagate--clone-exists-p
                 (expand-file-name "nonexistent" repo-dir)))))

(ert-deftest deb-packaging-test-propagate/clone-exists-p-worktree ()
  (deb-packaging-test--with-temp-git-repo
    (let ((worktree (concat (directory-file-name repo-dir) "-worktree")))
      (unwind-protect
          (progn
            (deb-packaging-test--git repo-dir "worktree" "add" "-q" "-b" "worktree" worktree)
            (should (file-regular-p (expand-file-name ".git" worktree)))
            (should (deb-packaging-propagate--clone-exists-p worktree)))
        (when (file-directory-p worktree)
          (delete-directory worktree t))))))

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

;;; Clone session flow (mocked)

(defun deb-packaging-test-propagate--wait-for-exit (proc)
  "Wait for PROC to exit and let its sentinel run."
  (while (memq (process-status proc) '(run stop open listen))
    (accept-process-output nil 0.05))
  (accept-process-output nil 0.1))

(defmacro deb-packaging-test-propagate--with-clone-mocks (answers &rest body)
  "Run BODY with `deb-packaging-propagate-clone' dependencies mocked.
ANSWERS is a list of `yes-or-no-p' answers consumed in order.  The
network step (`magit-run-git-async') is mocked to a real `true'
process; the post-network continuation runs from its sentinel, so BODY
must wait on `async-proc' before asserting continuation effects.
Within BODY, `git-calls' records `magit-call-git' argument lists,
`async-calls' records `magit-run-git-async' argument lists, `prompts'
records confirmation prompts, `messages' records echo-area messages,
`status-opened' counts `magit-status-setup-buffer' calls, and
`async-proc' holds the mock network process (nil when none started)."
  (declare (indent 1) (debug (form body)))
  `(let ((git-calls nil)
         (async-calls nil)
         (prompts nil)
         (messages nil)
         (status-opened 0)
         (async-proc nil)
         (remaining ,answers))
     (cl-letf (((symbol-function 'deb-packaging-propagate--clone-dir)
                (lambda (name) (expand-file-name (concat name "-clone")
                                                 temporary-file-directory)))
               ((symbol-function 'deb-packaging-propagate--clone-exists-p)
                (lambda (_dir) t))
               ((symbol-function 'deb-packaging-propagate--default-branch)
                (lambda (_dir) "main"))
               ((symbol-function 'deb-packaging-propagate--remote-branches)
                (lambda (_dir) nil))
               ((symbol-function 'deb-packaging-propagate--git-quiet)
                (lambda (_dir &rest args)
                  (when (equal (car args) "rev-parse") "abc123")))
               ((symbol-function 'deb-packaging-propagate--salsa-personal-url)
                (lambda (_name) nil))
               ((symbol-function 'read-string)
                (lambda (&rest _) "ignored"))
               ((symbol-function 'yes-or-no-p)
                (lambda (prompt) (push prompt prompts) (pop remaining)))
               ((symbol-function 'magit-run-git-async)
                (lambda (&rest args)
                  (push args async-calls)
                  ;; Spawn from a real directory: the command binds
                  ;; default-directory to the (mocked, nonexistent)
                  ;; clone dir around the call.
                  (let ((default-directory temporary-file-directory))
                    (setq async-proc (start-process "deb-prop-test" nil "true")
                          magit-this-process async-proc))))
               ((symbol-function 'magit-process-sentinel) #'ignore)
               ((symbol-function 'magit-call-git)
                (lambda (&rest args) (push args git-calls) 0))
               ((symbol-function 'magit-status-setup-buffer)
                (lambda (_dir) (cl-incf status-opened) (current-buffer)))
               ((symbol-function 'message)
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args) messages))))
        ,@body)))

(ert-deftest deb-packaging-test-propagate/clone-confirms-before-deleting-work-branch ()
  "Re-running clone asks before force-deleting the existing work branch."
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (deb-packaging-test-propagate--with-clone-mocks (list t t)
      (deb-packaging-propagate-clone)
      (deb-packaging-test-propagate--wait-for-exit async-proc)
      (should (cl-some (lambda (p) (string-match-p "Delete existing work branch" p))
                       prompts))
      (should (cl-some (lambda (c) (equal c '("branch" "-D" "ignored")))
                       git-calls))
      (should (= status-opened 1)))))

(ert-deftest deb-packaging-test-propagate/clone-declining-branch-delete-aborts ()
  "Declining the branch deletion keeps the branch and stops the run."
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (deb-packaging-test-propagate--with-clone-mocks (list t nil)
      (deb-packaging-propagate-clone)
      (deb-packaging-test-propagate--wait-for-exit async-proc)
      (should-not (cl-some (lambda (c) (equal (car c) "branch")) git-calls))
      (should (= status-opened 0))
      (should (cl-some (lambda (m) (string-match-p "Aborted" m)) messages)))))

(ert-deftest deb-packaging-test-propagate/clone-declining-reset-aborts-quietly ()
  "Saying no to the fetch/reset aborts with a message, not an error."
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (deb-packaging-test-propagate--with-clone-mocks (list nil)
      (deb-packaging-propagate-clone)
      (should (null git-calls))
      (should (null async-calls))
      (should (= status-opened 0))
      (should (cl-some (lambda (m) (string-match-p "Aborted" m)) messages)))))

;;; Clone minor mode keys

(ert-deftest deb-packaging-test-propagate/clone-mode-keymap-frees-magit-push ()
  "The clone minor mode must not take P from `magit-push'."
  (should (null (lookup-key deb-packaging-propagate-clone-mode-map "P")))
  (should (eq (lookup-key deb-packaging-propagate-clone-mode-map (kbd "C-c a"))
              #'deb-packaging-propagate-apply)))

(ert-deftest deb-packaging-test-propagate/clone-mode-header-advertises-key ()
  (with-temp-buffer
    (deb-packaging-propagate-clone-mode 1)
    (should (string-match-p "C-c a" (format "%s" header-line-format)))
    (deb-packaging-propagate-clone-mode -1)))

;;; Prompt and completion quality

(ert-deftest deb-packaging-test-propagate/clone-prompts-pass-real-defaults ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3" :vcs-git "https://salsa/foo.git")
    (let (rs-calls cr-calls (async-proc nil) (async-calls nil))
      (cl-letf (((symbol-function 'deb-packaging-propagate--clone-dir)
                 (lambda (name)
                   (expand-file-name (concat name "-clone")
                                     temporary-file-directory)))
                ((symbol-function 'deb-packaging-propagate--clone-exists-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'deb-packaging-propagate--default-branch)
                 (lambda (&rest _) "main"))
                ((symbol-function 'deb-packaging-propagate--remote-branches)
                 (lambda (&rest _) '("origin/main")))
                ((symbol-function 'deb-packaging-propagate--git-quiet)
                 (lambda (&rest _) nil))
                ((symbol-function 'deb-packaging-propagate--salsa-personal-url)
                 (lambda (&rest _) nil))
                ((symbol-function 'read-string)
                 (lambda (&rest args) (push args rs-calls) (nth 3 args)))
                ((symbol-function 'magit-completing-read)
                 (lambda (&rest args) (push args cr-calls) "main"))
                ((symbol-function 'magit-run-git-async)
                 (lambda (&rest args)
                   (push args async-calls)
                   (setq async-proc (start-process "deb-prop-test" nil "true")
                         magit-this-process async-proc)))
                ((symbol-function 'magit-process-sentinel) #'ignore)
                ((symbol-function 'magit-call-git) (lambda (&rest _) 0))
                ((symbol-function 'magit-status-setup-buffer)
                 (lambda (&rest _) (current-buffer)))
                ((symbol-function 'message) #'ignore))
        (deb-packaging-propagate-clone)
        ;; Fresh-clone path: one async network call with URL and target.
        (should (equal (car async-calls)
                       (list "clone" "https://salsa/foo.git" "foo-clone")))
        (deb-packaging-test-propagate--wait-for-exit async-proc)
        (let ((url-call (cadr rs-calls))
              (branch-call (car rs-calls))
              (base-call (car cr-calls)))
          (should (null (nth 1 url-call)))
          (should (equal (nth 3 url-call) "https://salsa/foo.git"))
          (should (null (nth 1 branch-call)))
          (should (equal (nth 3 branch-call) "wip/propagate-foo"))
          (should (null (nth 4 base-call)))
          (should (equal (nth 6 base-call) "main")))))))

(ert-deftest deb-packaging-test-propagate/patch-choices-clean-names-with-applied-flag ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :patches '(("fix-a.patch" . "x") ("fix-b.patch" . "y")))
    (cl-letf (((symbol-function 'deb-packaging-propagate--patch-applied-p)
               (lambda (path _clone) (string-suffix-p "a.patch" path))))
      (let ((choices (deb-packaging-propagate--patch-choices "clone")))
        (should (equal (mapcar #'car choices)
                       '("fix-a.patch" "fix-b.patch")))
        (should (plist-get (cdr (assoc "fix-a.patch" choices)) :applied))
        (should-not (plist-get (cdr (assoc "fix-b.patch" choices)) :applied))))))

(ert-deftest deb-packaging-test-propagate/annotated-table-marks-applied ()
  (let* ((choices '(("a" . (:applied t)) ("b" . (:applied nil))))
         (table (deb-packaging-propagate--annotated-table choices))
         (meta (funcall table "" nil 'metadata))
         (annot (cdr (assq 'annotation-function meta))))
    (should (equal (all-completions "" table) '("a" "b")))
    (should (equal (funcall annot "a") " ✓ applied"))
    (should (null (funcall annot "b")))))

(ert-deftest deb-packaging-test-propagate/read-range-offers-refs-and-validates ()
  (let (seen-collection)
    (cl-letf (((symbol-function 'deb-packaging-propagate--git-quiet)
               (lambda (_dir &rest args)
                 (when (equal (car args) "for-each-ref")
                   "main\nfeature-x")))
              ((symbol-function 'deb-packaging-propagate--git-ok-p)
               (lambda (&rest _) t))
              ((symbol-function 'completing-read)
               (lambda (_p coll &rest _)
                 (setq seen-collection coll) "HEAD~3..HEAD")))
      (let ((item (deb-packaging-propagate--read-range "/src")))
        (should (member "HEAD" seen-collection))
        (should (member "main" seen-collection))
        (should (member "feature-x" seen-collection))
        (should (equal (plist-get item :range) "HEAD~3..HEAD"))))))

(ert-deftest deb-packaging-test-propagate/read-range-invalid-errors ()
  (cl-letf (((symbol-function 'deb-packaging-propagate--git-quiet)
             (lambda (&rest _) ""))
            ((symbol-function 'deb-packaging-propagate--git-ok-p)
             (lambda (&rest _) nil))
            ((symbol-function 'completing-read)
             (lambda (&rest _) "bogus..ref")))
    (should-error (deb-packaging-propagate--read-range "/src")
                  :type 'user-error)))

(ert-deftest deb-packaging-test-propagate/read-commit-accepts-arbitrary-ref ()
  (cl-letf (((symbol-function 'deb-packaging-propagate--git-quiet)
             (lambda (_dir &rest args)
               (cond
                ((equal args '("log" "--oneline" "-20")) "abc123 recent one")
                ((equal (car args) "rev-parse") "def4567890")
                ((equal (car args) "log") "Older subject"))))
            ((symbol-function 'deb-packaging-propagate--git-ok-p)
             (lambda (&rest _) t))
            ((symbol-function 'completing-read)
             (lambda (&rest _) "HEAD~30")))
    (let ((item (deb-packaging-propagate--read-commit-one "/src" nil)))
      (should (equal (plist-get item :ref) "def4567890"))
      (should (equal (plist-get item :subject) "Older subject")))))

(ert-deftest deb-packaging-test-propagate/read-commit-invalid-ref-errors ()
  (cl-letf (((symbol-function 'deb-packaging-propagate--git-quiet)
             (lambda (_dir &rest args)
               (when (equal args '("log" "--oneline" "-20"))
                 "abc123 recent one")))
            ((symbol-function 'deb-packaging-propagate--git-ok-p)
             (lambda (&rest _) nil))
            ((symbol-function 'completing-read) (lambda (&rest _) "bogus")))
    (should-error (deb-packaging-propagate--read-commit-one "/src" nil)
                  :type 'user-error)))

;;; Clone robustness

(ert-deftest deb-packaging-test-propagate/clone-outside-package-errors ()
  "Outside a package tree the clone must error before any prompt; the
clone dir would otherwise be .../debian/nil and the stored source-dir
the literal string \"nil\"."
  (let ((default-directory (file-name-as-directory
                            (make-temp-file "deb-prop-test-" t))))
    (unwind-protect
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) (error "must not prompt"))))
          (should-error (deb-packaging-propagate-clone) :type 'user-error))
      (delete-directory default-directory t))))

(ert-deftest deb-packaging-test-propagate/clone-checkout-failure-errors ()
  "A failed checkout in the reset path must not silently continue.
The continuation runs from the sentinel, so the sentinel itself is
invoked manually here to observe the `user-error'."
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (deb-packaging-test-propagate--with-clone-mocks (list t)
      (let (sentinel)
        (cl-letf (((symbol-function 'magit-call-git)
                   (lambda (&rest args)
                     (if (equal (car args) "checkout") 1 0)))
                  ((symbol-function 'set-process-sentinel)
                   (lambda (_proc s) (setq sentinel s))))
          (deb-packaging-propagate-clone)
          (should async-proc)
          (deb-packaging-test-propagate--wait-for-exit async-proc)
          (should sentinel)
          (should-error (funcall sentinel async-proc "finished\n")
                        :type 'user-error)
          ;; Nothing past the failed checkout: no branch creation, no
          ;; source-dir config, no status handoff.
          (should-not (cl-some (lambda (c) (equal c '("checkout" "-b" "ignored")))
                               git-calls))
          (should-not (cl-some (lambda (c) (equal (car c) "config")) git-calls))
          (should (= status-opened 0)))))))

(ert-deftest deb-packaging-test-propagate/clone-network-failure-skips-setup ()
  "A failed network step skips the continuation and messages."
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (deb-packaging-test-propagate--with-clone-mocks (list t)
      (cl-letf (((symbol-function 'magit-run-git-async)
                 (lambda (&rest args)
                   (push args async-calls)
                   (let ((default-directory temporary-file-directory))
                     (setq async-proc (start-process "deb-prop-test" nil "false")
                           magit-this-process async-proc)))))
        (deb-packaging-propagate-clone)
        (deb-packaging-test-propagate--wait-for-exit async-proc)
        (should (null git-calls))
        (should (= status-opened 0))
        (should (cl-some (lambda (m) (string-match-p "failed" m)) messages))))))

(ert-deftest deb-packaging-test-propagate/clone-offers-to-browse-fork-page ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3")
    (let (browsed)
      (deb-packaging-test-propagate--with-clone-mocks (list t t)
        (cl-letf (((symbol-function 'deb-packaging-propagate--salsa-personal-url)
                   (lambda (&rest _) "https://salsa.debian.org/u/foo.git"))
                  ((symbol-function 'deb-packaging-propagate--fork-exists-p)
                   (lambda (&rest _) nil))
                  ((symbol-function 'deb-packaging-propagate--fork-url)
                   (lambda (&rest _) "https://salsa.debian.org/debian/foo/-/forks/new"))
                  ((symbol-function 'browse-url)
                   (lambda (url &rest _) (setq browsed url)))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (deb-packaging-propagate-clone)
          (deb-packaging-test-propagate--wait-for-exit async-proc)
          (should (equal browsed
                         "https://salsa.debian.org/debian/foo/-/forks/new")))))))

(ert-deftest deb-packaging-test-propagate/export-displays-patch-without-switching ()
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :patches '(("fix.patch" . "--- a/src/f.c\n+++ b/src/f.c\n@@ -1 +1 @@\n-old\n+new\n")))
    (let ((output (expand-file-name "out.patch" temporary-file-directory))
          (displayed nil)
          (switched nil))
      (unwind-protect
          (cl-letf (((symbol-function 'find-file-read-only)
                     (lambda (&rest _) (setq switched t)))
                    ((symbol-function 'display-buffer)
                     (lambda (buf &rest _) (setq displayed buf))))
            (deb-packaging-propagate-export-patch
             (list (list :type 'patch :name "fix.patch"
                         :path (expand-file-name "debian/patches/fix.patch"
                                                 pkg-dir)))
             output)
            (should-not switched)
            (should (buffer-live-p displayed))
            (should (equal (buffer-local-value 'buffer-file-name displayed)
                           output))
            (kill-buffer displayed))
        (when (file-exists-p output)
          (delete-file output))))))

;;; Apply-path normalization

(ert-deftest deb-packaging-test-propagate/apply-normalizes-quilt-paths ()
  "Quilt patches are normalized to a/ b/ prefixes for `git apply', as on export."
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3"
            :patches '(("fix.patch" . "--- src/f.c\n+++ src/f.c\n@@ -1 +1 @@\n-old\n+new\n")))
    (let* ((item (list :type 'patch :name "fix.patch"
                       :path (expand-file-name "debian/patches/fix.patch"
                                               pkg-dir)))
           (produced (deb-packaging-propagate--produce-patch-file item)))
      (unwind-protect
          (with-temp-buffer
            (insert-file-contents produced)
            (should (string-match-p "^--- a/src/f.c" (buffer-string)))
            (should (string-match-p "^\\+\\+\\+ b/src/f.c" (buffer-string))))
        (unless (equal produced (plist-get item :path))
          (delete-file produced))))))

;;; Stale pending patch

(ert-deftest deb-packaging-test-propagate/apply-quit-clears-pending-patch ()
  (let ((patch (make-temp-file "propagate-test-" nil ".patch")))
    (cl-letf (((symbol-function 'magit-toplevel) (lambda () "/repo"))
              ((symbol-function 'deb-packaging-propagate--read-fix-source-one)
               (lambda (&rest _) (list :type 'patch :name "p")))
              ((symbol-function 'deb-packaging-propagate--produce-patch-file)
               (lambda (&rest _) patch))
              ((symbol-function 'call-interactively) (lambda (&rest _) nil))
              ((symbol-function 'message) #'ignore)
              (transient-post-exit-hook nil))
      (with-temp-buffer
        (deb-packaging-propagate-apply)
        (should (equal deb-packaging-propagate--pending-patch patch))
        (run-hooks 'transient-post-exit-hook)
        (should-not (file-exists-p patch))
        (should (null deb-packaging-propagate--pending-patch))
        (should (null transient-post-exit-hook))))))

;;; Header-line preservation

(ert-deftest deb-packaging-test-propagate/clone-mode-restores-header-line ()
  (with-temp-buffer
    (setq header-line-format "preserved")
    (deb-packaging-propagate-clone-mode 1)
    (should (string-match-p "C-c a" (format "%s" header-line-format)))
    (deb-packaging-propagate-clone-mode -1)
    (should (equal header-line-format "preserved"))))

(ert-deftest deb-packaging-test-propagate/do-apply-deletes-temp-patch ()
  (let ((file (make-temp-file "propagate-test-" nil ".patch" "diff")))
    (cl-letf (((symbol-function 'magit-run-git) (lambda (&rest _) t)))
      (with-temp-buffer
        (setq deb-packaging-propagate--pending-patch file)
        (deb-packaging-propagate-do-apply '("--index"))
        (should-not (file-exists-p file))))))

(provide 'deb-packaging-test-propagate)
;;; deb-packaging-test-propagate.el ends here
