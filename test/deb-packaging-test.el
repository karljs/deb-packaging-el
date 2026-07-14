;;; deb-packaging-test.el --- Shared test helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Keywords: tools, debian, ubuntu, packaging

;;; Commentary:

;; Shared fixtures for the deb-packaging ERT suite. Defines no tests.
;; Macros:
;;   `deb-packaging-test--with-package-tree'   throwaway Debian source tree
;;   `deb-packaging-test--with-mocked-process' canned call-process/shell output
;;   `deb-packaging-test--with-temp-git-repo'  throwaway git repo

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'deb-packaging-detect)
(require 'deb-packaging-config)
(require 'deb-packaging-commands)

;;; Filesystem fixtures

(defun deb-packaging-test--write-file (path content)
  "Write CONTENT to PATH, creating parent directories as needed."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert content)))

(defun deb-packaging-test--changelog (name version distro)
  "Return a debian/changelog first stanza for NAME/VERSION/DISTRO."
  (format "%s (%s) %s; urgency=medium

  * Test entry.

 -- Test Maintainer <test@example.com>  Mon, 01 Jan 2024 00:00:00 +0000
"
          name version distro))

(defun deb-packaging-test--control (name bin-names maintainer vcs-git homepage)
  "Return debian/control text.
NAME is the source package; BIN-NAMES a list of binary packages.
MAINTAINER, VCS-GIT, and HOMEPAGE are optional field values."
  (concat
   (format "Source: %s\n" name)
   (format "Maintainer: %s\n" (or maintainer "Test Maintainer <test@example.com>"))
   "Section: devel\n"
   "Priority: optional\n"
   (when vcs-git (format "Vcs-Git: %s\n" vcs-git))
   (when homepage (format "Homepage: %s\n" homepage))
   "Standards-Version: 4.6.2\n"
   (mapconcat
    (lambda (bn)
      (format "\nPackage: %s\nArchitecture: any\nDepends: ${misc:Depends}\nDescription: test binary %s\n .\n"
              bn bn))
    (or bin-names (list name))
    "")))

(defmacro deb-packaging-test--with-package-tree (spec &rest body)
  "Create a temp Debian package tree per SPEC, run BODY, then clean up.

SPEC is a plist (evaluated) with keys:
  :name          source package name (required)
  :version       full version string (required)
  :distro        target distribution (default \"noble\")
  :bin-names     list of binary package names (default (list :name))
  :source-format source format string, e.g. \"3.0 (quilt)\"
  :patches       alist of (PATCH-NAME . CONTENT) for debian/patches/
  :series        explicit series file lines (list of strings); when nil
                 and :patches is set, the series lists patches in order
  :artifacts     alist of (FILENAME . CONTENT) placed in the parent dir
  :maintainer    Maintainer field value
  :vcs-git       Vcs-Git field value
  :homepage      Homepage field value
  :watch         debian/watch file content

Within BODY these locals are bound:
  `pkg-dir'         absolute path to the package directory (has debian/)
  `pkg-parent-dir'  absolute path to the parent (build-output) directory

`default-directory' is bound to `pkg-dir'."
  (declare (indent 1) (debug (form body)))
  (let ((root (make-symbol "root"))
        (s (make-symbol "spec")))
    `(let* ((,s ,spec)
            (,root (make-temp-file "deb-pkg-test-" t))
            (pkg-parent-dir (file-name-as-directory ,root))
            (pkg-dir (file-name-as-directory
                      (expand-file-name (plist-get ,s :name) ,root))))
       (unwind-protect
           (let ((default-directory pkg-dir))
             (deb-packaging-test--build-tree pkg-dir pkg-parent-dir ,s)
             ,@body)
         (delete-directory ,root t)))))

(defun deb-packaging-test--build-tree (pkg-dir parent-dir spec)
  "Materialize the fixture tree for SPEC under PKG-DIR and PARENT-DIR."
  (let ((name (plist-get spec :name))
        (version (plist-get spec :version))
        (distro (or (plist-get spec :distro) "noble"))
        (bin-names (plist-get spec :bin-names))
        (source-format (plist-get spec :source-format))
        (patches (plist-get spec :patches))
        (series (plist-get spec :series))
        (artifacts (plist-get spec :artifacts))
        (maintainer (plist-get spec :maintainer))
        (vcs-git (plist-get spec :vcs-git))
        (homepage (plist-get spec :homepage))
        (watch (plist-get spec :watch)))
    (make-directory pkg-dir t)
    (deb-packaging-test--write-file
     (expand-file-name "debian/changelog" pkg-dir)
     (deb-packaging-test--changelog name version distro))
    (deb-packaging-test--write-file
     (expand-file-name "debian/control" pkg-dir)
     (deb-packaging-test--control name bin-names maintainer vcs-git homepage))
    (when source-format
      (deb-packaging-test--write-file
       (expand-file-name "debian/source/format" pkg-dir)
       (concat source-format "\n")))
    (when watch
      (deb-packaging-test--write-file
       (expand-file-name "debian/watch" pkg-dir) watch))
    (when (or patches series)
      (deb-packaging-test--write-file
       (expand-file-name "debian/patches/series" pkg-dir)
       (concat (mapconcat #'identity
                          (or series (mapcar #'car patches))
                          "\n")
               "\n"))
      (dolist (p patches)
        (deb-packaging-test--write-file
         (expand-file-name (concat "debian/patches/" (car p)) pkg-dir)
         (cdr p))))
    (dolist (a artifacts)
      (deb-packaging-test--write-file
       (expand-file-name (car a) parent-dir) (or (cdr a) "")))))

;;; Process mocking

(defun deb-packaging-test--match-response (key responses)
  "Return the response for KEY from RESPONSES, or nil.
RESPONSES is an alist whose keys are matched against KEY (a string) by
`string-prefix-p'; the first match wins."
  (cdr (cl-find-if (lambda (pair) (string-prefix-p (car pair) key))
                   responses)))

(defmacro deb-packaging-test--with-mocked-process (responses &rest body)
  "Run BODY with `call-process' and `shell-command-to-string' mocked.
RESPONSES (evaluated) is an alist of (MATCH . RESPONSE).  MATCH is
prefix-compared against PROGRAM (call-process) or the command string
\(shell-command-to-string).  RESPONSE is a string (stdout, exit 0), an
integer (exit code, no stdout), or a cons (CODE . STRING).  An unmatched
call signals an error so tests fail loudly on unexpected external calls."
  (declare (indent 1) (debug (form body)))
  (let ((rs (make-symbol "responses")))
    `(let ((,rs ,responses))
       (cl-letf (((symbol-function 'call-process)
                  (lambda (program &optional _infile destination _display
                                   &rest args)
                    (deb-packaging-test--mock-call-process
                     ,rs program destination args)))
                 ((symbol-function 'shell-command-to-string)
                  (lambda (command)
                    (deb-packaging-test--mock-shell-command ,rs command))))
         ,@body))))

(defun deb-packaging-test--mock-call-process (responses program destination args)
  "Mock body for `call-process' using RESPONSES.
PROGRAM, DESTINATION, and ARGS are as passed to `call-process'."
  (let ((resp (deb-packaging-test--match-response program responses)))
    (when (null resp)
      (error "deb-packaging-test: unexpected call-process %S %S" program args))
    (let ((code (cond ((integerp resp) resp)
                      ((consp resp) (car resp))
                      (t 0)))
          (out (cond ((stringp resp) resp)
                     ((consp resp) (cdr resp))
                     (t nil))))
      ;; Insert stdout into the destination buffer when one is requested.
      (when (and out destination (not (eq destination 0)))
        (let ((buf (cond ((eq destination t) (current-buffer))
                         ((bufferp destination) destination)
                         ((and (consp destination) (bufferp (car destination)))
                          (car destination))
                         (t nil))))
          (when buf
            (with-current-buffer buf (insert out)))))
      code)))

(defun deb-packaging-test--mock-shell-command (responses command)
  "Mock body for `shell-command-to-string' using RESPONSES and COMMAND."
  (let ((resp (deb-packaging-test--match-response command responses)))
    (when (null resp)
      (error "deb-packaging-test: unexpected shell-command-to-string %S"
             command))
    (cond ((stringp resp) resp)
          ((consp resp) (cdr resp))
          (t ""))))

;;; Git fixtures

(defun deb-packaging-test--git (dir &rest args)
  "Run git in DIR with ARGS synchronously, erroring on non-zero exit."
  (let ((default-directory (file-name-as-directory dir)))
    (let ((code (apply #'call-process "git" nil nil nil args)))
      (unless (zerop code)
        (error "git %S failed with code %d in %s" args code dir)))))

(defmacro deb-packaging-test--with-temp-git-repo (&rest body)
  "Create a temp git repo, run BODY with `default-directory' bound to it.

The repo is initialized with a deterministic identity and an initial
commit on branch `main'.  Within BODY the local `repo-dir' is bound to
the repo's absolute path.  Cleans up on exit."
  (declare (indent 0) (debug (body)))
  (let ((root (make-symbol "root")))
    `(let* ((,root (make-temp-file "deb-git-test-" t))
            (repo-dir (file-name-as-directory ,root)))
       (unwind-protect
           (let ((default-directory repo-dir)
                 (process-environment
                  (append '("GIT_CONFIG_GLOBAL=/dev/null"
                            "GIT_CONFIG_SYSTEM=/dev/null"
                            "GIT_AUTHOR_NAME=Test"
                            "GIT_AUTHOR_EMAIL=test@example.com"
                            "GIT_COMMITTER_NAME=Test"
                            "GIT_COMMITTER_EMAIL=test@example.com")
                          process-environment)))
             (deb-packaging-test--git repo-dir "init" "-q" "-b" "main")
             (deb-packaging-test--write-file
              (expand-file-name "README" repo-dir) "init\n")
             (deb-packaging-test--git repo-dir "add" "-A")
             (deb-packaging-test--git repo-dir "commit" "-q" "-m" "initial")
             ,@body)
         (delete-directory ,root t)))))

(provide 'deb-packaging-test)
;;; deb-packaging-test.el ends here
