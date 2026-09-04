;;; deb-packaging-test-update.el --- New upstream update tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Keywords: tools, debian, ubuntu, packaging

;;; Commentary:

;; ERT tests for the new-upstream-version workflow in
;; deb-packaging-update.el: method detection, orig-tarball scanning,
;; target-version choice, and command construction.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'deb-packaging-test)
(require 'deb-packaging-update)

(defun deb-packaging-test-update--git (pkg-dir &rest args)
  "Run git in PKG-DIR with a fixed identity."
  (let ((process-environment
         (append '("GIT_CONFIG_GLOBAL=/dev/null"
                   "GIT_CONFIG_SYSTEM=/dev/null"
                   "GIT_AUTHOR_NAME=Test"
                   "GIT_AUTHOR_EMAIL=test@example.com"
                   "GIT_COMMITTER_NAME=Test"
                   "GIT_COMMITTER_EMAIL=test@example.com")
                 process-environment)))
    (apply #'deb-packaging-test--git pkg-dir args)))

(defun deb-packaging-test-update--init-repo (pkg-dir)
  "Initialize a committed git repository in PKG-DIR."
  (deb-packaging-test-update--git pkg-dir "init" "-q" "-b" "main")
  (deb-packaging-test-update--git pkg-dir "add" "-A")
  (deb-packaging-test-update--git pkg-dir "commit" "-q" "-m" "initial"))

;;; Method detection

(ert-deftest deb-packaging-test-update/default-method-no-git ()
  (deb-packaging-test--with-package-tree
      '(:name "foo" :version "1.0-1")
    (should (eq (deb-packaging-update--default-method pkg-dir) 'uupdate))))

(ert-deftest deb-packaging-test-update/default-method-git-without-gbp-signals ()
  (deb-packaging-test--with-package-tree
      '(:name "foo" :version "1.0-1")
    (deb-packaging-test-update--init-repo pkg-dir)
    (should (eq (deb-packaging-update--default-method pkg-dir) 'uupdate))))

(ert-deftest deb-packaging-test-update/default-method-gbp-conf ()
  (deb-packaging-test--with-package-tree
      '(:name "foo" :version "1.0-1")
    (deb-packaging-test-update--init-repo pkg-dir)
    (deb-packaging-test--write-file
     (expand-file-name "debian/gbp.conf" pkg-dir) "[DEFAULT]\n")
    (should (eq (deb-packaging-update--default-method pkg-dir) 'gbp))))

(ert-deftest deb-packaging-test-update/default-method-pristine-tar-branch ()
  (deb-packaging-test--with-package-tree
      '(:name "foo" :version "1.0-1")
    (deb-packaging-test-update--init-repo pkg-dir)
    (deb-packaging-test-update--git pkg-dir "branch" "pristine-tar")
    (should (eq (deb-packaging-update--default-method pkg-dir) 'gbp))
    (should (string= (deb-packaging-update--gbp-signal pkg-dir)
                     "pristine-tar branch"))))

(ert-deftest deb-packaging-test-update/default-method-upstream-branch ()
  (deb-packaging-test--with-package-tree
      '(:name "foo" :version "1.0-1")
    (deb-packaging-test-update--init-repo pkg-dir)
    (deb-packaging-test-update--git pkg-dir "branch" "upstream")
    (should (eq (deb-packaging-update--default-method pkg-dir) 'gbp))))

(ert-deftest deb-packaging-test-update/default-method-upstream-tag ()
  (deb-packaging-test--with-package-tree
      '(:name "foo" :version "1.0-1")
    (deb-packaging-test-update--init-repo pkg-dir)
    (deb-packaging-test-update--git pkg-dir "tag" "upstream/1.0")
    (should (eq (deb-packaging-update--default-method pkg-dir) 'gbp))
    (should (string= (deb-packaging-update--gbp-signal pkg-dir)
                     "upstream/* tags"))))

(ert-deftest deb-packaging-test-update/read-method-defaults-to-detected ()
  (deb-packaging-test--with-package-tree
      '(:name "foo" :version "1.0-1")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll _pred _rm _init _hist default) default)))
      (should (eq (deb-packaging-update--read-method pkg-dir) 'uupdate)))))

;;; Orig tarball scanning

(ert-deftest deb-packaging-test-update/tarball-version ()
  (should (string= (deb-packaging-update--tarball-version
                    "foo" "foo_1.2.4.orig.tar.gz")
                   "1.2.4"))
  (should (string= (deb-packaging-update--tarball-version
                    "foo" "foo_1.2.4.orig.tar.xz")
                   "1.2.4"))
  (should (string= (deb-packaging-update--tarball-version
                    "foo" "foo_1.2.4.orig-bar.tar.xz")
                   "1.2.4"))
  (should (null (deb-packaging-update--tarball-version
                 "foo" "foo-1.2.4.tar.gz")))
  (should (null (deb-packaging-update--tarball-version
                 "foo" "bar_9.9.orig.tar.gz"))))

(ert-deftest deb-packaging-test-update/tarballs ()
  (deb-packaging-test--with-package-tree
      '(:name "foo"
        :version "1.0-1"
        :artifacts (("foo_1.0.orig.tar.gz" . "")
                    ("foo_2.0.orig.tar.xz" . "")
                    ("foo_2.0.orig-bar.tar.xz" . "")
                    ("foo-2.0.tar.gz" . "")
                    ("bar_9.9.orig.tar.gz" . "")))
    (should (equal (deb-packaging-update--tarballs "foo" pkg-parent-dir)
                   '("foo_1.0.orig.tar.gz"
                     "foo_2.0.orig-bar.tar.xz"
                     "foo_2.0.orig.tar.xz")))))

;;; Target version choice

(ert-deftest deb-packaging-test-update/target-version-single-new ()
  (should (string= (deb-packaging-update--target-version
                    "foo" "1.0"
                    '("foo_1.0.orig.tar.gz" "foo_2.0.orig.tar.gz")
                    '("foo_2.0.orig.tar.gz"))
                   "2.0")))

(ert-deftest deb-packaging-test-update/target-version-rerun-no-download ()
  ;; Re-run after a failed uupdate: no new tarball, but the candidate
  ;; is still distinguishable from the current version.
  (should (string= (deb-packaging-update--target-version
                    "foo" "1.0"
                    '("foo_1.0.orig.tar.gz" "foo_2.0.orig.tar.gz")
                    nil)
                   "2.0")))

(ert-deftest deb-packaging-test-update/target-version-none ()
  (should (null (deb-packaging-update--target-version
                 "foo" "1.0" '("foo_1.0.orig.tar.gz") nil))))

(ert-deftest deb-packaging-test-update/target-version-components-collapse ()
  (should (string= (deb-packaging-update--target-version
                    "foo" "1.0"
                    '("foo_2.0.orig.tar.gz" "foo_2.0.orig-bar.tar.gz"))
                   "2.0")))

(ert-deftest deb-packaging-test-update/target-version-prompts-when-ambiguous ()
  (cl-letf (((symbol-function 'completing-read)
             (lambda (_prompt _coll _pred _rm _init _hist default) default)))
    ;; Stale tarball present, nothing downloaded: prompt; the last
    ;; candidate is the default.
    (should (string= (deb-packaging-update--target-version
                      "foo" "1.0"
                      '("foo_1.0.orig.tar.gz"
                        "foo_0.9.orig.tar.gz"
                        "foo_2.0.orig.tar.gz")
                      nil)
                     "2.0"))
    ;; A downloaded version wins the default over stale candidates.
    (should (string= (deb-packaging-update--target-version
                      "foo" "1.0"
                      '("foo_1.0.orig.tar.gz"
                        "foo_0.9.orig.tar.gz"
                        "foo_2.0.orig.tar.gz")
                      '("foo_2.0.orig.tar.gz"))
                     "2.0"))))

;;; Command construction

(ert-deftest deb-packaging-test-update/gbp-command-plain ()
  (deb-packaging-test--with-package-tree
      '(:name "foo" :version "1.0-1")
    (deb-packaging-test-update--init-repo pkg-dir)
    (should (string= (deb-packaging-update--gbp-command pkg-dir)
                     "gbp import-orig --uscan"))))

(ert-deftest deb-packaging-test-update/gbp-command-pristine-tar ()
  (deb-packaging-test--with-package-tree
      '(:name "foo" :version "1.0-1")
    (deb-packaging-test-update--init-repo pkg-dir)
    (deb-packaging-test-update--git pkg-dir "branch" "pristine-tar")
    (should (string= (deb-packaging-update--gbp-command pkg-dir)
                     "gbp import-orig --uscan --pristine-tar"))))

(ert-deftest deb-packaging-test-update/uscan-commands-reroute-exit-1 ()
  ;; uscan exits 1 for "no newer version"; compilation-mode must not
  ;; see that as a failure.
  (dolist (cmd (list deb-packaging-update--check-command
                     deb-packaging-update--download-command))
    (should (string-suffix-p "; rc=$?; [ $rc -le 1 ]" cmd))))

;;; Preflight

(ert-deftest deb-packaging-test-update/pkg-dir-requires-watch ()
  (deb-packaging-test--with-package-tree
      '(:name "foo" :version "1.0-1")
    (let ((default-directory pkg-dir))
      (should-error (deb-packaging-update--pkg-dir) :type 'user-error))))

(ert-deftest deb-packaging-test-update/pkg-dir-accepts-watch ()
  (deb-packaging-test--with-package-tree
      '(:name "foo" :version "1.0-1" :watch "version=4\n")
    (let ((default-directory pkg-dir))
      (should (string= (deb-packaging-update--pkg-dir) pkg-dir)))))

(ert-deftest deb-packaging-test-update/ensure-binaries ()
  (cl-letf (((symbol-function 'executable-find)
             (lambda (bin) (not (string= bin "gbp")))))
    (deb-packaging-update--ensure-binaries '("uscan"))
    (should-error (deb-packaging-update--ensure-binaries '("uscan" "gbp"))
                  :type 'user-error)))

;;; Transient header

(ert-deftest deb-packaging-test-update/transient-header ()
  (deb-packaging-test--with-package-tree
      '(:name "foo" :version "1.0-1")
    (deb-packaging-test-update--init-repo pkg-dir)
    (deb-packaging-test-update--git pkg-dir "branch" "pristine-tar")
    (should (string-match-p
             (regexp-quote
              "foo 1.0-1 (noble)\nDefault method: gbp (pristine-tar branch)")
             (deb-packaging-update--transient-header)))))

(provide 'deb-packaging-test-update)
;;; deb-packaging-test-update.el ends here
