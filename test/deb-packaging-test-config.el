;;; deb-packaging-test-config.el --- Distro derivation tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for `deb-packaging-config--effective-distro': the changelog
;; is the single distro source; outside a tree the fallback applies.

;;; Code:

(require 'ert)
(require 'deb-packaging-test)
(require 'deb-packaging-config)

(ert-deftest deb-packaging-test-config/effective-distro-from-changelog ()
  "Inside a package tree the changelog distro rules, whatever the
fallback is set to."
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3" :distro "oracular")
    (let ((deb-packaging-config-default-distro "noble"))
      (should (string= (deb-packaging-config--effective-distro) "oracular")))))

(ert-deftest deb-packaging-test-config/effective-distro-fallback-outside-tree ()
  "Outside a package tree the fallback variable applies."
  (let ((tmp (make-temp-file "deb-config-test-" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory tmp))
              (deb-packaging-config-default-distro "jammy"))
          (should (string= (deb-packaging-config--effective-distro) "jammy")))
      (delete-directory tmp t))))

(ert-deftest deb-packaging-test-config/effective-distro-tracks-changelog-edits ()
  "Editing the changelog distro is reflected on the next call; no
cached global can disagree with it."
  (deb-packaging-test--with-package-tree
      (list :name "foo" :version "1.2-3" :distro "noble")
    (should (string= (deb-packaging-config--effective-distro) "noble"))
    (deb-packaging-test--write-file
     (expand-file-name "debian/changelog" pkg-dir)
     (deb-packaging-test--changelog "foo" "1.2-3" "questing"))
    (should (string= (deb-packaging-config--effective-distro) "questing"))))

(ert-deftest deb-packaging-test-config/default-distro-is-user-tunable ()
  (should (stringp deb-packaging-config-default-distro)))

(provide 'deb-packaging-test-config)
;;; deb-packaging-test-config.el ends here
