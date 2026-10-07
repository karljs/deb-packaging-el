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

(ert-deftest deb-packaging-test-config/architecture-resolution-order ()
  (deb-packaging-test--with-package-tree
      '(:name "foo" :version "1.2-3" :distro "noble")
    (let* ((tmp (make-temp-file "deb-arch-test-" t))
           (process-environment (cons (format "XDG_CACHE_HOME=%s" tmp)
                                      process-environment))
           (deb-packaging-config-default-architecture "i386"))
      (unwind-protect
          (cl-letf (((symbol-function 'deb-packaging-detect--call-process-string)
                     (lambda (&rest _) "amd64")))
            (should (equal (deb-packaging-config--effective-architecture)
                           "i386"))
            (deb-packaging-config-save-architecture "foo" "noble" "arm64")
            (should (equal (deb-packaging-config--effective-architecture)
                           "arm64")))
        (delete-directory tmp t)))))

(ert-deftest deb-packaging-test-config/architecture-host-and-final-fallback ()
  (let ((tmp (make-temp-file "deb-config-test-" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory tmp))
              (process-environment (cons (format "XDG_CACHE_HOME=%s" tmp)
                                         process-environment))
              (deb-packaging-config-default-architecture nil))
          (cl-letf (((symbol-function 'deb-packaging-detect--call-process-string)
                     (lambda (&rest _) "ppc64el")))
            (should (equal (deb-packaging-config--effective-architecture)
                           "ppc64el")))
          (cl-letf (((symbol-function 'deb-packaging-detect--call-process-string)
                     (lambda (&rest _) nil)))
            (should (equal (deb-packaging-config--effective-architecture)
                           "amd64"))))
      (delete-directory tmp t))))

(ert-deftest deb-packaging-test-config/architecture-rejects-invalid-value ()
  (should-error
   (deb-packaging-config-save-architecture "foo" "noble" "arm64; nope")
   :type 'user-error))

(ert-deftest deb-packaging-test-config/architecture-rejects-invalid-default ()
  (let ((deb-packaging-config-default-architecture "not an arch"))
    (should-error (deb-packaging-config--effective-architecture)
                  :type 'user-error)))

(ert-deftest deb-packaging-test-config/emulation-missing ()
  (let ((tmp (make-temp-file "deb-binfmt-" t)))
    (unwind-protect
        (let ((deb-packaging-config--binfmt-dir (file-name-as-directory tmp)))
          (should-not (deb-packaging-config--emulation-missing "amd64" "amd64"))
          (should-not (deb-packaging-config--emulation-missing "i386" "amd64"))
          (should-not (deb-packaging-config--emulation-missing "armhf" "arm64"))
          (should (deb-packaging-config--emulation-missing "arm64" "amd64"))
          (should (deb-packaging-config--emulation-missing "loong64" "amd64"))
          ;; regression: noble's qemu-user-binfmt registers without F
          (write-region "enabled\ninterpreter /usr/bin/qemu-aarch64\nflags: PO\n"
                        nil (expand-file-name "qemu-aarch64" tmp))
          (should (deb-packaging-config--emulation-missing "arm64" "amd64"))
          (write-region "disabled\ninterpreter /usr/bin/qemu-aarch64\nflags: POF\n"
                        nil (expand-file-name "qemu-aarch64" tmp))
          (should (deb-packaging-config--emulation-missing "arm64" "amd64"))
          (write-region "enabled\ninterpreter /usr/bin/qemu-aarch64\nflags: POF\n"
                        nil (expand-file-name "qemu-aarch64" tmp))
          (should-not (deb-packaging-config--emulation-missing "arm64" "amd64")))
      (delete-directory tmp t))))

(provide 'deb-packaging-test-config)
;;; deb-packaging-test-config.el ends here
