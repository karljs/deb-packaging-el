;;; deb-packaging-test-config.el --- Distro state tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for target-distro seeding and selection in
;; deb-packaging-config.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'deb-packaging-test)
(require 'deb-packaging-config)

(ert-deftest deb-packaging-test-config/maybe-seed-nil-unchanged ()
  "`deb-packaging--maybe-seed-distro' with nil leaves state untouched."
  (let ((deb-packaging-target-distro "noble")
        (deb-packaging--distro-user-set nil))
    (should (null (deb-packaging--maybe-seed-distro nil)))
    (should (string= deb-packaging-target-distro "noble"))
    (should (null deb-packaging--distro-user-set))))

(ert-deftest deb-packaging-test-config/maybe-seed-empty-unchanged ()
  "`deb-packaging--maybe-seed-distro' with empty string leaves state untouched."
  (let ((deb-packaging-target-distro "noble")
        (deb-packaging--distro-user-set nil))
    (should (null (deb-packaging--maybe-seed-distro "")))
    (should (string= deb-packaging-target-distro "noble"))
    (should (null deb-packaging--distro-user-set))))

(ert-deftest deb-packaging-test-config/maybe-seed-respects-user-set ()
  "`deb-packaging--maybe-seed-distro' is a no-op when the user already set distro."
  (let ((deb-packaging-target-distro "focal")
        (deb-packaging--distro-user-set t))
    (should (null (deb-packaging--maybe-seed-distro "jammy")))
    (should (string= deb-packaging-target-distro "focal"))
    (should deb-packaging--distro-user-set)))

(ert-deftest deb-packaging-test-config/maybe-seed-fresh-seeds ()
  "`deb-packaging--maybe-seed-distro' seeds value and flag from a fresh state."
  (let ((deb-packaging-target-distro "noble")
        (deb-packaging--distro-user-set nil))
    (should (string= (deb-packaging--maybe-seed-distro "jammy") "jammy"))
    (should (string= deb-packaging-target-distro "jammy"))
    (should deb-packaging--distro-user-set)))

(ert-deftest deb-packaging-test-config/set-distro-from-fresh ()
  "`deb-packaging--set-distro' sets value and flag from a fresh state."
  (let ((deb-packaging-target-distro "noble")
        (deb-packaging--distro-user-set nil))
    (should (string= (deb-packaging--set-distro "oracular") "oracular"))
    (should (string= deb-packaging-target-distro "oracular"))
    (should deb-packaging--distro-user-set)))

(ert-deftest deb-packaging-test-config/set-distro-overwrites ()
  "`deb-packaging--set-distro' overwrites an already user-set distro."
  (let ((deb-packaging-target-distro "jammy")
        (deb-packaging--distro-user-set t))
    (should (string= (deb-packaging--set-distro "sid") "sid"))
    (should (string= deb-packaging-target-distro "sid"))
    (should deb-packaging--distro-user-set)))

(ert-deftest deb-packaging-test-config/effective-distro-user-set-unchanged ()
  "`deb-packaging--effective-distro' keeps the user-set value; scan is ignored."
  (let ((deb-packaging-target-distro "focal")
        (deb-packaging--distro-user-set t))
    (cl-letf (((symbol-function 'deb-packaging--scan-context)
               (lambda (&rest _) '(:distro "jammy"))))
      (should (string= (deb-packaging--effective-distro) "focal"))
      (should (string= deb-packaging-target-distro "focal"))
      (should deb-packaging--distro-user-set))))

(ert-deftest deb-packaging-test-config/effective-distro-seeds-from-scan ()
  "`deb-packaging--effective-distro' seeds from `deb-packaging--scan-context'."
  (let ((deb-packaging-target-distro "noble")
        (deb-packaging--distro-user-set nil))
    (cl-letf (((symbol-function 'deb-packaging--scan-context)
               (lambda (&rest _) '(:distro "jammy"))))
      (should (string= (deb-packaging--effective-distro) "jammy"))
      (should (string= deb-packaging-target-distro "jammy"))
      (should deb-packaging--distro-user-set))))

(ert-deftest deb-packaging-test-config/effective-distro-falls-back-to-default ()
  "`deb-packaging--effective-distro' falls back to default when scan returns nil."
  (let ((deb-packaging-target-distro "noble")
        (deb-packaging--distro-user-set nil))
    (cl-letf (((symbol-function 'deb-packaging--scan-context)
               (lambda (&rest _) nil)))
      (should (string= (deb-packaging--effective-distro) "noble"))
      (should (string= deb-packaging-target-distro "noble"))
      (should (null deb-packaging--distro-user-set)))))

(ert-deftest deb-packaging-test-config/distro-choices-known-member ()
  "`deb-packaging--distro-choices' returns the standard list for a known distro."
  (let ((deb-packaging-target-distro "noble")
        (deb-packaging--distro-user-set nil))
    (cl-letf (((symbol-function 'deb-packaging--effective-distro)
               (lambda (&rest _) "noble")))
      (let ((choices (deb-packaging--distro-choices)))
        (should (equal choices (append deb-packaging-ubuntu-distros
                                       deb-packaging-debian-distros)))
        (should (member "noble" choices))
        (should (member "jammy" choices))
        (should (member "sid" choices))))))

(ert-deftest deb-packaging-test-config/distro-choices-unknown-prepended ()
  "`deb-packaging--distro-choices' prepends an unknown current distro."
  (let ((deb-packaging-target-distro "noble")
        (deb-packaging--distro-user-set nil))
    (cl-letf (((symbol-function 'deb-packaging--effective-distro)
               (lambda (&rest _) "experimental-xyz")))
      (let ((choices (deb-packaging--distro-choices)))
        (should (string= (car choices) "experimental-xyz"))
        (should (member "noble" choices))
        (should (member "jammy" choices))
        (should (member "sid" choices))
        (should (equal (cdr choices) (append deb-packaging-ubuntu-distros
                                             deb-packaging-debian-distros)))))))

(ert-deftest deb-packaging-test-config/constants-contain-expected-distros ()
  "Distro constants include the expected Ubuntu and Debian entries."
  (should (member "noble" deb-packaging-ubuntu-distros))
  (should (member "jammy" deb-packaging-ubuntu-distros))
  (should (member "sid" deb-packaging-debian-distros)))

(provide 'deb-packaging-test-config)
;;; deb-packaging-test-config.el ends here
