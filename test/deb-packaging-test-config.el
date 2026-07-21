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
  "`deb-packaging-config--maybe-seed-distro' with nil leaves state untouched."
  (let ((deb-packaging-config-target-distro "noble")
        (deb-packaging-config--distro-user-set nil))
    (should (null (deb-packaging-config--maybe-seed-distro nil)))
    (should (string= deb-packaging-config-target-distro "noble"))
    (should (null deb-packaging-config--distro-user-set))))

(ert-deftest deb-packaging-test-config/maybe-seed-empty-unchanged ()
  "`deb-packaging-config--maybe-seed-distro' with empty string leaves state untouched."
  (let ((deb-packaging-config-target-distro "noble")
        (deb-packaging-config--distro-user-set nil))
    (should (null (deb-packaging-config--maybe-seed-distro "")))
    (should (string= deb-packaging-config-target-distro "noble"))
    (should (null deb-packaging-config--distro-user-set))))

(ert-deftest deb-packaging-test-config/maybe-seed-respects-user-set ()
  "`deb-packaging-config--maybe-seed-distro' is a no-op when the user already set distro."
  (let ((deb-packaging-config-target-distro "focal")
        (deb-packaging-config--distro-user-set t))
    (should (null (deb-packaging-config--maybe-seed-distro "jammy")))
    (should (string= deb-packaging-config-target-distro "focal"))
    (should deb-packaging-config--distro-user-set)))

(ert-deftest deb-packaging-test-config/maybe-seed-fresh-seeds ()
  "`deb-packaging-config--maybe-seed-distro' seeds value and flag from a fresh state."
  (let ((deb-packaging-config-target-distro "noble")
        (deb-packaging-config--distro-user-set nil))
    (should (string= (deb-packaging-config--maybe-seed-distro "jammy") "jammy"))
    (should (string= deb-packaging-config-target-distro "jammy"))
    (should deb-packaging-config--distro-user-set)))

(ert-deftest deb-packaging-test-config/set-distro-from-fresh ()
  "`deb-packaging-config--set-distro' sets value and flag from a fresh state."
  (let ((deb-packaging-config-target-distro "noble")
        (deb-packaging-config--distro-user-set nil))
    (should (string= (deb-packaging-config--set-distro "oracular") "oracular"))
    (should (string= deb-packaging-config-target-distro "oracular"))
    (should deb-packaging-config--distro-user-set)))

(ert-deftest deb-packaging-test-config/set-distro-overwrites ()
  "`deb-packaging-config--set-distro' overwrites an already user-set distro."
  (let ((deb-packaging-config-target-distro "jammy")
        (deb-packaging-config--distro-user-set t))
    (should (string= (deb-packaging-config--set-distro "sid") "sid"))
    (should (string= deb-packaging-config-target-distro "sid"))
    (should deb-packaging-config--distro-user-set)))

(ert-deftest deb-packaging-test-config/effective-distro-user-set-unchanged ()
  "`deb-packaging-config--effective-distro' keeps the user-set value; scan is ignored."
  (let ((deb-packaging-config-target-distro "focal")
        (deb-packaging-config--distro-user-set t))
    (cl-letf (((symbol-function 'deb-packaging-detect--scan-context)
               (lambda (&rest _) '(:distro "jammy"))))
      (should (string= (deb-packaging-config--effective-distro) "focal"))
      (should (string= deb-packaging-config-target-distro "focal"))
      (should deb-packaging-config--distro-user-set))))

(ert-deftest deb-packaging-test-config/effective-distro-seeds-from-scan ()
  "`deb-packaging-config--effective-distro' seeds from `deb-packaging-detect--scan-context'."
  (let ((deb-packaging-config-target-distro "noble")
        (deb-packaging-config--distro-user-set nil))
    (cl-letf (((symbol-function 'deb-packaging-detect--scan-context)
               (lambda (&rest _) '(:distro "jammy"))))
      (should (string= (deb-packaging-config--effective-distro) "jammy"))
      (should (string= deb-packaging-config-target-distro "jammy"))
      (should deb-packaging-config--distro-user-set))))

(ert-deftest deb-packaging-test-config/effective-distro-falls-back-to-default ()
  "`deb-packaging-config--effective-distro' falls back to default when scan returns nil."
  (let ((deb-packaging-config-target-distro "noble")
        (deb-packaging-config--distro-user-set nil))
    (cl-letf (((symbol-function 'deb-packaging-detect--scan-context)
               (lambda (&rest _) nil)))
      (should (string= (deb-packaging-config--effective-distro) "noble"))
      (should (string= deb-packaging-config-target-distro "noble"))
      (should (null deb-packaging-config--distro-user-set)))))

(ert-deftest deb-packaging-test-config/distro-choices-known-member ()
  "`deb-packaging-config--distro-choices' returns the standard list for a known distro."
  (let ((deb-packaging-config-target-distro "noble")
        (deb-packaging-config--distro-user-set nil))
    (cl-letf (((symbol-function 'deb-packaging-config--effective-distro)
               (lambda (&rest _) "noble")))
      (let ((choices (deb-packaging-config--distro-choices)))
        (should (equal choices (append deb-packaging-config-ubuntu-distros
                                       deb-packaging-config-debian-distros)))
        (should (member "noble" choices))
        (should (member "jammy" choices))
        (should (member "sid" choices))))))

(ert-deftest deb-packaging-test-config/distro-choices-unknown-prepended ()
  "`deb-packaging-config--distro-choices' prepends an unknown current distro."
  (let ((deb-packaging-config-target-distro "noble")
        (deb-packaging-config--distro-user-set nil))
    (cl-letf (((symbol-function 'deb-packaging-config--effective-distro)
               (lambda (&rest _) "experimental-xyz")))
      (let ((choices (deb-packaging-config--distro-choices)))
        (should (string= (car choices) "experimental-xyz"))
        (should (member "noble" choices))
        (should (member "jammy" choices))
        (should (member "sid" choices))
        (should (equal (cdr choices) (append deb-packaging-config-ubuntu-distros
                                             deb-packaging-config-debian-distros)))))))

(ert-deftest deb-packaging-test-config/constants-contain-expected-distros ()
  "Distro constants include the expected Ubuntu and Debian entries."
  (should (member "noble" deb-packaging-config-ubuntu-distros))
  (should (member "jammy" deb-packaging-config-ubuntu-distros))
  (should (member "resolute" deb-packaging-config-ubuntu-distros))
  (should (member "stonking" deb-packaging-config-ubuntu-distros))
  (should (member "sid" deb-packaging-config-debian-distros)))

(provide 'deb-packaging-test-config)
;;; deb-packaging-test-config.el ends here
