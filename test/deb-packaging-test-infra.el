;;; deb-packaging-test-infra.el --- Infrastructure command tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for deb-packaging-infra.el command preflight checks.

;;; Code:

(require 'ert)
(require 'deb-packaging-test)
(require 'deb-packaging-infra)

;;; sudo preflight for deletions

(ert-deftest deb-packaging-test-infra/delete-qemu-errors-when-sudo-not-cached ()
  (let (compiled)
    (deb-packaging-test--with-mocked-process '(("sudo" . 1))
      (cl-letf (((symbol-function 'deb-packaging-infra--list-qemu-images)
                 (lambda () (list (list :name "img" :path "/var/lib/img.qcow2"))))
                ((symbol-function 'yes-or-no-p) (lambda (_p) t))
                ((symbol-function 'compile)
                 (lambda (cmd &rest _) (push cmd compiled))))
        (should-error (deb-packaging-infra-delete-qemu "img")
                      :type 'user-error)
        (should (null compiled))))))

(ert-deftest deb-packaging-test-infra/delete-qemu-runs-when-sudo-cached ()
  (let (compiled)
    (deb-packaging-test--with-mocked-process '(("sudo" . 0))
      (cl-letf (((symbol-function 'deb-packaging-infra--list-qemu-images)
                 (lambda () (list (list :name "img" :path "/var/lib/img.qcow2"))))
                ((symbol-function 'yes-or-no-p) (lambda (_p) t))
                ((symbol-function 'compile)
                 (lambda (cmd &rest _) (push cmd compiled))))
        (deb-packaging-infra-delete-qemu "img")
        (should (equal (length compiled) 1))
        (should (string-match-p "sudo rm /var/lib/img.qcow2" (car compiled)))))))

(ert-deftest deb-packaging-test-infra/delete-schroot-errors-when-sudo-not-cached ()
  (let (compiled)
    (deb-packaging-test--with-mocked-process '(("sudo" . 1))
      (cl-letf (((symbol-function 'deb-packaging-infra--list-schroots)
                 (lambda ()
                   (list (list :name "s" :config-file "/etc/schroot/s"
                               :directory "/srv/schroot/s"))))
                ((symbol-function 'yes-or-no-p) (lambda (_p) t))
                ((symbol-function 'compile)
                 (lambda (cmd &rest _) (push cmd compiled))))
        (should-error (deb-packaging-infra-delete-schroot "s")
                      :type 'user-error)
        (should (null compiled))))))

(ert-deftest deb-packaging-test-infra/delete-schroot-runs-when-sudo-cached ()
  (let (compiled)
    (deb-packaging-test--with-mocked-process '(("sudo" . 0))
      (cl-letf (((symbol-function 'deb-packaging-infra--list-schroots)
                 (lambda ()
                   (list (list :name "s" :config-file "/etc/schroot/s"
                               :directory "/srv/schroot/s"))))
                ((symbol-function 'yes-or-no-p) (lambda (_p) t))
                ((symbol-function 'compile)
                 (lambda (cmd &rest _) (push cmd compiled))))
        (deb-packaging-infra-delete-schroot "s")
        (should (equal (length compiled) 1))
        (should (string-match-p "sudo rm -rf /srv/schroot/s" (car compiled)))))))

;;; ppa show rendering

(ert-deftest deb-packaging-test-infra/show-ppa-displays-special-buffer ()
  (let (displayed)
    (deb-packaging-test--with-mocked-process '(("ppa" . "owner: foo\ndesc"))
      (cl-letf (((symbol-function 'deb-packaging-display-buffer)
                 (lambda (buf cat) (setq displayed (cons buf cat)))))
        (deb-packaging-infra-show-ppa "ppa:foo/bar")
        (let ((buf (car displayed)))
          (should (eq (cdr displayed) 'report))
          (should (buffer-live-p buf))
          (with-current-buffer buf
            (should (derived-mode-p 'special-mode))
            (should (string-match-p "owner: foo" (buffer-string)))
            (should buffer-read-only))
          (kill-buffer buf))))))

(ert-deftest deb-packaging-test-infra/show-ppa-failure-is-user-error ()
  (deb-packaging-test--with-mocked-process '(("ppa" . (1 . "boom happened")))
    (should-error (deb-packaging-infra-show-ppa "ppa:foo/bar")
                  :type 'user-error)))

(provide 'deb-packaging-test-infra)
;;; deb-packaging-test-infra.el ends here
