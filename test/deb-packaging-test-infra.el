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

;;; Empty-list prompts

(ert-deftest deb-packaging-test-infra/update-schroots-no-schroots-errors ()
  (cl-letf (((symbol-function 'deb-packaging-infra--list-schroots)
             (lambda () nil))
            ((symbol-function 'deb-packaging-infra--chroot-targets)
             (lambda () nil))
            ((symbol-function 'compile)
             (lambda (&rest _) (error "must not compile"))))
    (should-error (deb-packaging-infra-update-schroots) :type 'user-error)))

(ert-deftest deb-packaging-test-infra/delete-schroot-no-schroots-errors ()
  (cl-letf (((symbol-function 'deb-packaging-infra--list-schroots)
             (lambda () nil))
            ((symbol-function 'deb-packaging-infra--section-value-at-point)
             (lambda (&rest _) nil))
            ((symbol-function 'compile)
             (lambda (&rest _) (error "must not compile"))))
    (should-error (deb-packaging-infra-delete-schroot) :type 'user-error)))

;;; Container visit path

(ert-deftest deb-packaging-test-infra/visit-lxd-container-uses-in-container-mount ()
  (let (visited)
    (cl-letf (((symbol-function 'dired)
               (lambda (path &rest _) (setq visited path)))
              ((symbol-function 'deb-packaging-dev--ensure-tramp-method)
               #'ignore))
      (deb-packaging-infra-visit-lxd-entry
       (list :name "deb-dev-foo-noble" :type 'container
             :raw (list :source "/home/karl/src/foo")))
      (should (equal visited "/lxc:deb-dev-foo-noble:/root/work/foo")))))

(ert-deftest deb-packaging-test-infra/visit-lxd-hyphenated-package-name ()
  (let (visited)
    (cl-letf (((symbol-function 'dired)
               (lambda (path &rest _) (setq visited path)))
              ((symbol-function 'deb-packaging-dev--ensure-tramp-method)
               #'ignore))
      (deb-packaging-infra-visit-lxd-entry
       (list :name "deb-dev-linux-tools-noble" :type 'container :raw nil))
      (should (equal visited "/lxc:deb-dev-linux-tools-noble:/root/work/linux-tools")))))

(ert-deftest deb-packaging-test-infra/visit-lxd-non-dev-container-falls-back ()
  (let (visited)
    (cl-letf (((symbol-function 'dired)
               (lambda (path &rest _) (setq visited path)))
              ((symbol-function 'deb-packaging-dev--ensure-tramp-method)
               #'ignore))
      (deb-packaging-infra-visit-lxd-entry
       (list :name "random-box" :type 'container :raw nil))
      (should (equal visited "/lxc:random-box:/root/work")))))

;;; RET only on container rows

(ert-deftest deb-packaging-test-infra/lxd-ret-only-on-container-rows ()
  (should (null (lookup-key deb-packaging-infra-lxd-mode-map (kbd "RET"))))
  (cl-letf (((symbol-function 'deb-packaging-infra--list-lxd-all)
             (lambda ()
               (list (list :name "img-1" :type 'image
                           :status "amd64" :detail "1G")
                     (list :name "deb-dev-foo-noble" :type 'container
                           :status "RUNNING" :detail "foo / noble")))))
    (with-temp-buffer
      (deb-packaging-infra-lxd-mode)
      (deb-packaging-infra-refresh-lxd)
      (goto-char (point-min))
      (search-forward "deb-dev-foo-noble")
      (should (get-text-property (point) 'keymap))
      (goto-char (point-min))
      (search-forward "img-1")
      (should-not (get-text-property (point) 'keymap)))))

(provide 'deb-packaging-test-infra)
;;; deb-packaging-test-infra.el ends here
