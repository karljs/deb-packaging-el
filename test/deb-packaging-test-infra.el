;;; deb-packaging-test-infra.el --- Infrastructure command tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for deb-packaging-infra.el command preflight checks.

;;; Code:

(require 'ert)
(require 'deb-packaging-test)
(require 'deb-packaging-test-run)
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

;;; Mode-map conventions

(ert-deftest deb-packaging-test-infra/lxd-map-s-sorts-t-starts ()
  (should (eq (lookup-key deb-packaging-infra-lxd-mode-map "S")
              #'tabulated-list-sort))
  (should (eq (lookup-key deb-packaging-infra-lxd-mode-map "t")
              #'deb-packaging-infra-start-lxd-entry))
  (should (eq (lookup-key deb-packaging-infra-lxd-mode-map "s")
              #'deb-packaging-infra-stop-lxd-entry)))

(ert-deftest deb-packaging-test-infra/schroots-map-question-opens-dispatch ()
  (should (eq (lookup-key deb-packaging-infra-schroots-mode-map "?")
              #'deb-packaging-infra-dispatch)))

;;; Real defaults in create prompts

(defun deb-packaging-test-infra--capture-prompts (fn)
  "Call FN with prompt functions mocked; return (read-string-args cr-args).
yes-or-no-p declines so nothing runs."
  (let (rs-args cr-args)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest args) (setq rs-args args) (nth 3 args)))
              ((symbol-function 'completing-read)
               (lambda (&rest args) (setq cr-args args) (nth 6 args)))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
      (funcall fn)
      (list rs-args cr-args))))

(ert-deftest deb-packaging-test-infra/create-schroot-passes-real-defaults ()
  (let* ((res (deb-packaging-test-infra--capture-prompts
               #'deb-packaging-infra-create-schroot))
         (rs (car res))
         (cr (cadr res)))
    (should (null (nth 1 rs)))
    (should (equal (nth 3 rs) deb-packaging-config-target-distro))
    (should (null (nth 4 cr)))
    (should (equal (nth 6 cr) "amd64"))))

(ert-deftest deb-packaging-test-infra/create-lxd-passes-real-defaults ()
  (let* ((res (deb-packaging-test-infra--capture-prompts
               #'deb-packaging-infra-create-lxd))
         (rs (car res))
         (cr (cadr res)))
    (should (null (nth 1 rs)))
    (should (equal (nth 3 rs) deb-packaging-config-target-distro))
    (should (null (nth 4 cr)))
    (should (equal (nth 6 cr) "amd64"))))

(ert-deftest deb-packaging-test-infra/create-qemu-passes-real-defaults ()
  (let* ((res (deb-packaging-test-infra--capture-prompts
               #'deb-packaging-infra-create-qemu))
         (rs (car res))
         (cr (cadr res)))
    (should (null (nth 1 rs)))
    (should (equal (nth 3 rs) deb-packaging-config-target-distro))
    (should (null (nth 4 cr)))
    (should (equal (nth 6 cr) "amd64"))))

;;; Refresh after delete

(ert-deftest deb-packaging-test-infra/delete-qemu-refreshes-on-success ()
  (let ((refreshed 0))
    (deb-packaging-test--with-mocked-process '(("sudo" . 0))
      (cl-letf (((symbol-function 'deb-packaging-infra--list-qemu-images)
                 (lambda () (list (list :name "img" :path "/x.img"))))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'deb-packaging-commands--compile)
                 (lambda (&rest _) (get-buffer-create " *c*")))
                ((symbol-function 'deb-packaging-infra-refresh-qemu-images)
                 (lambda () (cl-incf refreshed))))
        (unwind-protect
            (with-temp-buffer
              (deb-packaging-infra-qemu-images-mode)
              (deb-packaging-infra-delete-qemu "img")
              (run-hook-with-args 'compilation-finish-functions
                                  (get-buffer " *c*") "finished\n")
              (should (= refreshed 1)))
          (kill-buffer " *c*"))))))

(ert-deftest deb-packaging-test-infra/delete-qemu-no-refresh-on-failure ()
  (let ((refreshed 0))
    (deb-packaging-test--with-mocked-process '(("sudo" . 0))
      (cl-letf (((symbol-function 'deb-packaging-infra--list-qemu-images)
                 (lambda () (list (list :name "img" :path "/x.img"))))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'deb-packaging-commands--compile)
                 (lambda (&rest _) (get-buffer-create " *c*")))
                ((symbol-function 'deb-packaging-infra-refresh-qemu-images)
                 (lambda () (cl-incf refreshed))))
        (unwind-protect
            (with-temp-buffer
              (deb-packaging-infra-qemu-images-mode)
              (deb-packaging-infra-delete-qemu "img")
              (run-hook-with-args 'compilation-finish-functions
                                  (get-buffer " *c*") "abnormally\n")
              (should (= refreshed 0)))
          (kill-buffer " *c*"))))))

;;; Honest lxc start/stop

(ert-deftest deb-packaging-test-infra/stop-lxd-reports-failure ()
  (deb-packaging-test--with-mocked-process '(("lxc" . 1))
    (cl-letf (((symbol-function 'deb-packaging-infra-refresh-lxd) #'ignore))
      (should-error
       (deb-packaging-infra-stop-lxd-entry
        (list :name "deb-dev-foo-noble" :type 'container))
       :type 'user-error))))

(ert-deftest deb-packaging-test-infra/stop-lxd-refreshes-on-success ()
  (let ((refreshed 0))
    (deb-packaging-test--with-mocked-process '(("lxc" . 0))
      (cl-letf (((symbol-function 'deb-packaging-infra-refresh-lxd)
                 (lambda () (cl-incf refreshed))))
        (with-temp-buffer
          (deb-packaging-infra-lxd-mode)
          (deb-packaging-infra-stop-lxd-entry
           (list :name "deb-dev-foo-noble" :type 'container)))
        (should (= refreshed 1))))))

(ert-deftest deb-packaging-test-infra/start-lxd-reports-failure ()
  (deb-packaging-test--with-mocked-process '(("lxc" . 1))
    (cl-letf (((symbol-function 'deb-packaging-infra-refresh-lxd) #'ignore))
      (should-error
       (deb-packaging-infra-start-lxd-entry
        (list :name "deb-dev-foo-noble" :type 'container))
       :type 'user-error))))

(ert-deftest deb-packaging-test-infra/shell-lxd-errors-when-start-fails ()
  (deb-packaging-test--with-mocked-process '(("lxc" . 1))
    (cl-letf (((symbol-function 'make-comint)
               (lambda (&rest _) (error "must not comint"))))
      (should-error
       (deb-packaging-infra-shell-lxd-entry
        (list :name "deb-dev-foo-noble" :type 'container))
       :type 'user-error))))

;;; PPA list fetch failure

(ert-deftest deb-packaging-test-infra/ppa-list-failure-keeps-list-and-messages ()
  (let ((messages nil))
    (with-temp-buffer
      (deb-packaging-infra-ppas-mode)
      (setq tabulated-list-entries
            (list (deb-packaging-infra--make-ppa-entry "ppa:me/old")))
      (let ((temp-buf (generate-new-buffer " *t*"))
            (proc (make-process :name "f" :command '("false") :noquery t)))
        (unwind-protect
            (progn
              (deb-packaging-test-run--wait proc)
              (cl-letf (((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (push (apply #'format fmt args) messages))))
                (funcall (deb-packaging-infra--ppa-list-sentinel
                          (current-buffer) temp-buf)
                         proc "exited abnormally with code 1\n"))
              (should (assoc "ppa:me/old" tabulated-list-entries))
              (should (cl-some (lambda (m) (string-match-p "failed" m))
                               messages)))
          (kill-buffer temp-buf))))))

(ert-deftest deb-packaging-test-infra/ppa-list-success-appends ()
  (with-temp-buffer
    (deb-packaging-infra-ppas-mode)
    (setq tabulated-list-entries nil)
    (let ((temp-buf (generate-new-buffer " *t*"))
          (proc (make-process :name "t" :command '("true") :noquery t)))
      (unwind-protect
          (progn
            (with-current-buffer temp-buf
              (insert "Some header\n  ppa:me/new\n"))
            (deb-packaging-test-run--wait proc)
            (funcall (deb-packaging-infra--ppa-list-sentinel
                      (current-buffer) temp-buf)
                     proc "finished\n")
            (should (assoc "ppa:me/new" tabulated-list-entries)))
        (when (buffer-live-p temp-buf)
          (kill-buffer temp-buf))))))

;;; PPA reading without network

(ert-deftest deb-packaging-test-infra/read-ppa-uses-buffer-rows ()
  "Inside the PPAs buffer, candidates come from the rows, not `ppa list'."
  (let (seen-collection)
    (with-temp-buffer
      (deb-packaging-infra-ppas-mode)
      (setq tabulated-list-entries
            (list (deb-packaging-infra--make-ppa-entry "ppa:me/one")
                  (deb-packaging-infra--make-ppa-entry "ppa:me/two")))
      (cl-letf (((symbol-function 'deb-packaging-infra--list-ppas)
                 (lambda () (error "must not call ppa list")))
                ((symbol-function 'completing-read)
                 (lambda (_p coll &rest _)
                   (setq seen-collection coll) (car coll))))
        (should (equal (deb-packaging-infra--read-ppa "PPA: ") "ppa:me/one"))
        (should (equal seen-collection '("ppa:me/one" "ppa:me/two")))))))

(ert-deftest deb-packaging-test-infra/read-ppa-empty-rows-errors ()
  "No rows and no id at point is an upfront error, not a network call."
  (with-temp-buffer
    (deb-packaging-infra-ppas-mode)
    (setq tabulated-list-entries nil)
    (cl-letf (((symbol-function 'deb-packaging-infra--list-ppas)
               (lambda () (error "must not call ppa list"))))
      (should-error (deb-packaging-infra--read-ppa "PPA: ")
                    :type 'user-error))))

;;; Single-update confirmation

(ert-deftest deb-packaging-test-infra/update-schroots-confirms-before-compile ()
  (let ((asked nil) (compiled nil))
    (cl-letf (((symbol-function 'deb-packaging-infra--list-schroots)
               (lambda () (list (list :name "s1"))))
              ((symbol-function 'deb-packaging-infra--chroot-targets)
               (lambda () nil))
              ((symbol-function 'completing-read) (lambda (&rest _) "s1"))
              ((symbol-function 'y-or-n-p)
               (lambda (&rest _) (setq asked t) t))
              ((symbol-function 'compile)
               (lambda (&rest _) (setq compiled t))))
      (deb-packaging-infra-update-schroots)
      (should asked)
      (should compiled))))

(ert-deftest deb-packaging-test-infra/update-schroots-declined-runs-nothing ()
  (let ((compiled nil))
    (cl-letf (((symbol-function 'deb-packaging-infra--list-schroots)
               (lambda () (list (list :name "s1"))))
              ((symbol-function 'deb-packaging-infra--chroot-targets)
               (lambda () nil))
              ((symbol-function 'completing-read) (lambda (&rest _) "s1"))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
              ((symbol-function 'compile)
               (lambda (&rest _) (setq compiled t))))
      (deb-packaging-infra-update-schroots)
      (should-not compiled))))

;;; Empty PPA name

(ert-deftest deb-packaging-test-infra/create-ppa-empty-name-errors ()
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) ""))
            ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
    (should-error (deb-packaging-infra-create-ppa) :type 'user-error)))

(ert-deftest deb-packaging-test-infra/delete-ppa-empty-name-errors ()
  (cl-letf (((symbol-function 'deb-packaging-infra--read-ppa)
             (lambda (&rest _) ""))
            ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
    (should-error (call-interactively #'deb-packaging-infra-delete-ppa)
                  :type 'user-error)))

(ert-deftest deb-packaging-test-infra/show-ppa-empty-name-errors ()
  (cl-letf (((symbol-function 'deb-packaging-infra--read-ppa)
             (lambda (&rest _) "")))
    (should-error (call-interactively #'deb-packaging-infra-show-ppa)
                  :type 'user-error)))

;;; Refresh mode guards

(ert-deftest deb-packaging-test-infra/refresh-errors-outside-its-mode ()
  (dolist (fn '(deb-packaging-infra-refresh-schroots
                deb-packaging-infra-refresh-lxd
                deb-packaging-infra-refresh-qemu-images
                deb-packaging-infra-refresh-ppas))
    (with-temp-buffer
      (should-error (funcall fn) :type 'user-error))))

(provide 'deb-packaging-test-infra)
;;; deb-packaging-test-infra.el ends here
