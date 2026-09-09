;;; deb-packaging-test-infra.el --- Infrastructure command tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for deb-packaging-infra.el command preflight checks.

;;; Code:

(require 'ert)
(require 'deb-packaging-test)
(require 'deb-packaging-test-run)
(require 'deb-packaging-infra)

;;; Privileged commands run via the comint runner (pty for authd prompts)

(ert-deftest deb-packaging-test-infra/delete-qemu-plain-rm-when-writable ()
  (let (args)
    (cl-letf (((symbol-function 'deb-packaging-infra--list-qemu-images)
               (lambda () (list (list :name "img" :path "/x.img"))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'file-writable-p) (lambda (&rest _) t))
              ((symbol-function 'deb-packaging-commands--run-command)
               (lambda (_name a &rest _) (setq args a) nil)))
      (deb-packaging-infra-delete-qemu "img")
      (should (equal args '("rm" "/x.img"))))))

(ert-deftest deb-packaging-test-infra/delete-qemu-sudo-when-not-writable ()
  "Interactive sudo (no -n): the prompt renders in the comint buffer."
  (let (args)
    (cl-letf (((symbol-function 'deb-packaging-infra--list-qemu-images)
               (lambda () (list (list :name "img" :path "/x.img"))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'file-writable-p) (lambda (&rest _) nil))
              ((symbol-function 'deb-packaging-commands--run-command)
               (lambda (_name a &rest _) (setq args a) nil)))
      (deb-packaging-infra-delete-qemu "img")
      (should (equal args '("sudo" "rm" "/x.img"))))))

(ert-deftest deb-packaging-test-infra/delete-schroot-one-shell-two-sudos ()
  "Both sudo calls share one pty; the whole command is a single sh -c."
  (let (args)
    (cl-letf (((symbol-function 'deb-packaging-infra--list-schroots)
               (lambda ()
                 (list (list :name "s" :config-file "/etc/schroot/s"
                             :directory "/srv/schroot/s"))))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'deb-packaging-commands--run-command)
               (lambda (_name a &rest _) (setq args a) nil)))
      (deb-packaging-infra-delete-schroot "s")
      (should (equal args
                     (list "sh" "-c"
                           "sudo rm -rf /srv/schroot/s && sudo rm /etc/schroot/s"))))))

(ert-deftest deb-packaging-test-infra/create-schroot-runs-mk-sbuild-on-confirm ()
  "mk-sbuild self-sudos; no sudo prefix and no sudo preflight."
  (let (args (probed nil))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "noble"))
              ((symbol-function 'completing-read) (lambda (&rest _) "amd64"))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'call-process)
               (lambda (&rest _) (setq probed t) 0))
              ((symbol-function 'deb-packaging-commands--run-command)
               (lambda (_name a &rest _) (setq args a) nil)))
      (deb-packaging-infra-create-schroot)
      (should-not probed)
      (should (equal args '("mk-sbuild" "--arch=amd64" "noble"))))))

;;; ppa show rendering

(defun deb-packaging-test-infra--show-ppa-with-command (command)
  "Run `deb-packaging-infra-show-ppa' with make-process swapping in COMMAND.
Return the report buffer once its sentinel has fired."
  (let ((real-make-process (symbol-function 'make-process))
        (proc nil)
        (displayed nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest props)
                 (setq proc
                       (apply real-make-process
                              (plist-put props :command command)))))
              ((symbol-function 'deb-packaging-display-buffer)
               (lambda (buf _cat) (setq displayed buf))))
      (deb-packaging-infra-show-ppa "ppa:foo/bar")
      (deb-packaging-test-run--wait proc)
      displayed)))

(ert-deftest deb-packaging-test-infra/show-ppa-displays-special-buffer ()
  (let ((buf (deb-packaging-test-infra--show-ppa-with-command
              '("sh" "-c" "echo 'owner: foo'"))))
    (unwind-protect
        (progn
          (should (buffer-live-p buf))
          (with-current-buffer buf
            (should (derived-mode-p 'special-mode))
            (should (string-match-p "owner: foo" (buffer-string)))
            (should buffer-read-only)
            (should (eq deb-packaging-display-category 'report))))
      (kill-buffer buf))))

(ert-deftest deb-packaging-test-infra/show-ppa-failure-shows-error ()
  (let ((buf (deb-packaging-test-infra--show-ppa-with-command '("false"))))
    (unwind-protect
        (progn
          (should (buffer-live-p buf))
          (with-current-buffer buf
            (should (string-match-p "failed" (buffer-string)))))
      (kill-buffer buf))))

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

(ert-deftest deb-packaging-test-infra/lxd-map-s-sorts-s-starts-k-stops ()
  (should (eq (lookup-key deb-packaging-infra-lxd-mode-map "S")
              #'tabulated-list-sort))
  (should (eq (lookup-key deb-packaging-infra-lxd-mode-map "s")
              #'deb-packaging-infra-start-lxd-entry))
  (should (eq (lookup-key deb-packaging-infra-lxd-mode-map "k")
              #'deb-packaging-infra-stop-lxd-entry))
  (should (null (lookup-key deb-packaging-infra-lxd-mode-map "t"))))

(ert-deftest deb-packaging-test-infra/schroots-map-question-opens-dispatch ()
  (should (eq (lookup-key deb-packaging-infra-schroots-mode-map "?")
              #'deb-packaging-infra-schroots-dispatch)))

(ert-deftest deb-packaging-test-infra/list-maps-bind-question-to-own-dispatch ()
  "Every infra list buffer's ? shows that buffer's own commands."
  (should (eq (lookup-key deb-packaging-infra-lxd-mode-map "?")
              #'deb-packaging-infra-lxd-dispatch))
  (should (eq (lookup-key deb-packaging-infra-qemu-images-mode-map "?")
              #'deb-packaging-infra-qemu-dispatch))
  (should (eq (lookup-key deb-packaging-infra-ppas-mode-map "?")
              #'deb-packaging-infra-ppas-dispatch)))

(ert-deftest deb-packaging-test-infra/list-maps-bind-ret ()
  (should (eq (lookup-key deb-packaging-infra-schroots-mode-map (kbd "RET"))
              #'deb-packaging-infra-visit-schroot))
  (should (eq (lookup-key deb-packaging-infra-qemu-images-mode-map (kbd "RET"))
              #'deb-packaging-infra-visit-qemu-dir))
  (should (eq (lookup-key deb-packaging-infra-ppas-mode-map (kbd "RET"))
              #'deb-packaging-infra-visit-ppa)))

(ert-deftest deb-packaging-test-infra/schroots-ret-direds-chroot-directory ()
  (let (dired-arg)
    (cl-letf (((symbol-function 'deb-packaging-infra--list-schroots)
               (lambda ()
                 (list (list :name "noble-amd64-sbuild"
                             :config-file "/etc/schroot/noble"
                             :description "noble"
                             :directory "/srv/chroots/noble"))))
              ((symbol-function 'deb-packaging-infra--list-sessions)
               (lambda () nil))
              ((symbol-function 'file-directory-p) (lambda (&rest _) t))
              ((symbol-function 'dired)
               (lambda (dir &rest _) (setq dired-arg dir))))
      (with-temp-buffer
        (deb-packaging-infra-schroots-mode)
        (deb-packaging-infra-refresh-schroots)
        (goto-char (point-min))
        (search-forward "noble-amd64-sbuild")
        (deb-packaging-infra-visit-schroot)
        (should (equal dired-arg "/srv/chroots/noble"))))))

(ert-deftest deb-packaging-test-infra/schroots-ret-session-shows-info ()
  (let (run-args)
    (cl-letf (((symbol-function 'deb-packaging-infra--list-schroots)
               (lambda () nil))
              ((symbol-function 'deb-packaging-infra--list-sessions)
               (lambda () (list "noble-amd64-sbuild-abc123")))
              ((symbol-function 'deb-packaging-commands--run-command)
               (lambda (_name args &rest _) (setq run-args args) nil)))
      (with-temp-buffer
        (deb-packaging-infra-schroots-mode)
        (deb-packaging-infra-refresh-schroots)
        (goto-char (point-min))
        (search-forward "abc123")
        (deb-packaging-infra-visit-schroot)
        (should (equal run-args
                       '("schroot" "--info" "-c"
                         "noble-amd64-sbuild-abc123")))))))

(ert-deftest deb-packaging-test-infra/schroots-ret-elsewhere-errors ()
  (cl-letf (((symbol-function 'deb-packaging-infra--list-schroots)
             (lambda () nil))
            ((symbol-function 'deb-packaging-infra--list-sessions)
               (lambda () nil)))
    (with-temp-buffer
      (deb-packaging-infra-schroots-mode)
      (deb-packaging-infra-refresh-schroots)
      (goto-char (point-min))
      (should-error (deb-packaging-infra-visit-schroot) :type 'user-error))))

(ert-deftest deb-packaging-test-infra/visit-qemu-dir-direds-image-dir ()
  (let (dired-arg)
    (cl-letf (((symbol-function 'file-directory-p) (lambda (&rest _) t))
              ((symbol-function 'dired)
               (lambda (dir &rest _) (setq dired-arg dir))))
      (deb-packaging-infra-visit-qemu-dir)
      (should (equal dired-arg deb-packaging-infra-qemu-dir)))))

(ert-deftest deb-packaging-test-infra/visit-ppa-shows-ppa-at-point ()
  (let (shown)
    (with-temp-buffer
      (deb-packaging-infra-ppas-mode)
      (setq tabulated-list-entries
            (list (deb-packaging-infra--make-ppa-entry "ppa:me/one")))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (search-forward "one")
      (beginning-of-line)
      (cl-letf (((symbol-function 'deb-packaging-infra-show-ppa)
                 (lambda (name &rest _) (setq shown name))))
        (deb-packaging-infra-visit-ppa)
        (should (equal shown "ppa:me/one"))))))

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
    (should (equal (nth 3 rs) (deb-packaging-config--effective-distro)))
    (should (null (nth 4 cr)))
    (should (equal (nth 6 cr) "amd64"))))

(ert-deftest deb-packaging-test-infra/create-lxd-passes-real-defaults ()
  (let* ((res (deb-packaging-test-infra--capture-prompts
               #'deb-packaging-infra-create-lxd))
         (rs (car res))
         (cr (cadr res)))
    (should (null (nth 1 rs)))
    (should (equal (nth 3 rs) (deb-packaging-config--effective-distro)))
    (should (null (nth 4 cr)))
    (should (equal (nth 6 cr) "amd64"))))

(ert-deftest deb-packaging-test-infra/create-qemu-passes-real-defaults ()
  (let* ((res (deb-packaging-test-infra--capture-prompts
               #'deb-packaging-infra-create-qemu))
         (rs (car res))
         (cr (cadr res)))
    (should (null (nth 1 rs)))
    (should (equal (nth 3 rs) (deb-packaging-config--effective-distro)))
    (should (null (nth 4 cr)))
    (should (equal (nth 6 cr) "amd64"))))

;;; Refresh after delete (via the run-privileged sentinel)

(defun deb-packaging-test-infra--delete-qemu-with-command (command refreshed)
  "Run `deb-packaging-infra-delete-qemu' inside a live qemu-mode buffer.
--run-command is mocked to start COMMAND as a real process; each call of
`deb-packaging-infra-refresh-qemu-images' increments REFRESHED (a place).
The sentinel fires during the wait, while the mode buffer is alive."
  (let ((buf (generate-new-buffer " *qdel*")))
    (unwind-protect
        (with-temp-buffer
          (deb-packaging-infra-qemu-images-mode)
          (cl-letf (((symbol-function 'deb-packaging-infra--list-qemu-images)
                     (lambda () (list (list :name "img" :path "/x.img"))))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'file-writable-p) (lambda (&rest _) t))
                    ((symbol-function 'deb-packaging-infra-refresh-qemu-images)
                     (lambda () (cl-incf (car refreshed))))
                    ((symbol-function 'deb-packaging-commands--run-command)
                     (lambda (_name _a &rest _)
                       (make-process :name "qdel" :buffer buf
                                     :command command :noquery t)
                       buf)))
            (deb-packaging-infra-delete-qemu "img")
            (deb-packaging-test-run--wait
             (get-buffer-process buf))))
      (kill-buffer buf))))

(ert-deftest deb-packaging-test-infra/delete-qemu-refreshes-on-success ()
  (let ((refreshed (list 0)))
    (deb-packaging-test-infra--delete-qemu-with-command '("true") refreshed)
    (should (= (car refreshed) 1))))

(ert-deftest deb-packaging-test-infra/delete-qemu-no-refresh-on-failure ()
  (let ((refreshed (list 0)))
    (deb-packaging-test-infra--delete-qemu-with-command '("false") refreshed)
    (should (= (car refreshed) 0))))

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

(ert-deftest deb-packaging-test-infra/read-ppa-empty-rows-free-text ()
  "No rows, no id at point, and no cached candidates: the prompt falls
back to free text instead of erroring (the background warm may still be
in flight)."
  (with-temp-buffer
    (deb-packaging-infra-ppas-mode)
    (setq tabulated-list-entries nil)
    (let ((deb-packaging-infra--ppa-cache nil))
      (cl-letf (((symbol-function 'deb-packaging-infra--list-ppas)
                 (lambda () nil))
                ((symbol-function 'read-string)
                 (lambda (prompt &rest _) (cons 'read prompt)))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) (error "must not complete"))))
        (should (equal (deb-packaging-infra--read-ppa "PPA: ")
                       '(read . "PPA: ")))))))

;;; PPA candidate cache: non-blocking list, background warm

(ert-deftest deb-packaging-test-infra/list-ppas-fresh-cache-no-warm ()
  (let ((deb-packaging-infra--ppa-cache (cons '("ppa:me/x") (float-time)))
        (deb-packaging-infra--ppa-warm-proc nil))
    (cl-letf (((symbol-function 'deb-packaging-infra--warm-ppa-cache-async)
               (lambda () (error "must not warm"))))
      (should (equal (deb-packaging-infra--list-ppas) '("ppa:me/x"))))))

(ert-deftest deb-packaging-test-infra/list-ppas-stale-returns-stale-and-warms ()
  "A stale cache returns the old list immediately (no blocking fetch)
while the background refresh warms the next prompt."
  (let ((deb-packaging-infra--ppa-cache
         (cons '("ppa:me/old") (- (float-time) 1000)))
        (deb-packaging-infra--ppa-warm-proc nil)
        (warmed 0))
    (cl-letf (((symbol-function 'deb-packaging-infra--warm-ppa-cache-async)
               (lambda () (cl-incf warmed))))
      (should (equal (deb-packaging-infra--list-ppas) '("ppa:me/old")))
      (should (= warmed 1)))))

(ert-deftest deb-packaging-test-infra/list-ppas-cold-returns-nil-and-warms ()
  (let ((deb-packaging-infra--ppa-cache nil)
        (deb-packaging-infra--ppa-warm-proc nil)
        (warmed 0))
    (cl-letf (((symbol-function 'deb-packaging-infra--warm-ppa-cache-async)
               (lambda () (cl-incf warmed))))
      (should (null (deb-packaging-infra--list-ppas)))
      (should (= warmed 1)))))

(ert-deftest deb-packaging-test-infra/list-ppas-never-spawns-in-batch ()
  "Batch Emacs must not kick background network fetches, so tests stay
hermetic even with a cold cache and the real warm in place."
  (let ((deb-packaging-infra--ppa-cache nil)
        (deb-packaging-infra--ppa-warm-proc nil)
        (deb-packaging-infra--ppa-tool-missing-warned nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _) (error "must not spawn")))
              ((symbol-function 'message) #'ignore))
      (should (null (deb-packaging-infra--list-ppas))))))

(ert-deftest deb-packaging-test-infra/warm-ppa-cache-missing-tool-warns-once ()
  (let ((deb-packaging-infra--ppa-cache nil)
        (deb-packaging-infra--ppa-warm-proc nil)
        (deb-packaging-infra--ppa-tool-missing-warned nil)
        (messages nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) messages))))
      (let ((noninteractive nil))
        (deb-packaging-infra--warm-ppa-cache-async)
        (deb-packaging-infra--warm-ppa-cache-async))
      ;; Warned exactly once across two calls.
      (should (equal (cl-count-if
                      (lambda (m) (string-match-p "ppa tool not installed" m))
                      messages)
                     1))
      ;; Empty list cached for the TTL: the second call above was a
      ;; no-op against the timestamped empty cache.
      (should (null (car deb-packaging-infra--ppa-cache)))
      (should (cdr deb-packaging-infra--ppa-cache)))))

(defun deb-packaging-test-infra--warm-with-echo (output)
  "Run the real async warm with the fetch command replaced by an echo.
OUTPUT is the string the mock `ppa list' prints.  Returns the process."
  (let ((real-make-process (symbol-function 'make-process))
        (proc nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest props)
                 (setq proc
                       (apply real-make-process
                              (plist-put props :command
                                         (list "sh" "-c"
                                               (format "echo %s"
                                                       (shell-quote-argument
                                                        output)))))))))
      (let ((noninteractive nil))
        (deb-packaging-infra--warm-ppa-cache-async)))
    proc))
(ert-deftest deb-packaging-test-infra/warm-ppa-cache-updates-cache ()
  (let ((deb-packaging-infra--ppa-cache nil)
        (deb-packaging-infra--ppa-warm-proc nil)
        (deb-packaging-infra--ppa-tool-missing-warned t))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) t))
              ((symbol-function 'deb-packaging-infra--team-config-files)
               (lambda () nil)))
      (let ((proc (deb-packaging-test-infra--warm-with-echo
                   "Some header\n  ppa:me/one\n  ppa:me/two")))
        (unwind-protect
            (progn
              (deb-packaging-test-run--wait proc)
              (should (equal (car deb-packaging-infra--ppa-cache)
                             '("ppa:me/one" "ppa:me/two")))
              (should (cdr deb-packaging-infra--ppa-cache)))
          (when (process-live-p proc) (delete-process proc)))))))

(ert-deftest deb-packaging-test-infra/warm-ppa-cache-skips-when-in-flight ()
  "A refresh already running means no second spawn."
  (let ((deb-packaging-infra--ppa-cache nil)
        (deb-packaging-infra--ppa-warm-proc
         (make-process :name "warm-inflight" :command '("sleep" "30")
                       :noquery t))
        (deb-packaging-infra--ppa-tool-missing-warned t))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find) (lambda (_) t))
                  ((symbol-function 'make-process)
                   (lambda (&rest _) (error "must not spawn"))))
          (let ((noninteractive nil))
            (deb-packaging-infra--warm-ppa-cache-async)))
      (delete-process deb-packaging-infra--ppa-warm-proc))))

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

(ert-deftest deb-packaging-test-infra/ppa-list-killed-process-kills-temp-buffer ()
  (with-temp-buffer
    (deb-packaging-infra-ppas-mode)
    (let ((temp-buf (generate-new-buffer " *t*"))
          (proc (make-process :name "s" :command '("sleep" "30") :noquery t)))
      (delete-process proc)
      (funcall (deb-packaging-infra--ppa-list-sentinel (current-buffer) temp-buf)
               proc "deleted\n")
      (should-not (buffer-live-p temp-buf)))))

(ert-deftest deb-packaging-test-infra/ppa-list-failure-shows-honest-empty-state ()
  "A failed first fetch must not masquerade as an empty list."
  (with-temp-buffer
    (deb-packaging-infra-ppas-mode)
    (setq tabulated-list-entries nil)
    (let ((temp-buf (generate-new-buffer " *t*"))
          (proc (make-process :name "f" :command '("false") :noquery t)))
      (unwind-protect
          (progn
            (deb-packaging-test-run--wait proc)
            (cl-letf (((symbol-function 'message) #'ignore))
              (funcall (deb-packaging-infra--ppa-list-sentinel
                        (current-buffer) temp-buf)
                       proc "exited abnormally\n"))
            (should (string-match-p "failed" (buffer-string)))
            (should-not (string-match-p "No PPAs found" (buffer-string))))
        (when (buffer-live-p temp-buf)
          (kill-buffer temp-buf))))))

(provide 'deb-packaging-test-infra)
;;; deb-packaging-test-infra.el ends here
