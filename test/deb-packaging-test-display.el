;;; deb-packaging-test-display.el --- Tests for the display policy -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Keywords: tools, debian, ubuntu, packaging

;;; Commentary:

;; Tests for deb-packaging-display.el and the package's window policy.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'deb-packaging-test)
(require 'deb-packaging-display)
(require 'deb-packaging-transients)
(require 'deb-packaging-commands)
(require 'deb-packaging-dev)
(require 'deb-packaging-infra)
(require 'deb-packaging-status)
(require 'deb-packaging-ppa-tests)

;;; Action mapping

(ert-deftest deb-packaging-test-display/action-per-category ()
  "status/list/report use same-window; output/shell reuse-or-below."
  (dolist (cat '(status list report))
    (should (memq 'display-buffer-same-window
                  (car (deb-packaging-display--action cat)))))
  (dolist (cat '(output shell))
    (let ((fns (car (deb-packaging-display--action cat))))
      (should (memq 'deb-packaging-display--reuse-category-window fns))
      (should (memq 'display-buffer-below-selected fns)))))

(ert-deftest deb-packaging-test-display/action-unknown-category-errors ()
  (should-error (deb-packaging-display--action 'bogus)
                :type 'error))

;;; Category-window reuse

(defmacro deb-packaging-test-display--with-marked-buffers (specs &rest body)
  "Create temp buffers per SPECS, a list of (VAR CATEGORY), then run BODY.
Each VAR is bound to a buffer whose `deb-packaging-display-category' is
CATEGORY.  Buffers are killed afterwards."
  (declare (indent 1) (debug (form body)))
  (let ((bufs (mapcar #'car specs)))
    `(let ,(mapcar (lambda (v) `(,v (get-buffer-create
                                     ,(format "*dp-test-%s*" v))))
                   bufs)
       (unwind-protect
           (progn
             ,@(mapcar (lambda (s)
                         `(with-current-buffer ,(car s)
                            (setq deb-packaging-display-category ,(cadr s))))
                       specs)
             ,@body)
         ,@(mapcar (lambda (v) `(kill-buffer ,v)) bufs)))))

(ert-deftest deb-packaging-test-display/reuse-finds-category-window ()
  "A visible window showing a same-category buffer is reused."
  (save-window-excursion
    (deb-packaging-test-display--with-marked-buffers
        ((old-buf 'output) (new-buf 'output))
      (let ((win (split-window (selected-window) nil 'below)))
        (set-window-buffer win old-buf)
        (should (eq (deb-packaging-display--reuse-category-window new-buf nil)
                    win))
        (should (eq (window-buffer win) new-buf))))))

(ert-deftest deb-packaging-test-display/reuse-skips-dedicated-window ()
  "Dedicated windows are not reused."
  (save-window-excursion
    (deb-packaging-test-display--with-marked-buffers
        ((old-buf 'output) (new-buf 'output))
      (let ((win (split-window (selected-window) nil 'below)))
        (set-window-buffer win old-buf)
        (set-window-dedicated-p win t)
        (should-not (deb-packaging-display--reuse-category-window new-buf nil))
        (set-window-dedicated-p win nil)))))

(ert-deftest deb-packaging-test-display/reuse-skips-side-window ()
  "Side windows are not reused."
  (save-window-excursion
    (deb-packaging-test-display--with-marked-buffers
        ((old-buf 'output) (new-buf 'output))
      (let ((win (split-window (selected-window) nil 'below)))
        (set-window-buffer win old-buf)
        (set-window-parameter win 'window-side 'bottom)
        (should-not (deb-packaging-display--reuse-category-window new-buf nil))
        (set-window-parameter win 'window-side nil)))))

(ert-deftest deb-packaging-test-display/reuse-ignores-other-category ()
  "A window showing a different category is not reused."
  (save-window-excursion
    (deb-packaging-test-display--with-marked-buffers
        ((old-buf 'shell) (new-buf 'output))
      (let ((win (split-window (selected-window) nil 'below)))
        (set-window-buffer win old-buf)
        (should-not (deb-packaging-display--reuse-category-window new-buf nil))))))

;;; End-to-end display

(ert-deftest deb-packaging-test-display/status-takes-over-window ()
  "status displays in the selected window and keeps it selected."
  (save-window-excursion
    (let ((buf (get-buffer-create "*dp-test-status*"))
          (start (selected-window)))
      (unwind-protect
          (progn
            (deb-packaging-display-buffer buf 'status)
            (should (eq (selected-window) start))
            (should (eq (window-buffer start) buf)))
        (kill-buffer buf)))))

(ert-deftest deb-packaging-test-display/output-opens-below-and-selects ()
  "output opens a regular window below the invoking one and selects it."
  (save-window-excursion
    (deb-packaging-test-display--with-marked-buffers ((buf 'output))
      (let ((start (selected-window)))
        (deb-packaging-display-buffer buf 'output)
        (should-not (eq (selected-window) start))
        (should (eq (window-buffer (selected-window)) buf))
        (should-not (window-parameter (selected-window) 'window-side))))))

(ert-deftest deb-packaging-test-display/overrides-user-display-buffer-alist ()
  "User alist side-window rules must not grab package buffers."
  (save-window-excursion
    (let ((display-buffer-alist
           '((".*" (display-buffer-in-side-window (side . bottom) (slot . 0))))))
      (deb-packaging-test-display--with-marked-buffers ((buf 'output))
        (deb-packaging-display-buffer buf 'output)
        (let ((win (get-buffer-window buf)))
          (should win)
          (should-not (window-parameter win 'window-side)))))))

;;; Transient display action

(ert-deftest deb-packaging-test-display/transient-action-reuses-process-window ()
  "The transient menu replaces a visible output/shell window, no new split."
  (save-window-excursion
    (deb-packaging-test-display--with-marked-buffers ((log-buf 'output))
      (let ((menu-buf (get-buffer-create "*dp-test-menu*"))
            (status-buf (get-buffer-create "*dp-test-status*")))
        (unwind-protect
            (progn
              (set-window-buffer (selected-window) status-buf)
              (let ((log-win (split-window (selected-window) nil 'below)))
                (set-window-buffer log-win log-buf)
                (select-window (get-buffer-window status-buf))
                (display-buffer menu-buf deb-packaging-transients-display-action)
                (should (eq (get-buffer-window menu-buf) log-win))
                (should (= (length (window-list)) 2))
                ;; Transient exit kills the menu buffer; the log returns.
                (kill-buffer menu-buf)
                (should (eq (window-buffer log-win) log-buf))))
          (when (buffer-live-p menu-buf)
            (kill-buffer menu-buf)))))))

(ert-deftest deb-packaging-test-display/transient-action-fallback-below ()
  "With no output/shell window visible, the menu opens below, dedicated."
  (save-window-excursion
    (let ((menu-buf (get-buffer-create "*dp-test-menu*"))
          (start (selected-window)))
      (unwind-protect
          (progn
            (display-buffer menu-buf deb-packaging-transients-display-action)
            (let ((win (get-buffer-window menu-buf)))
              (should win)
              (should-not (eq win start))
              (should (window-dedicated-p win))
              (should-not (window-parameter win 'window-side))))
        (when (buffer-live-p menu-buf)
          (kill-buffer menu-buf))))))

;;; Call-site wiring: output and shell

(ert-deftest deb-packaging-test-display/run-command-displays-output ()
  "Build output displays in a regular window and is category-marked."
  (save-window-excursion
    (cl-letf (((symbol-function 'make-comint-in-buffer)
               (lambda (_name buf-name _program &rest _args)
                 (get-buffer-create buf-name))))
      (let ((buf-name (deb-packaging-commands--run-command "test" '("true"))))
        (unwind-protect
            (let ((win (get-buffer-window buf-name)))
              (should win)
              (should (eq (selected-window) win))
              (should-not (window-parameter win 'window-side))
              (should (eq (buffer-local-value 'deb-packaging-display-category
                                              (get-buffer buf-name))
                          'output)))
          (kill-buffer buf-name))))))

(ert-deftest deb-packaging-test-display/dev-exec-displays-shell ()
  "The dev container shell displays via the shell category."
  (deb-packaging-test--with-package-tree '(:name "mypkg" :version "1.0-1")
    (let (seen)
      (cl-letf (((symbol-function 'deb-packaging-dev--container-exists-p)
                 (lambda (_) t))
                ((symbol-function 'call-process) (lambda (&rest _) 0))
                ((symbol-function 'deb-packaging-dev--ensure-tramp-method)
                 #'ignore)
                ((symbol-function 'make-comint)
                 (lambda (name &rest _)
                   (get-buffer-create (format "*%s*" name))))
                ((symbol-function 'deb-packaging-display-buffer)
                 (lambda (_buf cat) (setq seen cat) (selected-window))))
        (unwind-protect
            (deb-packaging-dev-exec)
          (kill-buffer "*lxc:deb-dev-mypkg-noble*")))
      (should (eq seen 'shell)))))

(ert-deftest deb-packaging-test-display/infra-shell-displays-shell ()
  "The infra container shell displays via the shell category."
  (let (seen)
    (cl-letf (((symbol-function 'call-process) (lambda (&rest _) 0))
              ((symbol-function 'deb-packaging-dev--ensure-tramp-method)
               #'ignore)
              ((symbol-function 'make-comint)
               (lambda (name &rest _)
                 (get-buffer-create (format "*%s*" name))))
              ((symbol-function 'deb-packaging-display-buffer)
               (lambda (_buf cat) (setq seen cat) (selected-window))))
      (unwind-protect
          (deb-packaging-infra-shell-lxd-entry '(:type container :name "c1"))
        (kill-buffer "*lxc:c1*")))
    (should (eq seen 'shell))))

;;; Call-site wiring: same-window categories

(ert-deftest deb-packaging-test-display/status-displays-status ()
  "The status buffer displays via the status category."
  (deb-packaging-test--with-package-tree '(:name "mypkg" :version "1.0-1")
    (let (seen)
      (cl-letf (((symbol-function 'deb-packaging-status--render) #'ignore)
                ((symbol-function 'deb-packaging-status--goto-first-phase)
                 #'ignore)
                ((symbol-function 'deb-packaging-display-buffer)
                 (lambda (_buf cat) (setq seen cat) (selected-window))))
        (deb-packaging-status))
      (should (eq seen 'status)))))

(ert-deftest deb-packaging-test-display/infra-schroots-displays-list ()
  "The schroots list displays via the list category."
  (let (seen)
    (cl-letf (((symbol-function 'deb-packaging-infra-refresh-schroots) #'ignore)
              ((symbol-function 'deb-packaging-display-buffer)
               (lambda (_buf cat) (setq seen cat) (selected-window))))
      (deb-packaging-infra-schroots))
    (should (eq seen 'list))))

(ert-deftest deb-packaging-test-display/infra-list-sessions-parses ()
  "Only session: lines become session names."
  (cl-letf (((symbol-function 'deb-packaging-detect--call-process-string)
             (lambda (_prog &rest _args)
               "session:alpha\nchroot:noble-amd64\nsource:noble-amd64\nsession:beta")))
    (should (equal (deb-packaging-infra--list-sessions) '("alpha" "beta")))))

(ert-deftest deb-packaging-test-display/infra-sessions-appear-as-rows ()
  "Active sessions are appended to the schroots list as session rows."
  (cl-letf (((symbol-function 'deb-packaging-infra--list-schroots)
             (lambda () nil))
            ((symbol-function 'deb-packaging-infra--list-sessions)
             (lambda () '("stonking-amd64-abc"))))
    (with-temp-buffer
      (deb-packaging-infra-schroots-mode)
      (deb-packaging-infra-refresh-schroots)
      (should (equal (car (nth 0 tabulated-list-entries))
                     '(:session . "stonking-amd64-abc"))))))

(ert-deftest deb-packaging-test-display/infra-end-session-at-point ()
  "Ending a session row runs schroot -e -c NAME."
  (let (calls)
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional _infile _dest _display &rest args)
                 (push (cons program args) calls)
                 0))
              ((symbol-function 'y-or-n-p) #'always)
              ((symbol-function 'deb-packaging-infra-refresh-schroots) #'ignore))
      (with-temp-buffer
        (deb-packaging-infra-schroots-mode)
        (setq tabulated-list-entries
              (list (list '(:session . "sess-1")
                          (vector "sess-1" "active session" ""))))
        (tabulated-list-init-header)
        (tabulated-list-print t)
        (goto-char (point-min))
        (search-forward "sess-1")
        (deb-packaging-infra-end-session-at-point)))
    (should (equal (car calls) '("schroot" "-e" "-c" "sess-1")))))

(ert-deftest deb-packaging-test-display/infra-ppas-displays-list ()
  "The PPAs list displays via the list category."
  (let (seen)
    (cl-letf (((symbol-function 'deb-packaging-infra-refresh-ppas) #'ignore)
              ((symbol-function 'deb-packaging-display-buffer)
               (lambda (_buf cat) (setq seen cat) (selected-window))))
      (deb-packaging-infra-ppas))
    (should (eq seen 'list))))

(ert-deftest deb-packaging-test-display/ppa-tests-show-displays-report ()
  "The PPA test report displays via the report category."
  (deb-packaging-test--with-package-tree '(:name "mypkg" :version "1.0-1")
    (let (seen)
      (cl-letf (((symbol-function 'deb-packaging-commands--resolve-ppa)
                 (lambda (_) "ppa:test/ppa"))
                ((symbol-function 'deb-packaging-ppa-save) #'ignore)
                ((symbol-function 'deb-packaging-ppa-tests--fetch) #'ignore)
                ((symbol-function 'deb-packaging-display-buffer)
                 (lambda (_buf cat) (setq seen cat) (selected-window))))
        (deb-packaging-ppa-tests-show '("--ppa=ppa:test/ppa"))
        (should (eq seen 'report))))))

(provide 'deb-packaging-test-display)
;;; deb-packaging-test-display.el ends here
