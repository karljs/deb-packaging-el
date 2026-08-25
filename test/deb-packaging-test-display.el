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
  "status/list/report use same-window; output/shell reuse-or-same-window."
  (dolist (cat '(status list report))
    (should (memq 'display-buffer-same-window
                  (car (deb-packaging-display--action cat)))))
  (dolist (cat '(output shell))
    (let ((fns (car (deb-packaging-display--action cat))))
      (should (memq 'deb-packaging-display--reuse-category-window fns))
      (should (memq 'display-buffer-same-window fns))
      (should-not (memq 'display-buffer-below-selected fns))
      (should-not (assq 'inhibit-same-window
                        (cadr (deb-packaging-display--action cat)))))))

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

(ert-deftest deb-packaging-test-display/output-displays-in-invoking-window ()
  "output replaces the invoking window and keeps it selected."
  (save-window-excursion
    (deb-packaging-test-display--with-marked-buffers ((buf 'output))
      (let ((start (selected-window)))
        (deb-packaging-display-buffer buf 'output)
        (should (eq (selected-window) start))
        (should (eq (window-buffer start) buf))))))

(ert-deftest deb-packaging-test-display/output-reuses-visible-category-window ()
  "A new output buffer goes to a visible output window, not the invoking one."
  (save-window-excursion
    (deb-packaging-test-display--with-marked-buffers
        ((old-buf 'output) (new-buf 'output))
      (let ((other (split-window (selected-window) nil 'below)))
        (set-window-buffer other old-buf)
        (deb-packaging-display-buffer new-buf 'output)
        (should (eq (window-buffer other) new-buf))))))

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

(ert-deftest deb-packaging-test-display/run-command-buffer-dir-decouples-process-dir ()
  "Log buffer keeps BUFFER-DIR as `default-directory' when the process
runs in DIR, so package detection works from the buffer."
  (deb-packaging-test--with-package-tree '(:name "mypkg" :version "1.0-1")
    (save-window-excursion
      (cl-letf (((symbol-function 'make-comint-in-buffer)
                 (lambda (_name buf-name _program &rest _args)
                   (get-buffer-create buf-name))))
        (let ((buf-name (deb-packaging-commands--run-command
                         "test" '("true") pkg-parent-dir nil pkg-dir)))
          (unwind-protect
              (progn
                (should (equal (buffer-local-value 'default-directory
                                                   (get-buffer buf-name))
                               pkg-dir))
                (should (equal (with-current-buffer buf-name
                                 (deb-packaging-detect--find-package-dir))
                               pkg-dir)))
            (kill-buffer buf-name)))))))

(ert-deftest deb-packaging-test-display/run-command-buffer-dir-defaults-to-dir ()
  "Without BUFFER-DIR the log buffer's `default-directory' stays DIR."
  (deb-packaging-test--with-package-tree '(:name "mypkg" :version "1.0-1")
    (save-window-excursion
      (cl-letf (((symbol-function 'make-comint-in-buffer)
                 (lambda (_name buf-name _program &rest _args)
                   (get-buffer-create buf-name))))
        (let ((buf-name (deb-packaging-commands--run-command
                         "test" '("true") pkg-parent-dir)))
          (unwind-protect
              (should (equal (buffer-local-value 'default-directory
                                                 (get-buffer buf-name))
                             pkg-parent-dir))
            (kill-buffer buf-name)))))))

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

;;; Schroots buffer: two magit-section sections

(defmacro deb-packaging-test-display--with-schroots-buffer (chroots sessions &rest body)
  "Render the schroots buffer with mocked lists, then run BODY in it.
CHROOTS is a list of plists as from `deb-packaging-infra--list-schroots';
SESSIONS a list of session name strings."
  (declare (indent 2) (debug (form form body)))
  `(cl-letf (((symbol-function 'deb-packaging-infra--list-schroots)
              (lambda () ,chroots))
             ((symbol-function 'deb-packaging-infra--list-sessions)
              (lambda () ,sessions)))
     (with-temp-buffer
       (deb-packaging-infra-schroots-mode)
       (deb-packaging-infra-refresh-schroots)
       ,@body)))

(ert-deftest deb-packaging-test-display/infra-schroots-two-sections ()
  "Sessions and chroots render as separate sections."
  (deb-packaging-test-display--with-schroots-buffer
      '((:name "noble-amd64" :description "Noble" :directory "/srv/noble"))
      '("noble-amd64-abc123")
    (should (string-match-p "Sessions (1)" (buffer-string)))
    (should (string-match-p "Chroots (1)" (buffer-string)))
    (goto-char (point-min))
    (search-forward "noble-amd64-abc123")
    (should (eq (oref (magit-current-section) type)
                'deb-packaging-infra-session))
    (should (equal (oref (magit-current-section) value)
                   "noble-amd64-abc123"))
    (goto-char (point-min))
    (search-forward "/srv/noble")
    (should (eq (oref (magit-current-section) type)
                'deb-packaging-infra-chroot))))

(ert-deftest deb-packaging-test-display/infra-sessions-section-hidden-when-none ()
  "No Sessions section is shown when there are no active sessions."
  (deb-packaging-test-display--with-schroots-buffer
      '((:name "noble-amd64" :description "Noble" :directory "/srv/noble"))
      nil
    (should-not (string-match-p "Sessions" (buffer-string)))
    (should (string-match-p "Chroots (1)" (buffer-string)))))

(ert-deftest deb-packaging-test-display/infra-schroots-empty-state ()
  "With no chroots and no sessions, an empty-state message is shown."
  (deb-packaging-test-display--with-schroots-buffer nil nil
    (should (string-match-p "No schroots found" (buffer-string)))))

(ert-deftest deb-packaging-test-display/infra-session-chroot-prefix-match ()
  "The parent chroot of a session is found by longest name prefix."
  (let ((chroots '((:name "noble-amd64") (:name "noble-amd64-debug"))))
    (should (equal (deb-packaging-infra--session-chroot
                    "noble-amd64-debug-abc123" chroots)
                   "noble-amd64-debug"))
    (should (equal (deb-packaging-infra--session-chroot
                    "noble-amd64-abc123" chroots)
                   "noble-amd64"))
    (should-not (deb-packaging-infra--session-chroot "unrelated-xyz" chroots))))

(ert-deftest deb-packaging-test-display/infra-end-session-at-point ()
  "With point on a session row, `e' ends that session."
  (let (calls)
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional _infile _dest _display &rest args)
                 (push (cons program args) calls)
                 0))
              ((symbol-function 'y-or-n-p) #'always))
      (deb-packaging-test-display--with-schroots-buffer nil '("sess-1")
        (goto-char (point-min))
        (search-forward "sess-1")
        (deb-packaging-infra-end-sessions)))
    (should (equal (car calls) '("schroot" "-e" "-c" "sess-1")))))

(ert-deftest deb-packaging-test-display/infra-end-sessions-in-region ()
  "With an active region over session rows, `e' ends them all."
  (let (calls)
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional _infile _dest _display &rest args)
                 (push (cons program args) calls)
                 0))
              ((symbol-function 'y-or-n-p) #'always))
      (deb-packaging-test-display--with-schroots-buffer nil '("sess-1" "sess-2")
        (goto-char (point-min))
        (search-forward "sess-1")
        (beginning-of-line)
        (set-mark (point))
        (activate-mark)
        (search-forward "sess-2")
        (deb-packaging-infra-end-sessions)))
    (should (equal (length calls) 2))
    (should (member '("schroot" "-e" "-c" "sess-1") calls))
    (should (member '("schroot" "-e" "-c" "sess-2") calls))))

(ert-deftest deb-packaging-test-display/infra-end-sessions-on-heading ()
  "With point on the Sessions heading, `e' ends all sessions."
  (let (calls)
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional _infile _dest _display &rest args)
                 (push (cons program args) calls)
                 0))
              ((symbol-function 'y-or-n-p) #'always))
      (deb-packaging-test-display--with-schroots-buffer nil '("sess-1" "sess-2")
        (goto-char (point-min))
        (search-forward "Sessions (2)")
        (deb-packaging-infra-end-sessions)))
    (should (equal (length calls) 2))))

(ert-deftest deb-packaging-test-display/infra-end-all-sessions ()
  "`E' ends every active session without a compile buffer."
  (let (calls compiles)
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &optional _infile _dest _display &rest args)
                 (push (cons program args) calls)
                 0))
              ((symbol-function 'compile)
               (lambda (&rest _) (push t compiles) nil))
              ((symbol-function 'y-or-n-p) #'always)
              ((symbol-function 'deb-packaging-infra--list-sessions)
               (lambda () '("sess-1" "sess-2"))))
      (deb-packaging-infra-end-all-sessions))
    (should-not compiles)
    (should (equal (length calls) 2))
    (should (member '("schroot" "-e" "-c" "sess-1") calls))
    (should (member '("schroot" "-e" "-c" "sess-2") calls))))

(ert-deftest deb-packaging-test-display/infra-update-schroot-at-point ()
  "With point on a chroot row, `u' updates just that chroot."
  (let (compiles)
    (cl-letf (((symbol-function 'compile)
               (lambda (cmd &rest _) (push cmd compiles) nil))
              ((symbol-function 'y-or-n-p) #'always))
      (deb-packaging-test-display--with-schroots-buffer
          '((:name "noble-amd64" :description "Noble" :directory "/srv/noble")
            (:name "stonking-amd64" :description "Stonk" :directory "/srv/stonk"))
          nil
        (goto-char (point-min))
        (search-forward "stonking-amd64")
        (deb-packaging-infra-update-schroots)))
    (should (equal compiles '("sbuild-update -udcar stonking-amd64")))))

(ert-deftest deb-packaging-test-display/infra-update-schroots-in-region ()
  "With an active region over chroot rows, `u' updates them in one compile."
  (let (compiles)
    (cl-letf (((symbol-function 'compile)
               (lambda (cmd &rest _) (push cmd compiles) nil))
              ((symbol-function 'y-or-n-p) #'always))
      (deb-packaging-test-display--with-schroots-buffer
          '((:name "noble-amd64" :description "Noble" :directory "/srv/noble")
            (:name "stonking-amd64" :description "Stonk" :directory "/srv/stonk"))
          nil
        (goto-char (point-min))
        (search-forward "noble-amd64")
        (beginning-of-line)
        (set-mark (point))
        (activate-mark)
        (search-forward "stonking-amd64")
        (deb-packaging-infra-update-schroots)))
    (should (= (length compiles) 1))
    (should (string-match-p "sbuild-update -udcar noble-amd64" (car compiles)))
    (should (string-match-p "sbuild-update -udcar stonking-amd64" (car compiles)))))

(ert-deftest deb-packaging-test-display/infra-update-all-schroots ()
  "`U' updates every chroot in one compile command."
  (let (compiles)
    (cl-letf (((symbol-function 'compile)
               (lambda (cmd &rest _) (push cmd compiles) nil))
              ((symbol-function 'y-or-n-p) #'always)
              ((symbol-function 'deb-packaging-infra--list-schroots)
               (lambda () '((:name "noble-amd64") (:name "stonking-amd64")))))
      (deb-packaging-infra-update-all-schroots))
    (should (= (length compiles) 1))
    (should (string-match-p "sbuild-update -udcar noble-amd64" (car compiles)))
    (should (string-match-p "sbuild-update -udcar stonking-amd64" (car compiles)))
    (should (string-match-p ";" (car compiles)))))

(ert-deftest deb-packaging-test-display/infra-delete-schroot-at-point ()
  "With point on a chroot row, `d' deletes that chroot."
  (let (ran)
    (cl-letf (((symbol-function 'deb-packaging-commands--run-command)
               (lambda (_name args &rest _) (setq ran args) nil))
              ((symbol-function 'yes-or-no-p) #'always))
      (deb-packaging-test-display--with-schroots-buffer
          '((:name "noble-amd64" :description "Noble" :directory "/srv/noble"
             :config-file "/etc/schroot/chroot.d/noble"))
          nil
        (goto-char (point-min))
        (search-forward "noble-amd64")
        (deb-packaging-infra-delete-schroot)))
    (should (string-match-p "rm -rf /srv/noble" (nth 2 ran)))))

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
