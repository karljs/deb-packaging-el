;;; deb-packaging-ppa-tests.el --- Parsed PPA autopkgtest report -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Runs `ppa tests -L' asynchronously, parses the output, and renders a
;; magit-section report with trigger actions.  -L makes the tool print
;; trigger URLs as plain text; the default OSC 8 hyperlinks do not survive
;; comint's OSC filtering.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magit-section)
(require 'url)
(require 'transient)
(require 'deb-packaging-detect)
(require 'deb-packaging-config)
(require 'deb-packaging-ppa)
(require 'deb-packaging-commands)
(require 'deb-packaging-display)

;;; Parsing

;; In the regexps below, `\S-' is "non-whitespace".  `\S+' would eat the
;; `+' as a syntax-class designator and match any single character.

(defconst deb-packaging-ppa-tests--osc8-re
  "\e\\]8;;[^\e]*\e\\\\\\([^\e]*\\)\e\\]8;;\e\\\\"
  "Regexp matching an OSC 8 hyperlink; group 1 is the visible text.")

(defun deb-packaging-ppa-tests--strip-osc8 (text)
  "Replace OSC 8 hyperlinks in TEXT with their visible text."
  (replace-regexp-in-string deb-packaging-ppa-tests--osc8-re "\\1" text))

(defun deb-packaging-ppa-tests--parse-trigger-line (line pub)
  "Fold trigger LINE into publication plist PUB (mutated)."
  (when (string-match "^    \\+ @\\([^ :]+\\): \\(\\S-+\\)" line)
    (let* ((arch (match-string 1 line))
           (url (match-string 2 line))
           (entry (assoc arch (plist-get pub :arches))))
      (unless entry
        (setq entry (cons arch (list :basic nil :all-proposed nil)))
        (setf (plist-get pub :arches)
              (append (plist-get pub :arches) (list entry))))
      (if (string-match-p "all-proposed=1" url)
          (setf (plist-get (cdr entry) :all-proposed) url)
        (setf (plist-get (cdr entry) :basic) url)))))

(defun deb-packaging-ppa-tests--parse (output)
  "Parse `ppa tests -L' OUTPUT.
Return a plist with :triggers, :results, :running, :waiting."
  (let ((triggers nil)
        (results nil)
        (running nil)
        (waiting nil)
        (section nil)
        (pub nil)
        (result nil))
    (dolist (raw (split-string
                  (deb-packaging-ppa-tests--strip-osc8 output) "\n"))
      (let ((line (string-trim-right raw)))
        (cond
         ((string-prefix-p "* Triggers:" line) (setq section 'triggers))
         ((string-prefix-p "* Results:" line)  (setq section 'results))
         ((string-prefix-p "* Running:" line)  (setq section 'running))
         ((string-prefix-p "* Waiting:" line)  (setq section 'waiting))
         (t
          (pcase section
            ('triggers
             (if (string-match "^  - Source \\(.+\\): \\(\\S-.+\\)$" line)
                 (let ((parts (save-match-data
                               (split-string (match-string 1 line) "/"))))
                   (when (= (length parts) 3)
                     (setq pub (list :series (nth 0 parts)
                                     :package (nth 1 parts)
                                     :version (nth 2 parts)
                                     :status (match-string 2 line)
                                     :arches nil))
                     (push pub triggers)))
               (when pub
                 (deb-packaging-ppa-tests--parse-trigger-line line pub))))
            ('results
             (cond
              ((string-match
                "^  - \\([^ ]+\\): \\([^ /]+\\)/\\([^ /]+\\)/\\([^ ]+\\) \\[\\([^]]+\\)\\]$"
                line)
               ;; Group header; stray bullets must not attach to the old entry.
               (setq result nil))
              ((string-match
                "^    \\+ \\([✅❌⛔]\\) \\(.+\\) on \\([^ ]+\\) for \\([^ ]+\\) +@ \\([^ ]+ [^ ]+\\) *$"
                line)
               (setq result
                     (list :source (match-string 2 line)
                           :series (match-string 3 line)
                           :arch (match-string 4 line)
                           :status (pcase (match-string 1 line)
                                     ("✅" 'pass)
                                     ("❌" 'fail)
                                     ("⛔" 'bad))
                           :timestamp (match-string 5 line)
                           :log-url nil
                           :subtests nil))
               (push result results))
              ((and result
                    (string-match "^      • Log: \\(\\S-+\\)" line))
               (setf (plist-get result :log-url) (match-string 1 line)))
              ((and result
                    (string-match
                     "^      • \\(.+?\\) +\\(PASS\\|FAIL\\|BAD\\|SKIP\\|FLAKY\\) +\\S-+ *$"
                     line))
               (setf (plist-get result :subtests)
                     (append (plist-get result :subtests)
                             (list (cons (match-string 1 line)
                                         (match-string 2 line))))))))
            ('running
             (when (string-prefix-p "  - " line)
               (push (string-trim (substring line 2)) running)))
            ('waiting
             (when (string-prefix-p "  - " line)
               (push (string-trim (substring line 2)) waiting))))))))
    (list :triggers (nreverse triggers)
          :results (nreverse results)
          :running (nreverse running)
          :waiting (nreverse waiting))))

(defun deb-packaging-ppa-tests--summary (parsed)
  "Return a :pass/:fail/:bad/:running/:waiting count plist for PARSED."
  (let ((pass 0) (fail 0) (bad 0))
    (dolist (r (plist-get parsed :results))
      (pcase (plist-get r :status)
        ('pass (cl-incf pass))
        ('fail (cl-incf fail))
        ('bad (cl-incf bad))))
    (list :pass pass
          :fail fail
          :bad bad
          :running (length (plist-get parsed :running))
          :waiting (length (plist-get parsed :waiting)))))

;;; Report buffer

(defvar-local deb-packaging-ppa-tests--ppa nil
  "PPA shown in the current report buffer.")

(defvar-local deb-packaging-ppa-tests--package nil
  "Source package filter of the current report buffer.")

(defvar-local deb-packaging-ppa-tests--distro nil
  "Release filter of the current report buffer.")

(defun deb-packaging-ppa-tests--buffer-name (ppa)
  "Return the report buffer name for PPA."
  (format "*deb-ppa-tests: %s*" ppa))

(defvar-keymap deb-packaging-ppa-tests-mode-map
  :doc "Keymap for `deb-packaging-ppa-tests-mode'."
  :parent magit-section-mode-map
  "t"   #'deb-packaging-ppa-tests-trigger-basic
  "T"   #'deb-packaging-ppa-tests-trigger-all-proposed
  "RET" #'deb-packaging-ppa-tests-open-log
  "g"   #'deb-packaging-ppa-tests-refresh
  "q"   #'quit-window)

(define-derived-mode deb-packaging-ppa-tests-mode magit-section-mode
  "Deb-PPA-Tests"
  "Major mode for the parsed PPA autopkgtest report."
  :interactive nil)

(defconst deb-packaging-ppa-tests--status-icons
  '((pass . "✅") (fail . "❌") (bad . "⛔")))

(defun deb-packaging-ppa-tests--insert-note (text)
  "Insert an indented, dimmed note TEXT."
  (insert (format "    %s\n" (propertize text 'font-lock-face 'shadow))))

(defun deb-packaging-ppa-tests--insert-results (results)
  "Insert the Results section for RESULTS."
  (magit-insert-section (deb-packaging-ppa-tests-results)
    (magit-insert-heading "Results")
    (magit-insert-section-body
      (if (null results)
          (deb-packaging-ppa-tests--insert-note "none")
        (dolist (r results)
          (let ((status (plist-get r :status)))
            (magit-insert-section
                (deb-packaging-ppa-tests-result nil (eq status 'pass))
              (magit-insert-heading
                (concat "  "
                        (cdr (assq status
                                   deb-packaging-ppa-tests--status-icons))
                        " "
                        (propertize
                         (format "%s on %s for %s @ %s"
                                 (plist-get r :source)
                                 (plist-get r :series)
                                 (plist-get r :arch)
                                 (plist-get r :timestamp))
                         'font-lock-face
                         (if (eq status 'pass) 'success 'error))))
              (magit-insert-section-body
                (dolist (st (plist-get r :subtests))
                  (insert (format "      %-36s %s\n"
                                  (car st)
                                  (propertize
                                   (cdr st)
                                   'font-lock-face
                                   (if (member (cdr st) '("PASS" "SKIP"))
                                       'success
                                     'error)))))
                (when-let ((url (plist-get r :log-url)))
                  (insert "      "
                          (propertize "Log: " 'font-lock-face 'shadow)
                          (propertize
                           url
                           'font-lock-face 'link
                           'deb-packaging-ppa-tests-log-url url)
                          "\n"))))))))))

(defun deb-packaging-ppa-tests--insert-triggers (triggers)
  "Insert the Triggers section for TRIGGERS."
  (magit-insert-section (deb-packaging-ppa-tests-triggers)
    (magit-insert-heading "Triggers")
    (magit-insert-section-body
      (if (null triggers)
          (deb-packaging-ppa-tests--insert-note "none")
        (dolist (pub triggers)
          (magit-insert-section (deb-packaging-ppa-tests-publication)
            (magit-insert-heading
              (format "  %s/%s/%s: %s"
                      (plist-get pub :series)
                      (plist-get pub :package)
                      (plist-get pub :version)
                      (plist-get pub :status)))
            (magit-insert-section-body
              (dolist (entry (plist-get pub :arches))
                (let ((arch (car entry))
                      (start (point)))
                  (insert (format "    %-8s %s   %s\n"
                                  arch
                                  (propertize "t: trigger basic"
                                              'font-lock-face 'shadow)
                                  (propertize "T: trigger all-proposed"
                                              'font-lock-face 'shadow)))
                  (add-text-properties
                   start (1- (point))
                   (list 'deb-packaging-ppa-tests-basic-url
                         (plist-get (cdr entry) :basic)
                         'deb-packaging-ppa-tests-all-proposed-url
                         (plist-get (cdr entry) :all-proposed)
                         'deb-packaging-ppa-tests-desc
                         (format "%s on %s/%s"
                                 (plist-get pub :package)
                                 (plist-get pub :series)
                                 arch))))))))))))

(defun deb-packaging-ppa-tests--insert-queue (title rows)
  "Insert a Running/Waiting section titled TITLE for ROWS, when non-nil."
  (when rows
    (magit-insert-section (deb-packaging-ppa-tests-queue)
      (magit-insert-heading title)
      (magit-insert-section-body
        (dolist (row rows)
          (insert "    " (propertize row 'font-lock-face 'shadow) "\n"))))))

(defun deb-packaging-ppa-tests--render (parsed ppa)
  "Render PARSED report for PPA into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (deb-packaging-ppa-tests-root)
      (magit-insert-heading (format "PPA tests: %s" ppa))
      (deb-packaging-ppa-tests--insert-results (plist-get parsed :results))
      (deb-packaging-ppa-tests--insert-triggers (plist-get parsed :triggers))
      (deb-packaging-ppa-tests--insert-queue
       "Running" (plist-get parsed :running))
      (deb-packaging-ppa-tests--insert-queue
       "Waiting" (plist-get parsed :waiting))))
  (goto-char (point-min)))

;;; Triggering

(defun deb-packaging-ppa-tests--trigger (prop what)
  "GET the trigger URL in text property PROP at point, after confirm.
WHAT (\"basic\"/\"all-proposed\") is used in prompts and messages."
  (let ((url (get-text-property (point) prop))
        (desc (get-text-property (point) 'deb-packaging-ppa-tests-desc)))
    (unless url
      (user-error "No %s trigger on this line" what))
    (when (y-or-n-p (format "Trigger %s test for %s? " what desc))
      (url-retrieve
       url
       (lambda (status)
         (if (plist-get status :error)
             (message "Trigger failed: %s" (plist-get status :error))
           (message "Triggered %s test for %s" what desc)))))))

(defun deb-packaging-ppa-tests-trigger-basic ()
  "Trigger the basic autopkgtest run for the trigger row at point."
  (interactive)
  (deb-packaging-ppa-tests--trigger 'deb-packaging-ppa-tests-basic-url
                                    "basic"))

(defun deb-packaging-ppa-tests-trigger-all-proposed ()
  "Trigger the all-proposed autopkgtest run for the trigger row at point."
  (interactive)
  (deb-packaging-ppa-tests--trigger
   'deb-packaging-ppa-tests-all-proposed-url "all-proposed"))

(defun deb-packaging-ppa-tests-open-log ()
  "Open the log URL at point in a browser."
  (interactive)
  (if-let ((url (get-text-property
                 (point) 'deb-packaging-ppa-tests-log-url)))
      (browse-url url)
    (user-error "No log URL at point")))

;;; Runner

(defun deb-packaging-ppa-tests--fetch (ppa name distro)
  "Run `ppa tests' for PPA/NAME/DISTRO; render the report when done."
  (let ((report-buf (get-buffer-create
                     (deb-packaging-ppa-tests--buffer-name ppa)))
        (out-buf (generate-new-buffer " *deb-ppa-tests-output*")))
    (with-current-buffer report-buf
      (unless (derived-mode-p 'deb-packaging-ppa-tests-mode)
        (deb-packaging-ppa-tests-mode))
      (setq deb-packaging-ppa-tests--ppa ppa
            deb-packaging-ppa-tests--package name
            deb-packaging-ppa-tests--distro distro)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Fetching tests for %s...\n" ppa)
                            'font-lock-face 'shadow))))
    (deb-packaging-commands--record-run 'ppa-tests 'running nil)
    (deb-packaging-commands--notify-status-refresh)
    (condition-case err
        (make-process
         :name "deb-ppa-tests"
         :buffer out-buf
         :command (append (list "ppa" "tests" "-L" ppa)
                          (when name (list "-p" name))
                          (list "-r" distro))
         :sentinel
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (unwind-protect
                 (deb-packaging-ppa-tests--fetch-done proc out-buf report-buf ppa)
               (kill-buffer out-buf)))))
      ;; Spawn itself can signal (e.g. ppa not installed); don't leak the
      ;; buffer or leave the run record stuck on `running'.
      (error (kill-buffer out-buf)
             (deb-packaging-commands--record-run 'ppa-tests 'failure nil)
             (deb-packaging-commands--notify-status-refresh)
             (signal (car err) (cdr err))))))

(defun deb-packaging-ppa-tests--fetch-done (proc out-buf report-buf ppa)
  "Handle `ppa tests' exit: parse and render, or dump raw output on failure."
  (if (and (eq (process-status proc) 'exit)
           (zerop (process-exit-status proc)))
      (let* ((parsed (deb-packaging-ppa-tests--parse
                      (with-current-buffer out-buf (buffer-string))))
             (summary (deb-packaging-ppa-tests--summary parsed)))
        (deb-packaging-commands--record-run
         'ppa-tests 'success (buffer-name report-buf) summary)
        ;; The user may have killed the report buffer while we ran.
        (when (buffer-live-p report-buf)
          (with-current-buffer report-buf
            (deb-packaging-ppa-tests--render parsed ppa))))
    (deb-packaging-commands--record-run
     'ppa-tests 'failure (buffer-name report-buf))
    (when (buffer-live-p report-buf)
      (with-current-buffer report-buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize (format "ppa tests failed for %s:\n\n" ppa)
                              'font-lock-face 'error))
          (insert-buffer-substring out-buf)))))
  (deb-packaging-commands--notify-status-refresh))

(defun deb-packaging-ppa-tests-refresh ()
  "Re-run `ppa tests' for the current report buffer."
  (interactive)
  (unless deb-packaging-ppa-tests--ppa
    (user-error "No PPA associated with this buffer"))
  (deb-packaging-ppa-tests--fetch
   deb-packaging-ppa-tests--ppa
   deb-packaging-ppa-tests--package
   deb-packaging-ppa-tests--distro))

;;;###autoload
(defun deb-packaging-ppa-tests-show (&optional args)
  "Show the parsed autopkgtest report for a PPA.
ARGS comes from `deb-packaging-test-transient'.  Prompts when no PPA is
set; the used PPA is saved per package+distro."
  (interactive (list (transient-args 'deb-packaging-test-transient)))
  (let ((pkg-dir (deb-packaging-detect--find-package-dir nil t)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let* ((effective-args (or args '()))
           (ppa (deb-packaging-commands--resolve-ppa effective-args))
           (distro (or (transient-arg-value "--dist=" effective-args)
                       (deb-packaging-config--effective-distro)))
           (name (deb-packaging-detect--package-name pkg-dir)))
      (when name
        (deb-packaging-ppa-save name distro ppa))
      (deb-packaging-ppa-tests--fetch ppa name distro)
      (deb-packaging-display-buffer
       (deb-packaging-ppa-tests--buffer-name ppa) 'report))))

(provide 'deb-packaging-ppa-tests)
;;; deb-packaging-ppa-tests.el ends here
