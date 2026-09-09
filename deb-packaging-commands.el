;;; deb-packaging-commands.el --- Command execution for deb-packaging -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Build and execute commands for Debian packaging tools.
;; Each public runner takes an ARGS list from a transient and passes the
;; flags through to the tool.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'comint)
(require 'ansi-color)
(require 'compile)
(require 'transient)
(require 'deb-packaging-detect)
(require 'deb-packaging-config)
(require 'deb-packaging-ppa)
(require 'deb-packaging-regen)
(require 'deb-packaging-display)

(declare-function deb-packaging-infra--ppa-owner "deb-packaging-infra")
(declare-function deb-packaging-infra--ppa-name "deb-packaging-infra")
(declare-function deb-packaging-infra--list-ppas "deb-packaging-infra")
(declare-function deb-packaging-repos-save "deb-packaging-repos")

;;; Core Execution

(defun deb-packaging-commands--filter-osc-sequences (string)
  "Filter OSC and terminal query sequences from STRING for comint."
  (setq string (replace-regexp-in-string "\e\\][^\a\e]*\\(\a\\|\e\\\\\\)" "" string))
  (setq string (replace-regexp-in-string "\e\\[\\?[0-9;]*[a-zA-Z]" "" string))
  string)

;;; Run-outcome tracking

(defvar deb-packaging-commands--run-history nil
  "Alist mapping run KEY to its most recent record plist.
Keys: :status (`running'/`success'/`failure'), :time, :buffer, :summary.
Session-only.")

(defun deb-packaging-commands--record-run (key status buf-name &optional summary)
  "Store a run record for KEY with STATUS, BUF-NAME, and optional SUMMARY.
The :time stamp marks the start of a run: closing out an in-flight
`running' record keeps its start time; any other record stamps now,
so a re-run refreshes the displayed time."
  (when key
    (let* ((existing (alist-get key deb-packaging-commands--run-history))
           (in-flight (and existing
                           (eq (plist-get existing :status) 'running))))
      (setf (alist-get key deb-packaging-commands--run-history)
            (list :status status
                  :time (if in-flight
                            (plist-get existing :time)
                          (format-time-string "%H:%M:%S"))
                  :buffer buf-name
                  :summary summary)))))

(defun deb-packaging-commands-run-record (key)
  "Return the most recent run record plist for KEY, or nil."
  (alist-get key deb-packaging-commands--run-history))

(defun deb-packaging-commands--run-summary (key)
  "Return the summary plist for KEY's last run, or nil."
  (plist-get (deb-packaging-commands-run-record key) :summary))

(defun deb-packaging-commands--notify-status-refresh ()
  "Refresh the status buffer if it is live."
  (when (fboundp 'deb-packaging-status--maybe-refresh)
    (deb-packaging-status--maybe-refresh)))

(defun deb-packaging-commands--parse-lint-summary (buf-name)
  "Parse lintian counts from comint buffer BUF-NAME.
Return plist (:error N :warning N :info N) from E:/W:/I: line prefixes."
  (when (buffer-live-p (get-buffer buf-name))
    (with-current-buffer buf-name
      (let ((errors 0) (warnings 0) (infos 0))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^\\([EWI]\\):" nil t)
            (pcase (match-string 1)
              ("E" (cl-incf errors))
              ("W" (cl-incf warnings))
              ("I" (cl-incf infos)))))
        (list :error errors :warning warnings :info infos)))))

(defun deb-packaging-commands--parse-ubuntu-lint-summary (buf-name)
  "Parse ubuntu-lint counts from comint buffer BUF-NAME.
Return plist (:ok N :skip N :warn N :error N :fail N) from the final
`Summary: ran N lint checks (...)' line."
  (when (buffer-live-p (get-buffer buf-name))
    (with-current-buffer buf-name
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward
               "^Summary: ran [0-9]+ lint checks (\\([^)]*\\))" nil t)
          (let ((ok 0) (skip 0) (warn 0) (error 0) (fail 0))
            (dolist (pair (split-string (match-string 1) ", " t))
              (pcase (split-string pair ": " t)
                (`("OK" ,n)    (setq ok    (string-to-number n)))
                (`("SKIP" ,n)  (setq skip  (string-to-number n)))
                (`("WARN" ,n)  (setq warn  (string-to-number n)))
                (`("ERROR" ,n) (setq error (string-to-number n)))
                (`("FAIL" ,n)  (setq fail  (string-to-number n)))))
            (list :ok ok :skip skip :warn warn :error error :fail fail)))))))

(defun deb-packaging-commands--parse-sbuild-summary (buf-name)
  "Parse a kept schroot session from sbuild buffer BUF-NAME.
Return plist (:kept-session NAME), or nil when the session was ended."
  (when (buffer-live-p (get-buffer buf-name))
    (with-current-buffer buf-name
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^Keeping session: \\(\\S-+\\)" nil t)
          (list :kept-session (match-string 1)))))))

(defun deb-packaging-commands--run-summary-parser (key)
  "Return the summary parser for run KEY, or nil."
  (pcase key
    ((or 'lintian-source 'lintian-binary) #'deb-packaging-commands--parse-lint-summary)
    ('ubuntu-lint #'deb-packaging-commands--parse-ubuntu-lint-summary)
    ('sbuild #'deb-packaging-commands--parse-sbuild-summary)
    (_ nil)))

(defun deb-packaging-commands--wrap-sentinel (proc action)
  "Wrap PROC's sentinel, calling ACTION as (ACTION PROC EVENT) after exit.
Original sentinel is preserved and runs first."
  (let ((old (process-sentinel proc)))
    (set-process-sentinel
     proc
     (lambda (p event)
       (when (functionp old)
         (funcall old p event))
       (when (memq (process-status p) '(exit signal))
         (funcall action p event))))))

(defun deb-packaging-commands--attach-run-sentinel (proc key buf-name)
  "Attach a sentinel to PROC that records the outcome for KEY.
Lint-style keys also get findings counts stored as :summary."
  (deb-packaging-commands--wrap-sentinel
   proc
   (lambda (p _event)
     (let* ((status (if (and (eq (process-status p) 'exit)
                             (zerop (process-exit-status p)))
                        'success
                      'failure))
            (parser (deb-packaging-commands--run-summary-parser key))
            (summary (when parser (funcall parser buf-name))))
       (deb-packaging-commands--record-run key status buf-name summary)
       (deb-packaging-commands--notify-status-refresh)))))

(defun deb-packaging-commands--run-command (name args &optional dir key buffer-dir)
  "Run command in a comint buffer.
NAME forms the buffer name; ARGS is the full command list.
DIR sets the process working directory.  KEY (a symbol) enables run tracking.
BUFFER-DIR sets the buffer's `default-directory' when it differs from DIR,
so commands run from the log buffer (e.g. `deb-packaging-status') stay in
the package tree when the process itself must run in the parent build dir."
  (let* ((timestamp (format-time-string "%H:%M:%S"))
         (buf-name (format "*deb-%s-%s*" name timestamp))
         (cmd (mapconcat #'shell-quote-argument args " ")))
    ;; Bind `default-directory' only around `make-comint-in-buffer'.  A leaked
    ;; binding would make the status refresh scan the parent dir and report
    ;; "not in a package".
    (let ((default-directory (or dir default-directory)))
      (make-comint-in-buffer name buf-name shell-file-name nil
                             shell-command-switch cmd))
    (with-current-buffer buf-name
      (when-let ((buf-dir (or buffer-dir dir)))
        (setq default-directory buf-dir))
      (setq deb-packaging-display-category 'output)
      (add-hook 'comint-preoutput-filter-functions
                #'deb-packaging-commands--filter-osc-sequences nil t)
      (add-hook 'comint-output-filter-functions
                #'ansi-color-process-output nil t))
    (when key
      (deb-packaging-commands--record-run key 'running buf-name)
      (when-let* ((proc (get-buffer-process buf-name)))
        (deb-packaging-commands--attach-run-sentinel proc key buf-name))
      (deb-packaging-commands--notify-status-refresh))
    (deb-packaging-display-buffer buf-name 'output)
    buf-name))

;;; Compilation wrapper

(defun deb-packaging-commands--refresh-buffer (mode refresh-fn)
  "Call REFRESH-FN in the live buffer derived from MODE, if any.
REFRESH-FN may be a symbol that is not yet loaded; it is only called
when `fboundp'."
  (when (fboundp refresh-fn)
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (derived-mode-p mode)
            (funcall refresh-fn)))))))

(defun deb-packaging-commands--after-compile (buf action &optional on-failure)
  "Call ACTION (no args) when the compilation in BUF finishes successfully.
ON-FAILURE (no args) runs instead on a non-zero exit; the buffer already
shows the error, so it is for follow-up context, not the error itself.
One-shot `compilation-finish-functions' hook; the killed-buffer event (a
reused *compilation* buffer kills the first run and both hooks see it)
runs neither, so the affected row may need a manual refresh."
  (letrec ((hook (lambda (finished-buf msg)
                   (when (eq finished-buf buf)
                     (remove-hook 'compilation-finish-functions hook)
                     (cond ((string-match-p "finished" msg)
                            (funcall action))
                           ((and on-failure
                                 (not (string-match-p "\\`killed" msg)))
                            (funcall on-failure)))))))
    (add-hook 'compilation-finish-functions hook)))

(defun deb-packaging-commands--compile (cmd)
  "Run CMD via `compile' under the package's process conventions.
No save-buffer prompts, a running compilation is killed without asking,
and the window follows the package display policy.  Returns the
compilation buffer (nil under mocks)."
  (let ((compilation-ask-about-save nil)
        (compilation-always-kill t)
        (display-buffer-overriding-action
         (deb-packaging-display--action 'output)))
    (let ((buf (compile cmd)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq deb-packaging-display-category 'output)))
      buf)))

(defun deb-packaging-commands-kill-output-buffers ()
  "Kill all build-output buffers in bulk, after one confirmation.
Targets every live buffer carrying the package's \\='output display
category (comint run buffers and compilation buffers alike); shell
buffers are left alone.  Timestamped output buffers are kept around
deliberately so builds can be compared; this clears them on demand.
Any processes still writing into the buffers are stopped."
  (interactive)
  (let ((bufs (seq-filter
               (lambda (buf)
                 (eq (buffer-local-value
                      'deb-packaging-display-category buf)
                     'output))
               (buffer-list))))
    (if (null bufs)
        (message "No build-output buffers")
      (when (y-or-n-p (format "Kill %d build-output buffer%s? "
                              (length bufs)
                              (if (= (length bufs) 1) "" "s")))
        (dolist (buf bufs)
          ;; The count prompt is the confirmation; stop live processes
          ;; without a second per-buffer query.
          (when-let ((proc (get-buffer-process buf)))
            (set-process-query-on-exit-flag proc nil)
            (delete-process proc))
          (kill-buffer buf))
        (message "Killed %d build-output buffer%s"
                 (length bufs)
                 (if (= (length bufs) 1) "" "s"))))))

;;; dpkg-buildpackage

(defun deb-packaging-commands-source-build (&optional args)
  "Run dpkg-buildpackage with ARGS from the source-build transient."
  (interactive (list (transient-args 'deb-packaging-commands-source-build-transient)))
  (let ((pkg-dir (deb-packaging-detect--find-package-dir nil t)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (deb-packaging-commands--run-command "source-build"
                                 (cons "dpkg-buildpackage" args)
                                 pkg-dir
                                 'source-build)))

;;;###autoload
(defun deb-packaging-commands-export-orig ()
  "Fetch the orig tarball with `git ubuntu export-orig'."
  (interactive)
  (let ((pkg-dir (deb-packaging-detect--find-package-dir nil t)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (unless (executable-find "git-ubuntu")
      (user-error "git-ubuntu not found in `exec-path'"))
    (deb-packaging-commands--run-command "export-orig"
                                 '("git" "ubuntu" "export-orig")
                                 pkg-dir
                                 'export-orig)))

;;; lintian

(defconst deb-packaging-commands--lintian-arg-prefixes
  '("-i" "-I" "-P" "--pedantic" "--tag-display-limit=" "--color=")
  "Lintian arg prefixes for `deb-packaging-commands--filter-args'.
Entries ending in `=' match by prefix; bare entries match exactly.")

(defconst deb-packaging-commands--ubuntu-lint-arg-prefixes
  '("--verbose" "--json" "--all=")
  "ubuntu-lint arg prefixes for `deb-packaging-commands--filter-args'.")

(defun deb-packaging-commands--filter-args (args prefixes)
  "Return the members of ARGS matching any prefix in PREFIXES.
A prefix ending in `=' matches by string prefix; a bare prefix matches
exactly.  Lets lintian and ubuntu-lint share one transient."
  (cl-remove-if-not
   (lambda (a)
     (cl-some
      (lambda (p)
        (if (string-suffix-p "=" p)
            (string-prefix-p p a)
          (string= p a)))
      prefixes))
   args))

(defun deb-packaging-commands--run-lintian (targets args &optional key)
  "Run lintian on TARGETS (file paths) with ARGS, tracked as KEY.
ARGS is filtered to lintian's own flags.  TARGETS may be a .dsc, some
.debs, or a mix."
  (let ((pkg-dir (deb-packaging-detect--find-package-dir nil t)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let ((parent-dir (deb-packaging-detect--parent-dir pkg-dir))
          (lint-args (deb-packaging-commands--filter-args
                      args
                      deb-packaging-commands--lintian-arg-prefixes)))
      (deb-packaging-commands--run-command "lintian"
                                  (append (list "lintian") lint-args targets)
                                  parent-dir
                                  key
                                  pkg-dir))))

(defun deb-packaging-commands-lintian-source (&optional args)
  "Run lintian on the source .dsc file with ARGS."
  (interactive (list (transient-args 'deb-packaging-lint-transient)))
  (let* ((pkg-dir (deb-packaging-detect--find-package-dir nil t))
         (info (deb-packaging-detect--package-info pkg-dir))
         (parent-dir (deb-packaging-detect--parent-dir pkg-dir))
         (artifacts (deb-packaging-detect--scan-artifacts
                     (nth 0 info) (nth 1 info) parent-dir))
         (dsc (alist-get 'dsc artifacts)))
    (unless dsc
      (user-error "No .dsc file found; run a source build first"))
    (deb-packaging-commands--run-lintian (list dsc) args 'lintian-source)))

(defun deb-packaging-commands--lintian-binary-artifacts ()
  "Return the .deb files for the current package.
Signal `user-error' if none exist."
  (let* ((pkg-dir (deb-packaging-detect--find-package-dir nil t))
         (info (deb-packaging-detect--package-info pkg-dir))
         (parent-dir (when pkg-dir (deb-packaging-detect--parent-dir pkg-dir)))
         (artifacts (when info
                      (deb-packaging-detect--scan-artifacts
                       (nth 0 info) (nth 1 info) parent-dir)))
         (debs (alist-get 'debs artifacts)))
    (unless debs
      (user-error "No .deb files found; run a binary build first"))
    debs))

(defun deb-packaging-commands-lintian-binary (&optional args)
  "Run lintian on all .deb files with ARGS."
  (interactive (list (transient-args 'deb-packaging-lint-transient)))
  (let ((debs (deb-packaging-commands--lintian-binary-artifacts)))
    (deb-packaging-commands--run-lintian debs args 'lintian-binary)))

(defun deb-packaging-commands-lintian-binary-one (&optional args)
  "Run lintian on one .deb with ARGS, prompting for which."
  (interactive (list (transient-args 'deb-packaging-lint-transient)))
  (let* ((debs (deb-packaging-commands--lintian-binary-artifacts))
         (target (completing-read "Deb to lint: " debs nil t)))
    (deb-packaging-commands--run-lintian (list target) args 'lintian-binary)))

;;; ubuntu-lint

(defun deb-packaging-commands--ubuntu-lint-context-args (mode pkg-dir)
  "Return ubuntu-lint context flags for MODE rooted at PKG-DIR.
MODE is `changes' (default), `source-dir', or `changelog'.  Default adds
--source-dir and --changes-file when a source .changes exists."
  (pcase mode
    ("source-dir" (list "--source-dir" pkg-dir))
    ("changelog" (list "--changelog"
                       (expand-file-name "debian/changelog" pkg-dir)))
    (_
     (let* ((info (deb-packaging-detect--package-info pkg-dir))
            (name (nth 0 info))
            (version (nth 1 info))
            (parent-dir (deb-packaging-detect--parent-dir pkg-dir))
            (artifacts (when info
                         (deb-packaging-detect--scan-artifacts name version parent-dir)))
            (changes (alist-get 'source-changes artifacts)))
       (if changes
           (list "--source-dir" pkg-dir "--changes-file" changes)
         (list "--source-dir" pkg-dir))))))

(defun deb-packaging-commands-ubuntu-lint (&optional args)
  "Run ubuntu-lint with ARGS from the lint transient.
ARGS is filtered to ubuntu-lint's own flags.  `--context=MODE' selects the
context source (`changes' by default, or `source-dir' / `changelog')."
  (interactive (list (transient-args 'deb-packaging-lint-transient)))
  (let ((pkg-dir (deb-packaging-detect--find-package-dir nil t)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let* ((mode (or (transient-arg-value "--context=" args) "changes"))
           (ubuntu-args (deb-packaging-commands--filter-args
                         args
                         deb-packaging-commands--ubuntu-lint-arg-prefixes))
           (context-args (deb-packaging-commands--ubuntu-lint-context-args mode pkg-dir)))
      (deb-packaging-commands--run-command
       "ubuntu-lint"
       (append (list "ubuntu-lint") ubuntu-args context-args)
       pkg-dir
       'ubuntu-lint))))

;;; sbuild

(defvar deb-packaging-commands-sbuild-variants
  '(("proposed"
     . "deb http://archive.ubuntu.com/ubuntu/ %s-proposed main"))
  "Alist of short name to extra-repository template for sbuild (%s = distro).
Completion candidates for the binary-build --extra-repository option.
Launchpad \"ppa:owner/name\" values need no entry; they expand
automatically (see `deb-packaging-commands--expand-extra-repo').")

(defun deb-packaging-commands--ppa-repo-line (ppa distro)
  "Expand PPA (a \"ppa:owner/name\" string) into a sbuild repo line for DISTRO.
Return nil if PPA is not a recognisable ppa: address."
  (let ((owner (deb-packaging-infra--ppa-owner ppa))
        (name (deb-packaging-infra--ppa-name ppa)))
    (when (and owner name)
      (format "deb [trusted=yes] http://ppa.launchpadcontent.net/%s/%s/ubuntu/ %s main"
              owner name distro))))

(defun deb-packaging-commands--ppa-series-published-p (ppa distro)
  "Return whether PPA publishes a DISTRO series.
t means the Release file exists (HTTP 200); nil means definitively not
(403 for an empty/deleted PPA, 404 for an unpublished series);
\\='unknown means the probe could not answer (curl absent, timeout, 5xx)
and callers should fail open."
  (let ((owner (deb-packaging-infra--ppa-owner ppa))
        (name (deb-packaging-infra--ppa-name ppa)))
    (if (not (and owner name))
        'unknown
      (let ((code (deb-packaging-commands--probe-http-code
                   (format "http://ppa.launchpadcontent.net/%s/%s/ubuntu/dists/%s/Release"
                           owner name distro))))
        (pcase code
          ("200" t)
          ((or "403" "404") nil)
          (_ 'unknown))))))

(defun deb-packaging-commands--probe-http-code (url)
  "Return the HTTP status code string for a HEAD request to URL, or nil.
Nil when curl is missing, times out, or errors; the caller decides how
to treat an unanswerable probe."
  (let ((code (with-output-to-string
                (with-current-buffer standard-output
                  ;; 10s cap: a hanging probe must not stall dispatch.
                  (condition-case nil
                      (call-process "curl" nil t nil
                                    "-s" "-o" "/dev/null" "-w" "%{http_code}"
                                    "--head" "--max-time" "10" url)
                    (file-missing nil))))))
    (let ((trimmed (string-trim code)))
      (if (string-match-p "\\`[0-9][0-9][0-9]\\'" trimmed)
          trimmed
        nil))))

(defun deb-packaging-commands--expand-extra-repo (value distro)
  "Expand VALUE into an extra-repository string for DISTRO.
A `deb-packaging-commands-sbuild-variants' key expands its template; a
\"ppa:owner/name\" address expands to a Launchpad repo line; anything else
is returned unchanged."
  (if-let ((template (cdr (assoc value deb-packaging-commands-sbuild-variants))))
      (format template distro)
    (if (string-prefix-p "ppa:" value)
        (or (deb-packaging-commands--ppa-repo-line value distro) value)
      value)))

(defun deb-packaging-commands-sbuild (&optional args)
  "Run sbuild with ARGS from the binary-build transient.
The --dist chroot selection always comes from the changelog."
  (interactive (list (transient-args 'deb-packaging-binary-build-transient)))
  (let ((pkg-dir (deb-packaging-detect--find-package-dir nil t)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let* ((info (deb-packaging-detect--package-info pkg-dir))
           (parent-dir (deb-packaging-detect--parent-dir pkg-dir))
           (artifacts (when info
                        (deb-packaging-detect--scan-artifacts
                         (nth 0 info) (nth 1 info) parent-dir)))
           (dsc-file (alist-get 'dsc artifacts)))
      (unless dsc-file
        (user-error "No .dsc file found; run a source build first"))
      (let* ((distro (deb-packaging-config--effective-distro))
             (repo-args (cl-remove-if-not
                         (lambda (a) (string-prefix-p "--extra-repository=" a))
                         args))
             (extra-repo-arg
              (mapcar (lambda (a)
                        (concat "--extra-repository="
                                (deb-packaging-commands--expand-extra-repo
                                 (string-remove-prefix "--extra-repository=" a)
                                 distro)))
                      repo-args))
             (passthrough (cl-remove-if
                           (lambda (a) (string-prefix-p "--extra-repository=" a))
                           args)))
        ;; Pre-flight ppa: extra-repos: a PPA with no series for this
        ;; distro kills apt-get update minutes into the build; fail at
        ;; dispatch with the reason instead.  Only a definitive
        ;; negative (nil) blocks; an unanswerable probe ('unknown)
        ;; fails open.
        (let ((dead nil))
          (dolist (a repo-args)
            (let ((entry (string-remove-prefix "--extra-repository=" a)))
              (when (string-prefix-p "ppa:" entry)
                (when (null (deb-packaging-commands--ppa-series-published-p
                             entry distro))
                  (push entry dead)))))
          (when dead
            (user-error
             "PPA%s not published for %s (403 = empty/deleted PPA, 404 = series never published):
  %s
Remove %s from the binary-build -e menu, or publish the series."
             (if (cdr dead) "s" "") distro (string-join (nreverse dead) "\n  ")
             (if (cdr dead) "them" "it"))))
        (when (nth 0 info)
          (deb-packaging-repos-save
           (nth 0 info) distro
           (mapcar (lambda (a) (string-remove-prefix "--extra-repository=" a))
                   repo-args)))
        (deb-packaging-commands--run-command
         "sbuild"
         (append (list "sbuild")
                 (list (format "--dist=%s" distro))
                 passthrough
                 extra-repo-arg
                 (list dsc-file))
         parent-dir
         'sbuild
         pkg-dir)))))

;;; autopkgtest

(defvar deb-packaging-commands-test-runners
  '(("lxd"  . "autopkgtest/ubuntu/%s/amd64")
    ("qemu" . "/var/lib/adt-images/autopkgtest-%s-amd64.img"))
  "Alist of runner name to image path template (%s = distro).
Also the source of --runner completion.  For Debian, add entries like
(\"lxd\" . \"autopkgtest/debian/%s/amd64\").")

(defvar deb-packaging-commands-test-build-hints
  '(("lxd"  . "autopkgtest-build-lxd ubuntu-daily:%s")
    ("qemu" . "autopkgtest-buildvm-ubuntu-cloud -r %s"))
  "Alist of runner name to image-build command template (%s = distro).
Shown when a test image is missing.")

(defun deb-packaging-commands--runner-choices ()
  "Return the configured autopkgtest runner names from
`deb-packaging-commands-test-runners'."
  (mapcar #'car deb-packaging-commands-test-runners))

(defun deb-packaging-commands--lxd-image-exists-p (image)
  "Return non-nil if LXD IMAGE exists locally.
Nil when lxc is not installed (the image is then not available either)."
  (ignore-errors
    (zerop (call-process "lxc" nil nil nil "image" "info" image))))

(defun deb-packaging-commands--test-image-info (&optional runner distro)
  "Return a plist describing the test image for RUNNER and DISTRO.
RUNNER defaults to \"lxd\", DISTRO to `deb-packaging-config--effective-distro'.
Keys: :runner, :image, :exists."
  (let* ((runner (or runner "lxd"))
         (distro (or distro (deb-packaging-config--effective-distro)))
         (template (cdr (assoc runner deb-packaging-commands-test-runners)))
         (image (when template (format template distro)))
         (exists (when image
                   (cond
                    ((equal runner "lxd")
                     (deb-packaging-commands--lxd-image-exists-p image))
                    ((equal runner "qemu")
                     (file-exists-p image))
                    (t nil)))))
    (list :runner runner :image image :exists exists)))

(defun deb-packaging-commands--test-image-build-hint (runner distro)
  "Return the command string to build a missing test image for RUNNER, DISTRO.
Return nil if RUNNER has no registered hint."
  (when-let ((template (cdr (assoc runner deb-packaging-commands-test-build-hints))))
    (format template distro)))

(defun deb-packaging-commands-autopkgtest (&optional args)
  "Run autopkgtest with ARGS from the test transient.
The test image's distro comes from the changelog."
  (interactive (list (transient-args 'deb-packaging-test-transient)))
  (let ((pkg-dir (deb-packaging-detect--find-package-dir nil t)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let* ((info (deb-packaging-detect--package-info pkg-dir))
           (parent-dir (deb-packaging-detect--parent-dir pkg-dir))
           (artifacts (when info
                        (deb-packaging-detect--scan-artifacts
                         (nth 0 info) (nth 1 info) parent-dir)))
           (debs (alist-get 'debs artifacts)))
      (unless debs
        (user-error "No .deb files found; run a binary build first"))
      (let* ((runner (or (transient-arg-value "--runner=" args)
                         "lxd"))
             (distro (deb-packaging-config--effective-distro))
             (image-info (deb-packaging-commands--test-image-info runner distro))
             (image (plist-get image-info :image))
             (image-exists (plist-get image-info :exists))
             (passthrough (cl-remove-if
                           (lambda (a)
                             (or (string-prefix-p "--runner=" a)
                                 (string-prefix-p "--ppa=" a)))
                           args)))
        (when (and image (not image-exists))
          (user-error "%s image '%s' not found.\nBuild it with:\n  %s"
                      (capitalize runner)
                      image
                      (or (deb-packaging-commands--test-image-build-hint runner distro)
                          "(unknown; add an entry to deb-packaging-commands-test-build-hints)")))
        (deb-packaging-commands--run-command
         "autopkgtest"
         (append (list "autopkgtest")
                 passthrough
                 debs
                 (list "." "--" runner)
                 (when image (list image)))
         pkg-dir
         'autopkgtest)))))

(defun deb-packaging-commands--resolve-ppa (args)
  "Return the --ppa= value from ARGS, prompting when unset.
Completion candidates come from `deb-packaging-infra--list-ppas'.
Signals `user-error' on empty input."
  (let ((ppa (transient-arg-value "--ppa=" args)))
    (if (and ppa (not (string-empty-p ppa)))
        ppa
      (let ((choice (completing-read "PPA: "
                                     (deb-packaging-infra--list-ppas)
                                     nil nil)))
        (if (or (null choice) (string-empty-p choice))
            (user-error "No PPA set")
          choice)))))

;;; PPA upload (dput)

(defun deb-packaging-commands-dput-upload (&optional args)
  "Upload source .changes to a PPA with dput.
ARGS comes from `deb-packaging-upload-transient'.  Prompts when no PPA is
set; the used PPA is saved per package+distro (changelog distro)."
  (interactive (list (transient-args 'deb-packaging-upload-transient)))
  (let* ((ppa (deb-packaging-commands--resolve-ppa args))
         (distro (deb-packaging-config--effective-distro)))
    (let* ((pkg-dir (deb-packaging-detect--find-package-dir nil t))
           (info (deb-packaging-detect--package-info pkg-dir))
           (name (nth 0 info))
           (version (nth 1 info))
           (parent-dir (when pkg-dir (deb-packaging-detect--parent-dir pkg-dir)))
           (artifacts (when (and name version parent-dir)
                        (deb-packaging-detect--scan-artifacts
                         name version parent-dir)))
           (changes (alist-get 'source-changes artifacts)))
      (unless changes
        (user-error "No source .changes file found; run a source build first"))
      (let ((cmd-args (list "dput" ppa changes)))
        (when name
          (deb-packaging-ppa-save name distro ppa))
        (deb-packaging-commands--run-command "dput" cmd-args
                                     (or parent-dir default-directory)
                                     'dput
                                     pkg-dir)))))

;;; Clean artifacts

(defun deb-packaging-commands-clean (&optional args)
  "Remove build artifacts with ARGS from `deb-packaging-commands-clean-transient'.
Moves files to trash from the output (parent) directory only."
  (interactive (list (transient-args 'deb-packaging-commands-clean-transient)))
  (let ((pkg-dir (deb-packaging-detect--find-package-dir nil t)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let* ((do-artifacts (member "--artifacts" args))
           (do-stale     (member "--stale"     args))
           (name (deb-packaging-detect--package-name pkg-dir))
           (version (deb-packaging-detect--package-version pkg-dir))
           (parent-dir (deb-packaging-detect--parent-dir pkg-dir))
           (file-version (deb-packaging-detect--version-to-filename version))
           (files nil))
      (when do-artifacts
        (let* ((prefixes (or (deb-packaging-detect--owned-package-prefixes pkg-dir)
                             (list name)))
               (prefix-regex
                (mapconcat (lambda (p)
                             (concat "^" (regexp-quote p) "_"
                                     (regexp-quote file-version)))
                           prefixes "\\|")))
          (dolist (f (directory-files parent-dir nil prefix-regex))
            (push (expand-file-name f parent-dir) files))))
      (when do-stale
        (let ((stale (deb-packaging-detect--scan-stale-artifacts
                      name version parent-dir pkg-dir)))
          (dolist (f stale)
            (push (expand-file-name f parent-dir) files))))
      (if (null files)
          (message "Nothing to clean")
        (dolist (f files)
          (when (file-exists-p f)
            (move-file-to-trash f)))
        (deb-packaging-commands--record-run 'clean 'success nil)
        (deb-packaging-commands--notify-status-refresh)
        (message "Moved %d file(s) to trash" (length files))))))

;;; Regenerate templated files

(defun deb-packaging-commands-regenerate ()
  "Regenerate templated files (e.g. debian/control.in -> debian/control).
Prompts for the shell command, prefilled with the last one used for this
package+distro; the command runs verbatim in the package directory, so
env vars and multi-step chains work.  The chosen command is saved."
  (interactive)
  (let ((pkg-dir (deb-packaging-detect--find-package-dir nil t)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let* ((name (deb-packaging-detect--package-name pkg-dir))
           (distro (deb-packaging-config--effective-distro))
           (stored (when name (deb-packaging-regen-load name distro)))
           (default (or stored
                        (deb-packaging-regen--default-command pkg-dir)))
           (command (read-shell-command "Regenerate command: " default)))
      (when (string-empty-p command)
        (user-error "No command given"))
      (when name
        (deb-packaging-regen-save name distro command))
      (deb-packaging-commands--run-command "regen"
                                  (list "sh" "-c" command)
                                  pkg-dir
                                  'regen))))

;;; Reset source tree

(defun deb-packaging-commands-reset (&optional args)
  "Reset the source tree with ARGS from `deb-packaging-commands-reset-transient'.
Pops quilt patches, removes .pc/, and/or removes debian/files."
  (interactive (list (transient-args 'deb-packaging-commands-reset-transient)))
  (let ((pkg-dir (deb-packaging-detect--find-package-dir nil t)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let* ((do-quilt (member "--quilt" args))
           (do-pc    (member "--pc"    args))
           (do-files (member "--files" args))
           (steps '())
           (desc '()))
      (when do-quilt
        (push "quilt pop -a 2>/dev/null || true" steps)
        (push "pop quilt" desc))
      (when do-pc
        (push "rm -rf .pc/" steps)
        (push "rm -rf .pc/" desc))
      (when do-files
        (push "rm -f debian/files" steps)
        (push "rm debian/files" desc))
      (if (null steps)
          (message "Nothing selected to reset")
        (let ((script (concat
                       (format "cd %s && " (shell-quote-argument pkg-dir))
                       (string-join (nreverse steps) " && ")
                       (format " && echo 'Reset complete (%s)'"
                               (string-join (nreverse desc) ", ")))))
          (deb-packaging-commands--run-command
           "reset"
           (list "sh" "-c" script)
           pkg-dir
           'reset))))))

(provide 'deb-packaging-commands)
;;; deb-packaging-commands.el ends here
