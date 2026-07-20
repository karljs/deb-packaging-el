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
(require 'transient)
(require 'deb-packaging-detect)
(require 'deb-packaging-config)
(require 'deb-packaging-ppa)

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
  "Store a run record for KEY with STATUS, BUF-NAME, and optional SUMMARY."
  (when key
    (let ((existing (alist-get key deb-packaging-commands--run-history)))
      (setf (alist-get key deb-packaging-commands--run-history)
            (list :status status
                  :time (or (and existing (plist-get existing :time))
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
          (let ((stats (make-vector 5 0))
                (order '(("OK" . 0) ("SKIP" . 1) ("WARN" . 2)
                         ("ERROR" . 3) ("FAIL" . 4))))
            (dolist (pair (split-string (match-string 1) ", " t))
              (let ((kv (split-string pair ": " t)))
                (when (= (length kv) 2)
                  (let ((idx (cdr (assoc (car kv) order))))
                    (when idx
                      (aset stats idx
                            (string-to-number (cadr kv))))))))
            (list :ok (aref stats 0)
                  :skip (aref stats 1)
                  :warn (aref stats 2)
                  :error (aref stats 3)
                  :fail (aref stats 4))))))))

(defun deb-packaging-commands--run-summary-parser (key)
  "Return the summary parser for run KEY, or nil."
  (pcase key
    ((or 'lintian-source 'lintian-binary) #'deb-packaging-commands--parse-lint-summary)
    ('ubuntu-lint #'deb-packaging-commands--parse-ubuntu-lint-summary)
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

(defun deb-packaging-commands--run-command (name args &optional dir key)
  "Run command in a comint buffer.
NAME forms the buffer name; ARGS is the full command list.
DIR sets the process working directory.  KEY (a symbol) enables run tracking."
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
      (when dir
        (setq default-directory dir))
      (add-hook 'comint-preoutput-filter-functions
                #'deb-packaging-commands--filter-osc-sequences nil t)
      (add-hook 'comint-output-filter-functions
                #'ansi-color-process-output nil t))
    (when key
      (deb-packaging-commands--record-run key 'running buf-name)
      (when-let* ((proc (get-buffer-process buf-name)))
        (deb-packaging-commands--attach-run-sentinel proc key buf-name))
      (deb-packaging-commands--notify-status-refresh))
    (pop-to-buffer buf-name)
    buf-name))

;;; dpkg-buildpackage

(defun deb-packaging-commands-source-build (&optional args)
  "Run dpkg-buildpackage with ARGS from the source-build transient."
  (interactive (list (transient-args 'deb-packaging-commands-source-build-transient)))
  (let ((pkg-dir (deb-packaging-detect--find-package-dir nil t)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (deb-packaging-commands--run-command "source-build"
                                (cons "dpkg-buildpackage" (or args '()))
                                pkg-dir
                                'source-build)))

;;; lintian

(defconst deb-packaging-commands--lintian-arg-prefixes
  '("-i" "-I" "-P" "--pedantic" "--tag-display-limit=" "--color=")
  "Lintian arg prefixes for `deb-packaging-commands--filter-args'.
Entries ending in `=' match by prefix; bare entries match exactly.")

(defconst deb-packaging-commands--ubuntu-lint-arg-prefixes
  '("--verbose" "--json" "--context=" "--all=")
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
                      (or args '())
                      deb-packaging-commands--lintian-arg-prefixes)))
      (deb-packaging-commands--run-command "lintian"
                                  (append (list "lintian") lint-args targets)
                                  parent-dir
                                  key))))

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
    (deb-packaging-commands--run-lintian (list dsc) (or args '()) 'lintian-source)))

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
    (deb-packaging-commands--run-lintian debs (or args '()) 'lintian-binary)))

(defun deb-packaging-commands-lintian-binary-one (&optional args)
  "Run lintian on one .deb with ARGS, prompting for which."
  (interactive (list (transient-args 'deb-packaging-lint-transient)))
  (let* ((debs (deb-packaging-commands--lintian-binary-artifacts))
         (target (completing-read "Deb to lint: " debs nil t)))
    (deb-packaging-commands--run-lintian (list target) (or args '()) 'lintian-binary)))

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
    (let* ((effective-args (or args '()))
           (mode (or (transient-arg-value "--context=" effective-args) "changes"))
           (ubuntu-args (deb-packaging-commands--filter-args
                         effective-args
                         deb-packaging-commands--ubuntu-lint-arg-prefixes))
           (passthrough (cl-remove-if
                         (lambda (a) (string-prefix-p "--context=" a))
                         ubuntu-args))
           (context-args (deb-packaging-commands--ubuntu-lint-context-args mode pkg-dir)))
      (deb-packaging-commands--run-command
       "ubuntu-lint"
       (append (list "ubuntu-lint") passthrough context-args)
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

(defun deb-packaging-commands--expand-extra-repo (value distro)
  "Expand VALUE into an extra-repository string for DISTRO.
A `deb-packaging-commands-sbuild-variants' key expands its template; a
\"ppa:owner/name\" address expands to a Launchpad repo line; anything else
is returned unchanged."
  (cond
   ((cdr (assoc value deb-packaging-commands-sbuild-variants))
    (format (cdr (assoc value deb-packaging-commands-sbuild-variants)) distro))
   ((string-prefix-p "ppa:" value)
    (or (deb-packaging-commands--ppa-repo-line value distro) value))
   (t value)))

(defun deb-packaging-commands-sbuild (&optional args)
  "Run sbuild with ARGS from the binary-build transient."
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
      (let* ((effective-args (or args '()))
             (distro (or (transient-arg-value "--dist=" effective-args)
                         (deb-packaging-config--effective-distro)))
             (repo-args (cl-remove-if-not
                         (lambda (a) (string-prefix-p "--extra-repository=" a))
                         effective-args))
             (extra-repo-arg
              (mapcar (lambda (a)
                        (concat "--extra-repository="
                                (deb-packaging-commands--expand-extra-repo
                                 (string-remove-prefix "--extra-repository=" a)
                                 distro)))
                      repo-args))
             (passthrough (cl-remove-if
                           (lambda (a) (string-prefix-p "--extra-repository=" a))
                           effective-args)))
        ;; Default --dist= back if the user cleared it.
        (unless (transient-arg-value "--dist=" passthrough)
          (setq passthrough (cons (format "--dist=%s" distro) passthrough)))
        ;; Keep the global distro in sync for the status buffer.
        (deb-packaging-config--set-distro distro)
        (when (nth 0 info)
          (deb-packaging-repos-save
           (nth 0 info) distro
           (mapcar (lambda (a) (string-remove-prefix "--extra-repository=" a))
                   repo-args)))
        (deb-packaging-commands--run-command
         "sbuild"
         (append (list "sbuild")
                 passthrough
                 extra-repo-arg
                 (list dsc-file))
         parent-dir
         'sbuild)))))

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
  "Return non-nil if LXD IMAGE exists locally."
  (zerop (call-process "lxc" nil nil nil "image" "info" image)))

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
  "Run autopkgtest with ARGS from the test transient."
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
      (let* ((effective-args (or args '()))
             (runner (or (transient-arg-value "--runner=" effective-args)
                         "lxd"))
              (distro (or (transient-arg-value "--dist=" effective-args)
                          (deb-packaging-config--effective-distro)))
             (image-info (deb-packaging-commands--test-image-info runner distro))
             (image (plist-get image-info :image))
             (image-exists (plist-get image-info :exists))
              (passthrough (cl-remove-if
                            (lambda (a)
                              (or (string-prefix-p "--runner=" a)
                                  (string-prefix-p "--dist=" a)
                                  (string-prefix-p "--ppa=" a)))
                            effective-args)))
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
set; the used PPA is saved per package+distro."
  (interactive (list (transient-args 'deb-packaging-upload-transient)))
  (let* ((effective-args (or args '()))
         (ppa (deb-packaging-commands--resolve-ppa effective-args))
         (distro (or (transient-arg-value "--dist=" effective-args)
                     (deb-packaging-config--effective-distro))))
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
      (let* ((changes-file (if (consp changes) (car changes) changes))
             (cmd-args (list "dput" ppa changes-file)))
        (deb-packaging-config--set-distro distro)
        (when name
          (deb-packaging-ppa-save name distro ppa))
        (deb-packaging-commands--run-command "dput" cmd-args
                                     (or parent-dir default-directory)
                                     'dput)))))

;;; Clean artifacts

(defun deb-packaging-commands-clean (&optional args)
  "Remove build artifacts with ARGS from `deb-packaging-commands-clean-transient'.
Moves files to trash from the output (parent) directory only."
  (interactive (list (transient-args 'deb-packaging-commands-clean-transient)))
  (let ((pkg-dir (deb-packaging-detect--find-package-dir nil t)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let* ((effective-args (or args '()))
           (do-artifacts (member "--artifacts" effective-args))
           (do-stale     (member "--stale"     effective-args))
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

;;; Reset source tree

(defun deb-packaging-commands-reset (&optional args)
  "Reset the source tree with ARGS from `deb-packaging-commands-reset-transient'.
Pops quilt patches, removes .pc/, and/or removes debian/files."
  (interactive (list (transient-args 'deb-packaging-commands-reset-transient)))
  (let ((pkg-dir (deb-packaging-detect--find-package-dir nil t)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let* ((effective-args (or args '()))
           (do-quilt (member "--quilt" effective-args))
           (do-pc    (member "--pc"    effective-args))
           (do-files (member "--files" effective-args))
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
