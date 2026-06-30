;;; deb-packaging-commands.el --- Command execution for deb-packaging -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/example/deb-packaging
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Build and execute commands for Debian packaging tools.
;;
;; Each public runner takes an ARGS list from (transient-args 'PREFIX),
;; pulls values with `transient-arg-value', and passes the rest to the
;; tool.  No global presets; flags come from the calling transient.

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'ansi-color)
(require 'transient)
(require 'deb-packaging-detect)
(require 'deb-packaging-config)

(declare-function deb-packaging--effective-distro "deb-packaging-config")

;;; Core Execution

(defun deb-packaging--filter-osc-sequences (string)
  "Filter OSC and terminal query sequences from STRING for comint."
  (setq string (replace-regexp-in-string "\e\\][^\a\e]*\\(\a\\|\e\\\\\\)" "" string))
  (setq string (replace-regexp-in-string "\e\\[\\?[0-9;]*[a-zA-Z]" "" string))
  string)

;;; Run-outcome tracking

(defvar deb-packaging--run-history nil
  "Alist mapping run KEY to its most recent record.
Record is a plist: :status (`running', `success' or `failure'),
:time (HH:MM:SS string), :buffer (comint buffer name), and optional
:summary (lint findings with :error/:warning/:info keys).  Session-only.")

(defun deb-packaging--record-run (key status buf-name &optional summary)
  "Store a run record for KEY with STATUS, BUF-NAME, and optional SUMMARY.
SUMMARY is a plist of findings stored under :summary."
  (when key
    (let ((existing (alist-get key deb-packaging--run-history)))
      (setf (alist-get key deb-packaging--run-history)
            (list :status status
                  :time (or (and existing (plist-get existing :time))
                            (format-time-string "%H:%M:%S"))
                  :buffer buf-name
                  :summary summary)))))

(defun deb-packaging-run-record (key)
  "Return the most recent run record plist for KEY, or nil."
  (alist-get key deb-packaging--run-history))

(defun deb-packaging--run-summary (key)
  "Return the summary plist for KEY's last run, or nil."
  (plist-get (deb-packaging-run-record key) :summary))

(defun deb-packaging--notify-status-refresh ()
  "Refresh the status buffer if it is live."
  (when (fboundp 'deb-packaging-status--maybe-refresh)
    (deb-packaging-status--maybe-refresh)))

(defun deb-packaging--parse-lint-summary (buf-name)
  "Parse lintian counts from comint buffer BUF-NAME.
Return plist (:error N :warning N :info N) by counting lines matching
E:/W:/I: prefixes."
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

(defun deb-packaging--parse-ubuntu-lint-summary (buf-name)
  "Parse ubuntu-lint counts from comint buffer BUF-NAME.
Return plist (:ok N :skip N :warn N :error N :fail N) from the final
`Summary: ran N lint checks (...)' line."
  (when (buffer-live-p (get-buffer buf-name))
    (with-current-buffer buf-name
      (save-excursion
        (goto-char (point-min))
        ;; Summary line lists counts, e.g.:
        ;;   Summary: ran 12 lint checks (OK: 12, SKIP: 0, ...)
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

(defun deb-packaging--run-summary-parser (key)
  "Return the summary parser for run KEY, or nil.
Each lint-style tool has its own format; the sentinel uses this to pick
the parser."
  (pcase key
    ((or 'lintian-source 'lintian-binary) #'deb-packaging--parse-lint-summary)
    ('ubuntu-lint #'deb-packaging--parse-ubuntu-lint-summary)
    (_ nil)))

(defun deb-packaging--attach-run-sentinel (proc key buf-name)
  "Attach a sentinel to PROC that records the outcome for KEY.
For lint-style keys, parse findings counts and store them as :summary."
  (let ((old (process-sentinel proc)))
    (set-process-sentinel
      proc
      (lambda (p event)
        (when (functionp old)
          (funcall old p event))
        (when (memq (process-status p) '(exit signal))
          (let* ((status (if (and (eq (process-status p) 'exit)
                                  (zerop (process-exit-status p)))
                             'success
                           'failure))
                 (parser (deb-packaging--run-summary-parser key))
                 (summary (when parser (funcall parser buf-name))))
            (deb-packaging--record-run key status buf-name summary)
           (deb-packaging--notify-status-refresh)))))))

(defun deb-packaging--run-command (name args &optional dir key)
  "Run command in a comint buffer.
NAME forms the buffer name; ARGS is the full command list.
DIR sets the process's working directory without mutating the caller's.
KEY (a symbol) enables run tracking."
  (let* ((timestamp (format-time-string "%H:%M:%S"))
         (buf-name (format "*deb-%s-%s*" name timestamp))
         (cmd (mapconcat #'shell-quote-argument args " ")))
    ;; Bind `default-directory' only while creating the comint buffer.
    ;; `make-comint-in-buffer' uses the current buffer's default-directory,
    ;; so a leaked binding would make the status refresh scan from parent-dir
    ;; and report "not in a package".
    (if dir
        (let ((default-directory dir))
          (make-comint-in-buffer name buf-name shell-file-name nil
                                 shell-command-switch cmd))
      (make-comint-in-buffer name buf-name shell-file-name nil
                             shell-command-switch cmd))
    (with-current-buffer buf-name
      (when dir
        (setq default-directory dir))
      (add-hook 'comint-preoutput-filter-functions
                #'deb-packaging--filter-osc-sequences nil t)
      (add-hook 'comint-output-filter-functions
                #'ansi-color-process-output nil t))
    (when key
      (deb-packaging--record-run key 'running buf-name)
      (when-let ((proc (get-buffer-process buf-name)))
        (deb-packaging--attach-run-sentinel proc key buf-name))
      (deb-packaging--notify-status-refresh))
    (switch-to-buffer buf-name)
    buf-name))

;;; dpkg-buildpackage

(defun deb-packaging-source-build (&optional args)
  "Run dpkg-buildpackage with ARGS from the source-build transient."
  (interactive (list (transient-args 'deb-packaging-source-build-transient)))
  (let ((pkg-dir (deb-packaging--find-package-dir nil t)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (deb-packaging--run-command "source-build"
                                (cons "dpkg-buildpackage" (or args '()))
                                pkg-dir
                                'source-build)))

;;; lintian

(defconst deb-packaging--lintian-arg-prefixes
  '("-i" "-I" "-P" "--pedantic" "--tag-display-limit=" "--color=")
  "Lintian arg prefixes.
Used by `deb-packaging--filter-args' to keep lintian's command line clean
when both tools share the lint transient.  Entries ending in `=' match by
prefix; bare entries match exactly.")

(defconst deb-packaging--ubuntu-lint-arg-prefixes
  '("--verbose" "--json" "--context=" "--all=")
  "ubuntu-lint arg prefixes.
Used by `deb-packaging--filter-args' to keep ubuntu-lint's command line
clean when both tools share the lint transient.")

(defun deb-packaging--filter-args (args prefixes)
  "Return the members of ARGS matching any prefix in PREFIXES.
An entry ending in `=' matches by string prefix (e.g. \"--context=\");
a bare entry matches exactly (e.g. \"-i\")."
  (cl-remove-if-not
   (lambda (a)
     (cl-some
      (lambda (p)
        (if (string-suffix-p "=" p)
            (string-prefix-p p a)
          (string= p a)))
      prefixes))
   args))

(defun deb-packaging--run-lintian (targets args &optional key)
  "Run lintian on TARGETS (file paths) with ARGS, tracked as KEY.
ARGS is filtered to lintian's own flags so ubuntu-lint flags do not leak
onto lintian's command line.  TARGETS may contain one .dsc, several
.debs, or a mix."
  (let ((pkg-dir (deb-packaging--find-package-dir nil t)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let ((parent-dir (file-name-directory (directory-file-name pkg-dir)))
          (lint-args (deb-packaging--filter-args
                      (or args '())
                      deb-packaging--lintian-arg-prefixes)))
      (deb-packaging--run-command "lintian"
                                  (append (list "lintian") lint-args targets)
                                  parent-dir
                                  key))))

(defun deb-packaging-lintian-source (&optional args)
  "Run lintian on the source .dsc file with ARGS."
  (interactive (list (transient-args 'deb-packaging-lint-transient)))
  (let* ((pkg-dir (deb-packaging--find-package-dir nil t))
         (info (deb-packaging--parse-changelog pkg-dir))
         (parent-dir (file-name-directory (directory-file-name pkg-dir)))
         (artifacts (deb-packaging--scan-artifacts
                     (nth 0 info) (nth 1 info) parent-dir))
         (dsc (alist-get 'dsc artifacts)))
    (unless dsc
      (user-error "No .dsc file found -- run source build first"))
    (deb-packaging--run-lintian (list dsc) (or args '()) 'lintian-source)))

(defun deb-packaging--lintian-binary-artifacts ()
  "Return the .deb files for the current package.
Signal `user-error' if none exist."
  (let* ((pkg-dir (deb-packaging--find-package-dir nil t))
         (info (when pkg-dir (deb-packaging--parse-changelog pkg-dir)))
         (parent-dir (when pkg-dir
                       (file-name-directory (directory-file-name pkg-dir))))
         (artifacts (when info
                      (deb-packaging--scan-artifacts
                       (nth 0 info) (nth 1 info) parent-dir)))
         (debs (alist-get 'debs artifacts)))
    (unless debs
      (user-error "No .deb files found -- run binary build first"))
    debs))

(defun deb-packaging-lintian-binary (&optional args)
  "Run lintian on all .deb files with ARGS."
  (interactive (list (transient-args 'deb-packaging-lint-transient)))
  (let ((debs (deb-packaging--lintian-binary-artifacts)))
    (deb-packaging--run-lintian debs (or args '()) 'lintian-binary)))

(defun deb-packaging-lintian-binary-one (&optional args)
  "Run lintian on one .deb with ARGS, prompting for which.
Useful for iterating on a single binary in multi-binary packages."
  (interactive (list (transient-args 'deb-packaging-lint-transient)))
  (let* ((debs (deb-packaging--lintian-binary-artifacts))
         (target (completing-read "Deb to lint: " debs nil t)))
    (deb-packaging--run-lintian (list target) (or args '()) 'lintian-binary)))

;;; ubuntu-lint

(defun deb-packaging--ubuntu-lint-context-args (mode pkg-dir)
  "Return ubuntu-lint context flags for MODE rooted at PKG-DIR.
MODE is `changes', `source-dir', or `changelog'.  Default `changes' adds
--source-dir and --changes-file when a source .changes exists.  Other
modes pass only the named context."
  (pcase mode
    ("source-dir" (list "--source-dir" pkg-dir))
    ("changelog" (list "--changelog"
                       (expand-file-name "debian/changelog" pkg-dir)))
    (_
     (let* ((info (deb-packaging--parse-changelog pkg-dir))
            (name (nth 0 info))
            (version (nth 1 info))
            (parent-dir (file-name-directory (directory-file-name pkg-dir)))
            (artifacts (when info
                         (deb-packaging--scan-artifacts name version parent-dir)))
            (changes (alist-get 'source-changes artifacts)))
       (if changes
           (list "--source-dir" pkg-dir "--changes-file" changes)
         (list "--source-dir" pkg-dir))))))

(defun deb-packaging-ubuntu-lint (&optional args)
  "Run ubuntu-lint with ARGS from the lint transient.
ARGS is filtered to ubuntu-lint's own flags so lintian flags do not leak.
`--context=MODE' selects the context source (`changes' by default, or
`source-dir' / `changelog'); remaining flags pass through.  Run from the
package directory under the `ubuntu-lint' key."
  (interactive (list (transient-args 'deb-packaging-lint-transient)))
  (let ((pkg-dir (deb-packaging--find-package-dir nil t)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let* ((effective-args (or args '()))
           (mode (or (transient-arg-value "--context=" effective-args) "changes"))
           (ubuntu-args (deb-packaging--filter-args
                         effective-args
                         deb-packaging--ubuntu-lint-arg-prefixes))
           (passthrough (cl-remove-if
                         (lambda (a) (string-prefix-p "--context=" a))
                         ubuntu-args))
           (context-args (deb-packaging--ubuntu-lint-context-args mode pkg-dir)))
      (deb-packaging--run-command
       "ubuntu-lint"
       (append (list "ubuntu-lint") passthrough context-args)
       pkg-dir
       'ubuntu-lint))))

;;; sbuild

(defvar deb-packaging-sbuild-variants
  '(("rust-ppa"
     . "deb [trusted=yes] http://ppa.launchpadcontent.net/rust-toolchain/staging/ubuntu/ %s main")
    ("proposed"
     . "deb http://archive.ubuntu.com/ubuntu/ %s-proposed main"))
  "Alist of short name to extra-repository string for sbuild.
%s is replaced with the target distro.  Used as completion candidates in
the binary-build transient's --extra-repository option.")

(defun deb-packaging--expand-extra-repo (value distro)
  "Expand VALUE into an extra-repository string for DISTRO.
If VALUE matches a key in `deb-packaging-sbuild-variants', substitute the
template; otherwise return VALUE unchanged for custom repos."
  (if-let ((template (cdr (assoc value deb-packaging-sbuild-variants))))
      (format template distro)
    value))

(defun deb-packaging-sbuild (&optional args)
  "Run sbuild with ARGS from the binary-build transient."
  (interactive (list (transient-args 'deb-packaging-binary-build-transient)))
  (let ((pkg-dir (deb-packaging--find-package-dir nil t)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let* ((info (deb-packaging--parse-changelog pkg-dir))
           (parent-dir (file-name-directory (directory-file-name pkg-dir)))
           (artifacts (when info
                        (deb-packaging--scan-artifacts
                         (nth 0 info) (nth 1 info) parent-dir)))
           (dsc-file (alist-get 'dsc artifacts)))
      (unless dsc-file
        (user-error "No .dsc file found -- run source build first"))
      (let* ((effective-args (or args '()))
             (distro (or (transient-arg-value "--dist=" effective-args)
                         (deb-packaging--effective-distro)))
             (variant-name (transient-arg-value "--extra-repository=" effective-args))
             ;; Expand the extra-repository alias if needed, leaving the flag
             ;; name unchanged.
             (extra-repo-arg
              (when variant-name
                (list (concat "--extra-repository="
                              (deb-packaging--expand-extra-repo variant-name distro)))))
             (passthrough (cl-remove-if
                           (lambda (a) (string-prefix-p "--extra-repository=" a))
                           effective-args)))
        ;; Ensure --dist= is present; if the user cleared it, default it back.
        (unless (transient-arg-value "--dist=" passthrough)
          (setq passthrough (cons (format "--dist=%s" distro) passthrough)))
        ;; Update the global so the status buffer stays in sync.
        (deb-packaging--set-distro distro)
        (deb-packaging--run-command
         "sbuild"
         (append (list "sbuild")
                 passthrough
                 extra-repo-arg
                 (list dsc-file))
         parent-dir
         'sbuild)))))

;;; autopkgtest

(defvar deb-packaging-test-runners
  '(("lxd"  . "autopkgtest/ubuntu/%s/amd64")
    ("qemu" . "/var/lib/adt-images/autopkgtest-%s-amd64.img"))
  "Alist of runner name to image path template.  %s is the target distro.
For Debian, add entries like (lxd . autopkgtest/debian/%s/amd64).
Also used for --runner completion.")

(defvar deb-packaging-test-build-hints
  '(("lxd"  . "autopkgtest-build-lxd ubuntu-daily:%s")
    ("qemu" . "autopkgtest-buildvm-ubuntu-cloud -r %s"))
  "Alist of runner name to image-build command template.  %s is the distro.
Shown when a test image is missing.")

(defun deb-packaging--runner-choices ()
  "Return the list of configured autopkgtest runner names.
Derived from `deb-packaging-test-runners' so that variable is the single
source of truth for the test transient's --runner option."
  (mapcar #'car deb-packaging-test-runners))

(defun deb-packaging--lxd-image-exists-p (image)
  "Return non-nil if LXD IMAGE exists locally."
  (zerop (call-process "lxc" nil nil nil "image" "info" image)))

(defun deb-packaging--test-image-info (&optional runner distro)
  "Return a plist describing the test image for RUNNER and DISTRO.
RUNNER defaults to \"lxd\", DISTRO to `deb-packaging--effective-distro'.
Plist keys are :runner, :image, and :exists."
  (let* ((runner (or runner "lxd"))
         (distro (or distro (deb-packaging--effective-distro)))
         (template (cdr (assoc runner deb-packaging-test-runners)))
         (image (when template (format template distro)))
         (exists (when image
                   (cond
                    ((equal runner "lxd")
                     (deb-packaging--lxd-image-exists-p image))
                    ((equal runner "qemu")
                     (file-exists-p image))
                    (t nil)))))
    (list :runner runner :image image :exists exists)))

(defun deb-packaging--test-image-build-hint (runner distro)
  "Return the command string to build a missing test image.
RUNNER is the runner name (e.g. \"lxd\"), DISTRO the target distro.
Returns nil if RUNNER has no registered hint."
  (when-let ((template (cdr (assoc runner deb-packaging-test-build-hints))))
    (format template distro)))

(defun deb-packaging-autopkgtest (&optional args)
  "Run autopkgtest with ARGS from the test transient."
  (interactive (list (transient-args 'deb-packaging-test-transient)))
  (let ((pkg-dir (deb-packaging--find-package-dir nil t)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let* ((info (deb-packaging--parse-changelog pkg-dir))
           (parent-dir (file-name-directory (directory-file-name pkg-dir)))
           (artifacts (when info
                        (deb-packaging--scan-artifacts
                         (nth 0 info) (nth 1 info) parent-dir)))
           (debs (alist-get 'debs artifacts)))
      (unless debs
        (user-error "No .deb files found -- run binary build first"))
      (let* ((effective-args (or args '()))
             (runner (or (transient-arg-value "--runner=" effective-args)
                         "lxd"))
              (distro (or (transient-arg-value "--dist=" effective-args)
                          (deb-packaging--effective-distro)))
             (image-info (deb-packaging--test-image-info runner distro))
             (image (plist-get image-info :image))
             (image-exists (plist-get image-info :exists))
             ;; Pass through flags autopkgtest itself understands
             (passthrough (cl-remove-if
                           (lambda (a)
                             (or (string-prefix-p "--runner=" a)
                                 (string-prefix-p "--dist=" a)))
                           effective-args)))
        ;; Validate image availability
        (when (and image (not image-exists))
          (user-error "%s image '%s' not found.\nBuild it with:\n  %s"
                      (capitalize runner)
                      image
                      (or (deb-packaging--test-image-build-hint runner distro)
                          "(unknown -- add an entry to deb-packaging-test-build-hints)")))
        (deb-packaging--run-command
         "autopkgtest"
         (append (list "autopkgtest")
                 passthrough
                 debs
                 (list "." "--" runner)
                 (when image (list image)))
         pkg-dir
         'autopkgtest)))))

;;; PPA upload (dput)

(defun deb-packaging-dput-upload (&optional args)
  "Upload source .changes to a PPA with dput.
ARGS comes from `deb-packaging-upload-transient'.  PPA is mandatory;
distro defaults to `deb-packaging-target-distro'."
  (interactive (list (transient-args 'deb-packaging-upload-transient)))
  (let* ((effective-args (or args '()))
         (ppa (transient-arg-value "--ppa=" effective-args))
         (distro (or (transient-arg-value "--dist=" effective-args)
                     (deb-packaging--effective-distro))))
    (unless (and ppa (not (string-empty-p ppa)))
      (user-error "No PPA specified -- set it with the -p option"))
    (let* ((pkg-dir (deb-packaging--find-package-dir nil t))
           (info (when pkg-dir (deb-packaging--parse-changelog pkg-dir)))
           (name (nth 0 info))
           (version (nth 1 info))
           (parent-dir (when pkg-dir
                         (file-name-directory (directory-file-name pkg-dir))))
           (artifacts (when (and name version parent-dir)
                        (deb-packaging--scan-artifacts
                         name version parent-dir)))
           (changes (alist-get 'source-changes artifacts)))
      (unless changes
        (user-error "No source .changes file found -- run source build first"))
      (let* ((changes-file (if (consp changes) (car changes) changes))
             (cmd-args (list "dput" ppa changes-file)))
        (deb-packaging--set-distro distro)
        (deb-packaging--run-command "dput" cmd-args
                                    (or parent-dir default-directory)
                                    'dput)))))

;;; PPA (Launchpad) testing

(defun deb-packaging-ppa-tests (&optional args)
  "Show autopkgtest results for a PPA.
ARGS is the argument list from `deb-packaging-upload-transient'."
  (interactive (list (transient-args 'deb-packaging-upload-transient)))
  (let* ((effective-args (or args '()))
         (ppa (transient-arg-value "--ppa=" effective-args))
         (distro (or (transient-arg-value "--dist=" effective-args)
                     (deb-packaging--effective-distro))))
    (unless (and ppa (not (string-empty-p ppa)))
      (user-error "No PPA specified -- set it with the -p option"))
    (let* ((pkg-dir (deb-packaging--find-package-dir nil t))
           (info (when pkg-dir (deb-packaging--parse-changelog pkg-dir)))
           (name (nth 0 info))
           (cmd-args (append (list "ppa" "tests" ppa)
                             (when name (list "-p" name))
                             (list "-r" distro))))
      (deb-packaging--run-command "ppa-tests" cmd-args
                                  (or pkg-dir default-directory)
                                  'ppa-tests))))

;;; Clean artifacts

(defun deb-packaging-clean (&optional args)
  "Remove build artifacts with ARGS from `deb-packaging-clean-transient'.
Moves files to trash, not permanently.  Only removes files from the
output (parent) directory."
  (interactive (list (transient-args 'deb-packaging-clean-transient)))
  (let ((pkg-dir (deb-packaging--find-package-dir nil t)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let* ((effective-args (or args '()))
           (do-artifacts (member "--artifacts" effective-args))
           (do-stale     (member "--stale"     effective-args))
           (info (deb-packaging--parse-changelog pkg-dir))
           (name (nth 0 info))
           (version (nth 1 info))
           (parent-dir (file-name-directory (directory-file-name pkg-dir)))
           (file-version (deb-packaging--version-to-filename version))
           (files nil))
      (when do-artifacts
        (let* ((prefixes (or (deb-packaging--owned-package-prefixes pkg-dir)
                             (list name)))
               (prefix-regex
                (mapconcat (lambda (p)
                             (concat "^" (regexp-quote p) "_"
                                     (regexp-quote file-version)))
                           prefixes "\\|")))
          (dolist (f (directory-files parent-dir nil prefix-regex))
            (push (expand-file-name f parent-dir) files))))
      (when do-stale
        (let ((stale (deb-packaging--scan-stale-artifacts
                      name version parent-dir pkg-dir)))
          (dolist (f stale)
            (push (expand-file-name f parent-dir) files))))
      (if (null files)
          (message "Nothing to clean")
        (dolist (f files)
          (when (file-exists-p f)
            (move-file-to-trash f)))
        (deb-packaging--record-run 'clean 'success nil)
        (deb-packaging--notify-status-refresh)
        (message "Moved %d file(s) to trash" (length files))))))

;;; Reset source tree

(defun deb-packaging-reset (&optional args)
  "Reset the source tree with ARGS from `deb-packaging-reset-transient'.
Pops quilt patches, removes .pc/, and/or removes debian/files."
  (interactive (list (transient-args 'deb-packaging-reset-transient)))
  (let ((pkg-dir (deb-packaging--find-package-dir nil t)))
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
        (push "rm .pc/" desc))
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
          (deb-packaging--run-command
           "reset"
           (list "sh" "-c" script)
           pkg-dir
           'reset))))))

(provide 'deb-packaging-commands)
;;; deb-packaging-commands.el ends here
