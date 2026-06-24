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
;; Each public runner function accepts an ARGS list as produced by
;; (transient-args 'PREFIX), extracts what it needs via
;; `transient-arg-value', and passes the remainder straight through to
;; the underlying tool.  There is no global preset/mode system; all
;; per-invocation flags come from the calling transient.

;;; Code:

(require 'comint)
(require 'ansi-color)
(require 'deb-packaging-detect)
(require 'deb-packaging-presets)

;;; Core Execution

(defun deb-packaging--filter-osc-sequences (string)
  "Filter OSC and terminal query sequences from STRING for comint."
  (setq string (replace-regexp-in-string "\e\\][^\a\e]*\\(\a\\|\e\\\\\\)" "" string))
  (setq string (replace-regexp-in-string "\e\\[\\?[0-9;]*[a-zA-Z]" "" string))
  string)

;;; Run-outcome tracking

(defvar deb-packaging--run-history nil
  "Alist mapping a command KEY symbol to its most recent run record.
Each record is a plist: :status (`running', `success' or `failure'),
:time (HH:MM:SS string), :buffer (comint buffer name).  Session-only.")

(defun deb-packaging--record-run (key status buf-name)
  "Store a run record for KEY with STATUS and BUF-NAME, timestamped now."
  (when key
    (setf (alist-get key deb-packaging--run-history)
          (list :status status
                :time (format-time-string "%H:%M:%S")
                :buffer buf-name))))

(defun deb-packaging-run-record (key)
  "Return the most recent run record plist for KEY, or nil."
  (alist-get key deb-packaging--run-history))

(defun deb-packaging--notify-status-refresh ()
  "Refresh the status buffer if it is live."
  (when (fboundp 'deb-packaging-status--maybe-refresh)
    (deb-packaging-status--maybe-refresh)))

(defun deb-packaging--attach-run-sentinel (proc key buf-name)
  "Attach a sentinel to PROC that records the run outcome for KEY."
  (let ((old (process-sentinel proc)))
    (set-process-sentinel
     proc
     (lambda (p event)
       (when (functionp old)
         (funcall old p event))
       (when (memq (process-status p) '(exit signal))
         (let ((status (if (and (eq (process-status p) 'exit)
                                (zerop (process-exit-status p)))
                           'success
                         'failure)))
           (deb-packaging--record-run key status buf-name)
           (deb-packaging--notify-status-refresh)))))))

(defun deb-packaging--run-command (name args &optional dir key)
  "Run command in a comint buffer.
NAME is used to form the buffer name; ARGS is the full command list.
DIR sets the comint process's `default-directory' (working directory);
it is scoped to buffer creation only and never mutates the caller's
`default-directory'.  KEY (a symbol) enables run tracking."
  (let* ((timestamp (format-time-string "%H:%M:%S"))
         (buf-name (format "*deb-%s-%s*" name timestamp))
         (cmd (mapconcat #'shell-quote-argument args " ")))
    ;; Scope the cwd binding to comint creation only: `make-comint-in-buffer'
    ;; starts the process in the current buffer's `default-directory', and we
    ;; must not let it leak into the run-tracking/refresh below — otherwise, when
    ;; called from the status buffer with DIR=parent-dir, the synchronous status
    ;; refresh would scan from parent-dir and report "not in a package".
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
    (pop-to-buffer buf-name)
    buf-name))

;;; dpkg-buildpackage

(defun deb-packaging-source-build (&optional args)
  "Run dpkg-buildpackage with ARGS from the source-build transient."
  (interactive (list (transient-args 'deb-packaging-source-build-transient)))
  (let ((pkg-dir (deb-packaging--find-package-dir)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (deb-packaging--run-command "source-build"
                                (cons "dpkg-buildpackage" (or args '()))
                                pkg-dir
                                'source-build)))

;;; lintian

(defun deb-packaging--run-lintian (targets args &optional key)
  "Run lintian on TARGETS (a list of file paths) with ARGS, tracking under KEY.
Lintian accepts multiple files on its command line, so TARGETS may contain
a single .dsc, several .debs, or any mix."
  (let ((pkg-dir (deb-packaging--find-package-dir)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let ((parent-dir (file-name-directory (directory-file-name pkg-dir))))
      (deb-packaging--run-command "lintian"
                                  (append (list "lintian") args targets)
                                  parent-dir
                                  key))))

(defun deb-packaging-lintian-source (&optional args)
  "Run lintian on the source .dsc file with ARGS."
  (interactive (list (transient-args 'deb-packaging-lint-transient)))
  (let* ((pkg-dir (deb-packaging--find-package-dir))
         (info (deb-packaging--parse-changelog pkg-dir))
         (parent-dir (file-name-directory (directory-file-name pkg-dir)))
         (artifacts (deb-packaging--scan-artifacts
                     (nth 0 info) (nth 1 info) parent-dir))
         (dsc (alist-get 'dsc artifacts)))
    (unless dsc
      (user-error "No .dsc file found — run source build first"))
    (deb-packaging--run-lintian (list dsc) (or args '()) 'lintian-source)))

(defun deb-packaging--lintian-binary-artifacts ()
  "Return the list of .deb files for the current package.
Signals `user-error' when no .debs have been built."
  (let* ((pkg-dir (deb-packaging--find-package-dir))
         (info (when pkg-dir (deb-packaging--parse-changelog pkg-dir)))
         (parent-dir (when pkg-dir
                       (file-name-directory (directory-file-name pkg-dir))))
         (artifacts (when info
                      (deb-packaging--scan-artifacts
                       (nth 0 info) (nth 1 info) parent-dir)))
         (debs (alist-get 'debs artifacts)))
    (unless debs
      (user-error "No .deb files found — run binary build first"))
    debs))

(defun deb-packaging-lintian-binary (&optional args)
  "Run lintian on all .deb files with ARGS."
  (interactive (list (transient-args 'deb-packaging-lint-transient)))
  (let ((debs (deb-packaging--lintian-binary-artifacts)))
    (deb-packaging--run-lintian debs (or args '()) 'lintian-binary)))

(defun deb-packaging-lintian-binary-one (&optional args)
  "Run lintian on a single .deb with ARGS, prompting for which one.
Useful when iterating on one binary's lint findings without waiting for
lintian to process every .deb in a multi-binary package."
  (interactive (list (transient-args 'deb-packaging-lint-transient)))
  (let* ((debs (deb-packaging--lintian-binary-artifacts))
         (target (completing-read "Deb to lint: " debs nil t)))
    (deb-packaging--run-lintian (list target) (or args '()) 'lintian-binary)))

;;; sbuild

(defun deb-packaging--expand-extra-repo (variant-name distro)
  "Expand VARIANT-NAME from `deb-packaging-sbuild-variants' with DISTRO."
  (when-let ((template (cdr (assoc variant-name deb-packaging-sbuild-variants))))
    (format template distro)))

(defun deb-packaging-sbuild (&optional args)
  "Run sbuild with ARGS from the binary-build transient."
  (interactive (list (transient-args 'deb-packaging-binary-build-transient)))
  (let ((pkg-dir (deb-packaging--find-package-dir)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let* ((info (deb-packaging--parse-changelog pkg-dir))
           (parent-dir (file-name-directory (directory-file-name pkg-dir)))
           (artifacts (when info
                        (deb-packaging--scan-artifacts
                         (nth 0 info) (nth 1 info) parent-dir)))
           (dsc-file (alist-get 'dsc artifacts)))
      (unless dsc-file
        (user-error "No .dsc file found — run source build first"))
      (let* ((effective-args (or args '()))
             (distro (or (transient-arg-value "--dist=" effective-args)
                         deb-packaging-target-distro))
             (variant-name (transient-arg-value "--extra-repository=" effective-args))
             ;; Strip our synthetic options before passing to sbuild
             (passthrough (cl-remove-if
                           (lambda (a)
                             (or (string-prefix-p "--dist=" a)
                                 (string-prefix-p "--extra-repository=" a)))
                           effective-args))
             (extra-repo-arg
              (when variant-name
                (when-let ((expanded (deb-packaging--expand-extra-repo
                                      variant-name distro)))
                  (list (concat "--extra-repository=" expanded))))))
        ;; Update the global so the status buffer stays in sync
        (setq deb-packaging-target-distro distro
              deb-packaging--distro-user-set t)
        (deb-packaging--run-command
         "sbuild"
         (append (list "sbuild")
                 passthrough
                 extra-repo-arg
                 (list (format "-d%s" distro) dsc-file))
         parent-dir
         'sbuild)))))

;;; autopkgtest

(defun deb-packaging--lxd-image-exists-p (image)
  "Return non-nil if LXD IMAGE exists locally."
  (zerop (call-process "lxc" nil nil nil "image" "info" image)))

(defun deb-packaging--test-image-info (&optional runner distro)
  "Return a plist describing the test image for RUNNER and DISTRO.
RUNNER defaults to \"lxd\", DISTRO to `deb-packaging-target-distro'.
The plist keys are :runner, :image (the expanded path/alias, or nil),
and :exists (non-nil if the image is available locally, or nil when
the runner or template is unknown)."
  (let* ((runner (or runner "lxd"))
         (distro (or distro deb-packaging-target-distro))
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
RUNNER is the runner name (e.g. \"lxd\"), DISTRO is the target distro.
Reads from `deb-packaging-test-build-hints'; returns nil if RUNNER
is not registered."
  (when-let ((template (cdr (assoc runner deb-packaging-test-build-hints))))
    (format template distro)))

(defun deb-packaging-autopkgtest (&optional args)
  "Run autopkgtest with ARGS from the test transient."
  (interactive (list (transient-args 'deb-packaging-test-transient)))
  (let ((pkg-dir (deb-packaging--find-package-dir)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let* ((info (deb-packaging--parse-changelog pkg-dir))
           (parent-dir (file-name-directory (directory-file-name pkg-dir)))
           (artifacts (when info
                        (deb-packaging--scan-artifacts
                         (nth 0 info) (nth 1 info) parent-dir)))
           (debs (alist-get 'debs artifacts)))
      (unless debs
        (user-error "No .deb files found — run binary build first"))
      (let* ((effective-args (or args '()))
             (runner (or (transient-arg-value "--runner=" effective-args)
                         "lxd"))
             (distro (or (transient-arg-value "--dist=" effective-args)
                         deb-packaging-target-distro))
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
                          "(unknown — add an entry to deb-packaging-test-build-hints)")))
        (deb-packaging--run-command
         "autopkgtest"
         (append (list "autopkgtest")
                 passthrough
                 debs
                 (list "." "--" runner)
                 (when image (list image)))
         pkg-dir
         'autopkgtest)))))

;;; PPA (Launchpad) testing

(defun deb-packaging-ppa-tests (&optional args)
  "Show autopkgtest results for a PPA.
ARGS is the argument list from `deb-packaging-upload-transient'."
  (interactive (list (transient-args 'deb-packaging-upload-transient)))
  (let* ((effective-args (or args '()))
         (ppa (transient-arg-value "--ppa=" effective-args))
         (distro (or (transient-arg-value "--dist=" effective-args)
                     deb-packaging-target-distro)))
    (unless (and ppa (not (string-empty-p ppa)))
      (user-error "No PPA specified — set it with the -p option"))
    (let* ((pkg-dir (deb-packaging--find-package-dir))
           (info (when pkg-dir (deb-packaging--parse-changelog pkg-dir)))
           (name (nth 0 info))
           (cmd-args (append (list "ppa" "tests" ppa)
                             (when name (list "-p" name))
                             (list "-r" distro))))
      (deb-packaging--run-command "ppa-tests" cmd-args
                                  (or pkg-dir default-directory)
                                  'ppa-tests))))

;;; Clean

(defun deb-packaging-clean (&optional args)
  "Clean build artifacts according to ARGS from `deb-packaging-clean-transient'."
  (interactive (list (transient-args 'deb-packaging-clean-transient)))
  (let ((pkg-dir (deb-packaging--find-package-dir)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let* ((effective-args (or args '()))
           (do-quilt     (member "--quilt"     effective-args))
           (do-sessions  (member "--sessions"  effective-args))
           (do-artifacts (member "--artifacts" effective-args))
           (do-stale     (member "--stale"     effective-args))
           (do-pc        (member "--pc"        effective-args))
           (do-files     (member "--files"     effective-args))
           (info (deb-packaging--parse-changelog pkg-dir))
           (name (nth 0 info))
           (version (nth 1 info))
           (parent-dir (file-name-directory (directory-file-name pkg-dir)))
           (file-version (deb-packaging--version-to-filename version))
           (current-pattern (format "%s[_-]*%s*" name file-version))
           ;; Build a shell pipeline from the selected steps
           (steps '())
           (desc '()))
      (when do-quilt
        (push "quilt pop -a 2>/dev/null || true" steps)
        (push "pop quilt" desc))
      (when do-sessions
        (push "schroot -e --all-sessions 2>/dev/null || true" steps)
        (push "end schroot sessions" desc))
      (when do-artifacts
        (push (format "rm -f %s/%s"
                      (shell-quote-argument parent-dir)
                      current-pattern)
              steps)
        (push "rm current artifacts" desc))
      (when do-stale
        (let ((stale (deb-packaging--scan-stale-artifacts
                      name version parent-dir)))
          (when stale
            (dolist (f stale)
              (push (format "rm -f %s"
                            (shell-quote-argument
                             (expand-file-name f parent-dir)))
                    steps))
            (push (format "rm %d stale" (length stale)) desc))))
      (when do-pc
        (push "rm -rf .pc/" steps)
        (push "rm .pc/" desc))
      (when do-files
        (push "rm -f debian/files" steps)
        (push "rm debian/files" desc))
      (if (null steps)
          (message "Nothing selected to clean")
        (let ((script (concat
                       (format "cd %s && " (shell-quote-argument pkg-dir))
                       (string-join (nreverse steps) " && ")
                       (format " && echo 'Clean complete (%s)'"
                               (string-join (nreverse desc) ", ")))))
          (deb-packaging--run-command
           "clean"
           (list "sh" "-c" script)
           pkg-dir
           'clean))))))

(provide 'deb-packaging-commands)
;;; deb-packaging-commands.el ends here
