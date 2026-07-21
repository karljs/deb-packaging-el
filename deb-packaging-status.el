;;; deb-packaging-status.el --- Status landing page for deb-packaging -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Magit-style status buffer for deb-packaging. Shows packaging phases
;; (source build, binary build, lint, test, upload) in flow order, each
;; heading ending in a colored status word. The next actionable phase and
;; any running/failed phase expand by default; the rest fold.
;;
;; No cache: every render re-scans via `deb-packaging-detect--scan-context'
;; (shared with the dispatch transient) and on window selection.
;;
;; Entry point: `deb-packaging-status'.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'deb-packaging-detect)
(require 'deb-packaging-config)
(require 'deb-packaging-commands)
(require 'deb-packaging-ppa)
(require 'deb-packaging-transients)
(require 'deb-packaging-display)

;; Cross-file references not pulled in by require (avoids load cycles).
(declare-function deb-packaging-dispatch "deb-packaging")
(declare-function deb-packaging-infra-dispatch "deb-packaging-infra")
(declare-function deb-packaging-dev--list-containers "deb-packaging-dev")
(declare-function deb-packaging-propagate-transient "deb-packaging-propagate")
(declare-function deb-packaging-pq-transient "deb-packaging-pq")
(declare-function deb-packaging-pq--state "deb-packaging-pq")

;;; Buffer-local context

(defvar-local deb-packaging-status--context nil
  "Buffer-local plist describing the package shown.
Keys: :name :version :distro :pkg-dir :parent-dir :artifacts :stale
:source-format :orig-tarball :arch :maintainer.")

(defun deb-packaging-status--buffer-name (name)
  "Return the status buffer name for package NAME."
  (format "*deb-packaging: %s*" (or name "?")))

(defun deb-packaging-status--collect-context ()
  "Gather fresh package context from `default-directory'.
Return a plist, or nil outside a Debian package tree. Seeds the target
distro once without clobbering user choice."
  (let ((ctx (deb-packaging-detect--scan-context)))
    (when ctx
      (deb-packaging-config--maybe-seed-distro (plist-get ctx :distro)))
    ctx))

(defun deb-packaging-status--current-context ()
  "Return the context of the live status buffer, if any.
Does not create or refresh a buffer."
  (catch 'found
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (derived-mode-p 'deb-packaging-status-mode)
                     deb-packaging-status--context)
            (throw 'found deb-packaging-status--context)))))
    nil))

;;; Section -> action dispatch
;;
;; RET walks up the section tree to a registered type and invokes the
;; matching command. Mnemonic keys are shortcuts to the same commands.

(defconst deb-packaging-status--section-actions
  '((deb-packaging-source         . deb-packaging-commands-source-build-transient)
    (deb-packaging-binary         . deb-packaging-binary-build-transient)
    (deb-packaging-check          . deb-packaging-lint-transient)
    (deb-packaging-test           . deb-packaging-test-transient)
    (deb-packaging-upload         . deb-packaging-upload-transient)
    (deb-packaging-stale          . deb-packaging-commands-clean-transient)
    (deb-packaging-dev            . deb-packaging-dev-transient)
    (deb-packaging-pq             . deb-packaging-pq-transient))
  "Map status-buffer section types to the transient RET opens.")

;;; Status words
;;
;; The word backs up color for non-color terminals and accessibility.

(defconst deb-packaging-status--state-words
  '((running . ("running" . deb-packaging-status-running))
    (failed  . ("failed"  . deb-packaging-status-failed))
    (done    . ("done"    . deb-packaging-status-done))
    (ready   . ("ready"   . deb-packaging-status-ready))
    (blocked . ("blocked" . deb-packaging-status-blocked)))
  "Map phase state symbol to (WORD . FACE).")

(defun deb-packaging-status--state-word (state)
  "Return the propertized trailing status word for STATE."
  (let ((entry (alist-get state deb-packaging-status--state-words)))
    (propertize (car entry) 'font-lock-face (cdr entry))))

(defun deb-packaging-status--run-time-note (key)
  "Return a dimmed \" (HH:MM:SS)\" note for KEY's last run, or empty string."
  (if-let* ((record (deb-packaging-commands-run-record key))
            (time (plist-get record :time)))
      (propertize (format " %s" time) 'font-lock-face 'shadow)
    ""))

(defun deb-packaging-status--kept-session-note ()
  "Return a note naming the session kept by the last sbuild run, or nil."
  (when-let* ((summary (deb-packaging-commands--run-summary 'sbuild))
              (kept (plist-get summary :kept-session)))
    (format "Session kept: %s ('e' on it in the infra schroots list ends it)"
            kept)))

(defun deb-packaging-status--lint-summary-note (key)
  "Return a colored findings summary for KEY's last lint run, or empty.
Counts colored by severity, e.g. \" 2E 5W 12I\" or \" 1F 2E 3W\"."
  (if-let* ((summary (deb-packaging-commands--run-summary key)))
      (let ((fmt (lambda (n face)
                   (propertize (format "%d" n) 'font-lock-face face))))
        (pcase key
          ('ubuntu-lint
           (concat
            "  "
            (funcall fmt (plist-get summary :fail)    'deb-packaging-status-failed) "F "
            (funcall fmt (plist-get summary :error)   'deb-packaging-status-failed) "E "
            (funcall fmt (plist-get summary :warn)    'deb-packaging-status-running) "W"))
          (_
           (concat
            "  "
            (funcall fmt (plist-get summary :error)   'deb-packaging-status-failed) "E "
            (funcall fmt (plist-get summary :warning) 'deb-packaging-status-running) "W "
            (funcall fmt (plist-get summary :info)    'shadow) "I"))))
    ""))

(defun deb-packaging-status--ppa-tests-summary-note ()
  "Return a colored PPA-test counts string, or empty.
Counts come from the last ppa-tests run summary: \" 3P 1F 0B\"."
  (if-let* ((summary (deb-packaging-commands--run-summary 'ppa-tests)))
      (concat "  "
              (propertize (format "%dP" (plist-get summary :pass))
                          'font-lock-face 'deb-packaging-status-done)
              " "
              (propertize (format "%dF" (plist-get summary :fail))
                          'font-lock-face 'deb-packaging-status-failed)
              " "
              (propertize (format "%dB" (plist-get summary :bad))
                          'font-lock-face 'deb-packaging-status-failed))
    ""))

(defun deb-packaging-status--insert-ppa-tests-row ()
  "Insert the PPA tests row: saved PPA, last run time, result counts.
Builds the row directly rather than via `--insert-state-row', whose
whole-value re-propertizing would flatten the inner faces."
  (let* ((pkg-name (deb-packaging-detect--package-name))
         (ppa (and pkg-name
                   (deb-packaging-ppa-load
                    pkg-name (deb-packaging-config--effective-distro))))
         (record (deb-packaging-commands-run-record 'ppa-tests))
         (time (plist-get record :time)))
    (insert "    "
            (propertize "PPA tests" 'font-lock-face 'shadow)
            ": "
            (or ppa (propertize "not set" 'font-lock-face 'shadow))
            (if time
                (propertize (format " (%s)" time) 'font-lock-face 'shadow)
              "")
            (deb-packaging-status--ppa-tests-summary-note)
            "\n")))

;;; Phase state and fold decisions
;;
;; Smart fold sets only the initial state; magit-section preserves manual
;; TAB toggles across refresh.

(defun deb-packaging-status--phase-state (key done ready &optional keep-ready)
  "Return a phase state symbol for KEY.
DONE means artifacts exist or the phase succeeded. READY means
prerequisites are met, else blocked. Precedence: running, failed,
done, ready. KEEP-READY keeps success as ready so it can re-run (lint)."
  (let ((status (plist-get (deb-packaging-commands-run-record key) :status)))
    (cond ((eq status 'running) 'running)
          ((eq status 'failure) 'failed)
          ((and (not keep-ready) (or done (eq status 'success))) 'done)
          (ready 'ready)
          (t 'blocked))))

(defun deb-packaging-status--actionable-state-p (state)
  "Return non-nil when STATE is running, failed, or ready."
  (memq state '(running failed ready)))

(defun deb-packaging-status--hide-phase-p (state next-key key)
  "Return non-nil if a phase in STATE should collapse by default.
Expand running/failed phases and the next actionable phase (KEY equals
NEXT-KEY); collapse the rest."
  (cond
   ((memq state '(failed running)) nil)
   ((eq key next-key) nil)
   (t t)))

(defun deb-packaging-status--source-ready-p (ctx)
  "Return non-nil when the source build inputs are in place.
A non-native package needs its .orig.tar.* beside the tree; a native
package builds from the tree alone.  A missing version means a partial
context, which must not block."
  (let ((version (plist-get ctx :version))
        (orig (plist-get ctx :orig-tarball)))
    (or orig
        (null version)
        (deb-packaging-detect--native-version-p version))))

;;; Rendering helpers
;;
;; magit-section-mode sets font-lock-defaults, so use `font-lock-face' on
;; inserted text; the `face' property is ignored.

(defface deb-packaging-status-title
  '((t :inherit magit-section-heading :weight bold :height 1.2))
  "Face for the package name in the title line.")

(defface deb-packaging-status-version
  '((t :inherit magit-section-secondary-heading :weight normal))
  "Face for the version in the title line.")

(defface deb-packaging-status-distro
  '((t :inherit success))
  "Face for the target distribution in the title line.")

(defface deb-packaging-status-path
  '((t :inherit shadow))
  "Face for the repository path line under the title.")

(defface deb-packaging-status-key
  '((t :inherit shadow))
  "Face for a settings key label (e.g. \"Mode\").")

(defface deb-packaging-status-value
  '((t :inherit default))
  "Face for a settings value (e.g. the current distro).")

(defface deb-packaging-status-done
  '((t :inherit success))
  "Face for the `done' status word.")

(defface deb-packaging-status-failed
  '((t :inherit error :weight bold))
  "Face for the `failed' status word.")

(defface deb-packaging-status-running
  '((t :inherit warning :weight bold))
  "Face for the `running' status word.")

(defface deb-packaging-status-ready
  '((((class color) (background light)) :foreground "DodgerBlue4" :weight bold)
    (((class color) (background dark))  :foreground "DeepSkyBlue1" :weight bold)
    (t :weight bold))
  "Face for the `ready' status word.")

(defface deb-packaging-status-blocked
  '((t :inherit shadow))
  "Face for the `blocked' status word.")

(defconst deb-packaging-status--label-width 16
  "Column width for phase labels so trailing status words align.")

(defun deb-packaging-status--pad (text width)
  "Left-justify TEXT to WIDTH columns."
  (format (format "%%-%ds" width) text))

(defun deb-packaging-status--file-mtime (path)
  "Return a formatted \"Jun 24 14:32\" timestamp for PATH, or empty string."
  (condition-case nil
      (format-time-string "%b %e %H:%M"
                          (file-attribute-status-change-time
                           (file-attributes path)))
    (error "")))

(defun deb-packaging-status--file-size (path)
  "Return a human-readable size string for PATH, or empty string."
  (condition-case nil
      (file-size-human-readable
       (file-attribute-size (file-attributes path)))
    (error "")))

(defun deb-packaging-status--insert-file-line (path)
  "Insert an indented PATH line with size and modification time."
  (let* ((base (file-name-nondirectory path))
         (size (deb-packaging-status--file-size path))
         (mtime (deb-packaging-status--file-mtime path)))
    (insert (format "    %-45s %8s  %s\n"
                    (propertize base 'font-lock-face
                                'magit-section-secondary-heading)
                    (propertize size 'font-lock-face 'shadow)
                    (propertize mtime 'font-lock-face 'shadow)))))

(defun deb-packaging-status--insert-note (text)
  "Insert an indented, dimmed informational note TEXT."
  (insert (format "    %s\n" (propertize text 'font-lock-face 'shadow))))

(defun deb-packaging-status--insert-state-row (pairs)
  "Insert a state row from PAIRS, a list of (label . value) cells.
Renders each as \"Label: value\"."
  (when pairs
    (let ((parts (mapcar
                  (lambda (pair)
                    (concat
                     (propertize (car pair) 'font-lock-face 'shadow)
                     ": "
                     (propertize (cdr pair) 'font-lock-face 'default)))
                  pairs)))
      (insert "    "
              (mapconcat #'identity parts
                         (propertize "    " 'font-lock-face 'shadow))
              "\n"))))

;;; Section inserters

(defun deb-packaging-status--insert-header (ctx)
  "Insert the package title, path, and stale indicator from CTX."
  (let ((name (plist-get ctx :name))
        (version (plist-get ctx :version))
        (distro (plist-get ctx :distro))
        (pkg-dir (plist-get ctx :pkg-dir))
        (stale (plist-get ctx :stale)))
    (insert (propertize name 'font-lock-face 'deb-packaging-status-title)
            " "
            (propertize version 'font-lock-face 'deb-packaging-status-version)
            "  "
            (propertize (or distro deb-packaging-config-target-distro)
                        'font-lock-face 'deb-packaging-status-distro)
            "\n")
    (insert (propertize (abbreviate-file-name pkg-dir)
                        'font-lock-face 'deb-packaging-status-path)
            "\n")
    (when stale
      (insert (propertize (format "⚠ %d stale" (length stale))
                          'font-lock-face 'warning)
              "\n"))
    (insert "\n")))

(defun deb-packaging-status--phase-heading (state label &optional key detail)
  "Return a propertized phase heading line.
LABEL is the phase name, STATE drives the status word, KEY adds the
last-run time, DETAIL is an optional dimmed fragment."
  (concat
   (propertize (deb-packaging-status--pad label deb-packaging-status--label-width)
               'font-lock-face 'magit-section-heading)
   (deb-packaging-status--state-word state)
   (if key (deb-packaging-status--run-time-note key) "")
   (or detail "")))

(defun deb-packaging-status--insert-source (ctx hide)
  "Insert the Source phase section from CTX, collapsed when HIDE."
  (let* ((arts (plist-get ctx :artifacts))
         (dsc (alist-get 'dsc arts))
         (src-changes (alist-get 'source-changes arts))
         (buildinfo (alist-get 'buildinfo arts))
         (source-format (plist-get ctx :source-format))
         (orig-tarball (plist-get ctx :orig-tarball))
         (parent-dir (plist-get ctx :parent-dir))
         (done (and dsc src-changes))
         (ready (deb-packaging-status--source-ready-p ctx))
         (state (deb-packaging-status--phase-state 'source-build done ready)))
    (magit-insert-section (deb-packaging-source nil hide)
      (magit-insert-heading
        (deb-packaging-status--phase-heading state "Source build" 'source-build))
      (magit-insert-section-body
        (deb-packaging-status--insert-state-row
         (delq nil
               (list
                (when parent-dir
                  (cons "Output" (abbreviate-file-name parent-dir)))
                (cons "Orig tarball"
                      (if orig-tarball
                          (propertize (concat "✓ "
                                  (file-name-nondirectory orig-tarball))
                                      'font-lock-face 'deb-packaging-status-done)
                        (propertize "none" 'font-lock-face 'shadow)))
                (when source-format
                  (cons "Format" source-format)))))
        (cond
         (done
          (when dsc (deb-packaging-status--insert-file-line dsc))
          (when src-changes (deb-packaging-status--insert-file-line src-changes))
          (dolist (b buildinfo)
            (when (string-match-p "_source\\.buildinfo$" b)
              (deb-packaging-status--insert-file-line b))))
         ((not ready)
          (deb-packaging-status--insert-note
           "missing orig tarball; run git ubuntu export-orig")))))))

(defun deb-packaging-status--transient-args (prefix)
  "Return PREFIX's saved/default transient args, or nil.
Defensive: a missing prefix yields nil rather than signaling."
  (ignore-errors (transient-args prefix)))

(defun deb-packaging-status--transient-flag-p (prefix flag)
  "Return non-nil if FLAG is in PREFIX's saved/default transient args."
  (let ((args (deb-packaging-status--transient-args prefix)))
    (and args (member flag args) t)))

(defun deb-packaging-status--insert-binary (ctx hide)
  "Insert the Binary phase section from CTX, collapsed when HIDE."
  (let* ((arts (plist-get ctx :artifacts))
         (dsc (alist-get 'dsc arts))
         (bin-changes (alist-get 'binary-changes arts))
         (debs (alist-get 'debs arts))
         (arch (plist-get ctx :arch))
         (distro (deb-packaging-config--effective-distro))
         (schroot (when (and distro arch)
                    (deb-packaging-detect--schroot-exists-p distro arch)))
         (done (and bin-changes debs))
         (state (deb-packaging-status--phase-state 'sbuild done dsc))
         (detail (when debs
                   (propertize (format "  %d debs" (length debs))
                               'font-lock-face 'magit-section-child-count))))
    (magit-insert-section (deb-packaging-binary nil hide)
      (magit-insert-heading
        (deb-packaging-status--phase-heading state "Binary build" 'sbuild detail))
      (magit-insert-section-body
        (deb-packaging-status--insert-state-row
         (delq nil
               (list
                (cons "Schroot"
                      (if schroot
                          (propertize (concat "✓ " schroot)
                                      'font-lock-face 'deb-packaging-status-done)
                        (propertize "none" 'font-lock-face 'shadow)))
                (when arch (cons "Arch" arch))
                (cons "Dsc"
                      (if dsc
                          (propertize "✓ ready" 'font-lock-face
                                      'deb-packaging-status-done)
                        (propertize "none" 'font-lock-face 'shadow))))))
        (cond
         (done
          (dolist (c bin-changes)
            (deb-packaging-status--insert-file-line c))
          (dolist (d debs)
            (deb-packaging-status--insert-file-line d)))
         ((not dsc)
          (deb-packaging-status--insert-note "waiting on source build")))
        (when (deb-packaging-status--transient-flag-p
               'deb-packaging-binary-build-transient
               deb-packaging-transients-sbuild-shell-flag)
          (deb-packaging-status--insert-note
           "Drops into a chroot shell on build failure"))
        (when-let ((note (deb-packaging-status--kept-session-note)))
          (deb-packaging-status--insert-note note))))))

(defun deb-packaging-status--insert-lintian-child (section-type key label artifacts)
  "Insert one Lint child section of SECTION-TYPE for run key KEY.
LABEL is the heading; ARTIFACTS are files to lint, absence blocks the
child. Lint never reaches done: KEEP-READY returns success to ready so
it can re-run. SECTION-TYPE must be registered for RET to act on it."
  (let ((state (deb-packaging-status--phase-state key nil (and artifacts t) t)))
    (magit-insert-section ((eval section-type))
      (magit-insert-heading
        (concat "  "
                (propertize (deb-packaging-status--pad
                             label (- deb-packaging-status--label-width 2))
                            'font-lock-face 'magit-section-secondary-heading)
                (deb-packaging-status--state-word state)
                (deb-packaging-status--lint-summary-note key)
                (deb-packaging-status--run-time-note key)))
      (magit-insert-section-body
        (if artifacts
            (dolist (a artifacts)
              (deb-packaging-status--insert-file-line a))
          (deb-packaging-status--insert-note "waiting on build"))))))

(defun deb-packaging-status--lint-rollup-state (ctx)
  "Return a status symbol summarising the Lint section's children.
Priority: failed > running > ready > done > blocked. Children never
reach done and ubuntu-lint is always ready, so Lint is never blocked."
  (let* ((arts (plist-get ctx :artifacts))
         (dsc (alist-get 'dsc arts))
         (debs (alist-get 'debs arts))
         (states
          (list (deb-packaging-status--phase-state
                 'lintian-source nil (and dsc t) t)
                (deb-packaging-status--phase-state
                 'lintian-binary nil (and debs t) t)
                (deb-packaging-status--phase-state
                 'ubuntu-lint nil t t))))
    (cl-find-if (lambda (s) (memq s states))
                '(failed running ready done blocked))))

(defun deb-packaging-status--lint-hide-p (ctx)
  "Return non-nil if the Lint section should collapse by default."
  (let ((state (deb-packaging-status--lint-rollup-state ctx)))
    (not (memq state '(failed running)))))

(defun deb-packaging-status--insert-ubuntu-lint-child ()
  "Insert the Ubuntu lint child section under Lint.
Always ready inside a package. Like lintian, KEEP-READY keeps success
as ready."
  (let ((state (deb-packaging-status--phase-state 'ubuntu-lint nil t t)))
    (magit-insert-section (deb-packaging-commands-ubuntu-lint)
      (magit-insert-heading
        (concat "  "
                (propertize (deb-packaging-status--pad
                             "Ubuntu lint" (- deb-packaging-status--label-width 2))
                            'font-lock-face 'magit-section-secondary-heading)
                (deb-packaging-status--state-word state)
                (deb-packaging-status--lint-summary-note 'ubuntu-lint)
                (deb-packaging-status--run-time-note 'ubuntu-lint)))
      (magit-insert-section-body
        (deb-packaging-status--insert-note
         "Ubuntu upload policy checks (SRU, maintainer, bug references)")))))

(defun deb-packaging-status--insert-check (ctx hide)
  "Insert the Lint phase: lintian source/binary children plus ubuntu-lint."
  (let ((state (deb-packaging-status--lint-rollup-state ctx)))
    (magit-insert-section (deb-packaging-check nil hide)
      (magit-insert-heading
        (deb-packaging-status--phase-heading state "Lint"))
      (magit-insert-section-body
        (let* ((arts (plist-get ctx :artifacts))
               (dsc (alist-get 'dsc arts))
               (debs (alist-get 'debs arts)))
          (deb-packaging-status--insert-lintian-child
           'deb-packaging-commands-lintian-source 'lintian-source "Source"
           (when dsc (list dsc)))
          (deb-packaging-status--insert-lintian-child
           'deb-packaging-commands-lintian-binary 'lintian-binary "Binary" debs)
          (deb-packaging-status--insert-ubuntu-lint-child))))))

(defun deb-packaging-status--insert-test (ctx hide)
  "Insert the Test (autopkgtest) phase section from CTX, collapsed when HIDE."
  (let* ((arts (plist-get ctx :artifacts))
         (debs (alist-get 'debs arts))
         (arch (plist-get ctx :arch))
         (state (deb-packaging-status--phase-state 'autopkgtest nil debs)))
    (magit-insert-section (deb-packaging-test nil hide)
      (magit-insert-heading
        (deb-packaging-status--phase-heading state "Test" 'autopkgtest))
      (magit-insert-section-body
        (if (not debs)
            (deb-packaging-status--insert-note "waiting on binary build")
          (let* ((info (deb-packaging-commands--test-image-info))
                 (runner (plist-get info :runner))
                 (image (plist-get info :image))
                 (exists (plist-get info :exists)))
            (deb-packaging-status--insert-state-row
             (delq nil
                   (list
                    (when runner (cons "Runner" runner))
                    (when arch (cons "Arch" arch))
                    (when image
                      (cons "Image"
                            (if exists
                                (propertize (concat "✓ " image)
                                            'font-lock-face
                                            'deb-packaging-status-done)
                              (propertize (concat "✗ " image)
                                          'font-lock-face
                                          'deb-packaging-status-running)))))))
            (when (and image (not exists))
              (when-let ((hint (deb-packaging-commands--test-image-build-hint
                                 runner (deb-packaging-config--effective-distro))))
                (deb-packaging-status--insert-note
                 (format "Build it with: %s" hint))))
            (dolist (d debs)
              (deb-packaging-status--insert-file-line d))
            (when (deb-packaging-status--transient-flag-p
                   'deb-packaging-test-transient
                   "--shell-fail")
              (deb-packaging-status--insert-note
               "Drops into a testbed shell on test failure")))))
        (deb-packaging-status--insert-ppa-tests-row))))

(defun deb-packaging-status--transient-arg-value (prefix flag)
  "Return the value of FLAG from PREFIX's saved/default transient args.
Returns nil if the flag is unset or the prefix is unavailable."
  (let ((args (deb-packaging-status--transient-args prefix)))
    (when args
      (transient-arg-value flag args))))

(defun deb-packaging-status--insert-upload (ctx hide)
  "Insert the Upload (Launchpad PPA) phase section, collapsed when HIDE."
  (let* ((arts (plist-get ctx :artifacts))
         (changes (alist-get 'source-changes arts))
         (ppa (deb-packaging-status--transient-arg-value
               'deb-packaging-upload-transient "--ppa="))
         (state (deb-packaging-status--phase-state 'dput nil t)))
    (magit-insert-section (deb-packaging-upload nil hide)
      (magit-insert-heading
        (deb-packaging-status--phase-heading state "Upload" 'dput))
      (magit-insert-section-body
        (deb-packaging-status--insert-state-row
         (delq nil
               (list
                (cons "PPA"
                      (or ppa
                          (propertize "not set" 'font-lock-face 'shadow)))
                (cons "Changes"
                      (if changes
                          (file-name-nondirectory changes)
                        (propertize "waiting on source build"
                                    'font-lock-face 'shadow))))))
        (when changes
          (deb-packaging-status--insert-file-line changes))))))

(defun deb-packaging-status--group-stale-by-version (stale-files)
  "Group STALE-FILES by version, returning an alist of (version . files).
Unparseable versions group under \"unknown\"."
  (let ((groups nil))
    (dolist (f stale-files)
      (let ((ver (or (deb-packaging-detect--filename-version f) "unknown")))
        (setf (alist-get ver groups nil 'remove)
              (nconc (alist-get ver groups nil 'remove)
                     (list f)))))
    (sort groups (lambda (a b) (string< (car a) (car b))))))

(defun deb-packaging-status--insert-stale (ctx hide)
  "Insert the Stale artifacts section from CTX, collapsed when HIDE."
  (let ((stale (plist-get ctx :stale)))
    (when stale
      (magit-insert-section (deb-packaging-stale nil hide)
        (magit-insert-heading
          (concat
           (propertize (deb-packaging-status--pad
                        "Stale artifacts" deb-packaging-status--label-width)
                       'font-lock-face 'magit-section-heading)
           (propertize (format "%d" (length stale))
                       'font-lock-face 'warning)
           (propertize "  from other versions"
                       'font-lock-face 'shadow)))
        (magit-insert-section-body
          (dolist (group (deb-packaging-status--group-stale-by-version stale))
            (let ((ver (car group))
                  (files (cdr group)))
              (insert (format "    %s:\n"
                              (propertize ver 'font-lock-face
                                          'magit-section-secondary-heading)))
              (dolist (f files)
                (insert (format "      %s\n"
                                (propertize f 'font-lock-face
                                            'magit-section-secondary-heading)))))))))))

;;; Buffer rendering

(defun deb-packaging-status--insert-dev (ctx hide)
  "Insert a Dev section showing LXD dev containers for CTX's package.
Collapsed when HIDE. Containers match by name prefix."
  (let* ((name (plist-get ctx :name))
         (distro (plist-get ctx :distro))
         (containers (deb-packaging-dev--list-containers
                      (when name (format "deb-dev-%s-" name))))
         (target (when (and name distro)
                   (format "deb-dev-%s-%s" name distro)))
         (target-found (and target
                            (cl-find target containers
                                     :key (lambda (c) (plist-get c :name))
                                     :test #'equal))))
    (when containers
      (magit-insert-section (deb-packaging-dev nil hide)
        (magit-insert-heading
          (concat
           (propertize (deb-packaging-status--pad
                        "Dev shell" deb-packaging-status--label-width)
                       'font-lock-face 'magit-section-heading)
           (propertize
            (if target
                (if target-found "ready" "none")
              (format "%d" (length containers)))
            'font-lock-face
            (if target-found
                'deb-packaging-status-done
              'shadow))))
        (magit-insert-section-body
          (dolist (c containers)
            (let* ((cname (plist-get c :name))
                   (status (plist-get c :status))
                   (source (plist-get c :source))
                   (currentp (equal cname target)))
              (insert
               (format "    %s %s %s\n"
                       (propertize cname 'font-lock-face
                                   (if currentp
                                       'magit-section-secondary-heading
                                     'shadow))
                       (propertize (downcase status) 'font-lock-face
                                   (if (string= status "RUNNING")
                                       'deb-packaging-status-done
                                     'shadow))
                       (if source
                           (propertize (concat "  " source)
                                       'font-lock-face 'shadow)
                          ""))))))))))

(defun deb-packaging-status--insert-pq (ctx hide)
  "Insert a Patch Queue (gbp pq) section for 3.0 (quilt) source format.
Shows whether a patch-queue branch exists and whether point is on it.
Collapsed when HIDE."
  (let ((source-format (plist-get ctx :source-format)))
    (when (and source-format (string= source-format "3.0 (quilt)"))
      (let* ((state (deb-packaging-pq--state))
             (on-pq (plist-get state :on-pq-p))
             (exists (plist-get state :exists-p))
             (branch (plist-get state :branch))
             (pq-branch (plist-get state :pq-branch))
             (state-word
              (cond
               (on-pq
                (propertize "editing"
                            'font-lock-face 'deb-packaging-status-running))
               (exists
                (propertize "ready"
                            'font-lock-face 'deb-packaging-status-ready))
               (t
                (propertize "none"
                            'font-lock-face 'shadow))))
             (detail
              (cond
               (on-pq
                (propertize (format "  on %s, export when done" branch)
                            'font-lock-face 'shadow))
               (exists
                (propertize (format "  %s exists, switch to edit" pq-branch)
                            'font-lock-face 'shadow))
               (t
                (propertize "  import to start editing patches as commits"
                            'font-lock-face 'shadow)))))
        (magit-insert-section (deb-packaging-pq nil hide)
          (magit-insert-heading
            (concat
             (propertize (deb-packaging-status--pad
                          "Patch queue" deb-packaging-status--label-width)
                         'font-lock-face 'magit-section-heading)
             state-word
             detail)))))))

(defun deb-packaging-status--next-actionable-key (ctx)
  "Return the run-history key of the first ready phase in CTX, or nil.
Walks phases in flow order; picks which phase smart-fold expands."
  (let* ((arts (plist-get ctx :artifacts))
         (dsc (alist-get 'dsc arts))
         (src-changes (alist-get 'source-changes arts))
         (bin-changes (alist-get 'binary-changes arts))
         (debs (alist-get 'debs arts))
         (phases
          (list (cons 'source-build
                      (deb-packaging-status--phase-state
                       'source-build (and dsc src-changes)
                       (deb-packaging-status--source-ready-p ctx)))
                (cons 'sbuild
                      (deb-packaging-status--phase-state
                       'sbuild (and bin-changes debs) dsc))
                (cons 'autopkgtest
                      (deb-packaging-status--phase-state 'autopkgtest nil debs))
                (cons 'dput
                      ;; Upload is always ready; PPA is set inside its transient.
                      (deb-packaging-status--phase-state 'dput nil t)))))
    (car (cl-find 'ready phases :key #'cdr))))

(defun deb-packaging-status--render ()
  "Render the status buffer from freshly collected context.
Point ends on the first phase heading."
  (let ((ctx (deb-packaging-status--collect-context))
        (inhibit-read-only t))
    (setq deb-packaging-status--context ctx)
    (erase-buffer)
    (magit-insert-section (deb-packaging-status-root)
      (if (null ctx)
          (insert (propertize "Not in a Debian package directory."
                              'font-lock-face 'error)
                  "\n\nVisit a tree containing debian/changelog, then press g.\n")
        (let* ((next (deb-packaging-status--next-actionable-key ctx))
               (arts (plist-get ctx :artifacts))
               (dsc (alist-get 'dsc arts))
               (src-done (and dsc (alist-get 'source-changes arts)))
               (bin-done (and (alist-get 'binary-changes arts)
                              (alist-get 'debs arts)))
               (debs (alist-get 'debs arts)))
          (deb-packaging-status--insert-header ctx)
          (deb-packaging-status--insert-source
           ctx (deb-packaging-status--hide-phase-p
                (deb-packaging-status--phase-state
                 'source-build src-done
                 (deb-packaging-status--source-ready-p ctx))
                next 'source-build))
          (deb-packaging-status--insert-binary
           ctx (deb-packaging-status--hide-phase-p
                (deb-packaging-status--phase-state 'sbuild bin-done dsc)
                next 'sbuild))
          ;; Lint groups two children and has no single phase state, so
          ;; use the rollup fold decision instead.
          (deb-packaging-status--insert-check
           ctx (deb-packaging-status--lint-hide-p ctx))
          (deb-packaging-status--insert-test
           ctx (deb-packaging-status--hide-phase-p
                (deb-packaging-status--phase-state 'autopkgtest nil debs)
                next 'autopkgtest))
          (deb-packaging-status--insert-upload
           ctx (deb-packaging-status--hide-phase-p
                (deb-packaging-status--phase-state 'dput nil t)
                next 'dput))
          (deb-packaging-status--insert-stale ctx t)
          (deb-packaging-status--insert-dev ctx t)
          (deb-packaging-status--insert-pq ctx t))))
     ;; Show the root once so fold indicators appear before any manual toggle.
     (when magit-root-section
       (magit-section-show magit-root-section))))

(defun deb-packaging-status--goto-first-phase ()
  "Move point to the first phase heading (Source build)."
  (goto-char (point-min))
  (when-let ((section (magit-get-section
                       '((deb-packaging-source)
                         (deb-packaging-status-root)))))
    (goto-char (oref section start))))

(defun deb-packaging-status-refresh ()
  "Refresh the status buffer, keeping point on the same section.
On a fresh buffer, point lands on the first phase heading."
  (interactive)
  (when (derived-mode-p 'deb-packaging-status-mode)
    (let* ((section (magit-current-section))
           (was-root (or (null section)
                         (eq (oref section type) 'deb-packaging-status-root)))
           (line (and (not was-root)
                      (count-lines (oref section start) (point))))
           (char (and (not was-root)
                      (- (point) (line-beginning-position)))))
      (deb-packaging-status--render)
      (if was-root
          (deb-packaging-status--goto-first-phase)
        (if-let ((new (and section
                           (magit-get-section (magit-section-ident section)))))
            (progn
              (goto-char (oref new start))
              (forward-line line)
              (forward-char (min char (- (line-end-position) (point)))))
          (deb-packaging-status--goto-first-phase))))))

(defun deb-packaging-status--maybe-refresh ()
  "Refresh every live status buffer.
Called from process sentinels after a run finishes."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'deb-packaging-status-mode)
          (deb-packaging-status-refresh))))))

(defvar-local deb-packaging-status--refresh-timer nil
  "Idle timer for debounced window-selection refresh, or nil.")

(defun deb-packaging-status--on-window-selected (_window)
  "Re-scan and refresh the status buffer when it gains selection.
Debounced via idle timer so rapid window switches do not scan the
filesystem each time."
  (when (and (derived-mode-p 'deb-packaging-status-mode)
             (eq (current-buffer) (window-buffer (selected-window))))
    (when deb-packaging-status--refresh-timer
      (cancel-timer deb-packaging-status--refresh-timer))
    (setq deb-packaging-status--refresh-timer
          (run-with-idle-timer 0.4 nil
                               #'deb-packaging-status-refresh))))

;;; Actions

(defun deb-packaging-status--open (transient-prefix)
  "Open TRANSIENT-PREFIX from the status buffer."
  (call-interactively transient-prefix))

(defun deb-packaging-status-visit ()
  "Open the transient for the section at point.
Walks up the section tree to the nearest registered type."
  (interactive)
  (let ((section (magit-current-section))
        (prefix nil))
    (while (and section (not prefix))
      (setq prefix (alist-get (oref section type)
                               deb-packaging-status--section-actions))
      (setq section (oref section parent)))
    (if prefix
        (deb-packaging-status--open prefix)
      (user-error "No action for the section at point"))))

(defun deb-packaging-status-build-source ()
  "Open the source-build transient."
  (interactive)
  (deb-packaging-status--open #'deb-packaging-commands-source-build-transient))

(defun deb-packaging-status-build-binary ()
  "Open the binary-build transient."
  (interactive)
  (deb-packaging-status--open #'deb-packaging-binary-build-transient))

(defun deb-packaging-status-lint ()
  "Open the lint transient."
  (interactive)
  (deb-packaging-status--open #'deb-packaging-lint-transient))

(defun deb-packaging-status-test ()
  "Open the autopkgtest transient."
  (interactive)
  (deb-packaging-status--open #'deb-packaging-test-transient))

(defun deb-packaging-status-upload ()
  "Open the PPA upload transient."
  (interactive)
  (deb-packaging-status--open #'deb-packaging-upload-transient))

(defun deb-packaging-status-clean ()
  "Open the clean artifacts transient."
  (interactive)
  (deb-packaging-status--open #'deb-packaging-commands-clean-transient))

(defun deb-packaging-status-reset ()
  "Open the source-tree reset transient."
  (interactive)
  (deb-packaging-status--open #'deb-packaging-commands-reset-transient))

(defun deb-packaging-status-dev ()
  "Open the dev shell transient."
  (interactive)
  (deb-packaging-status--open #'deb-packaging-dev-transient))

(defun deb-packaging-status-pq ()
  "Open the patch-queue (gbp pq) transient."
  (interactive)
  (deb-packaging-status--open #'deb-packaging-pq-transient))

;;; Major mode

(defvar-keymap deb-packaging-status-mode-map
  :doc "Keymap for `deb-packaging-status-mode'.
RET opens the section's transient. Mnemonic verbs open tool transients.
Navigation and folding come from `magit-section-mode'."
  :parent magit-section-mode-map
  "RET" #'deb-packaging-status-visit
  "s"   #'deb-packaging-status-build-source
  "b"   #'deb-packaging-status-build-binary
  "l"   #'deb-packaging-status-lint
  "t"   #'deb-packaging-status-test
  "U"   #'deb-packaging-status-upload
  "c"   #'deb-packaging-status-clean
  "r"   #'deb-packaging-status-reset
  "e"   #'deb-packaging-status-dev
  "u"   #'deb-packaging-status-pq
  "i"   #'deb-packaging-infra-dispatch
  "P"   #'deb-packaging-propagate-transient
  "?"   #'deb-packaging-dispatch
  "g"   #'deb-packaging-status-refresh
  "q"   #'quit-window)

(define-derived-mode deb-packaging-status-mode magit-section-mode "Deb-Status"
  "Major mode for the Debian packaging status landing page."
  :interactive nil
  ;; Buffer-local hook cleans up with the buffer.
  (add-hook 'window-selection-change-functions
            #'deb-packaging-status--on-window-selected nil t))

;;;###autoload
(defun deb-packaging-status ()
  "Open the Debian packaging status buffer."
  (interactive)
  (let* ((pkg-dir (deb-packaging-detect--find-package-dir nil t))
         (name (deb-packaging-detect--package-name pkg-dir))
         (buf (get-buffer-create (deb-packaging-status--buffer-name name))))
    (with-current-buffer buf
      (when pkg-dir
        (setq default-directory pkg-dir))
      (unless (derived-mode-p 'deb-packaging-status-mode)
        (deb-packaging-status-mode))
      (deb-packaging-status--render)
      (deb-packaging-status--goto-first-phase))
    (deb-packaging-display-buffer buf 'status)))

(provide 'deb-packaging-status)
;;; deb-packaging-status.el ends here
