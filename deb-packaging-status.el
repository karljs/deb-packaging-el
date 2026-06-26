;;; deb-packaging-status.el --- Status landing page for deb-packaging -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/example/deb-packaging
;; Package-Requires: ((emacs "28.1") (magit-section "3.3"))

;;; Commentary:

;; A Magit-style status buffer that is the primary entry point for the
;; deb-packaging workflow.  It shows the package as it moves through its
;; phases (source build -> binary build -> lint -> test -> upload), with a
;; status indicator per phase derived from on-disk artifacts and in-memory
;; run history.
;;
;; Interaction model (Magit-idiomatic):
;;
;;   RET   run the action for the section at point (build/lint/test/...).
;;   TAB   fold/unfold the section at point.
;;   n p   move between sections (inherited from `magit-section-mode').
;;   s b t c   mnemonic verbs for the common flow actions.
;;   ?     open the `deb-packaging-dispatch' transient, which owns all the
;;         configuration (mode/variant/runner/distro/PPA) and infrastructure.
;;   g     refresh.   q  quit.
;;
;; The status buffer drives FLOW (what is the state, do the next thing); the
;; transient drives CONFIGURATION (how the next thing should run).
;;
;; Layout and folding: the work phases come first, in flow order, as terse
;; headings.  State is carried by a single color-coded status word at the end
;; of each heading (ready/running/done/failed/blocked) rather than by glyphs or
;; bullets, so colour is the primary signal and the headings stay consistent.
;; Per-phase detail (command line, artifacts) lives in a foldable body.
;; Smart-fold defaults expand the next actionable phase and any failed/running
;; phase while collapsing the rest; because the default is passed as the HIDE
;; argument to `magit-insert-section' (which yields to a section's previous
;; visibility on refresh), a user's manual TAB toggles persist.  Stale-artifact
;; warnings and a collapsed Settings section sit at the bottom.  Point opens on
;; the first phase heading.
;;
;; State: there is no private cache.  Every render calls the shared,
;; side-effect-free `deb-packaging--scan-context', which both this buffer and
;; the dispatch transient read, so the two never disagree.  The buffer also
;; re-scans on window selection (see `deb-packaging-status--on-window-selected')
;; so external changes are picked up without an explicit refresh.
;;
;; Two directories matter and are surfaced explicitly in the header:
;;
;;   Source  the repository containing debian/changelog (the buffer's
;;           `default-directory'; commands run from here).
;;   Output  the PARENT of the source tree, where dpkg-buildpackage and
;;           sbuild drop their artifacts.  This directory is typically
;;           shared across packages and versions, so the buffer scopes the
;;           current package's artifacts by name+version and warns about
;;           leftover artifacts from other versions of this same package.
;;
;; Main entry point: `deb-packaging-status'.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'deb-packaging-detect)
(require 'deb-packaging-presets)
(require 'deb-packaging-commands)

(declare-function deb-packaging--effective-distro "deb-packaging-presets")
(declare-function transient-args "transient")
(declare-function transient-arg-value "transient")

;; Transient prefixes opened by RET/mnemonic keys; loaded by deb-packaging.el.
(declare-function deb-packaging-source-build-transient "deb-packaging-transients")
(declare-function deb-packaging-binary-build-transient "deb-packaging-transients")
(declare-function deb-packaging-lint-transient "deb-packaging-transients")
(declare-function deb-packaging-test-transient "deb-packaging-transients")
(declare-function deb-packaging-upload-transient "deb-packaging-transients")
(declare-function deb-packaging-clean-transient "deb-packaging-transients")
(declare-function deb-packaging-reset-transient "deb-packaging-transients")
;; Other cross-file references
(declare-function deb-packaging--run-summary "deb-packaging-commands")
(declare-function deb-packaging--schroot-exists-p "deb-packaging-detect")
(declare-function deb-packaging--filename-version "deb-packaging-detect")
(declare-function deb-packaging-dispatch "deb-packaging")
(declare-function deb-packaging-infra-dispatch "deb-packaging-infra")

;;; Buffer-local context

(defvar-local deb-packaging-status--context nil
  "Buffer-local plist describing the package shown in the status buffer.
Keys: :name :version :distro :pkg-dir :parent-dir :artifacts :stale
:source-format :orig-tarball :arch :maintainer.")

(defun deb-packaging-status--buffer-name (name)
  "Return the status buffer name for package NAME."
  (format "*deb-packaging: %s*" (or name "?")))

(defun deb-packaging-status--collect-context ()
  "Gather fresh package context from the current `default-directory'.
Return a plist, or nil when not inside a Debian package tree.  Delegates to
the shared, side-effect-free `deb-packaging--scan-context' so the status
buffer and the dispatch transient always agree, then seeds the target distro
once (never clobbering a value the user has chosen)."
  (let ((ctx (deb-packaging--scan-context)))
    (when ctx
      (deb-packaging--maybe-seed-distro (plist-get ctx :distro)))
    ctx))

(defun deb-packaging-status--current-context ()
  "Return the context of the live status buffer, if one exists.
Used by `deb-packaging.el' so the dispatch transient reflects exactly what
the status buffer shows.  Does not create or refresh a buffer."
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
;; Each actionable section carries a distinct type symbol.  `RET' walks up
;; from the section at point until it finds a type in this table and invokes
;; the corresponding command.  This is the primary, context-sensitive way to
;; act; the mnemonic verbs below are just shortcuts to the same commands.

(defconst deb-packaging-status--section-actions
  '((deb-packaging-source         . deb-packaging-source-build-transient)
    (deb-packaging-binary         . deb-packaging-binary-build-transient)
    (deb-packaging-check          . deb-packaging-lint-transient)
    (deb-packaging-test           . deb-packaging-test-transient)
    (deb-packaging-upload         . deb-packaging-upload-transient)
    (deb-packaging-stale          . deb-packaging-clean-transient))
  "Map status-buffer section types to the transient `RET' should open.
The Lint phase (`deb-packaging-check') is a parent section whose children
are display-only; RET on the parent opens the lint transient directly,
so no expand is needed.")

;;; Status words
;;
;; Instead of decorative bullet glyphs, each phase heading ends in a single
;; colored status word.  Colour is the primary signal (green=done, red=failed,
;; yellow=running, dim=pending), the way magit colours branch state; the word
;; is there for non-colour terminals and accessibility.

(defconst deb-packaging-status--state-words
  '((running . ("running" . deb-packaging-status-running))
    (failed  . ("failed"  . deb-packaging-status-failed))
    (done    . ("done"    . deb-packaging-status-done))
    (ready   . ("ready"   . deb-packaging-status-ready))
    (blocked . ("blocked" . deb-packaging-status-blocked)))
  "Map a phase state symbol to its (WORD . FACE) for the trailing status word.")

(defun deb-packaging-status--state-word (state)
  "Return the propertized trailing status word for STATE."
  (let ((entry (alist-get state deb-packaging-status--state-words)))
    (propertize (car entry) 'font-lock-face (cdr entry))))

(defun deb-packaging-status--run-time-note (key)
  "Return a dimmed \" (HH:MM:SS)\" note for KEY's last run, or empty string."
  (if-let* ((record (deb-packaging-run-record key))
            (time (plist-get record :time)))
      (propertize (format " %s" time) 'font-lock-face 'shadow)
    ""))

(defun deb-packaging-status--lint-summary-note (key)
  "Return a colored findings summary for KEY's lintian run, or empty string.
Format: \" 2E 5W 12I\" with each count colored by severity."
  (if-let* ((summary (deb-packaging--run-summary key)))
      (let* ((errs (plist-get summary :error))
             (warns (plist-get summary :warning))
             (infos (plist-get summary :info))
             (fmt (lambda (n face)
                    (propertize (format "%d" n) 'font-lock-face face))))
        (concat
         "  "
         (funcall fmt errs 'deb-packaging-status-failed) "E "
         (funcall fmt warns 'deb-packaging-status-running) "W "
         (funcall fmt infos 'shadow) "I"))
    ""))

;;; Phase state and fold decisions
;;
;; A phase is "done" when its on-disk artifacts are present OR its last run
;; succeeded; "running"/"failed" come from the in-memory run history.  The
;; smart-fold rule (see `deb-packaging-status--insert-phase') collapses done
;; phases and expands the first not-yet-done phase plus any failed phase, so the
;; buffer foregrounds the next action.  This is only the *default*: it is passed
;; as the HIDE argument to `magit-insert-section', which `magit-section' ignores
;; on refresh in favour of the section's previous visibility — so once you TAB a
;; section open or closed, your choice sticks across refreshes.

(defun deb-packaging-status--phase-state (key done ready &optional keep-ready)
  "Return a phase state symbol for KEY.
DONE means the phase's artifacts are present (or it has succeeded).  READY
means the phase's prerequisites are met so it can run now (a phase that is
neither done nor ready is `blocked' waiting on an earlier phase).  Returns
one of `running', `failed', `done', `ready' or `blocked'.  A live run wins;
then a failed run; then completion; then readiness.

When KEEP-READY is non-nil, a successful run does NOT flip to `done' — the
phase stays `ready' (runnable) so the user can re-run after fixing findings.
This is used for lint, which is an iterative check rather than a phase that
produces a deliverable."
  (let ((status (plist-get (deb-packaging-run-record key) :status)))
    (cond ((eq status 'running) 'running)
          ((eq status 'failure) 'failed)
          ((and (not keep-ready) (or done (eq status 'success))) 'done)
          (ready 'ready)
          (t 'blocked))))

(defun deb-packaging-status--actionable-state-p (state)
  "Return non-nil when STATE represents a phase worth foregrounding."
  (memq state '(running failed ready)))

(defun deb-packaging-status--hide-phase-p (state next-key key)
  "Return non-nil if a phase in STATE should collapse by default.
Expand `failed' and `running' phases and the single next actionable phase
\(its KEY equals NEXT-KEY); collapse everything else."
  (cond
   ((memq state '(failed running)) nil)
   ((eq key next-key) nil)
   (t t)))

;;; Rendering helpers
;;
;; IMPORTANT: `magit-section-mode' sets `font-lock-defaults' to (nil t), i.e.
;; keywords-only fontification, under which the buffer honours the
;; `font-lock-face' text property and IGNORES the plain `face' property.  All
;; propertized text inserted here must therefore use `font-lock-face', not
;; `face', or it will appear unstyled.

(defface deb-packaging-status-title
  '((t :inherit magit-section-heading :weight bold :height 1.2))
  "Face for the package name in the title line."
  :group 'deb-packaging)

(defface deb-packaging-status-version
  '((t :inherit magit-section-secondary-heading :weight normal))
  "Face for the version in the title line."
  :group 'deb-packaging)

(defface deb-packaging-status-distro
  '((t :inherit success))
  "Face for the target distribution in the title line."
  :group 'deb-packaging)

(defface deb-packaging-status-path
  '((t :inherit shadow))
  "Face for the repository path line under the title."
  :group 'deb-packaging)

(defface deb-packaging-status-key
  '((t :inherit shadow))
  "Face for a settings key label (e.g. \"Mode\")."
  :group 'deb-packaging)

(defface deb-packaging-status-value
  '((t :inherit default))
  "Face for a settings value (e.g. the current distro)."
  :group 'deb-packaging)

(defface deb-packaging-status-done
  '((t :inherit success))
  "Face for the `done' status word."
  :group 'deb-packaging)

(defface deb-packaging-status-failed
  '((t :inherit error :weight bold))
  "Face for the `failed' status word."
  :group 'deb-packaging)

(defface deb-packaging-status-running
  '((t :inherit warning :weight bold))
  "Face for the `running' status word."
  :group 'deb-packaging)

(defface deb-packaging-status-ready
  '((((class color) (background light)) :foreground "DodgerBlue4" :weight bold)
    (((class color) (background dark))  :foreground "DeepSkyBlue1" :weight bold)
    (t :weight bold))
  "Face for the `ready' status word — the actionable next step."
  :group 'deb-packaging)

(defface deb-packaging-status-blocked
  '((t :inherit shadow))
  "Face for the `blocked' status word."
  :group 'deb-packaging)

(defconst deb-packaging-status--label-width 16
  "Column width for phase labels, so trailing status words align.")

(defun deb-packaging-status--pad (text width)
  "Left-justify TEXT to WIDTH columns (Emacs `format' has no dynamic width)."
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
  "Insert an indented PATH line with size and modification time.
Aligns the basename to a fixed column, then shows size and date."
  (let* ((base (file-name-nondirectory path))
         (size (deb-packaging-status--file-size path))
         (mtime (deb-packaging-status--file-mtime path)))
    (insert (format "    %-45s %8s  %s\n"
                    (propertize base 'font-lock-face
                                'magit-section-secondary-heading)
                    (propertize size 'font-lock-face 'shadow)
                    (propertize mtime 'font-lock-face 'shadow)))))

(defun deb-packaging-status--insert-note (text)
  "Insert an indented, dimmed informational note TEXT inside a body."
  (insert (format "    %s\n" (propertize text 'font-lock-face 'shadow))))

(defun deb-packaging-status--insert-state-row (pairs)
  "Insert a state row from PAIRS, a list of (label . value) cons cells.
Each pair is rendered as \"Label: value\" with the label dimmed and the
value in the default face.  Pairs are separated by 4 spaces to use
horizontal space, wrapping naturally when the frame is narrow."
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
  "Insert the package title line, path, and optional stale indicator from CTX."
  (let ((name (plist-get ctx :name))
        (version (plist-get ctx :version))
        (distro (plist-get ctx :distro))
        (pkg-dir (plist-get ctx :pkg-dir))
        (stale (plist-get ctx :stale)))
    (insert (propertize name 'font-lock-face 'deb-packaging-status-title)
            " "
            (propertize version 'font-lock-face 'deb-packaging-status-version)
            "  "
            (propertize (or distro deb-packaging-target-distro)
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
LABEL is the phase name (rendered in the section-heading face); STATE drives
the trailing colored status word; KEY adds the last-run time; DETAIL is an
optional dimmed fragment shown right after the label (e.g. a count)."
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
         (state (deb-packaging-status--phase-state 'source-build done t)))
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
        (when done
          (when dsc (deb-packaging-status--insert-file-line dsc))
          (when src-changes (deb-packaging-status--insert-file-line src-changes))
          (dolist (b buildinfo)
            (when (string-match-p "_source\\.buildinfo$" b)
              (deb-packaging-status--insert-file-line b))))))))

(defun deb-packaging-status--transient-flag-p (prefix flag)
  "Return non-nil if FLAG is present in PREFIX's saved/default transient args."
  (and (fboundp 'transient-args)
       (let ((args (ignore-errors (transient-args prefix))))
         (and args (member flag args) t))))

(defun deb-packaging-status--insert-binary (ctx hide)
  "Insert the Binary phase section from CTX, collapsed when HIDE."
  (let* ((arts (plist-get ctx :artifacts))
         (dsc (alist-get 'dsc arts))
         (bin-changes (alist-get 'binary-changes arts))
         (debs (alist-get 'debs arts))
         (arch (plist-get ctx :arch))
         (distro (deb-packaging--effective-distro))
         (schroot (when (and distro arch)
                    (deb-packaging--schroot-exists-p distro arch)))
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
               "--build-failed-commands=%SBUILD_SHELL")
          (deb-packaging-status--insert-note
           "Debug shell enabled, drops into chroot on build failure"))))))

(defun deb-packaging-status--insert-lintian-child (section-type key label artifacts)
  "Insert one Lint child section of SECTION-TYPE for run key KEY.
LABEL is the heading text; ARTIFACTS is the list of files to lint (or nil),
whose absence makes the child `blocked'.  Lint never reaches `done': a
successful run returns to `ready' (KEEP-READY) so the user can re-run
after fixing findings.  When a lintian run has completed, the heading
also shows a colored findings summary (e.g. \"2E 5W 12I\").
SECTION-TYPE must be registered in `deb-packaging-status--section-actions'
so RET acts on it."
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

(defun deb-packaging-status--lint-rollup-state (dsc debs)
  "Return a single status symbol summarising the two lintian children.
DSC is the source .dsc (or nil); DEBS is the list of .deb files (or nil).
The worst/most-actionable child wins:
  failed > running > ready > done > blocked.
Lint children never reach `done'
\(see `deb-packaging-status--insert-lintian-child')."
  (let ((states
         (list (deb-packaging-status--phase-state
                'lintian-source nil (and dsc t) t)
               (deb-packaging-status--phase-state
                'lintian-binary nil (and debs t) t))))
    (cl-find-if (lambda (s) (memq s states))
                '(failed running ready done blocked))))

(defun deb-packaging-status--insert-check (ctx hide)
  "Insert the Lint (lintian) phase as two actionable child sections.
Source lint targets the .dsc; binary lint targets the .deb files."
  (let* ((arts (plist-get ctx :artifacts))
         (dsc (alist-get 'dsc arts))
         (debs (alist-get 'debs arts))
         (state (deb-packaging-status--lint-rollup-state dsc debs)))
    (magit-insert-section (deb-packaging-check nil hide)
      (magit-insert-heading
        (deb-packaging-status--phase-heading state "Lint"))
      (magit-insert-section-body
        (deb-packaging-status--insert-lintian-child
         'deb-packaging-lintian-source 'lintian-source "Source"
         (when dsc (list dsc)))
        (deb-packaging-status--insert-lintian-child
         'deb-packaging-lintian-binary 'lintian-binary "Binary" debs)))))

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
          (let* ((info (deb-packaging--test-image-info))
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
              (when-let ((hint (deb-packaging--test-image-build-hint
                                 runner (deb-packaging--effective-distro))))
                (deb-packaging-status--insert-note
                 (format "Build it with: %s" hint))))
            (dolist (d debs)
              (deb-packaging-status--insert-file-line d))
            (when (deb-packaging-status--transient-flag-p
                   'deb-packaging-test-transient
                   "--shell-fail")
              (deb-packaging-status--insert-note
               "Shell on failure enabled, drops into testbed on failure"))))))))

(defun deb-packaging-status--transient-arg-value (prefix flag)
  "Return the value of FLAG from PREFIX's saved/default transient args.
Returns nil if the flag is not set or transient is unavailable."
  (and (fboundp 'transient-args)
       (let ((args (ignore-errors (transient-args prefix))))
         (when args
           (transient-arg-value flag args)))))

(defun deb-packaging-status--insert-upload (ctx hide)
  "Insert the Upload (Launchpad PPA) phase section, collapsed when HIDE."
  (let* ((arts (plist-get ctx :artifacts))
         (changes (alist-get 'source-changes arts))
         (ppa (deb-packaging-status--transient-arg-value
               'deb-packaging-upload-transient "--ppa="))
         (state (deb-packaging-status--phase-state 'ppa-tests nil t)))
    (magit-insert-section (deb-packaging-upload nil hide)
      (magit-insert-heading
        (deb-packaging-status--phase-heading state "Upload" 'ppa-tests))
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
Files whose version cannot be parsed are grouped under \"unknown\"."
  (let ((groups nil))
    (dolist (f stale-files)
      (let ((ver (or (deb-packaging--filename-version f) "unknown")))
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

(defun deb-packaging-status--next-actionable-key (ctx)
  "Return the run-history key of the first `ready' phase in CTX, or nil.
Walks the phases in flow order; the result is the phase the smart-fold
default leaves expanded (in addition to any running/failed phase)."
  (let* ((arts (plist-get ctx :artifacts))
         (dsc (alist-get 'dsc arts))
         (src-changes (alist-get 'source-changes arts))
         (bin-changes (alist-get 'binary-changes arts))
         (debs (alist-get 'debs arts))
         (phases
          (list (cons 'source-build
                      (deb-packaging-status--phase-state
                       'source-build (and dsc src-changes) t))
                (cons 'sbuild
                      (deb-packaging-status--phase-state
                       'sbuild (and bin-changes debs) dsc))
                (cons 'autopkgtest
                      (deb-packaging-status--phase-state 'autopkgtest nil debs))
                (cons 'ppa-tests
                      ;; Upload is always ready; PPA is set inside its transient.
                      (deb-packaging-status--phase-state 'ppa-tests nil t)))))
    (car (cl-find 'ready phases :key #'cdr))))

(defun deb-packaging-status--render ()
  "Render the status buffer from freshly collected context.
Phases come first as terse headings whose colored trailing word carries the
state; per-phase detail lives in foldable bodies.  The next actionable phase
and any running/failed phase are expanded by default while the rest collapse.
Stale artifacts and configuration are folded sections at the bottom.  Point
is left on the first phase heading."
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
                (deb-packaging-status--phase-state 'source-build src-done t)
                next 'source-build))
          (deb-packaging-status--insert-binary
           ctx (deb-packaging-status--hide-phase-p
                (deb-packaging-status--phase-state 'sbuild bin-done dsc)
                next 'sbuild))
          ;; Lint groups two children; expand when either child is live or
          ;; failed (there is no single phase state for the group).
          (deb-packaging-status--insert-check
           ctx (not (or (memq (plist-get (deb-packaging-run-record 'lintian-source) :status)
                              '(running failure))
                        (memq (plist-get (deb-packaging-run-record 'lintian-binary) :status)
                              '(running failure)))))
          (deb-packaging-status--insert-test
           ctx (deb-packaging-status--hide-phase-p
                (deb-packaging-status--phase-state 'autopkgtest nil debs)
                next 'autopkgtest))
          (deb-packaging-status--insert-upload
           ctx (deb-packaging-status--hide-phase-p
                (deb-packaging-status--phase-state 'ppa-tests nil t)
                                 next 'ppa-tests))
          (deb-packaging-status--insert-stale ctx t))))
     ;; Walk the freshly-built tree once, applying each section's initial
     ;; visibility through the show/hide path.  This is what creates the fold
     ;; indicators; without it the `>'/`v' cue is missing until the first manual
     ;; toggle.
     (when magit-root-section
       (magit-section-show magit-root-section))))

(defun deb-packaging-status--goto-first-phase ()
  "Move point to the first phase heading (the Source build section)."
  (goto-char (point-min))
  (when-let ((section (magit-get-section
                       '((deb-packaging-source)
                         (deb-packaging-status-root)))))
    (goto-char (oref section start))))

(defun deb-packaging-status-refresh ()
  "Refresh the status buffer, keeping point on the same section.
On a freshly rendered buffer point lands on the first phase heading."
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
Called from the command layer's process sentinels after a run completes."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (derived-mode-p 'deb-packaging-status-mode)
          (deb-packaging-status-refresh))))))

(defun deb-packaging-status--on-window-selected (_window)
  "Re-scan and refresh the status buffer when it gains selection.
Registered on `window-selection-change-functions' so returning to the buffer
picks up external changes (a changelog edit, artifacts built outside Emacs)
in addition to the explicit `g' and the process-completion refresh.  The
underlying scan is side-effect-free and folding is preserved across the
refresh, so this is safe to run on every selection."
  (when (and (derived-mode-p 'deb-packaging-status-mode)
             (eq (current-buffer) (window-buffer (selected-window))))
    (deb-packaging-status-refresh)))

;;; Actions

(defun deb-packaging-status--open (transient-prefix)
  "Open TRANSIENT-PREFIX from the status buffer.
The process-sentinel refresh will update the buffer when any run completes."
  (call-interactively transient-prefix))

(defun deb-packaging-status-visit ()
  "Open the transient for the section at point.
Walks up the section tree to the nearest section whose type is registered
in `deb-packaging-status--section-actions'."
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
  (deb-packaging-status--open #'deb-packaging-source-build-transient))

(defun deb-packaging-status-build-binary ()
  "Open the binary-build transient."
  (interactive)
  (deb-packaging-status--open #'deb-packaging-binary-build-transient))

(defun deb-packaging-status-lint ()
  "Open the lintian transient."
  (interactive)
  (deb-packaging-status--open #'deb-packaging-lint-transient))

(defun deb-packaging-status-test ()
  "Open the autopkgtest transient."
  (interactive)
  (deb-packaging-status--open #'deb-packaging-test-transient))

(defun deb-packaging-status-clean ()
  "Open the clean artifacts transient."
  (interactive)
  (deb-packaging-status--open #'deb-packaging-clean-transient))

(defun deb-packaging-status-reset ()
  "Open the source-tree reset transient."
  (interactive)
  (deb-packaging-status--open #'deb-packaging-reset-transient))

;;; Major mode

(defvar-keymap deb-packaging-status-mode-map
  :doc "Keymap for `deb-packaging-status-mode'.
`RET' opens the transient for the section at point.  Mnemonic verbs open
the corresponding per-tool transient directly.  Section navigation and
folding (TAB, n/p, M-n/M-p) come from `magit-section-mode'."
  :parent magit-section-mode-map
  "RET" #'deb-packaging-status-visit
  "s"   #'deb-packaging-status-build-source
  "b"   #'deb-packaging-status-build-binary
  "l"   #'deb-packaging-status-lint
  "t"   #'deb-packaging-status-test
  "c"   #'deb-packaging-status-clean
  "r"   #'deb-packaging-status-reset
  "i"   #'deb-packaging-infra-dispatch
  "?"   #'deb-packaging-dispatch
  "g"   #'deb-packaging-status-refresh
  "q"   #'quit-window)

(define-derived-mode deb-packaging-status-mode magit-section-mode "Deb-Status"
  "Major mode for the Debian packaging status landing page."
  :group 'deb-packaging
  :interactive nil
  ;; Re-scan when the user returns to the buffer.  Buffer-local so it only
  ;; fires for status buffers and is torn down with the buffer.
  (add-hook 'window-selection-change-functions
            #'deb-packaging-status--on-window-selected nil t))

;;;###autoload
(defun deb-packaging-status ()
  "Open the Debian packaging status buffer for the current package.
This is the primary entry point for the deb-packaging workflow."
  (interactive)
  (let* ((pkg-dir (deb-packaging--find-package-dir))
         (info (and pkg-dir (deb-packaging--parse-changelog pkg-dir)))
         (name (nth 0 info))
         (buf (get-buffer-create (deb-packaging-status--buffer-name name))))
    (with-current-buffer buf
      (when pkg-dir
        (setq default-directory pkg-dir))
      (unless (derived-mode-p 'deb-packaging-status-mode)
        (deb-packaging-status-mode))
      (deb-packaging-status--render)
      (deb-packaging-status--goto-first-phase))
    (switch-to-buffer buf)))

(provide 'deb-packaging-status)
;;; deb-packaging-status.el ends here
