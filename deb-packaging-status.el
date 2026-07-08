;;; deb-packaging-status.el --- Status landing page for deb-packaging -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging

;;; Commentary:

;; Magit-style status buffer for deb-packaging. Shows package phases
;; (source build, binary build, lint, test, upload) with a status word
;; per phase derived from artifacts and run history.
;;
;; Keys:
;;   RET     run action for section at point.
;;   TAB     fold/unfold section.
;;   n/p     navigate sections.
;;   s b t c mnemonic flow actions.
;;   ?       open `deb-packaging-dispatch'.
;;   g       refresh.   q  quit.
;;
;; The buffer drives state; the transient drives configuration.
;;
;; Layout: phases in flow order. Each heading ends in a colored status word
;; (ready/running/done/failed/blocked). Detail is folded by default except
;; for the next actionable phase and any running/failed phase. Stale
;; artifacts and a collapsed Settings section sit at the bottom.
;;
;; No private cache: every render calls `deb-packaging--scan-context', shared
;; with the dispatch transient. The buffer also refreshes on window selection.
;;
;; Directories:
;;   Source  repository containing debian/changelog; `default-directory'.
;;   Output  parent of source tree; dpkg-buildpackage and sbuild drop
;;           artifacts here. Leftover artifacts from other versions are
;;           flagged as stale.
;;
;; Entry point: `deb-packaging-status'.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'deb-packaging-detect)
(require 'deb-packaging-config)
(require 'deb-packaging-commands)
(require 'deb-packaging-transients)

(declare-function deb-packaging--effective-distro "deb-packaging-config")
;; Other cross-file references
(declare-function deb-packaging--run-summary "deb-packaging-commands")
(declare-function deb-packaging--schroot-exists-p "deb-packaging-detect")
(declare-function deb-packaging--filename-version "deb-packaging-detect")
(declare-function deb-packaging-dispatch "deb-packaging")
(declare-function deb-packaging-infra-dispatch "deb-packaging-infra")
(declare-function deb-packaging-dev--list-containers "deb-packaging-dev")
(declare-function deb-packaging-propagate-transient "deb-packaging-propagate")

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
Return a plist, or nil outside a Debian package tree. Delegates to
`deb-packaging--scan-context' so status buffer and transient agree.
Seeds the target distro once, without clobbering user choice."
  (let ((ctx (deb-packaging--scan-context)))
    (when ctx
      (deb-packaging--maybe-seed-distro (plist-get ctx :distro)))
    ctx))

(defun deb-packaging-status--current-context ()
  "Return the context of the live status buffer, if any.
Used by `deb-packaging.el' so the transient matches the status buffer.
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
;; Actionable sections carry type symbols. RET walks up the section tree
;; to a registered type and invokes the matching command. Mnemonic keys
;; below are shortcuts to the same commands.

(defconst deb-packaging-status--section-actions
  '((deb-packaging-source         . deb-packaging-source-build-transient)
    (deb-packaging-binary         . deb-packaging-binary-build-transient)
    (deb-packaging-check          . deb-packaging-lint-transient)
    (deb-packaging-test           . deb-packaging-test-transient)
    (deb-packaging-upload         . deb-packaging-upload-transient)
    (deb-packaging-stale          . deb-packaging-clean-transient)
    (deb-packaging-dev            . deb-packaging-dev-transient))
  "Map status-buffer section types to the transient RET opens.
Lint is a parent section; RET on a child walks up and opens the lint
transient.")

;;; Status words
;;
;; Each phase heading ends in a colored status word. Colour signals
;; state; the word helps non-colour terminals and accessibility.

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
  (if-let* ((record (deb-packaging-run-record key))
            (time (plist-get record :time)))
      (propertize (format " %s" time) 'font-lock-face 'shadow)
    ""))

(defun deb-packaging-status--lint-summary-note (key)
  "Return a colored findings summary for KEY's last lint run, or empty.
For lintian: \" 2E 5W 12I\". For ubuntu-lint: \" 1F 2E 3W\".
Each count is colored by severity."
  (if-let* ((summary (deb-packaging--run-summary key)))
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

;;; Phase state and fold decisions
;;
;; A phase is done when its artifacts exist or its last run succeeded.
;; running/failed come from run history. Smart fold expands the first
;; not-done phase and any running/failed phase; everything else collapses.
;; This is only the default: magit-section preserves manual TAB state on
;; refresh.

(defun deb-packaging-status--phase-state (key done ready &optional keep-ready)
  "Return a phase state symbol for KEY.
DONE means artifacts exist or the phase succeeded. READY means
prerequisites are met; otherwise it is blocked. Live run wins, then
failed run, then completion, then readiness.
If KEEP-READY is non-nil, success stays ready so the phase can be
re-run. Used for lint."
  (let ((status (plist-get (deb-packaging-run-record key) :status)))
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
Expand running/failed phases and the single next actionable phase
\(KEY equals NEXT-KEY); collapse the rest."
  (cond
   ((memq state '(failed running)) nil)
   ((eq key next-key) nil)
   (t t)))

;;; Rendering helpers
;;
;; magit-section-mode uses font-lock-defaults (nil t), so only the
;; `font-lock-face' property works; `face' is ignored. Use
;; `font-lock-face' for all inserted text.

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
  "Insert an indented PATH line with size and modification time.
Aligns the basename, then shows size and date."
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
Renders each as \"Label: value\". Pairs are separated by 4 spaces."
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
LABEL is the phase name; STATE drives the trailing status word; KEY
adds the last-run time; DETAIL is an optional dimmed fragment."
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

(defun deb-packaging-status--transient-args (prefix)
  "Return PREFIX's saved/default transient args, or nil.
Wraps `transient-args' defensively so a missing prefix yields nil
rather than signaling."
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
               deb-packaging-sbuild-shell-flag)
          (deb-packaging-status--insert-note
           "Debug shell enabled, drops into chroot on build failure"))))))

(defun deb-packaging-status--insert-lintian-child (section-type key label artifacts)
  "Insert one Lint child section of SECTION-TYPE for run key KEY.
LABEL is the heading text; ARTIFACTS are files to lint. Absence blocks
the child. Lint never reaches done: success returns to ready
\(KEEP-READY) so it can be re-run. Completed lintian runs show a colored
findings summary. SECTION-TYPE must be registered so RET acts on it."
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
Priority: failed > running > ready > done > blocked.
Children are lintian source/binary and ubuntu-lint. Lint children never
reach done, and ubuntu-lint is always ready inside a package, so Lint is
never blocked."
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
  "Return non-nil if the Lint section should collapse by default.
Expand when any child is running or failed; collapse otherwise."
  (let ((state (deb-packaging-status--lint-rollup-state ctx)))
    (not (memq state '(failed running)))))

(defun deb-packaging-status--insert-ubuntu-lint-child ()
  "Insert the Ubuntu lint child section under Lint.
ubuntu-lint checks source metadata, so it is always ready inside a
package. Like lintian, success returns to ready (KEEP-READY)."
  (let ((state (deb-packaging-status--phase-state 'ubuntu-lint nil t t)))
    (magit-insert-section (deb-packaging-ubuntu-lint)
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
  "Insert the Lint phase: lintian source/binary children plus
ubuntu-lint. Source lint targets the .dsc; binary lint targets .debs;
ubuntu-lint targets source package metadata."
  (let ((state (deb-packaging-status--lint-rollup-state ctx)))
    (magit-insert-section (deb-packaging-check nil hide)
      (magit-insert-heading
        (deb-packaging-status--phase-heading state "Lint"))
      (magit-insert-section-body
        (let* ((arts (plist-get ctx :artifacts))
               (dsc (alist-get 'dsc arts))
               (debs (alist-get 'debs arts)))
          (deb-packaging-status--insert-lintian-child
           'deb-packaging-lintian-source 'lintian-source "Source"
           (when dsc (list dsc)))
          (deb-packaging-status--insert-lintian-child
           'deb-packaging-lintian-binary 'lintian-binary "Binary" debs)
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
Unparseable versions are grouped under \"unknown\"."
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

(defun deb-packaging-status--insert-dev (ctx hide)
  "Insert a Dev section showing LXD dev containers for CTX's package.
Collapsed when HIDE. Containers match by name prefix."
  (let* ((name (plist-get ctx :name))
         (distro (plist-get ctx :distro))
         (containers (deb-packaging-dev--list-containers
                      (when name (format "deb-dev-%s-" name))))
         (target (when (and name distro)
                   (format "deb-dev-%s-%s" name distro))))
    (when containers
      (magit-insert-section (deb-packaging-dev nil hide)
        (magit-insert-heading
          (concat
           (propertize (deb-packaging-status--pad
                        "Dev shell" deb-packaging-status--label-width)
                       'font-lock-face 'magit-section-heading)
           (propertize
            (if target
                (if (cl-find target containers
                             :key (lambda (c) (plist-get c :name))
                             :test #'equal)
                    "ready"
                  "none")
              (format "%d" (length containers)))
            'font-lock-face
            (if (and target
                     (cl-find target containers
                              :key (lambda (c) (plist-get c :name))
                              :test #'equal))
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

(defun deb-packaging-status--next-actionable-key (ctx)
  "Return the run-history key of the first ready phase in CTX, or nil.
Walks phases in flow order; used to decide which phase smart-fold
expands by default."
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
                (cons 'dput
                      ;; Upload is always ready; PPA is set inside its transient.
                      (deb-packaging-status--phase-state 'dput nil t)))))
    (car (cl-find 'ready phases :key #'cdr))))

(defun deb-packaging-status--render ()
  "Render the status buffer from freshly collected context.
Phases appear first as terse headings with colored status words.
Detail lives in foldable bodies. The next actionable phase and any
running/failed phase expand by default. Stale artifacts and config sit
at the bottom. Point ends on the first phase heading."
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
          (deb-packaging-status--insert-dev ctx t))))
     ;; Walk the freshly-built tree once, applying each section's initial
     ;; visibility through the show/hide path.  This is what creates the fold
     ;; indicators; without it the `>'/`v' cue is missing until the first manual
     ;; toggle.
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
Registered on `window-selection-change-functions' so external changes
are picked up. Debounced via an idle timer so rapid window switches do
not trigger a filesystem scan each time. The scan is side-effect-free
and folding is preserved."
  (when (and (derived-mode-p 'deb-packaging-status-mode)
             (eq (current-buffer) (window-buffer (selected-window))))
    (when deb-packaging-status--refresh-timer
      (cancel-timer deb-packaging-status--refresh-timer))
    (setq deb-packaging-status--refresh-timer
          (run-with-idle-timer 0.4 nil
                               #'deb-packaging-status-refresh))))

;;; Actions

(defun deb-packaging-status--open (transient-prefix)
  "Open TRANSIENT-PREFIX from the status buffer.
The sentinel refresh updates the buffer when a run completes."
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
  (deb-packaging-status--open #'deb-packaging-source-build-transient))

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
  (deb-packaging-status--open #'deb-packaging-clean-transient))

(defun deb-packaging-status-reset ()
  "Open the source-tree reset transient."
  (interactive)
  (deb-packaging-status--open #'deb-packaging-reset-transient))

(defun deb-packaging-status-dev ()
  "Open the dev shell transient."
  (interactive)
  (deb-packaging-status--open #'deb-packaging-dev-transient))

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
  "p"   #'deb-packaging-status-upload
  "c"   #'deb-packaging-status-clean
  "r"   #'deb-packaging-status-reset
  "e"   #'deb-packaging-status-dev
  "i"   #'deb-packaging-infra-dispatch
  "P"   #'deb-packaging-propagate-transient
  "?"   #'deb-packaging-dispatch
  "g"   #'deb-packaging-status-refresh
  "q"   #'quit-window)

(define-derived-mode deb-packaging-status-mode magit-section-mode "Deb-Status"
  "Major mode for the Debian packaging status landing page."
  :interactive nil
  ;; Re-scan when the user returns. Buffer-local hook cleans up with buffer.
  (add-hook 'window-selection-change-functions
            #'deb-packaging-status--on-window-selected nil t))

;;;###autoload
(defun deb-packaging-status ()
  "Open the Debian packaging status buffer.
Primary entry point for the deb-packaging workflow."
  (interactive)
  (let* ((pkg-dir (deb-packaging--find-package-dir nil t))
         (name (deb-packaging--package-name pkg-dir))
         (buf (get-buffer-create (deb-packaging-status--buffer-name name))))
    (with-current-buffer buf
      (when pkg-dir
        (setq default-directory pkg-dir))
      (unless (derived-mode-p 'deb-packaging-status-mode)
        (deb-packaging-status-mode))
      (deb-packaging-status--render)
      (deb-packaging-status--goto-first-phase))
    (pop-to-buffer buf)))

(provide 'deb-packaging-status)
;;; deb-packaging-status.el ends here
