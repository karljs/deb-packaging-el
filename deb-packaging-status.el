;;; deb-packaging-status.el --- Status landing page for deb-packaging -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Magit-style status buffer for deb-packaging.  A header describes the
;; package, then two groups of rows: Local (source package, binaries,
;; lint, autopkgtest) and Launchpad (upload, builds, tests).  Each row is
;; a verb ending in a status word; RET opens its menu.
;;
;; No cache: every render re-scans via `deb-packaging-detect--scan-context'
;; (shared with the dispatch transient) and on window selection.
;;
;; Entry point: `deb-packaging-status'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'magit-section)
(require 'deb-packaging-detect)
(require 'deb-packaging-commands)
(require 'deb-packaging-ppa)
(require 'deb-packaging-transients)
(require 'deb-packaging-display)
(require 'deb-packaging-develop)

;; Cross-file references not pulled in by require (avoids load cycles).
(declare-function deb-packaging-dispatch "deb-packaging")
(declare-function deb-packaging-infra-dispatch "deb-packaging-infra")
(declare-function deb-packaging-infra-show-ppa-package "deb-packaging-infra")
(declare-function deb-packaging-dev--list-containers "deb-packaging-dev")
(declare-function deb-packaging-propagate-transient "deb-packaging-propagate")
(declare-function deb-packaging-propagate--existing-clone "deb-packaging-propagate")
(declare-function deb-packaging-update-transient "deb-packaging-update")
(declare-function deb-packaging-ppa-tests-show "deb-packaging-ppa-tests")

(defvar-local deb-packaging-status--context nil
  "Buffer-local package context plist for the package shown.")

(defun deb-packaging-status--buffer-name (name pkg-dir)
  "Return the status buffer name for package NAME at PKG-DIR."
  (format "*deb-packaging: %s [%s]*"
          (or name "?")
          (abbreviate-file-name
           (directory-file-name (file-truename pkg-dir)))))

(defun deb-packaging-status--collect-context ()
  "Gather fresh package context from `default-directory'.
Return a plist, or nil outside a Debian package tree."
  (deb-packaging-transients--context))

;;; Section -> action dispatch
;;
;; RET walks up the section tree to a registered type and invokes the
;; matching command.

(defconst deb-packaging-status--section-actions
  '((deb-packaging-source     . deb-packaging-commands-source-build-transient)
    (deb-packaging-binary     . deb-packaging-binary-build-transient)
    (deb-packaging-check      . deb-packaging-lint-transient)
    (deb-packaging-commands-lintian-source . deb-packaging-lintian-transient)
    (deb-packaging-commands-lintian-binary . deb-packaging-lintian-transient)
    (deb-packaging-commands-ubuntu-lint    . deb-packaging-ubuntu-lint-transient)
    (deb-packaging-test       . deb-packaging-test-transient)
    (deb-packaging-upload     . deb-packaging-upload-transient)
    (deb-packaging-ppa-builds . deb-packaging-infra-show-ppa-package)
    (deb-packaging-ppa-test   . deb-packaging-ppa-tests-show)
    (deb-packaging-ppa        . deb-packaging-upload-transient)
    (deb-packaging-stale      . deb-packaging-commands-clean-transient)
    (deb-packaging-branch     . deb-packaging-branch-transient)
    (deb-packaging-patches    . deb-packaging-patches-transient)
    (deb-packaging-changelog  . deb-packaging-changelog-transient)
    (deb-packaging-upstream   . deb-packaging-update-transient)
    (deb-packaging-dev        . deb-packaging-dev-transient)
    (deb-packaging-submit     . deb-packaging-submit-transient)
    (deb-packaging-forward    . deb-packaging-propagate-transient))
  "Map status-buffer section types to the command RET runs.")

;;; Faces
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
  '((t :inherit success :weight bold))
  "Face for the `ready' status word.")

(defface deb-packaging-status-blocked
  '((t :inherit shadow))
  "Face for the `blocked' status word.")

;;; Status words
;;
;; The word backs up color for non-color terminals and accessibility.

(defconst deb-packaging-status--state-words
  '((running   . ("running"   . deb-packaging-status-running))
    (failed    . ("failed"    . deb-packaging-status-failed))
    (done      . ("done"      . deb-packaging-status-done))
    (submitted . ("submitted" . deb-packaging-status-done))
    (ready     . ("ready"     . deb-packaging-status-ready))
    (blocked   . ("blocked"   . deb-packaging-status-blocked)))
  "Map phase state symbol to (WORD . FACE).")

(defun deb-packaging-status--state-word (state)
  "Return the propertized status word for STATE."
  (let ((entry (alist-get state deb-packaging-status--state-words)))
    (propertize (car entry) 'font-lock-face (cdr entry))))

;;; Layout

(defconst deb-packaging-status--label-width 20
  "Column where row status words start.")

(defconst deb-packaging-status--word-width 11
  "Column width of the status word, so details after it align.")

(defconst deb-packaging-status--key-width 15
  "Column width of `Key:' labels in header and row bodies.")

(defun deb-packaging-status--pad (text width)
  "Return TEXT padded to WIDTH columns, always followed by two spaces."
  (concat text (make-string (max 2 (- width (string-width text))) ?\s)))

(defun deb-packaging-status--dim (text)
  "Return TEXT in the shadow face."
  (propertize text 'font-lock-face 'shadow))

(defvar deb-packaging-status--indent "    "
  "Prefix for row body lines; lint children bind a deeper one.")

(defun deb-packaging-status--insert-note (text)
  "Insert an indented, dimmed note TEXT."
  (insert deb-packaging-status--indent (deb-packaging-status--dim text) "\n"))

(defun deb-packaging-status--insert-fields (pairs)
  "Insert one aligned `Key: value' line per (KEY . VALUE) in PAIRS.
Nil entries are skipped.  Plain values get the default face."
  (dolist (pair (delq nil pairs))
    (let ((value (cdr pair)))
      (insert deb-packaging-status--indent
              (deb-packaging-status--dim
               (deb-packaging-status--pad (concat (car pair) ":")
                                          deb-packaging-status--key-width))
              (if (text-property-not-all 0 (length value) 'font-lock-face nil value)
                  value
                (propertize value 'font-lock-face 'default))
              "\n"))))

(defun deb-packaging-status--visitable-file-p (path)
  "Return non-nil when PATH is usefully visited read-only.
Binary packages would render as garbage."
  (not (string-match-p "\\.\\(u?deb\\|ddeb\\)\\'" path)))

(defun deb-packaging-status--insert-file-line (path)
  "Insert an indented PATH line with size and modification time.
Text artifacts are wrapped in a `deb-packaging-file' section so RET
visits them."
  (if (deb-packaging-status--visitable-file-p path)
      (magit-insert-section (deb-packaging-file path)
        (deb-packaging-status--insert-file-line-1 path))
    (deb-packaging-status--insert-file-line-1 path)))

(defun deb-packaging-status--insert-file-line-1 (path)
  "Insert an indented PATH line with size and modification time."
  (let* ((attrs (ignore-errors (file-attributes path)))
         (size (if attrs (file-size-human-readable (file-attribute-size attrs)) ""))
         (mtime (if attrs
                    (format-time-string "%b %e %H:%M"
                                        (file-attribute-modification-time attrs))
                  "")))
    (insert deb-packaging-status--indent
            (propertize (deb-packaging-status--pad (file-name-nondirectory path) 44)
                        'font-lock-face 'magit-section-secondary-heading)
            (deb-packaging-status--dim (format "%6s  %s" size mtime))
            "\n")))

(defun deb-packaging-status--run-time-note (key)
  "Return the dimmed time of KEY's last run, or empty string."
  (if-let* ((record (deb-packaging-commands-run-record key))
            (time (plist-get record :time)))
      (deb-packaging-status--dim (concat "  " time))
    ""))

(defun deb-packaging-status--row-heading (label state &optional key detail indent)
  "Return a row heading: LABEL, STATE word, DETAIL, then KEY's run time.
INDENT is the number of leading spaces (default 2)."
  (let ((indent (make-string (or indent 2) ?\s)))
    (concat
     indent
     (propertize (deb-packaging-status--pad
                  label (- deb-packaging-status--label-width (length indent)))
                 'font-lock-face 'magit-section-heading)
     (if detail
         (concat (deb-packaging-status--pad (deb-packaging-status--state-word state)
                                            deb-packaging-status--word-width)
                 detail)
       (deb-packaging-status--state-word state))
     (if key (deb-packaging-status--run-time-note key) ""))))

(defun deb-packaging-status--via (state builder)
  "Return a dimmed \"via BUILDER\" detail while STATE is still pending."
  (when (memq state '(ready blocked))
    (deb-packaging-status--dim (concat "via " builder))))

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

(defun deb-packaging-status--hide-phase-p (state next-key key)
  "Return non-nil if a phase in STATE should collapse by default.
Expand running/failed phases and the next actionable phase (KEY equals
NEXT-KEY); collapse the rest."
  (not (or (memq state '(failed running)) (eq key next-key))))

(defun deb-packaging-status--source-ready-p (ctx)
  "Return non-nil when the source build inputs are in place.
A non-native package needs its .orig.tar.* beside the tree; a native
package builds from the tree alone.  A missing version means a partial
context, which must not block."
  (let ((version (plist-get ctx :version)))
    (or (plist-get ctx :orig-tarball)
        (null version)
        (deb-packaging-detect--native-version-p version))))

(defun deb-packaging-status--builder (prefix default)
  "Return the --builder= value of transient PREFIX, or DEFAULT."
  (or (transient-arg-value "--builder=" (ignore-errors (transient-args prefix)))
      default))

(defun deb-packaging-status--binary-builder ()
  "Return the builder the Binaries menu would use."
  (deb-packaging-status--builder 'deb-packaging-binary-build-transient "sbuild"))

(defun deb-packaging-status--cross-p (ctx)
  "Return non-nil when CTX targets an architecture other than the host's."
  (not (equal (plist-get ctx :target-arch) (plist-get ctx :host-arch))))

(defun deb-packaging-status--test-runner (ctx)
  "Return the runner the Autopkgtest menu would use for CTX."
  (or (transient-arg-value
       "--runner=" (ignore-errors (transient-args 'deb-packaging-test-transient)))
      (deb-packaging-commands--default-runner ctx)))

(defun deb-packaging-status--source-builder ()
  "Return the builder the Source package menu would use."
  (deb-packaging-status--builder 'deb-packaging-commands-source-build-transient
                                 "dpkg-buildpackage"))

(defun deb-packaging-status--blockers (ctx)
  "Return an alist of (run-key . reason) for CTX's flow phases.
REASON says why the phase cannot run now, or is nil when it can."
  (let* ((arts (plist-get ctx :artifacts))
         (source-builder (deb-packaging-status--source-builder))
         (binary-builder (deb-packaging-status--binary-builder))
         (tool (lambda (name) (unless (executable-find name)
                                (format "%s is not installed" name))))
         (git (lambda (builder) (and (equal builder "gbp")
                                     (not (plist-get ctx :repo-dir))
                                     "gbp needs a Git repository"))))
    (list (cons 'source-build
                (or (funcall tool source-builder)
                    (funcall git source-builder)
                    (unless (deb-packaging-status--source-ready-p ctx)
                      "Needs the orig tarball (press s, then e or g)")))
          (cons 'binary-build
                (or (funcall tool binary-builder)
                    (funcall git binary-builder)
                    (cond ((equal binary-builder "sbuild")
                           (or (deb-packaging-config--emulation-missing
                                (plist-get ctx :target-arch) (plist-get ctx :host-arch))
                               (unless (alist-get 'dsc arts) "Needs a source package")))
                          ((deb-packaging-status--cross-p ctx)
                           (format "Only sbuild can build for %s"
                                   (plist-get ctx :target-arch))))))
          (cons 'autopkgtest
                (or (funcall tool "autopkgtest")
                    (deb-packaging-commands--runner-unusable
                     (deb-packaging-status--test-runner ctx)
                     (plist-get ctx :target-arch) (plist-get ctx :host-arch))
                    (unless (alist-get 'debs arts) "Needs binaries")))
          (cons 'dput
                (or (funcall tool "dput")
                    (and (equal (plist-get ctx :changelog-distro) "UNRELEASED")
                         "Changelog is UNRELEASED (C, then r)")
                    (unless (alist-get 'source-changes arts)
                      "Needs a source package"))))))

(defun deb-packaging-status--phase-states (ctx)
  "Return an alist of (run-key . state) for the flow phases in CTX."
  (let* ((arts (plist-get ctx :artifacts))
         (blockers (deb-packaging-status--blockers ctx))
         (state (lambda (key done)
                  (deb-packaging-status--phase-state
                   key done (not (alist-get key blockers))))))
    (list (cons 'source-build
                (funcall state 'source-build (and (alist-get 'dsc arts)
                                                  (alist-get 'source-changes arts))))
          (cons 'binary-build
                (if (deb-packaging-resume--load ctx)
                    'failed
                  (funcall state 'binary-build (and (alist-get 'binary-changes arts)
                                                    (alist-get 'debs arts)))))
          (cons 'autopkgtest (funcall state 'autopkgtest nil))
          (cons 'dput (funcall state 'dput nil)))))

(defun deb-packaging-status--next-actionable-key (ctx)
  "Return the run-history key of the first ready phase in CTX, or nil.
Walks phases in flow order; picks which phase smart-fold expands."
  (car (cl-find 'ready (deb-packaging-status--phase-states ctx) :key #'cdr)))

;;; Header

(defun deb-packaging-status--group-stale-by-version (stale-files)
  "Group STALE-FILES by version, returning an alist of (version . files).
Unparseable versions group under \"unknown\"."
  (sort (seq-group-by
         (lambda (f) (or (deb-packaging-detect--filename-version f) "unknown"))
         stale-files)
        (lambda (a b) (string< (car a) (car b)))))

(defun deb-packaging-status--header-field (key value)
  "Return an aligned header `KEY: VALUE' string."
  (concat (deb-packaging-status--dim
           (deb-packaging-status--pad (concat key ":") deb-packaging-status--key-width))
          value))

(defun deb-packaging-status--insert-header (ctx)
  "Insert the package title and context lines from CTX."
  (let* ((host-arch (plist-get ctx :host-arch))
         (target-arch (or (plist-get ctx :target-arch) host-arch)))
    (insert (propertize (plist-get ctx :name) 'font-lock-face 'deb-packaging-status-title)
            " "
            (propertize (plist-get ctx :version)
                        'font-lock-face 'deb-packaging-status-version)
            "\n"
            (propertize (abbreviate-file-name (plist-get ctx :pkg-dir))
                        'font-lock-face 'deb-packaging-status-path)
            "\n"
            (propertize (plist-get ctx :distro) 'font-lock-face 'deb-packaging-status-distro)
            (let ((raw (plist-get ctx :changelog-distro)))
              (if (and raw (not (equal raw (plist-get ctx :distro))))
                  (format " (changelog: %s)" raw)
                ""))
            " | "
            (cond ((and host-arch (not (equal target-arch host-arch)))
                   (format "%s (host %s)" target-arch host-arch))
                  (target-arch)
                  (t "unknown arch"))
            " | "
            (if (plist-get ctx :repo-dir)
                (concat (if (deb-packaging-develop--git-ubuntu-p) "git-ubuntu " "")
                        (or (plist-get ctx :branch) "detached HEAD")
                        (if (plist-get ctx :dirty-p) " (modified)" ""))
              "not a git repository")
            "\n\n")))

(defun deb-packaging-status--insert-ppa-line (ctx)
  "Insert the default PPA line from CTX."
  (magit-insert-section (deb-packaging-ppa)
    (insert (deb-packaging-status--header-field
             "PPA"
             (or (plist-get ctx :default-ppa)
                 (deb-packaging-status--dim "not set (U to choose)")))
            "\n")))

(defun deb-packaging-status--insert-stale (ctx)
  "Insert the stale-artifacts line from CTX; TAB lists the files."
  (when-let ((stale (plist-get ctx :stale)))
    (magit-insert-section (deb-packaging-stale nil t)
      (magit-insert-heading
        (deb-packaging-status--header-field
         "Stale files"
         (concat (propertize (format "%d from other versions" (length stale))
                             'font-lock-face 'warning)
                 (deb-packaging-status--dim " (c to clean)"))))
      (magit-insert-section-body
        (dolist (group (deb-packaging-status--group-stale-by-version stale))
          (insert "  " (propertize (car group)
                                   'font-lock-face 'magit-section-secondary-heading)
                  "\n")
          (dolist (f (cdr group))
            (insert "    " (deb-packaging-status--dim (file-name-nondirectory f)) "\n")))))))

;;; Develop rows

(defun deb-packaging-status--info-heading (label value &optional indent)
  "Return a row heading showing LABEL and a VALUE instead of a status word."
  (let ((indent (make-string (or indent 2) ?\s)))
    (concat indent
            (propertize (deb-packaging-status--pad
                         label (- deb-packaging-status--label-width (length indent)))
                        'font-lock-face 'magit-section-heading)
            value)))

(defun deb-packaging-status--warn (text)
  "Return TEXT in the warning face."
  (propertize text 'font-lock-face 'warning))

(defun deb-packaging-status--insert-warnings (warnings)
  "Insert each non-nil entry of WARNINGS as an indented warning line."
  (dolist (w (delq nil warnings))
    (insert deb-packaging-status--indent (deb-packaging-status--warn w) "\n")))

(defun deb-packaging-status--on-base-branch-p (facts)
  "Return non-nil when FACTS show an unmodified mirror of the upstream branch."
  (let ((branch (plist-get facts :branch))
        (upstream (plist-get facts :upstream)))
    (and branch upstream (eq (plist-get facts :ahead) 0)
         (string-suffix-p branch upstream))))

(defun deb-packaging-status--insert-branch (ctx facts)
  "Insert the Branch row from CTX and FACTS."
  (let* ((branch (plist-get facts :branch))
         (upstream (plist-get facts :upstream))
         (ahead (plist-get facts :ahead))
         (bug (plist-get facts :bug))
         (base (deb-packaging-status--on-base-branch-p facts)))
    (magit-insert-section (deb-packaging-branch nil (not base))
      (magit-insert-heading
        (deb-packaging-status--info-heading
         "Branch"
         (concat
          (cond ((not (plist-get ctx :repo-dir))
                 (deb-packaging-status--dim "not a git repository"))
                ((null branch) (deb-packaging-status--warn "detached HEAD"))
                (upstream
                 (format "%s, %d commit%s ahead of %s"
                         branch ahead (if (= ahead 1) "" "s") upstream))
                (t branch))
          (if bug (deb-packaging-status--dim (format "  LP: #%s" bug)) ""))))
      (magit-insert-section-body
        (when base
          (deb-packaging-status--insert-note
           "Start a fix branch before committing (f, then n)"))))))

(defun deb-packaging-status--insert-patches (ctx facts)
  "Insert the Patches row from CTX and FACTS; TAB lists the patches."
  (let* ((patches (plist-get facts :patches))
         (pq (plist-get facts :pq))
         (format (plist-get ctx :source-format))
         (quilt (or (null format) (string-match-p "quilt\\|1\\.0" format))))
    (magit-insert-section (deb-packaging-patches nil (not (plist-get pq :on-pq-p)))
      (magit-insert-heading
        (deb-packaging-status--info-heading
         "Patches"
         (cond ((not quilt)
                (deb-packaging-status--dim "native package: edit the source directly"))
               (t (concat (if patches
                              (format "%d in debian/patches" (length patches))
                            (deb-packaging-status--dim "none"))
                          (cond ((plist-get pq :on-pq-p)
                                 (propertize "  editing as commits"
                                             'font-lock-face 'deb-packaging-status-running))
                                ((plist-get pq :exists-p)
                                 (deb-packaging-status--dim "  patch queue exists"))
                                (t "")))))))
      (magit-insert-section-body
        (when (plist-get pq :on-pq-p)
          (deb-packaging-status--insert-note
           "Each patch is a commit here; finish with a, then x"))
        (dolist (patch patches)
          (deb-packaging-status--insert-file-line (cdr patch)))))))

(defun deb-packaging-status--changelog-warnings (facts)
  "Return warnings about the changelog for FACTS."
  (list (and (plist-get facts :ahead) (> (plist-get facts :ahead) 0)
             (not (plist-get facts :changelog-changed))
             "No changelog entry for these commits yet (C, then a)")
        (and (plist-get facts :unreleased)
             "UNRELEASED: finalize before uploading (C, then r)")
        (and (plist-get facts :maintainer-stale)
             "Maintainer is still the Debian one (C, then m)")))

(defun deb-packaging-status--insert-changelog (ctx facts)
  "Insert the Changelog row from CTX and FACTS."
  (let ((warnings (delq nil (deb-packaging-status--changelog-warnings facts))))
    (magit-insert-section (deb-packaging-changelog nil (null warnings))
      (magit-insert-heading
        (deb-packaging-status--info-heading
         "Changelog"
         (concat (plist-get ctx :version) " "
                 (let ((raw (plist-get ctx :changelog-distro)))
                   (if (equal raw "UNRELEASED") (deb-packaging-status--warn raw) raw)))))
      (magit-insert-section-body
        (deb-packaging-status--insert-warnings warnings)))))

(defun deb-packaging-status--insert-upstream (ctx facts)
  "Insert the Upstream row from CTX and FACTS when debian/watch exists."
  (when (plist-get facts :watch)
    (let* ((record (deb-packaging-commands-run-record 'upstream-check))
           (summary (plist-get record :summary))
           (current (deb-packaging-detect--upstream-version (plist-get ctx :version))))
      (magit-insert-section (deb-packaging-upstream)
        (insert
         (deb-packaging-status--info-heading
          "Upstream"
          (concat
           current
           (cond ((eq (plist-get record :status) 'running)
                  (propertize "  checking" 'font-lock-face 'deb-packaging-status-running))
                 ((plist-get summary :newer)
                  (deb-packaging-status--warn
                   (format "  %s available (N, then u)" (plist-get summary :newest))))
                 (summary (deb-packaging-status--dim "  up to date"))
                 (t (deb-packaging-status--dim "  not checked (N, then c)")))))
         "\n")))))

(defun deb-packaging-status--insert-dev (ctx)
  "Insert the Dev shell row for CTX's LXD dev container."
  (let* ((name (plist-get ctx :name))
         (target (format "deb-dev-%s-%s" name (plist-get ctx :distro)))
         (current (cl-find target (deb-packaging-dev--list-containers
                                   (format "deb-dev-%s-" name))
                           :key (lambda (c) (plist-get c :name)) :test #'equal)))
    (magit-insert-section (deb-packaging-dev)
      (insert (deb-packaging-status--info-heading
               "Dev shell"
               (if current
                   (format "%s %s" target (downcase (plist-get current :status)))
                 (deb-packaging-status--dim
                  "none (e: container with build-deps and LSP)")))
              "\n"))))

;;; Submit rows

(defun deb-packaging-status--submit-blocker (facts)
  "Return why a merge proposal cannot be submitted per FACTS, or nil."
  (cond ((not (executable-find "git-ubuntu")) "git-ubuntu is not installed")
        ((not (and (plist-get facts :ahead) (> (plist-get facts :ahead) 0)))
         "Needs commits on a fix branch")))

(defun deb-packaging-status--insert-submit (facts)
  "Insert the Merge proposal row for a git-ubuntu clone per FACTS."
  (when (plist-get facts :git-ubuntu)
    (let* ((blocker (deb-packaging-status--submit-blocker facts))
           (state (deb-packaging-status--phase-state 'submit nil (not blocker))))
      (magit-insert-section (deb-packaging-submit nil (not blocker))
        (magit-insert-heading
          (deb-packaging-status--row-heading
           "Merge proposal" (if (eq state 'done) 'submitted state) 'submit
           (deb-packaging-status--dim "via git ubuntu submit")))
        (magit-insert-section-body
          (deb-packaging-status--insert-blocker state blocker))))))

(defun deb-packaging-status--insert-forward ()
  "Insert the Forward row (send the fix to Debian or upstream)."
  (magit-insert-section (deb-packaging-forward)
    (insert (deb-packaging-status--info-heading
             "Forward"
             (if (deb-packaging-propagate--existing-clone)
                 "Debian clone ready (P, then o)"
               (deb-packaging-status--dim "to Debian (salsa) or upstream")))
            "\n")))

;;; Local rows

(defun deb-packaging-status--insert-blocker (state blocker)
  "Insert BLOCKER as a note while STATE is blocked."
  (when (and blocker (eq state 'blocked))
    (deb-packaging-status--insert-note blocker)))

(defun deb-packaging-status--insert-source (ctx state blocker hide)
  "Insert the Source package row from CTX in STATE, collapsed when HIDE.
BLOCKER says why it cannot run, or is nil."
  (let* ((arts (plist-get ctx :artifacts))
         (dsc (alist-get 'dsc arts))
         (changes (alist-get 'source-changes arts))
         (orig (plist-get ctx :orig-tarball))
         (artifact-dir (plist-get ctx :artifact-dir)))
    (magit-insert-section (deb-packaging-source nil hide)
      (magit-insert-heading
        (deb-packaging-status--row-heading
         "Source package" state 'source-build
         (deb-packaging-status--via state (deb-packaging-status--source-builder))))
      (magit-insert-section-body
        (deb-packaging-status--insert-blocker state blocker)
        (deb-packaging-status--insert-fields
         (list (when orig
                 (cons "Orig tarball" (file-name-nondirectory orig)))
               (unless (equal artifact-dir (plist-get ctx :parent-dir))
                 (cons "Output" (abbreviate-file-name artifact-dir)))))
        (when dsc (deb-packaging-status--insert-file-line dsc))
        (when changes (deb-packaging-status--insert-file-line changes))
        (dolist (b (alist-get 'buildinfo arts))
          (when (string-match-p "_source\\.buildinfo\\'" b)
            (deb-packaging-status--insert-file-line b)))))))

(defun deb-packaging-status--insert-binary (ctx state blocker hide)
  "Insert the Binaries row from CTX in STATE, collapsed when HIDE.
BLOCKER says why it cannot run, or is nil."
  (let* ((arts (plist-get ctx :artifacts))
         (bin-changes (alist-get 'binary-changes arts))
         (debs (alist-get 'debs arts))
         (builder (deb-packaging-status--binary-builder))
         (sbuild-p (equal builder "sbuild")))
    (magit-insert-section (deb-packaging-binary nil hide)
      (magit-insert-heading
        (deb-packaging-status--row-heading
         "Binaries" state 'binary-build
         (deb-packaging-status--via state builder)))
      (magit-insert-section-body
        (deb-packaging-status--insert-blocker state blocker)
        (when sbuild-p
          (let* ((distro (plist-get ctx :distro))
                 (arch (plist-get ctx :target-arch))
                 (chroot (and distro arch
                              (deb-packaging-detect--schroot-exists-p distro arch)))
                 (repos (deb-packaging-transients--effective-repos)))
            (deb-packaging-status--insert-fields
             (list (cons "Chroot"
                         (or chroot
                             (propertize "missing (press b, then c)"
                                         'font-lock-face 'deb-packaging-status-failed)))
                   (cons "Extra repos"
                         (if repos (string-join repos ", ")
                           (deb-packaging-status--dim "none")))))
            (when (member deb-packaging-transients-sbuild-shell-flag
                          (ignore-errors
                            (transient-args 'deb-packaging-binary-build-transient)))
              (deb-packaging-status--insert-note
               "Drops into a chroot shell on build failure"))
            (when-let ((kept (deb-packaging-resume--load ctx)))
              (deb-packaging-status--insert-warnings
               (list "Failed build kept: fix it in the checkout, then b, then r to resume"))
              (deb-packaging-status--insert-fields
               (list (cons "Kept tree" (abbreviate-file-name
                                        (deb-packaging-resume--host-path
                                         (plist-get kept :tree)))))))))
        (dolist (c bin-changes) (deb-packaging-status--insert-file-line c))
        (dolist (d debs) (deb-packaging-status--insert-file-line d))))))

(defun deb-packaging-status--lint-summary-note (key)
  "Return a colored findings summary for KEY's last lint run, or empty.
Counts colored by severity, e.g. \" 2E 5W 12I\" or \" 1F 2E 3W\"."
  (if-let* ((summary (deb-packaging-commands--run-summary key)))
      (let ((fmt (lambda (n face suffix)
                   (concat (propertize (format "%d" n) 'font-lock-face face)
                           suffix))))
        (pcase key
          ('ubuntu-lint
           (concat
            (funcall fmt (plist-get summary :fail)  'deb-packaging-status-failed "F ")
            (funcall fmt (plist-get summary :error) 'deb-packaging-status-failed "E ")
            (funcall fmt (plist-get summary :warn)  'deb-packaging-status-running "W")))
          (_
           (concat
            (funcall fmt (plist-get summary :error)   'deb-packaging-status-failed "E ")
            (funcall fmt (plist-get summary :warning) 'deb-packaging-status-running "W ")
            (funcall fmt (plist-get summary :info)    'shadow "I")))))
    ""))

(defun deb-packaging-status--unmet (tool input input-note)
  "Return why a check using TOOL on INPUT cannot run, or nil.
INPUT-NOTE explains a missing INPUT."
  (cond ((not (executable-find tool)) (format "%s is not installed" tool))
        ((not input) input-note)))

(defun deb-packaging-status--insert-lint-child (section-type key label tool unmet
                                                             &optional files)
  "Insert one Lint child of SECTION-TYPE for run KEY labelled LABEL.
TOOL is shown in the heading, or UNMET (why it is blocked) instead.
FILES are its inputs.  Lint never reaches done: success
returns to ready so it can re-run."
  (let ((state (deb-packaging-status--phase-state key nil (not unmet) t))
        (summary (deb-packaging-status--lint-summary-note key)))
    (magit-insert-section ((eval section-type) nil t)
      (magit-insert-heading
        (deb-packaging-status--row-heading
         label state key
         (concat (deb-packaging-status--dim
                  (if (eq state 'blocked) unmet (concat "via " tool)))
                 (if (string-empty-p summary) "" (concat "  " summary)))
         4))
      (magit-insert-section-body
        (let ((deb-packaging-status--indent "      "))
          (mapc #'deb-packaging-status--insert-file-line files))))))

(defun deb-packaging-status--lint-unmet (ctx)
  "Return an alist of (run-key . unmet-reason-or-nil) for CTX's lint checks."
  (let ((arts (plist-get ctx :artifacts)))
    (list (cons 'lintian-source
                (deb-packaging-status--unmet
                 "lintian" (alist-get 'dsc arts) "Needs a source package"))
          (cons 'lintian-binary
                (deb-packaging-status--unmet
                 "lintian" (alist-get 'debs arts) "Needs binaries"))
          (cons 'ubuntu-lint
                (deb-packaging-status--unmet "ubuntu-lint" t nil)))))

(defun deb-packaging-status--lint-rollup-state (ctx)
  "Return a status symbol summarising the Lint section's children.
Priority: failed > running > ready > done > blocked."
  (let ((states (mapcar (lambda (check)
                          (deb-packaging-status--phase-state
                           (car check) nil (not (cdr check)) t))
                        (deb-packaging-status--lint-unmet ctx))))
    (cl-find-if (lambda (s) (memq s states))
                '(failed running ready done blocked))))

(defun deb-packaging-status--lint-hide-p (ctx)
  "Return non-nil if the Lint section should collapse by default."
  (not (memq (deb-packaging-status--lint-rollup-state ctx) '(failed running))))

(defun deb-packaging-status--insert-check (ctx hide)
  "Insert the Lint row: lintian on source and binaries, ubuntu-lint upload rules."
  (let* ((arts (plist-get ctx :artifacts))
         (dsc (alist-get 'dsc arts))
         (unmet (deb-packaging-status--lint-unmet ctx)))
    (magit-insert-section (deb-packaging-check nil hide)
      (magit-insert-heading
        (deb-packaging-status--row-heading
         "Lint" (deb-packaging-status--lint-rollup-state ctx)))
      (magit-insert-section-body
        (deb-packaging-status--insert-lint-child
         'deb-packaging-commands-lintian-source 'lintian-source "Source package"
         "lintian" (alist-get 'lintian-source unmet) (and dsc (list dsc)))
        (deb-packaging-status--insert-lint-child
         'deb-packaging-commands-lintian-binary 'lintian-binary "Binaries"
         "lintian" (alist-get 'lintian-binary unmet) (alist-get 'debs arts))
        (deb-packaging-status--insert-lint-child
         'deb-packaging-commands-ubuntu-lint 'ubuntu-lint "Upload rules"
         "ubuntu-lint" (alist-get 'ubuntu-lint unmet))))))

(defun deb-packaging-status--insert-test (ctx state blocker hide)
  "Insert the Autopkgtest row from CTX in STATE, collapsed when HIDE.
BLOCKER says why it cannot run, or is nil."
  (let* ((debs (alist-get 'debs (plist-get ctx :artifacts)))
         (distro (plist-get ctx :distro))
         (arch (plist-get ctx :target-arch)))
    (magit-insert-section (deb-packaging-test nil hide)
      (magit-insert-heading
        (deb-packaging-status--row-heading "Autopkgtest" state 'autopkgtest))
      (magit-insert-section-body
        (deb-packaging-status--insert-blocker state blocker)
        (when debs
          (let* ((runner (deb-packaging-status--test-runner ctx))
                 (info (deb-packaging-commands--test-image-info runner distro arch))
                 (image (plist-get info :image))
                 (exists (plist-get info :exists)))
            (deb-packaging-status--insert-fields
             (list (cons "Runner" runner)
                   (when image
                     (cons "Image"
                           (if exists
                               image
                             (propertize (concat image " missing (press t, then i)")
                                         'font-lock-face
                                         'deb-packaging-status-failed))))))
            (when (equal runner "schroot")
              (deb-packaging-status--insert-note
               "Skips tests needing isolation-container, isolation-machine or reboots"))
            (when (member "--shell-fail"
                          (ignore-errors (transient-args 'deb-packaging-test-transient)))
              (deb-packaging-status--insert-note
               "Drops into a testbed shell on test failure"))))))))

;;; Launchpad rows

(defun deb-packaging-status--insert-upload (ctx state blocker hide)
  "Insert the Upload row from CTX in STATE, collapsed when HIDE.
BLOCKER says why it cannot run, or is nil."
  (let ((changes (alist-get 'source-changes (plist-get ctx :artifacts))))
    (magit-insert-section (deb-packaging-upload nil hide)
      (magit-insert-heading
        (deb-packaging-status--row-heading
         "Upload" (if (eq state 'done) 'submitted state) 'dput))
      (magit-insert-section-body
        (deb-packaging-status--insert-blocker state blocker)
        (when changes
          (deb-packaging-status--insert-file-line changes))))))

(defun deb-packaging-status--ppa-tool-note ()
  "Return a note when the `ppa' tool is missing, else nil."
  (unless (executable-find "ppa")
    "ppa is not installed (ppa-dev-tools)"))

(defun deb-packaging-status--insert-ppa-builds (ctx)
  "Insert the Launchpad Builds row for CTX."
  (let ((missing (deb-packaging-status--ppa-tool-note)))
    (magit-insert-section (deb-packaging-ppa-builds)
      (insert (deb-packaging-status--row-heading
               "Builds" (if missing 'blocked 'ready) nil
               (deb-packaging-status--dim
                (or missing
                    (format "for %s/%s"
                            (plist-get ctx :distro) (plist-get ctx :target-arch)))))
              "\n"))))

(defun deb-packaging-status--ppa-tests-summary-note ()
  "Return a readable PPA-test counts string, or nil."
  (when-let ((summary (deb-packaging-commands--run-summary 'ppa-tests)))
    (format "%d passed, %d failed, %d bad"
            (or (plist-get summary :pass) 0)
            (or (plist-get summary :fail) 0)
            (or (plist-get summary :bad) 0))))

(defun deb-packaging-status--insert-ppa-tests ()
  "Insert the Launchpad Tests row."
  (magit-insert-section (deb-packaging-ppa-test)
    (let ((missing (deb-packaging-status--ppa-tool-note)))
      (insert (deb-packaging-status--row-heading
               "Tests" (deb-packaging-status--phase-state 'ppa-tests nil (not missing) t)
               'ppa-tests (if missing
                              (deb-packaging-status--dim missing)
                            (deb-packaging-status--ppa-tests-summary-note)))
              "\n"))))

;;; Buffer rendering

(defun deb-packaging-status--render ()
  "Render the status buffer from freshly collected context."
  (let* ((deb-packaging-detect--memo (make-hash-table :test #'equal))
         (ctx (deb-packaging-status--collect-context))
         (inhibit-read-only t))
    (setq deb-packaging-status--context ctx)
    (erase-buffer)
    (magit-insert-section (deb-packaging-status-root)
      (if (null ctx)
          (insert (propertize "Not in a Debian package directory."
                              'font-lock-face 'error)
                  "\n\nVisit a tree containing debian/changelog, then press g.\n")
        (let* ((facts (deb-packaging-develop--facts ctx))
               (phases (deb-packaging-status--phase-states ctx))
               (blockers (deb-packaging-status--blockers ctx))
               ;; Nothing ready: expand the first blocked row so it says why.
               (next (car (or (cl-find 'ready phases :key #'cdr)
                              (cl-find 'blocked phases :key #'cdr))))
               (hide (lambda (key)
                       (deb-packaging-status--hide-phase-p
                        (alist-get key phases) next key)))
               (row (lambda (insert key)
                      (funcall insert ctx (alist-get key phases)
                               (alist-get key blockers)
                               (funcall hide key)))))
          (deb-packaging-status--insert-header ctx)
          (deb-packaging-status--insert-ppa-line ctx)
          (deb-packaging-status--insert-stale ctx)
          (insert "\n")
          (magit-insert-section (deb-packaging-develop)
            (magit-insert-heading
              (propertize "Develop" 'font-lock-face 'magit-section-heading))
            (deb-packaging-status--insert-branch ctx facts)
            (deb-packaging-status--insert-patches ctx facts)
            (deb-packaging-status--insert-changelog ctx facts)
            (deb-packaging-status--insert-upstream ctx facts)
            (deb-packaging-status--insert-dev ctx)
            (insert "\n"))
          (magit-insert-section (deb-packaging-local)
            (magit-insert-heading
              (propertize "Local" 'font-lock-face 'magit-section-heading))
            (funcall row #'deb-packaging-status--insert-source 'source-build)
            (funcall row #'deb-packaging-status--insert-binary 'binary-build)
            (deb-packaging-status--insert-check ctx (deb-packaging-status--lint-hide-p ctx))
            (funcall row #'deb-packaging-status--insert-test 'autopkgtest)
            (insert "\n"))
          (magit-insert-section (deb-packaging-launchpad)
            (magit-insert-heading
              (propertize "Launchpad" 'font-lock-face 'magit-section-heading))
            (funcall row #'deb-packaging-status--insert-upload 'dput)
            (deb-packaging-status--insert-ppa-builds ctx)
            (deb-packaging-status--insert-ppa-tests)
            (insert "\n"))
          (magit-insert-section (deb-packaging-submit-group)
            (magit-insert-heading
              (propertize "Submit" 'font-lock-face 'magit-section-heading))
            (deb-packaging-status--insert-submit facts)
            (deb-packaging-status--insert-forward))))
      ;; Show the root once so fold indicators appear before any manual toggle.
      (when magit-root-section
        (magit-section-show magit-root-section)))))

(defun deb-packaging-status--goto-first-phase ()
  "Move point to the first row (Source package)."
  (goto-char (point-min))
  (when-let ((section (magit-get-section
                       '((deb-packaging-source)
                         (deb-packaging-local)
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

(defconst deb-packaging-status--section-run-keys
  '((deb-packaging-source . source-build)
    (deb-packaging-binary . binary-build)
    (deb-packaging-commands-lintian-source . lintian-source)
    (deb-packaging-commands-lintian-binary . lintian-binary)
    (deb-packaging-commands-ubuntu-lint . ubuntu-lint)
    (deb-packaging-test . autopkgtest)
    (deb-packaging-ppa-test . ppa-tests)
    (deb-packaging-upload . dput)
    (deb-packaging-submit . submit))
  "Map status row section types to their latest run keys.")

(defun deb-packaging-status--run-key-at-point ()
  "Return the nearest tracked run key for the section at point."
  (let ((section (magit-current-section)) key)
    (while (and section (not key))
      (setq key (alist-get (oref section type)
                           deb-packaging-status--section-run-keys)
            section (oref section parent)))
    key))

(defun deb-packaging-status-open-output ()
  "Open the latest recorded output for the section at point."
  (interactive)
  (let* ((key (or (deb-packaging-status--run-key-at-point)
                  (user-error "No tracked operation for this section")))
         (record (deb-packaging-commands-run-record key))
         (buffer (and record (get-buffer (plist-get record :buffer)))))
    (unless (buffer-live-p buffer)
      (user-error "No live output buffer for %s" key))
    (deb-packaging-display-buffer
     buffer (or (buffer-local-value 'deb-packaging-display-category buffer)
                'output))))

(defun deb-packaging-status-visit ()
  "Visit the artifact file at point, or open the section's transient.
RET on a text artifact line (e.g. the .changes before upload) opens the
file read-only.  Elsewhere it opens the transient of the nearest
registered phase section."
  (interactive)
  (let ((section (magit-current-section))
        (prefix nil))
    (while (and section
                (not (or prefix
                         (eq (oref section type) 'deb-packaging-file))))
      (setq prefix (alist-get (oref section type)
                               deb-packaging-status--section-actions))
      (setq section (oref section parent)))
    (cond ((and section (eq (oref section type) 'deb-packaging-file))
           (find-file-read-only (oref section value)))
          (prefix (call-interactively prefix))
          (t (user-error "No action for the section at point")))))

;;; Major mode

(defvar-keymap deb-packaging-status-mode-map
  :doc "Keymap for `deb-packaging-status-mode'.
RET visits the artifact file at point, else opens the section's
transient. Mnemonic verbs open tool transients.
Navigation and folding come from `magit-section-mode'."
  :parent magit-section-mode-map
  "RET" #'deb-packaging-status-visit
  "o"   #'deb-packaging-status-open-output
  "f"   #'deb-packaging-branch-transient
  "a"   #'deb-packaging-patches-transient
  "C"   #'deb-packaging-changelog-transient
  "N"   #'deb-packaging-update-transient
  "e"   #'deb-packaging-dev-transient
  "s"   #'deb-packaging-commands-source-build-transient
  "b"   #'deb-packaging-binary-build-transient
  "l"   #'deb-packaging-lint-transient
  "t"   #'deb-packaging-test-transient
  "U"   #'deb-packaging-upload-transient
  "B"   #'deb-packaging-infra-show-ppa-package
  "T"   #'deb-packaging-ppa-tests-show
  "M"   #'deb-packaging-submit-transient
  "P"   #'deb-packaging-propagate-transient
  "c"   #'deb-packaging-commands-clean-transient
  "K"   #'deb-packaging-commands-kill-output-buffers
  "r"   #'deb-packaging-commands-reset-transient
  "R"   #'deb-packaging-commands-regenerate
  "G"   #'deb-packaging-get-transient
  "A"   #'deb-packaging-commands-set-architecture
  "i"   #'deb-packaging-infra-dispatch
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
  "Open the Debian packaging status buffer.
Outside a package tree, offer to clone or open one instead."
  (interactive)
  (if-let ((pkg-dir (condition-case nil
                        (deb-packaging-detect--find-package-dir nil t)
                      (user-error nil))))
      (deb-packaging-status--open pkg-dir)
    (call-interactively #'deb-packaging-get-transient)))

(defun deb-packaging-status--open (pkg-dir)
  "Open and render the status buffer for PKG-DIR."
  (let* ((name (deb-packaging-detect--package-name pkg-dir))
         (buf (get-buffer-create
               (deb-packaging-status--buffer-name name pkg-dir))))
    ;; Pre-warm the PPA candidate cache in the background so the first
    ;; upload/test PPA prompt usually has completion.  No-op when fresh.
    (when (fboundp 'deb-packaging-infra--warm-ppa-cache-async)
      (deb-packaging-infra--warm-ppa-cache-async))
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
