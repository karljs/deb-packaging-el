# Multiple extra-repository PPAs for binary builds — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Allow `sbuild` binary builds to use multiple extra-repository PPAs in a single build, and persist the selected set per (package x distro) so it restores automatically when returning to a package.

**Architecture:** A new `deb-packaging-repos.el` module provides plain-text per-(package x distro) persistence under the cache dir. The binary-build transient's `--extra-repository=` option becomes multi-valued (mirroring the existing `--extra-package=` pattern). `deb-packaging-commands-sbuild` collects all `--extra-repository=` args, expands each, emits one flag per entry to sbuild, and saves the pre-expansion set after dispatch. The transient's default-value function loads the saved set on open.

**Tech Stack:** Emacs Lisp (Emacs 29.1+), transient 0.4.0+, ERT for tests. Build/test via `make test`, `make compile`, `make lint`.

## Global Constraints

- Emacs 29.1+ (lexically scoped, `subr-x` available).
- `make compile` treats byte-compile warnings as errors — no new warnings.
- `make lint` runs package-lint — no new lint issues.
- Follow existing file conventions: `;;; Code:`/`provide`/`;;; ends here` boilerplate, `declare-function` for cross-module calls, forward `(defvar ...)` for variables defined in other modules.
- New source file goes in Makefile `SRC` list (after `deb-packaging-config.el`, before `deb-packaging-commands.el`).
- New test file goes in `TEST_SRC` list (after `test/deb-packaging-test-config.el`, before `test/deb-packaging-test-propagate.el`).
- No comments unless the code is genuinely non-obvious (per AGENTS.md).

---

### Task 1: Create `deb-packaging-repos.el` persistence module

**Files:**
- Create: `deb-packaging-repos.el`
- Create: `test/deb-packaging-test-repos.el`
- Modify: `Makefile` (SRC and TEST_SRC lists)
- Modify: `deb-packaging.el:26` (add require)

**Interfaces:**
- Consumes: `deb-packaging-detect--cache-dir` (from `deb-packaging-detect.el`) — returns base cache dir string.
- Produces:
  - `deb-packaging-repos--file (package distro)` → string path
  - `deb-packaging-repos-load (package distro)` → list of strings, or nil
  - `deb-packaging-repos-save (package distro entries)` → writes file, returns nil

- [ ] **Step 1: Write the failing tests**

Create `test/deb-packaging-test-repos.el`:

```elisp
;;; deb-packaging-test-repos.el --- Extra-repo persistence tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for deb-packaging-repos.el: round-trip, empty-set, missing-file.

;;; Code:

(require 'ert)
(require 'deb-packaging-repos)

(defmacro deb-packaging-test-repos--with-cache (&rest body)
  "Run BODY with the cache dir set to a temp directory."
  (declare (indent 0) (debug (body)))
  (let ((tmp (make-symbol "tmp")))
    `(let* ((,tmp (make-temp-file "deb-repos-test-" t))
            (process-environment (cons (format "XDG_CACHE_HOME=%s" ,tmp)
                                       process-environment)))
       (unwind-protect
           ,@body
         (delete-directory ,tmp t)))))

(ert-deftest deb-packaging-test-repos/round-trip ()
  "Save then load returns the same entries, in order."
  (deb-packaging-test-repos--with-cache
    (let ((entries '("ppa:me/x" "proposed" "deb http://example.com/ubuntu noble main")))
      (deb-packaging-repos-save "mypkg" "noble" entries)
      (should (equal (deb-packaging-repos-load "mypkg" "noble") entries)))))

(ert-deftest deb-packaging-test-repos/empty-set-persists ()
  "Saving an empty list writes a file that loads as nil."
  (deb-packaging-test-repos--with-cache
    (deb-packaging-repos-save "mypkg" "noble" '("ppa:me/x"))
    (deb-packaging-repos-save "mypkg" "noble" nil)
    (should (null (deb-packaging-repos-load "mypkg" "noble")))))

(ert-deftest deb-packaging-test-repos/missing-file-returns-nil ()
  "Loading when no file exists returns nil, not an error."
  (deb-packaging-test-repos--with-cache
    (should (null (deb-packaging-repos-load "nonsuch" "noble")))))

(ert-deftest deb-packaging-test-repos/per-distro-isolation ()
  "Saving for noble does not affect jammy."
  (deb-packaging-test-repos--with-cache
    (deb-packaging-repos-save "mypkg" "noble" '("ppa:me/x"))
    (should (null (deb-packaging-repos-load "mypkg" "jammy")))
    (should (equal (deb-packaging-repos-load "mypkg" "noble") '("ppa:me/x")))))

(provide 'deb-packaging-test-repos)
;;; deb-packaging-test-repos.el ends here
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `make test`
Expected: FAIL — `deb-packaging-repos` not found / `require` error.

- [ ] **Step 3: Create the module**

Create `deb-packaging-repos.el`:

```elisp
;;; deb-packaging-repos.el --- Per-package extra-repository persistence -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Plain-text store for the set of extra-repository entries (variant names,
;; ppa: addresses, raw deb lines) selected for a source package and distro.
;; One file per (package . distro) under the cache dir, one entry per line.
;; Loaded to seed the binary-build transient; saved on build dispatch.

;;; Code:

(require 'subr-x)
(require 'deb-packaging-detect)

(defun deb-packaging-repos--file (package distro)
  "Return the cache file path for PACKAGE and DISTRO."
  (expand-file-name
   (format "%s.%s" package distro)
   (expand-file-name "deb-packaging/extra-repos"
                     (deb-packaging-detect--cache-dir))))

(defun deb-packaging-repos-load (package distro)
  "Return saved extra-repo entries for PACKAGE and DISTRO, or nil.
Entries are pre-expansion values (variant names, ppa: addresses, raw deb
lines).  Returns nil if the file is missing or empty."
  (let ((file (deb-packaging-repos--file package distro)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let (entries)
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
              (unless (string-empty-p line)
                (push line entries)))
            (forward-line 1))
          (nreverse entries))))))

(defun deb-packaging-repos-save (package distro entries)
  "Write ENTRIES (a list of strings) for PACKAGE and DISTRO to the cache.
Empty list writes an empty file so a cleared set sticks.  Creates the
parent directory if needed."
  (let ((file (deb-packaging-repos--file package distro)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (when entries
        (insert (mapconcat #'identity entries "\n"))
        (insert "\n")))))

(provide 'deb-packaging-repos)
;;; deb-packaging-repos.el ends here
```

- [ ] **Step 4: Add to Makefile SRC list**

In `Makefile`, add `deb-packaging-repos.el` to the `SRC` variable, after `deb-packaging-config.el` and before `deb-packaging-commands.el`:

```makefile
SRC = deb-packaging-detect.el \
      deb-packaging-config.el \
      deb-packaging-repos.el \
      deb-packaging-commands.el \
      deb-packaging-transients.el \
      deb-packaging-infra.el \
      deb-packaging-dev.el \
      deb-packaging-propagate.el \
      deb-packaging-pq.el \
      deb-packaging-status.el \
      deb-packaging.el
```

- [ ] **Step 5: Add test file to Makefile TEST_SRC list**

In `Makefile`, add `test/deb-packaging-test-repos.el` to `TEST_SRC`, after `test/deb-packaging-test-config.el` and before `test/deb-packaging-test-propagate.el`:

```makefile
TEST_SRC = test/deb-packaging-test-version.el \
           test/deb-packaging-test-detect.el \
           test/deb-packaging-test-commands.el \
           test/deb-packaging-test-config.el \
           test/deb-packaging-test-repos.el \
           test/deb-packaging-test-propagate.el \
           test/deb-packaging-test-status.el \
           test/deb-packaging-test-run.el \
           test/deb-packaging-test-pq.el \
           test/deb-packaging-test-dev.el
```

- [ ] **Step 6: Add require to deb-packaging.el**

In `deb-packaging.el`, add `(require 'deb-packaging-repos)` after `(require 'deb-packaging-config)` (line 20):

```elisp
(require 'deb-packaging-detect)
(require 'deb-packaging-config)
(require 'deb-packaging-repos)
(require 'deb-packaging-commands)
```

- [ ] **Step 7: Run tests to verify they pass**

Run: `make test`
Expected: PASS — all repos tests pass, no regressions in existing tests.

- [ ] **Step 8: Byte-compile check**

Run: `make compile`
Expected: PASS — no warnings.

- [ ] **Step 9: Commit**

```bash
git add deb-packaging-repos.el test/deb-packaging-test-repos.el Makefile deb-packaging.el
git commit -m "feat: add deb-packaging-repos per-(package x distro) persistence module"
```

---

### Task 2: Make `--extra-repository=` multi-valued and save on build

**Files:**
- Modify: `deb-packaging-config.el` (add `deb-packaging-config-extra-ppas` defvar)
- Modify: `deb-packaging-transients.el:78-91` (reader), `:93-103` (format-value), `:159-162` (option spec)
- Modify: `deb-packaging-commands.el:25-26` (declare-function), `:340-377` (sbuild function)
- Modify: `test/deb-packaging-test-commands.el` (add multi-value sbuild test)

**Interfaces:**
- Consumes: `deb-packaging-repos-save` (from Task 1), `deb-packaging-commands--expand-extra-repo` (existing), `deb-packaging-config-extra-ppas` (new defvar, same task).
- Produces: `deb-packaging-commands-sbuild` now accepts multiple `--extra-repository=` args and calls `deb-packaging-repos-save` after dispatch. The `--extra-repository=` transient option is multi-valued.

- [ ] **Step 1: Write the failing test**

Add to `test/deb-packaging-test-commands.el`, after the existing `deb-packaging-test-commands/expand-extra-repo-raw` test (around line 126):

```elisp
;;; deb-packaging-commands-sbuild multi-value

(ert-deftest deb-packaging-test-commands/sbuild-multiple-extra-repos ()
  "sbuild receives one expanded --extra-repository= flag per entry."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble"
              :artifacts (("mypkg_1.0-1.dsc" . "")))
    (let (captured-args captured-save)
      (cl-letf (((symbol-function 'deb-packaging-commands--run-command)
                 (lambda (_name args &optional _dir _key)
                   (setq captured-args args)))
                ((symbol-function 'deb-packaging-repos-save)
                 (lambda (pkg distro entries)
                   (setq captured-save (list pkg distro entries)))))
        (deb-packaging-commands-sbuild
         '("--dist=noble"
           "--extra-repository=ppa:me/x"
           "--extra-repository=proposed"
           "--extra-repository=deb http://example.com/ubuntu noble main"))))
      (should (member "--extra-repository=deb [trusted=yes] http://ppa.launchpadcontent.net/me/x/ubuntu/ noble main"
                      captured-args))
      (should (member "--extra-repository=deb http://archive.ubuntu.com/ubuntu/ noble-proposed main"
                      captured-args))
      (should (member "--extra-repository=deb http://example.com/ubuntu noble main"
                      captured-args))
      (should (equal captured-save
                     '("mypkg" "noble"
                       ("ppa:me/x" "proposed" "deb http://example.com/ubuntu noble main")))))))

(ert-deftest deb-packaging-test-commands/sbuild-no-extra-repos-saves-empty ()
  "sbuild with no --extra-repository saves an empty set."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble"
              :artifacts (("mypkg_1.0-1.dsc" . "")))
    (let (captured-save)
      (cl-letf (((symbol-function 'deb-packaging-commands--run-command)
                 (lambda (_name _args &optional _dir _key)))
                ((symbol-function 'deb-packaging-repos-save)
                 (lambda (pkg distro entries)
                   (setq captured-save (list pkg distro entries)))))
        (deb-packaging-commands-sbuild '("--dist=noble")))
      (should (equal captured-save '("mypkg" "noble" nil))))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `make test`
Expected: FAIL — sbuild only emits one `--extra-repository=` flag (the first), and does not call `deb-packaging-repos-save`.

- [ ] **Step 3: Add `deb-packaging-config-extra-ppas` defvar**

In `deb-packaging-config.el`, add after the propagation section (before `deb-packaging-config--distro-choices`, around line 80):

```elisp
;;; Extra PPA candidates

(defvar deb-packaging-config-extra-ppas nil
  "List of ppa:owner/name strings for binary-build completion candidates.
Merged into the --extra-repository completion list alongside owned PPAs
and sbuild variants.  Defaults to nil; per-package persistence handles
remembering across sessions.  Set in your init file if you want certain
dependency PPAs always available as candidates.")
```

- [ ] **Step 4: Add forward-declarations to deb-packaging-transients.el**

In `deb-packaging-transients.el`, add these forward-declarations near the existing ones (after line 27, the `defvar` for `deb-packaging-commands-sbuild-variants`):

```elisp
(defvar deb-packaging-config-extra-ppas)
(declare-function deb-packaging-repos-load "deb-packaging-repos")
```

- [ ] **Step 5: Update the `--extra-repository=` reader to be multi-value-compatible**

In `deb-packaging-transients.el`, replace the `transient-infix-read` method for `deb-packaging-transients--extra-repo-argument` (lines 81-91) with:

```elisp
(cl-defmethod transient-infix-read ((obj deb-packaging-transients--extra-repo-argument))
  "Read an extra-repository value.
Completes against `deb-packaging-commands-sbuild-variants' names, known
PPAs, and `deb-packaging-config-extra-ppas'.  A variant name or ppa:
address expands at build time; anything else is passed to sbuild verbatim.
Returns nil on empty input to end multi-value entry."
  (let* ((variants (mapcar #'car deb-packaging-commands-sbuild-variants))
         (ppas (deb-packaging-infra--list-ppas))
         (choices (delete-dups
                   (append variants ppas deb-packaging-config-extra-ppas))))
    (let ((choice (completing-read
                   "Extra apt repo (empty to stop): "
                   choices nil nil nil)))
      (when (and choice (not (string-empty-p choice)))
        choice))))
```

Key changes from the original: `(oref obj value)` initial-input removed (was a string, now would be a list under `:multi-value`); prompt says "empty to stop"; returns nil on empty input so transient's multi-value loop terminates.

- [ ] **Step 6: Update `transient-format-value` to handle a list**

In `deb-packaging-transients.el`, replace the `transient-format-value` method for `deb-packaging-transients--extra-repo-argument` (lines 93-103) with:

```elisp
(cl-defmethod transient-format-value ((obj deb-packaging-transients--extra-repo-argument))
  "Show each chosen value and, when it differs, its expanded repo line."
  (let ((v (oref obj value)))
    (if v
        (mapconcat
         (lambda (entry)
           (let ((expanded (deb-packaging-commands--expand-extra-repo
                            entry (deb-packaging-config--effective-distro))))
             (if (string= entry expanded)
                 (propertize entry 'face 'transient-value)
               (concat (propertize entry 'face 'transient-value)
                       (propertize (format " → %s" expanded)
                                   'face 'transient-inactive-value)))))
         (if (listp v) v (list v))
         (propertize "," 'face 'transient-inactive-value))
      (propertize "none" 'face 'transient-inactive-value))))
```

Key change: handles `v` as a list (from `:multi-value`), maps over entries, comma-separated. Mirrors the `--extra-package=` format-value pattern at lines 132-141.

- [ ] **Step 7: Add `:multi-value t` to the option spec**

In `deb-packaging-transients.el`, add `:multi-value t` to the `--extra-repository=` option in `deb-packaging-binary-build-transient` (around line 159-162):

```elisp
    ("-e" "Extra repository"
     "--extra-repository="
     :class deb-packaging-transients--extra-repo-argument
     :multi-value t
     :description "Extra apt repo")
```

- [ ] **Step 8: Add forward-declaration and require to deb-packaging-commands.el**

In `deb-packaging-commands.el`, add after the existing `declare-function` lines (around line 26):

```elisp
(declare-function deb-packaging-repos-save "deb-packaging-repos")
```

Also add `(require 'subr-x)` after `(require 'cl-lib)` (line 18), since we will use `string-remove-prefix`:

```elisp
(require 'cl-lib)
(require 'subr-x)
(require 'comint)
```

- [ ] **Step 9: Replace single-value sbuild handling with multi-value**

In `deb-packaging-commands.el`, replace the `let*` bindings in `deb-packaging-commands-sbuild` (lines 354-364) and the `run-command` call. The full replacement for lines 354-377:

```elisp
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
```

Key changes from the original:
- `variant-name` (single `transient-arg-value`) replaced by `repo-args` (all matching args via `cl-remove-if-not`).
- `extra-repo-arg` maps over `repo-args`, expanding each.
- Save call added before `run-command` (synchronous; persists regardless of async build outcome).
- `passthrough` filter unchanged (already strips all `--extra-repository=` prefixed args).

- [ ] **Step 10: Run tests to verify they pass**

Run: `make test`
Expected: PASS — sbuild multi-value test passes, save called with correct args, no regressions.

- [ ] **Step 11: Byte-compile check**

Run: `make compile`
Expected: PASS — no warnings (including the new `subr-x` require and `declare-function`).

- [ ] **Step 12: Lint check**

Run: `make lint`
Expected: PASS — no new lint issues.

- [ ] **Step 13: Commit**

```bash
git add deb-packaging-config.el deb-packaging-transients.el deb-packaging-commands.el test/deb-packaging-test-commands.el
git commit -m "feat: multi-valued --extra-repository with per-package save on build"
```

---

### Task 3: Restore saved repos when the binary-build transient opens

**Files:**
- Modify: `deb-packaging-transients.el:73-76` (default-value function)
- Modify: `test/deb-packaging-test-commands.el` (add restore test)

**Interfaces:**
- Consumes: `deb-packaging-repos-load` (from Task 1), `deb-packaging-detect--package-name` (existing), `deb-packaging-config--effective-distro` (existing).
- Produces: `deb-packaging-transients--binary-default-value` now returns `--extra-repository=` args seeded from the saved set.

- [ ] **Step 1: Write the failing test**

Add to `test/deb-packaging-test-commands.el`, after the sbuild multi-value tests:

```elisp
;;; deb-packaging-transients--binary-default-value restore

(ert-deftest deb-packaging-test-commands/binary-default-value-seeds-repos ()
  "The binary-build default value includes saved extra-repo entries."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble")
    (let* ((tmp (make-temp-file "deb-repos-test-" t))
           (process-environment (cons (format "XDG_CACHE_HOME=%s" tmp)
                                      process-environment))
           (deb-packaging-config-target-distro "noble")
           (deb-packaging-config--distro-user-set t))
      (unwind-protect
          (progn
            (deb-packaging-repos-save "mypkg" "noble"
                                      '("ppa:me/x" "proposed"))
            (let ((default (deb-packaging-transients--binary-default-value)))
              (should (member "--extra-repository=ppa:me/x" default))
              (should (member "--extra-repository=proposed" default))
              (should (member "--dist=noble" default))))
        (delete-directory tmp t)))))

(ert-deftest deb-packaging-test-commands/binary-default-value-no-saved-repos ()
  "With no saved repos, the default value has no --extra-repository= entries."
  (deb-packaging-test--with-package-tree
      '(:name "mypkg" :version "1.0-1" :distro "noble")
    (let* ((tmp (make-temp-file "deb-repos-test-" t))
           (process-environment (cons (format "XDG_CACHE_HOME=%s" tmp)
                                      process-environment))
           (deb-packaging-config-target-distro "noble")
           (deb-packaging-config--distro-user-set t))
      (unwind-protect
          (let ((default (deb-packaging-transients--binary-default-value)))
            (should (member "--dist=noble" default))
            (should-not (cl-some (lambda (a)
                                   (string-prefix-p "--extra-repository=" a))
                                 default)))
        (delete-directory tmp t)))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `make test`
Expected: FAIL — `--extra-repository=` entries not present in default value (current function only returns `--dist=` and `-A`).

- [ ] **Step 3: Extend `deb-packaging-transients--binary-default-value`**

In `deb-packaging-transients.el`, replace the function at lines 73-76 with:

```elisp
(defun deb-packaging-transients--binary-default-value ()
  "Dynamic default for the binary-build transient, seeding distro from changelog.
Also restores the saved extra-repository set for the current package and distro."
  (let* ((distro (deb-packaging-config--effective-distro))
         (pkg-name (deb-packaging-detect--package-name))
         (repos (when pkg-name
                  (deb-packaging-repos-load pkg-name distro))))
    (append (list (format "--dist=%s" distro) "-A")
            (mapcar (lambda (r) (concat "--extra-repository=" r))
                    repos))))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `make test`
Expected: PASS — default value includes saved repos, no regressions.

- [ ] **Step 5: Byte-compile check**

Run: `make compile`
Expected: PASS — no warnings.

- [ ] **Step 6: Lint check**

Run: `make lint`
Expected: PASS.

- [ ] **Step 7: Commit**

```bash
git add deb-packaging-transients.el test/deb-packaging-test-commands.el
git commit -m "feat: restore saved extra-repos when binary-build transient opens"
```
