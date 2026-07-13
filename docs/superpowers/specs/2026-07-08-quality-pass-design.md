# Quality pass for deb-packaging.el

Date: 2026-07-08
Status: approved (design only; executing)

## Goal

Prepare the `deb-packaging` Emacs package for sharing: refactor shared
functionality, fix behavioral bugs, smooth rough edges, normalize metadata,
rewrite the README as a concise human description. No tests (deferred). No
behavior change beyond the bug fixes and rough-edge items listed here.

Baseline includes the uncommitted change in `deb-packaging-propagate.el`
(`magit-git-string`→`magit-git-output`, transient flag reorder, clone arg
refactor).

## Approach

Inline shared helpers into existing files (Approach A). `detect.el` hosts pure
helpers; `commands.el` hosts the process-sentinel wrapper; `infra.el` hosts its
read-entry helper; `status.el` consolidates transient-args; `dev.el` hosts its
container-exists-p helper. No new files.

## 1. Shared-helper extraction

Add to `deb-packaging-detect.el`:
- `deb-packaging--parent-dir` — `(file-name-directory (directory-file-name dir))`. Replaces ~9 inline copies in commands/propagate/status.
- `deb-packaging--package-info` — returns `(name version)` from `parse-changelog`; or thin `--package-name`/`--package-version` accessors. Replaces ~14 `(info (parse-changelog)) (nth 0/1 info))` triplets across commands/dev/propagate/status.
- `deb-packaging--call-process-string` — `with-output-to-string` + `with-current-buffer standard-output` + `call-process`. Used in detect (architecture, schroot-exists-p) and dev (list-containers, marker probe).
- `deb-packaging--cache-dir` — `(or (getenv "XDG_CACHE_HOME") (expand-file-name "~/.cache"))` base. Used by config (propagate cache) and dev (langs cache).

Add to `deb-packaging-commands.el`:
- `deb-packaging--wrap-sentinel` — generalized sentinel-wrapping extracted from `--attach-run-sentinel`; reused by `deb-packaging-dev--open-on-success`.

Add to `deb-packaging-infra.el`:
- `deb-packaging-infra--read-entry` — deduplicates the 6 near-identical "item at point or completing-read" `interactive` forms.

Add to `deb-packaging-status.el`:
- `deb-packaging-status--transient-args` — returns the args list; `--transient-flag-p` and `--transient-arg-value` become thin callers (or are replaced).

Add to `deb-packaging-dev.el`:
- `deb-packaging-dev--container-exists-p` — `(zerop (call-process "lxc" nil nil nil "info" name))`, used 4×.

## 2. Bug fixes

- **Upload status key mismatch** (status.el:577 vs commands.el:507): upload phase queries run-history under `'ppa-tests` but `deb-packaging-dput-upload` records under `'dput`. Align: upload phase queries `'dput`; the ppa-tests row keeps `'ppa-tests`.
- **Dead `user-error` in `deb-packaging-propagate-apply`** (propagate.el:566): `(or ... default-directory (user-error ...))` is unreachable because `default-directory` is always non-nil. Rework so it errors when not inside a git repo (drop the always-true fallback; use `magit-toplevel` and error if nil).
- **`rust-ppa` default** (commands.el:325): remove from `deb-packaging-sbuild-variants` default value; document as an example users can add.
- **`when-let` → `when-let*`** (config.el:59, commands.el:168).

## 3. Rough edges

- **`autopkgtest` arg order** (commands.el:467): verify against `autopkgtest(1)` before changing; correct if wrong.
- **`%SBUILD_SHELL` coupling** (transients.el:111 ↔ status.el:432): replace the literal-string cross-file match with a defined constant.
- **Rescan-on-window-select** (status.el:898): debounce or gate so a full `--scan-context` doesn't run on every window selection.
- **Hardcoded git trailer** (propagate.el:324): drop the fake `2.43.0` version.
- **`parse-changelog` 1024-byte cap** (detect.el:41): read the full first stanza instead.
- **infra LXD CSV indices** (infra.el:200): use `lxc image list --format=json` + keyed access.
- **`dev--list-containers` symbol-name assumption** (dev.el:461): bind `json-object-type`/`json-key-type` explicitly.
- **`schroot-exists-p` return type** (detect.el:163): keep returning the name; fix the docstring to say so.
- **`reset` desc mismatch** (commands.el:600): align desc strings with actual commands.
- **Buffer display:** `switch-to-buffer` → `pop-to-buffer` at all 8 sites (commands.el:171, dev.el:426, status.el:917, infra.el:181/357/421/520/750). Idiomatic; respects `display-buffer-alist`.

### Deferred (separate project)

- **OSC filter overlap** (commands.el:33 + `ansi-color-process-output`): leave as-is; address separately.

## 4. Dead / defensive code

Drop `fboundp` guards for always-loaded functions: `transient-args`,
`magit-toplevel`, `deb-packaging-infra--list-ppas`, `deb-packaging-dev--list-containers`.

## 5. Packaging metadata & headers

- Add `Version: 0.1.0` to config/transients/infra headers.
- Add missing `Copyright (C) 2024 Karl Smeltzer` to detect.el.
- Fix `deb-packaging-transients.el` Commentary "Six per-tool transients" → eight.
- Fix status.el indentation (the extra-space `--insert-upload`/`--insert-stale`/`--insert-dev` calls).
- Normalize autoload forms (plain `;;;###autoload` vs `(autoload ...)` form) within each file.
- **Remove the placeholder `URL: https://github.com/example/deb-packaging` lines** from all 5 files (cleaner than a fake URL; user adds the real one at publish time).

## 6. README

Rewrite to a concise, human project description: what it is (Magit-style
Debian/Ubuntu packaging workflow for Emacs), roughly what it can do
(build/lint/test/upload to PPAs; LXD dev containers with LSP; propagate fixes
to Debian salsa via git-am), license. No keybind dump, no URL, no "Not ready"
warning. Keep it short.

## Verification

After each phase: `byte-compile-file` every `.el` and fix warnings. Run
`checkdoc`-style review of headers. No test suite (deferred).
