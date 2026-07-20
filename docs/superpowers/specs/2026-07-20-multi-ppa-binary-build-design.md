# Multiple extra-repository PPAs for binary builds

Date: 2026-07-20
Status: approved (design finalized, pending implementation plan)

## Goal

Allow `sbuild` binary builds to use multiple extra-repository PPAs in a single
build, and remember the selected set per (package x distro) so returning to a
package after working elsewhere restores the same PPAs without re-tracking.

## Problem

Today `--extra-repository=` is single-valued (`deb-packaging-transients.el:159`
has no `:multi-value t`, unlike `--extra-package=` at `:166`). sbuild accepts
repeated `--extra-repository=` flags but the transient only passes one. There is
also no persistence: the selected repo is forgotten when the transient closes,
forcing the user to re-enter dependency PPAs each session.

## UX decisions

- Multi-value entry mirrors `--extra-package=`: loop the reader, empty input to
  stop. Consistent with the sibling option in the same transient.
- Free-text `ppa:owner/name` entry for non-owned dependency PPAs keeps working
  (the reader already uses `require-match=nil`).
- The selected set is persisted per (package x distro) to a plain-text file and
  restored automatically when the binary-build transient opens. The user never
  writes config code or touches customize.
- Per-distro keying: a PPA may only ship builds for one suite, so coupling the
  key to distro avoids restoring noble-only repos for a jammy build.
- Optional `defvar deb-packaging-config-extra-ppas` (nil by default) lets power
  users pre-seed global candidate PPAs in init. Not required; per-package
  persistence handles remembering.

## Components

### 1. Multi-value reader — `deb-packaging-transients.el`

- Add `:multi-value t` to the `--extra-repository=` option in
  `deb-packaging-binary-build-transient`.
- `deb-packaging-transients--extra-repo-argument` reader: behavior unchanged
  per-prompt (completing-read against variants + owned PPAs + global list),
  but transient's `:multi-value` machinery loops it until empty input.
- `transient-format-value`: render the list comma-separated with per-entry
  expansion display (mirror `--extra-package=` formatting).

### 2. Optional global candidate list — `deb-packaging-config.el`

- `defvar deb-packaging-config-extra-ppas nil` — list of `ppa:owner/name`
  strings merged into completion candidates. Defaults to nil; no customize.
- Reader merges: `(delete-dups (append variants ppas deb-packaging-config-extra-ppas))`.

### 3. Per-(package x distro) persistence — new module `deb-packaging-repos.el`

Plain-text file store, one entry per line (pre-expansion values: variant names,
`ppa:` addresses, raw deb lines).

- **Dir:** `<cache-dir>/deb-packaging/extra-repos/` (cache dir from
  `deb-packaging-detect--cache-dir`, same root as propagate config).
- **Filename:** `<source-package-name>.<distro>` (e.g. `gcc-14.noble`).
  Source name via `deb-packaging-detect--package-info`; distro via
  `deb-packaging-config--effective-distro`.
- **Load:** `deb-packaging-repos-load (package distro)` → list of strings, or
  nil. Returns nil if file missing; does not signal.
- **Save:** `deb-packaging-repos-save (package distro entries)` — writes one
  entry per line; empty list writes empty file so "cleared" sticks.
- **Internal:** `deb-packaging-repos--file (package distro)` → path; creates
  dir on save if missing.

### 4. Restore on transient open — `deb-packaging-transients.el`

`deb-packaging-transients--binary-default-value` (already seeds `--dist=`) is
extended: after computing distro, call `deb-packaging-repos-load` for the
detected package + distro and prepend a `--extra-repository=<entry>` arg per
saved entry to the returned value list. No-op when file missing or empty.

### 5. Save on build — `deb-packaging-commands.el`

`deb-packaging-commands-sbuild`: after dispatching the build command, collect
the current pre-expansion `--extra-repository=` values and call
`deb-packaging-repos-save`. Save runs unconditionally (the build runs async via
`compile`; the declared set is the user's intent and the args are the source of
truth, so persisting regardless of later build outcome is correct).

### 6. sbuild invocation — `deb-packaging-commands.el`

Replace the single-value handling (`deb-packaging-commands.el:357-361`) with:
collect all `--extra-repository=` args from `effective-args`, expand each via
`deb-packaging-commands--expand-extra-repo`, emit one
`--extra-repository=LINE` per entry. The existing passthrough filter
(`deb-packaging-commands.el:362-364`) already strips all `--extra-repository=`
prefixed args, so it generalizes without change.

## Data flow

1. User opens binary-build transient.
2. `--binary-default-value` seeds `--dist=` from changelog, loads saved repos
   for (package, distro), sets `--extra-repository=` per saved entry.
3. User adds/removes repos via the multi-value reader (free-text or completion).
4. User triggers build.
5. `deb-packaging-commands-sbuild` expands each entry, emits one
   `--extra-repository=LINE` per entry to sbuild.
6. After invocation, current pre-expansion set is saved to
   `<cache>/deb-packaging/extra-repos/<pkg>.<distro>`.

## Tests

- `deb-packaging-commands--expand-extra-repo`: unchanged (variant/ppa/raw
  already covered).
- New: `deb-packaging-commands-sbuild` with multiple `--extra-repository=`
  args produces multiple expanded `--extra-repository=LINE` flags (mock
  `run-command`).
- New: `deb-packaging-repos-save` + `deb-packaging-repos-load` round-trip
  (write set → reload → equal).
- New: empty set persists (empty file written, load returns nil, no seed on
  transient open).
- New: `--binary-default-value` seeds `--extra-repository=` from saved file
  for the current package+distro (mock detect + load).

## Out of scope

- customize interface (explicitly rejected by user).
- Auto-accumulating every typed PPA into a global candidate cache (per-package
  persistence already remembers them; a second layer would duplicate).
- Per-distro UI for inspecting/clearing saved sets (file is plain text under
  cache dir; `cat`/`rm` suffices).

## Future extensions

- Status buffer command to view/clear the saved repo set for the current
  package+distro.
- Migrate the key to include suite/arch if multi-arch builds need it.
