# gbp pq module for deb-packaging

Date: 2026-07-08
Status: approved (executing)

## Goal

A module that wraps the `gbp pq` lifecycle so quilt patches can be maintained
as git commits, edited with vanilla Magit between `import` and `export`.

## User workflow

1. **Start:** in a 3.0 (quilt) Debian git repo. Patches live in
   `debian/patches/`.
2. **Import:** `gbp pq import` creates `patch-queue/<branch>` and switches to
   it. deb-packaging opens `magit-status` on the repo so the user can edit.
3. **Edit:** the user cherry-picks, `git am`s, or manually edits on the
   patch-queue branch using Magit. No deb-packaging involvement here.
4. **Export:** `gbp pq export --commit --drop` writes patches back to
   `debian/patches/`, commits on the packaging branch, drops the patch-queue
   branch. deb-packaging refreshes the status buffer.

## UX decisions

- After `import`: open `magit-status` automatically (the user needs to edit on
  the patch-queue branch — hand them the tool).
- After `export`: refresh status buffer, message "Exported, back on <branch>".
- Status section shows patch-queue state with clear hints:
  - "on patch-queue" — editing; hint to export when done.
  - "ready" — branch exists, can switch.
  - "none" — no patch-queue, can import.
- Section only appears for 3.0 (quilt) repos.
- Transient header shows current branch + patch-queue state.
- Dispatch key: `u`.

## New file: deb-packaging-pq.el

**Dependencies:** cl-lib, magit, transient, deb-packaging-detect,
deb-packaging-commands.

**Commands** (all interactive, `;;;###autoload`):

| Command | Runs | After |
|---|---|---|
| `deb-packaging-pq-import` | `gbp pq import` | open magit-status |
| `deb-packaging-pq-switch` | `gbp pq switch` | message state |
| `deb-packaging-pq-rebase` | `gbp pq rebase` | message result |
| `deb-packaging-pq-export` | `gbp pq export --commit --drop` | refresh status |
| `deb-packaging-pq-drop` | `gbp pq drop` | refresh status |

**Execution:** `compile` (gbp is a separate binary, not git). Output in
`*compilation*` so conflicts are visible.

**Pre-flight:** `deb-packaging-pq--ensure-quilt-repo` — checks
`magit-toplevel` non-nil and source-format = "3.0 (quilt)".

**State helper:** `deb-packaging-pq--state` — returns plist
`(:on-pq-p :pq-branch :exists-p :branch)`.

**Transient:** `deb-packaging-pq-transient` with dynamic header.

## Integration

- `deb-packaging.el`: require, dispatch entry `u`.
- `deb-packaging-status.el`: section-actions entry, `--insert-pq` function,
  `u` keymap binding, `deb-packaging-status-pq` command, render call.
