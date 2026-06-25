# deb-packaging.el

A context-aware Emacs interface for Debian/Ubuntu package maintenance.

## Features

- **Context-aware**: Detects package name, version, and existing artifacts
- **Per-tool transients**: Each action (source build, sbuild, lintian, autopkgtest, PPA, clean) has its own transient with its own flags
- **Workflow hints**: A Magit-style status buffer shows what phase comes next
- **Multiple tools**: dpkg-buildpackage, sbuild, lintian, autopkgtest, ppa

## Installation

Add the package directory to your load-path:

```elisp
(add-to-list 'load-path "/path/to/deb-packaging-el")
(require 'deb-packaging)
(deb-packaging-setup-keys)  ; binds C-c d
```

Or with use-package:

```elisp
(use-package deb-packaging
  :load-path "/path/to/deb-packaging-el"
  :bind ("C-c d" . deb-packaging-status))
```

## Usage

In a directory containing a Debian package (with `debian/changelog`):

`C-c d` opens the **status landing page** — a Magit-style buffer showing the
package moving through its phases:
- A title line with package name, version, and target distro. If stale
  artifacts from other versions exist in the output directory, a `⚠ N stale`
  indicator appears here.
- The work phases in flow order (Source → Binary → Lint → Test → Upload),
  each a terse heading ending in a color-coded status word: `ready` (the next
  action), `running`, `done`, `failed`, or `blocked` (waiting on an earlier
  phase). Per-phase detail lives in a foldable body.
- **Smart folding**: the next actionable phase and any failed/running phase are
  expanded; everything else collapses. Manual `TAB` toggles persist across
  refreshes. Point starts on the first phase.
- `RET` on a phase opens that phase's per-tool transient so you can review and
  adjust flags before running. Mnemonic keys (`s`/`b`/`l`/`t`/`c`) do the same.

The buffer re-scans automatically when you return to it (in addition to `g` and
when a build finishes), so it never shows stale state.

### Status buffer keys

| Key | Action |
|-----|--------|
| RET | Open the transient for the section at point |
| s | Source build transient |
| b | Binary build transient |
| l | Lintian transient |
| t | Autopkgtest transient |
| c | Clean transient |
| i | Infrastructure menu |
| ? | Top-level dispatch (hub to all transients) |
| g | Refresh the buffer |
| q | Quit window |

Section navigation/folding from `magit-section` (TAB, `n`/`p`, `M-n`/`M-p`) is
also available.

### Per-tool transients

Each transient has an **Arguments** group and a **Run** action.  Flag state
is persisted per-transient by transient's own save mechanism (`C-x C-s` to
save permanently, `C-x s` for this session).

| Transient | Key in dispatch | Arguments |
|-----------|----------------|-----------|
| Source build | `s` | `-S`, `-nc`, `-d`, `-sa`, `-I`, `-i` |
| Binary build | `b` | `--dist=`, `-A`, `-v`, `--build-failed-commands=%SBUILD_SHELL`, `--extra-repository=` |
| Lintian | `l` | `-i`, `-I`, `--pedantic`, `--tag-display-limit=`, `--color=` |
| Autopkgtest | `t` | `--apt-upgrade`, `--shell-fail`, `--runner=`, `--dist=` |
| PPA upload / tests | `p` | `--ppa=`, `--dist=` |
| Clean | `c` | `--quilt`, `--sessions`, `--artifacts`, `--stale`, `--pc`, `--files` |

### Distribution defaults and completions

The `--dist=` option is shown in the binary-build, autopkgtest, and PPA
upload transients.  It defaults to the distribution named at the top of
`debian/changelog`, falling back to `noble`.  The completion list offers both
Ubuntu and Debian values (`sid`, `stable`, `testing`, `focal`, `jammy`, `noble`,
etc.), plus the changelog distro if it is not already known.

### Debug shells on failure

The binary-build and autopkgtest transients can drop you into an interactive
shell when a build or test fails:

- **sbuild**: enable `--build-failed-commands=%SBUILD_SHELL`.  The comint
  command buffer becomes the shell; type commands and `exit` when done.
- **autopkgtest**: enable `--shell-fail` (`-s`).  The same comint buffer
  becomes the testbed shell.

Because command buffers are `comint`/pty buffers, the interactive shell is
usable just like a terminal.  The status buffer shows a note in the Binary or
Test phase body when either shell-debug flag is active.

### sbuild extra repository

The binary-build transient's `--extra-repository=` option accepts a short name
from `deb-packaging-sbuild-variants` (default: `rust-ppa`, `proposed`).  The
full apt repository string (with distro substituted) is expanded at run time.
You can also type any full repository string directly; it is passed through
unchanged.

### PPA workflow

PPA support uses the [`ppa`](https://snapcraft.io/ppa-dev-tools) tool. Set the
PPA with `--ppa=` inside the upload transient; it is not session-global state.

PPA lifecycle management (create/delete/set/show/list) lives in the
Infrastructure menu (`i`), under the "PPA (Launchpad)" section.

### Infrastructure management

Open the infrastructure menu from the status buffer with `i`, then choose:

| Key | Buffer |
|-----|--------|
| `s` | Schroots (sbuild) |
| `l` | LXD images (autopkgtest) |
| `v` | QEMU images (autopkgtest) |
| `p` | Launchpad PPAs |

Each infrastructure buffer is a `tabulated-list-mode` table.  Press `S` to sort by
the current column, `{` and `}` to resize columns, and click a header to sort by
mouse.

Common keys shared by every infrastructure buffer:

| Key | Action |
|-----|--------|
| `c` | Create a new entry |
| `d` | Delete the entry at point |
| `g` | Refresh the list |
| `q` | Quit window |

Buffer-specific keys:

| Buffer | Extra keys |
|--------|------------|
| Schroots | `u` update |
| PPAs | `s` show, `e` edit/set-config |

## Customization

### Per-directory settings

Use `.dir-locals.el` to pin the distro:

```elisp
((nil . ((deb-packaging-target-distro . "jammy"))))
```

The distro is also seeded once from `debian/changelog` on first visit.

### Extending sbuild variants and test runners

See `deb-packaging-sbuild-variants` and `deb-packaging-test-runners` in
`deb-packaging-presets.el`.  Add entries to offer additional completion
candidates in the binary-build and test transients.

## License

GPL-3.0-or-later
