# deb-packaging.el

A context-aware Emacs interface for Debian/Ubuntu package maintenance.

## Features

- **Context-aware**: Detects package name, version, and existing artifacts
- **Preset-based**: Configure common flag combinations as presets
- **Workflow hints**: Shows what actions are available based on current state
- **Multiple tools**: dpkg-buildpackage, sbuild, lintian, autopkgtest

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
  :bind ("C-c d" . deb-packaging-dispatch))
```

## Usage

In a directory containing a Debian package (with `debian/changelog`):

`C-c d` opens the dispatch menu showing:
- Current package info and detected artifacts
- Available actions with their commands
- Settings for distro, mode, and variants

### Actions

| Key | Action |
|-----|--------|
| s | Source build (dpkg-buildpackage) |
| l | Lintian |
| b | Binary build (sbuild) |
| t | Autopkgtest (local: lxd/qemu) |
| p | PPA tests (Launchpad autopkgtest results) |
| c | Clean artifacts |

### Settings

| Key | Setting |
|-----|---------|
| d | Target distribution |
| m | Global mode (default/debug/upload) |
| v | sbuild variant |
| r | Test runner (lxd/qemu) |
| P | Current PPA (session) |
| g | Refresh state |

### PPA workflow

PPA support uses the [`ppa`](https://snapcraft.io/ppa-dev-tools) tool to drive
Launchpad PPAs. The target PPA is session state (set with `P`), not a
per-directory setting, since a directory may target different PPAs across
branches/versions.

- `p` in the dispatch runs `ppa tests` for the current PPA, scoped to the
  current package and target distro.
- PPA lifecycle management (create/destroy/set/show/list) lives in the
  Infrastructure menu (`i`), under the "PPA (Launchpad)" section.

Note: the `with-rust-ppa` / `with-proposed` sbuild variants are unrelated — they
add an extra apt repository to the local build chroot, not a Launchpad PPA.

## Customization

### Per-directory settings

Use `.dir-locals.el`:

```elisp
((nil . ((deb-packaging-target-distro . "jammy")
         (deb-packaging-sbuild-variant . with-rust-ppa))))
```

### Custom presets

See `deb-packaging-mode-presets`, `deb-packaging-sbuild-variants`, and `deb-packaging-test-runners`.

## License

GPL-3.0-or-later
