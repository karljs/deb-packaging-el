# deb-packaging.el

An Emacs workspace for Debian and Ubuntu package repositories. It provides a
Magit-style status buffer, command transients, build output, PPA views, and
helpers for packaging infrastructure.

## Requirements

- Emacs 29.1 or newer.
- `magit` 3.3 or newer and `transient` 0.4 or newer.
- Git and the Debian packaging tools used by the workflow, such as
  `dpkg-buildpackage` and `dpkg-parsechangelog`.

Optional tools enable their matching workflows:

| Tool | Workflow |
| --- | --- |
| `sbuild`, `mk-sbuild` | Cross-architecture and chroot builds |
| `autopkgtest`, `lxc` | Local package tests and LXD images |
| `lintian`, `ubuntu-lint` | Package checks |
| `dput`, `ppa` | Launchpad uploads, PPA inventory, builds, and tests |
| `gbp` | git-buildpackage builds, clones, and patch queues |
| `git-ubuntu` | Ubuntu package clones, exports, and patch workflows |

`M-x deb-packaging-doctor` reports which optional tools are available and opens
the `deb-packaging` Customize group. It does not install tools.

## Install

Put this repository on `load-path`, then load the package:

```elisp
(add-to-list 'load-path "/path/to/deb-packaging-el")
(require 'deb-packaging)
(global-set-key (kbd "C-c d") #'deb-packaging-status)
```

Open `M-x customize-group RET deb-packaging` to configure the default target
architecture, PPA team files, extra PPA candidates, autopkgtest images,
display behavior, and dev-container settings.

## First use

Run `M-x deb-packaging-status`. Inside a package checkout it opens the status
buffer; anywhere else it offers to clone an Ubuntu package (`git ubuntu
clone`), clone a Debian packaging repo (`gbp clone`), or open a checkout.

The status buffer follows the flow of a fix, top to bottom:

| Group | Rows (key) |
| --- | --- |
| Develop | Branch (`f`), Patches (`a`), Changelog (`C`), Upstream (`N`), Dev shell (`e`) |
| Local | Source package (`s`), Binaries (`b`), Lint (`l`), Autopkgtest (`t`) |
| Launchpad | Upload (`U`), Builds (`B`), Tests (`T`) |
| Submit | Merge proposal (`M`), Forward to Debian or upstream (`P`) |

Rows that need attention expand and say what to press, e.g. "Start a fix
branch before committing (f, then n)". `RET` on a row opens its menu; the
same keys work in the command hub (`?`, or `M-x deb-packaging-dispatch`).
Menu actions that cannot run are greyed out with the reason.

Other keys: `o` reopens the selected operation's output, `g` refreshes, `c`
cleans artifacts, `r` resets the tree, `R` regenerates debian/control, `G`
gets another package, `i` opens infrastructure (chroots, images, PPAs).

## Fixing a package

1. `G`, then `u`: clone the package. Status opens on `ubuntu/devel`.
2. `f`, then `n`: start a fix branch. Give the Launchpad bug and the branch
   defaults to `lpNNN`; it tracks `pkg/ubuntu/devel`, so Branch shows how many
   commits the fix has.
3. `a` for patches:
   - `u` imports an upstream fix from a commit or pull-request URL (or a
     file) as a DEP-3 patch with `Origin` and `Bug-Ubuntu`, and checks it
     applies with fuzz 0, as dpkg-source requires.
   - `e` edits the patches as git commits with `gbp pq`; `x` writes them back.
4. `C`, then `a`: add a changelog entry. It opens a new Ubuntu version, or
   appends while the entry is UNRELEASED, adding `(LP: #N)`. `m` updates the
   Maintainer field for a new Ubuntu delta; `r` finalizes the release.
5. Build and test under Local, upload to a PPA under Launchpad. Upload stays
   blocked while the changelog is UNRELEASED.
6. `M`, then `s`: push the branch and open a merge proposal with `git ubuntu
   submit`. `P` forwards the fix to Debian (a salsa clone with a work branch)
   or exports it as a `.patch` for upstream.

While the changelog is UNRELEASED, builds target the last released series, or
Ubuntu devel for a new delta on a Debian upload. The header shows both.

## Build and verify

- Source package: `--builder=` picks `dpkg-buildpackage` (default) or `gbp`
  (`gbp buildpackage -S`). Orig tarball actions (git-ubuntu, gbp) live in the
  same menu for non-native packages.
- Binaries: `--builder=` picks `sbuild` (default), `dpkg-buildpackage -b`, or
  `gbp buildpackage -b`. Only sbuild builds for a non-host architecture. The
  target architecture is saved per package and distribution. Save a builder
  choice with `C-x s` in the menu.
- Autopkgtest: LXD and QEMU images are selected by distribution and target
  architecture.
- Lint: `lintian` checks the source package and binaries against Debian
  policy; `ubuntu-lint` checks Ubuntu upload rules (changelog, maintainer, bug
  references). `RET` on a Lint row opens that tool's menu; `l` opens both.
- Big packages: with "Keep failed builds" (on by default) a failed sbuild
  keeps its session, with build-deps installed, and its build tree. Fix it
  in the checkout (a patch, debian/rules), then `b`, then `r` to resume: only
  changed debian/ files are copied in, changed patches are re-applied, and
  `dpkg-buildpackage -nc` continues in the same session, so make or ninja
  rebuilds just what the fix touched. A configure-step change needs a fresh
  build. `o` opens the tree, `x` opens a shell in the session, and `d`
  discards it. "Skip tests" builds with the nocheck profile. Requires
  sbuild's schroot mode.
- Dev shell: an LXD container with the build-deps and language servers,
  mounting the checkout, for editing upstream code with eglot over TRAMP.

## Target architecture

The target resolves from the saved package and distribution choice, the
configured default, the host architecture, then `amd64` as a final fallback.
The status buffer shows host and target separately when they differ. Architecture
selection is passed to sbuild and autopkgtest. Local `dpkg-buildpackage -b`
builds are limited to the host architecture.

## Launchpad PPAs

Authenticate the `ppa` tool using its `ppa credentials` command before managing
PPAs or requesting build reports. The package delegates credentials and remote
mutations to `ppa-dev-tools`.

Team configurations are read from
`~/.config/ppa-dev-tools/teams` by default. Set
`deb-packaging-infra-ppa-team-config-dir` to another directory. Each YAML file
should include the `list.owner_name` setting used by `ppa list -C FILE`, for
example:

```yaml
list:
  owner_name: ubuntu-toolchain-r
```

Open the PPA workspace from the command hub. Inventory rows show whether a PPA
came from the personal or a team config. Details, architecture configuration,
and package build reports use that config. The package build view filters by the
current package, distribution, and target architecture. PPA autopkgtest reports
show queues, results, logs, and available trigger actions. Press `w` in the PPA
inventory to open the archive in Launchpad for browser-only build retries.

Uploads use the PPA selected in the upload transient or saved for the package
and distribution. A successful `dput` is reported as submitted. Launchpad
acceptance and build status are shown by the PPA workspace.

## Cleanup and troubleshooting

`Clean artifacts...` removes generated package artifacts. `Reset source tree...`
resets repository state and confirms destructive actions. Review the exact
target shown by destructive prompts before continuing.

If a workflow is unavailable, run `M-x deb-packaging-doctor` and inspect the
missing tool. If a PPA team is missing from completion, check the configured
team directory and its `list.owner_name` value, then refresh the PPA workspace.

## License

GPL-3.0-or-later
