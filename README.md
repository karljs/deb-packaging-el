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

Visit a source tree containing `debian/changelog`, then run
`M-x deb-packaging-status`. The status buffer shows the package name, changelog
distribution, target architecture, repository branch, and available build,
verification, publish, and workspace actions.

Common keys:

- `RET`: open the selected action or visit a text artifact.
- `o`: reopen output from the selected operation.
- `g`: refresh local package state.
- `?`: open the command hub for the current surface.
- `q`: exit the current surface.

The main command hub is `M-x deb-packaging-dispatch`.

## Build and verify

- Source packages: `Source build...` in the command hub.
- Native working-tree binaries: `Working-tree binary build` runs
  `dpkg-buildpackage -b` without first creating a source package.
- Chroot builds: `sbuild binary build...`. The target architecture is saved
  per package and distribution. Use sbuild for cross-architecture builds.
- Local tests: `Autopkgtest...`. LXD and QEMU images are selected by
  distribution and target architecture.
- Lint: `Lint...` runs Lintian and Ubuntu policy checks.

Artifacts and operation output are available from the status buffer. The `o`
key reopens the latest output for tracked operations.

Typical source-to-PPA flow:

1. Open status in the package checkout.
2. Build a source package, then build binaries with sbuild.
3. Run lint and autopkgtest.
4. Press `U`, select a PPA, and upload the source `.changes` file.
5. Open `PPA builds` from status to inspect Launchpad acceptance and builds.
6. Open the PPA test report to view logs or trigger another test run.

## Git workflows

The package supports explicit peer workflows:

- Use `gbp clone` and `gbp buildpackage` for git-buildpackage repositories.
- Use `git ubuntu clone`, export, and update actions for git-ubuntu repositories.
- A `Vcs-Git` branch declaration is retained when cloning. gbp configuration
  remains the authority for branch names and export directories.

Patch queues, upstream updates, backports, and propagation are available from
the command hub and status buffer.

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
