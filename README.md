# deb-packaging.el

Emacs workflow for Debian/Ubuntu packaging, in the spirit of Magit.

It detects the package you're in and gives you a Magit-style status buffer
plus per-tool transients for the common packaging tasks: source and binary
builds (dpkg-buildpackage, sbuild), linting (lintian, ubuntu-lint),
autopkgtest runs, PPA uploads (dput), and artifact cleanup.

Beyond running commands, it manages the surrounding infrastructure:
schroots, LXD/QEMU autopkgtest images, and Launchpad PPAs. For upstream
work it spins up persistent LXD dev containers that bind-mount your source
tree and serve a language server from inside the container, so headers
resolve while you edit quilt patches.

A separate propagate workflow exports quilt patches or git commits as
git-am-friendly patches and prepares a salsa.debian.org clone (with an
optional personal fork remote) to apply them.

Built on `transient` and `magit-section`. Requires Emacs 28.1+.

## License

GPL-3.0-or-later
