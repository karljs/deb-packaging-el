# deb-packaging.el

> [!WARNING]
> This is not ready to use, nor reviewed by me.

This package provides a workflow for building deb packages, including
linting, uploading to PPAs, and managing some of the related
infrastructure.

It relies heavily on the extended magit libraries, like transient.

## License

GPL-3.0-or-later

## Dev shell (LXD)

`deb-packaging-dev-shell` brings up a persistent LXD container with the
package's build-deps installed, bind-mounts the source tree into it, and
opens a TRAMP dired buffer at `/lxc:deb-dev-<pkg>-<release>:/root/work/<pkg>/`.
Run eglot inside (`deb-packaging-dev-eglot`) so the language server resolves
headers/modules against the container's installed build-deps.

One container per package per release.  Provisioning is idempotent: it
skips the apt/mk-build-deps install when the fingerprint (apt list,
extra-setup commands, debian/control) hasn't changed.  `C-u` forces
re-provisioning.

Smoke test against `hello`:

1. `apt source hello && cd hello-*`
2. `M-x deb-packaging-status` then `e` to bring up the dev shell.
3. Once dired opens, visit `src/hello.c` and `M-x deb-packaging-dev-eglot`.
4. `k` from the dispatch (or `C-u k` to pick any dev container) tears it down.
