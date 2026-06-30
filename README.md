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

One container per package per release.  When provisioning is needed (first
run, changed debian/control, or changed language selection), it prompts for
language servers from `deb-packaging-dev-language-profiles` (C/C++, Python,
Rust, Go, Bash, JS/TS by default; extend by adding entries).  Extra dev
tools go in `deb-packaging-dev-extra-packages`.  Provisioning is idempotent:
a fingerprint of the selected languages and debian/control skips the install
when unchanged.  `C-u` forces re-provisioning and re-prompts.

For C/C++, clangd needs a compile database.  After provisioning, run
`deb-packaging-dev-compile-db` (or `B` from the dev transient) to generate
`compile_commands.json` via a bear-wrapped build.  Even a partially-failed
build produces a usable database.  Re-run after changing build flags or
adding source files.

LXD images and dev containers are listed together in the infrastructure
dispatch (`i` from status, `l` for LXD).  The list shows both types with a
Type column.

Smoke test against `hello`:

1. `apt source hello && cd hello-*`
2. `M-x deb-packaging-status` then `e` to open the dev transient.
3. Press `e` again to bring up the dev shell, select "C/C++" at the prompt.
4. Once dired opens, `e` then `B` to generate compile_commands.json.
5. Visit `src/hello.c` and `M-x deb-packaging-dev-eglot`.
6. `e` then `k` to destroy the container (or `i` then `l` to manage it).
