EMACS ?= emacs

# All package source files, in dependency order.
SRC = deb-packaging-detect.el \
      deb-packaging-config.el \
      deb-packaging-repos.el \
      deb-packaging-ppa.el \
      deb-packaging-commands.el \
      deb-packaging-ppa-tests.el \
      deb-packaging-transients.el \
      deb-packaging-infra.el \
      deb-packaging-dev.el \
      deb-packaging-propagate.el \
      deb-packaging-pq.el \
      deb-packaging-status.el \
      deb-packaging-clone.el \
      deb-packaging.el

# Test files, in load order (helpers first).
TEST_SRC = test/deb-packaging-test-version.el \
           test/deb-packaging-test-detect.el \
           test/deb-packaging-test-commands.el \
           test/deb-packaging-test-ppa-tests.el \
           test/deb-packaging-test-config.el \
           test/deb-packaging-test-repos.el \
           test/deb-packaging-test-ppa.el \
           test/deb-packaging-test-propagate.el \
           test/deb-packaging-test-status.el \
           test/deb-packaging-test-run.el \
           test/deb-packaging-test-pq.el \
           test/deb-packaging-test-dev.el \
           test/deb-packaging-test-clone.el

# Put installed ELPA packages (magit, transient, magit-section, deps) plus
# the package root and test/ on the load-path.
LOADPATH = --eval '(dolist (d (directory-files "~/.emacs.d/elpa" t "^[^.]")) \
                     (when (file-directory-p d) (add-to-list (quote load-path) d)))' \
           --eval "(add-to-list 'load-path default-directory)" \
           --eval "(add-to-list 'load-path (expand-file-name \"test\" default-directory))"

.PHONY: all test compile lint clean

all: compile test

## Run the full ERT suite in batch mode.
test:
	$(EMACS) -Q --batch $(LOADPATH) \
	  $(patsubst %,-l %,$(TEST_SRC)) \
	  -f ert-run-tests-batch-and-exit

## Byte-compile every source and test file, treating warnings as errors.
compile:
	$(EMACS) -Q --batch $(LOADPATH) \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(SRC) $(TEST_SRC)
	@$(MAKE) clean

## Run package-lint on source files (excludes auto-generated pkg.el).
lint:
	$(EMACS) -Q --batch $(LOADPATH) \
	  --eval "(require 'package-lint)" \
	  -f package-lint-batch-and-exit \
	  $(SRC)

## Remove byte-compiled output.
clean:
	rm -f *.elc test/*.elc
