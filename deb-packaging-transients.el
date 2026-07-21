;;; deb-packaging-transients.el --- Tool transients -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Per-tool transients that forward their flags to the runners in
;; deb-packaging-commands.el.  Flags persist per-prefix via transient.
;; Distro options seed from `deb-packaging-config-target-distro'.

;;; Code:

(require 'transient)
(require 'subr-x)
(require 'deb-packaging-detect)
(require 'deb-packaging-config)
(require 'deb-packaging-ppa)

;; Forward-declare helpers to silence the byte-compiler.
(declare-function deb-packaging-commands--runner-choices "deb-packaging-commands")

;; Tool-specific variables live in deb-packaging-commands.el.
(defvar deb-packaging-commands-sbuild-variants)

(defvar deb-packaging-config-extra-ppas)
(declare-function deb-packaging-repos-load "deb-packaging-repos")

;; Shared with the status buffer so it can detect this flag without
;; duplicating the literal.
(defconst deb-packaging-transients-sbuild-shell-flag
  "--build-failed-commands=%SBUILD_SHELL")

(defconst deb-packaging-transients-display-action
  '(display-buffer-in-side-window (side . bottom) (slot . -1)
                                  (dedicated . t) (inhibit-same-window . t))
  "Side-window action for this package's transients.
Slot -1 avoids reusing the slot-0 side window that user
`display-buffer-alist' rules commonly assign to comint buffers.
Reusing such a window makes transient mangle it: on minibuffer
suspend/resume it grows to fit the wrong buffer's contents.")

(defun deb-packaging-transients--env (fn)
  "Run FN with the package's transient display action bound.
Used as :environment for the prefixes in this package."
  (let ((transient-display-buffer-action
         deb-packaging-transients-display-action))
    (funcall fn)))

;; Forward-declare command functions.
(declare-function deb-packaging-commands-source-build "deb-packaging-commands")
(declare-function deb-packaging-commands-export-orig "deb-packaging-commands")
(declare-function deb-packaging-commands-sbuild "deb-packaging-commands")
(declare-function deb-packaging-commands-lintian-source "deb-packaging-commands")
(declare-function deb-packaging-commands-lintian-binary "deb-packaging-commands")
(declare-function deb-packaging-commands-lintian-binary-one "deb-packaging-commands")
(declare-function deb-packaging-commands-ubuntu-lint "deb-packaging-commands")
(declare-function deb-packaging-commands-autopkgtest "deb-packaging-commands")
(declare-function deb-packaging-commands-dput-upload "deb-packaging-commands")
(declare-function deb-packaging-ppa-tests-show "deb-packaging-ppa-tests")
(declare-function deb-packaging-commands-clean "deb-packaging-commands")
(declare-function deb-packaging-commands-reset "deb-packaging-commands")
(declare-function deb-packaging-infra--list-ppas "deb-packaging-infra")
(declare-function deb-packaging-dev-shell "deb-packaging-dev")
(declare-function deb-packaging-dev-eglot "deb-packaging-dev")
(declare-function deb-packaging-dev-compile-db "deb-packaging-dev")
(declare-function deb-packaging-dev-destroy "deb-packaging-dev")
(declare-function deb-packaging-dev-open "deb-packaging-dev")
(declare-function deb-packaging-dev-project "deb-packaging-dev")
(declare-function deb-packaging-dev-exec "deb-packaging-dev")

;;; 1. Source build (dpkg-buildpackage)

;;;###autoload(autoload 'deb-packaging-commands-source-build-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-commands-source-build-transient ()
  "Build a Debian source package, or fetch its orig tarball.
dpkg-buildpackage arguments apply only to \"Build source\" (the
lint-transient pattern)."
  :value '("-S" "-d" "-nc" "-sa" "-I" "-i")
  :environment #'deb-packaging-transients--env
  ["dpkg-buildpackage arguments"
   ("-S" "Source build"            "-S")
   ("-d" "Skip build-dep check"    "-d")
   ("-nc" "No pre-clean"           "-nc")
   ("-sa" "Include orig tarball"   "-sa")
   ("-I"  "Tar ignore pattern"     "-I")
   ("-i"  "Diff ignore pattern"    "-i")]
  ["Run"
   ("s" "Build source" deb-packaging-commands-source-build)
   ("e" "Export orig (git ubuntu)" deb-packaging-commands-export-orig)])

;;; 2. Binary build (sbuild)

(defun deb-packaging-transients--binary-default-value ()
  "Dynamic default for the binary-build transient, seeding distro from changelog.
Also restores the saved extra-repository set for the current package and distro."
  (let* ((distro (deb-packaging-config--effective-distro))
         (pkg-name (deb-packaging-detect--package-name))
         (repos (when pkg-name
                  (deb-packaging-repos-load pkg-name distro))))
    (append (list (format "--dist=%s" distro) "-A")
            (mapcar (lambda (r) (concat "--extra-repository=" r))
                    repos))))

(defclass deb-packaging-transients--extra-repo-argument (transient-option) ()
  "sbuild --extra-repository= option, expanded to a repo string at build time.")

(defun deb-packaging-transients--extra-repo-read (current)
  "Read one extra-repository entry and toggle it against CURRENT.
CURRENT is the entry list, a legacy single-entry string, or nil.
Completes against `deb-packaging-commands-sbuild-variants' names, known
PPAs, and `deb-packaging-config-extra-ppas'.  Selecting an entry already
in the set removes it; empty input keeps the set.  Returns the new list
of entries, or nil when empty.  A variant name or ppa: address expands
at build time; anything else is passed to sbuild verbatim."
  (let* ((current (cond ((listp current) current)
                        (current (list current))))
         (variants (mapcar #'car deb-packaging-commands-sbuild-variants))
         (ppas (deb-packaging-infra--list-ppas))
         (choices (delete-dups
                   (append variants ppas deb-packaging-config-extra-ppas)))
         (prompt (if current
                     (format "Extra apt repo (%s): "
                             (mapconcat #'identity current ", "))
                   "Extra apt repo: "))
         (choice (completing-read prompt choices nil nil)))
    (cond
     ((or (null choice) (string-empty-p choice)) current)
     ((member choice current) (remove choice current))
     (t (append current (list choice))))))

(cl-defmethod transient-infix-read ((obj deb-packaging-transients--extra-repo-argument))
  "Toggle one extra-repository entry against OBJ's current set."
  (deb-packaging-transients--extra-repo-read
   (and (slot-boundp obj 'value) (oref obj value))))

(cl-defmethod transient-init-value ((obj deb-packaging-transients--extra-repo-argument))
  "Seed OBJ's entries from flat --extra-repository= args in the prefix value.
Works around upstream repeat-mode init-value, which keeps whole arg
strings and so doubles the argument when the value is re-emitted."
  (oset obj value
        (mapcar (lambda (a) (string-remove-prefix "--extra-repository=" a))
                (seq-filter
                 (lambda (a)
                   (and (stringp a)
                        (string-prefix-p "--extra-repository=" a)))
                 (oref transient--prefix value)))))

(cl-defmethod transient-format-value ((obj deb-packaging-transients--extra-repo-argument))
  "Show the chosen entries, comma-separated."
  (let ((v (and (slot-boundp obj 'value) (oref obj value))))
    (if v
        (mapconcat (lambda (entry)
                     (propertize entry 'face 'transient-value))
                   (if (listp v) v (list v))
                   (propertize "," 'face 'transient-inactive-value))
      (propertize "none" 'face 'transient-inactive-value))))

(defclass deb-packaging-transients--extra-package-argument (transient-option) ()
  "sbuild --extra-package= option.
Completes against .deb files in the build-output directory but accepts
any path.  Multi-valued: each .deb becomes a separate --extra-package=.")

(cl-defmethod transient-infix-read ((obj deb-packaging-transients--extra-package-argument))
  "Toggle one extra-package .deb against OBJ's current set."
  (deb-packaging-transients--extra-package-read
   (and (slot-boundp obj 'value) (oref obj value))))

(defun deb-packaging-transients--extra-package-read (current)
  "Read one extra-package .deb and toggle it against CURRENT.
CURRENT is the path list, a legacy single-path string, or nil.
Completes against .deb files in the build-output directory, falling
back to file-name reading.  Selecting a path already in the set
removes it; empty input keeps the set.  Returns absolute paths, or
nil when empty."
  (let* ((current (cond ((listp current) current)
                        (current (list current))))
         (pkg-dir (deb-packaging-detect--find-package-dir))
         (parent-dir (when pkg-dir (deb-packaging-detect--parent-dir pkg-dir)))
         (debs (when (and parent-dir (file-directory-p parent-dir))
                 (directory-files parent-dir t "\\.deb\\'")))
         (choice (if debs
                     (completing-read
                      (if current
                          (format "Extra package .deb (%s): "
                                  (mapconcat #'file-name-nondirectory
                                             current ", "))
                        "Extra package .deb: ")
                      debs nil nil)
                   (read-file-name "Extra package (.deb): "
                                   (file-name-as-directory
                                    (or parent-dir default-directory))
                                   nil t))))
    (if (or (null choice) (string-empty-p choice))
        current
      (let ((path (expand-file-name choice)))
        (if (member path current)
            (remove path current)
          (append current (list path)))))))

(cl-defmethod transient-init-value ((obj deb-packaging-transients--extra-package-argument))
  "Seed OBJ's paths from flat --extra-package= args in the prefix value.
Works around upstream repeat-mode init-value, which keeps whole arg
strings and so doubles the argument when the value is re-emitted."
  (oset obj value
        (mapcar (lambda (a) (string-remove-prefix "--extra-package=" a))
                (seq-filter
                 (lambda (a)
                   (and (stringp a)
                        (string-prefix-p "--extra-package=" a)))
                 (oref transient--prefix value)))))

(cl-defmethod transient-format-value ((obj deb-packaging-transients--extra-package-argument))
  "Show the selected .deb file(s) by base name."
  (let ((v (oref obj value)))
    (if v
        (mapconcat
         (lambda (f) (propertize (file-name-nondirectory f)
                                 'face 'transient-value))
         (if (listp v) v (list v))
         (propertize "," 'face 'transient-inactive-value))
      (propertize "none" 'face 'transient-inactive-value))))

;;;###autoload(autoload 'deb-packaging-binary-build-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-binary-build-transient ()
  "Build a Debian binary package with sbuild."
  :value #'deb-packaging-transients--binary-default-value
  :environment #'deb-packaging-transients--env
  ["Arguments"
   ("-d" "Distribution"
    "--dist="
    :class transient-option
    :choices deb-packaging-config--distro-choices
    :always-read t
    :allow-empty nil)
   ("-A" "Build arch-all packages"  "-A")
   ("-v" "Verbose"                  "-v")
   ("-u" "apt upgrade"              "--apt-upgrade")
    ("-F" "Shell on build failure"
     "--build-failed-commands=%SBUILD_SHELL")
    ("-e" "Extra repository"
     "--extra-repository="
     :class deb-packaging-transients--extra-repo-argument
     :multi-value repeat
     :description "Extra apt repo")
    ("-p" "Extra package"
     "--extra-package="
     :class deb-packaging-transients--extra-package-argument
     :multi-value repeat
     :description "Local .deb to install in chroot")]
  ["Build"
   ("b" "Build binary" deb-packaging-commands-sbuild)])

;;; 3. Lint (lintian + ubuntu-lint)

;;;###autoload(autoload 'deb-packaging-lint-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-lint-transient ()
  "Run a linter against the current package.
lintian inspects built artifacts; ubuntu-lint checks Ubuntu policy.
Each action reads only its own flags."
  :value '("-i" "--tag-display-limit=0" "--context=changes" "--all=warn")
  :environment #'deb-packaging-transients--env
  ["Lintian arguments"
   ("-i"  "Show informational tags"   "-i")
   ("-I"  "Pedantic (info+)"          "-I")
   ("-P"  "--pedantic"                "--pedantic")
   ("-t"  "Tag display limit"
    "--tag-display-limit="
    :class transient-option
    :prompt "Limit (0=unlimited): ")
   ("-c"  "Color output"
    "--color="
    :class transient-option
    :choices ("auto" "never" "always" "html")
    :always-read t)]
  ["Ubuntu-lint arguments"
   ("-v"  "Verbose"                "--verbose")
   ("-j"  "JSON output"            "--json")
   ("-C"  "Context source"
    "--context="
    :class transient-option
    :choices ("changes" "source-dir" "changelog")
    :always-read t
    :allow-empty nil)
   ("-a"  "Set level for all checks"
    "--all="
    :class transient-option
    :choices ("auto" "off" "warn" "fail")
    :prompt "Level for all checks: ")]
  ["Run"
   ("l" "Lintian source"        deb-packaging-commands-lintian-source)
   ("L" "Lintian binary (all)"  deb-packaging-commands-lintian-binary)
   ("o" "Lintian one binary..." deb-packaging-commands-lintian-binary-one)
   ("u" "Ubuntu-lint"           deb-packaging-commands-ubuntu-lint)])

;;; 4. Autopkgtest

(defun deb-packaging-transients--saved-ppa-arg ()
  "Return a one-element --ppa= list seeded from the saved PPA, or nil."
  (when-let* ((pkg-name (deb-packaging-detect--package-name))
              (ppa (deb-packaging-ppa-load
                    pkg-name (deb-packaging-config--effective-distro))))
    (list (concat "--ppa=" ppa))))

(defun deb-packaging-transients--test-default-value ()
  "Dynamic default for the test transient."
  (append (deb-packaging-transients--saved-ppa-arg)
          (list "--apt-upgrade"
                "--runner=lxd"
                (format "--dist=%s" (deb-packaging-config--effective-distro)))))

;;;###autoload(autoload 'deb-packaging-test-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-test-transient ()
  "Run autopkgtest locally, or view PPA test results.
Local flags apply only to \"Run autopkgtest\"; the PPA group applies only
to \"PPA test report\" (the lint-transient pattern)."
  :value #'deb-packaging-transients--test-default-value
  :environment #'deb-packaging-transients--env
  ["Local autopkgtest"
   ("-u"  "Upgrade packages before test"  "--apt-upgrade")
   ("-f"  "Drop to shell on failure"      "--shell-fail")
    ("-r"  "Test runner"
     "--runner="
     :class transient-option
     :choices deb-packaging-commands--runner-choices
     :always-read t
     :allow-empty nil)
   ("-d"  "Distribution (image)"
    "--dist="
    :class transient-option
    :choices deb-packaging-config--distro-choices
    :always-read t
    :allow-empty nil)]
  ["PPA tests"
   ("-p" "PPA"
    "--ppa="
    :class transient-option
    :prompt "PPA (e.g. ppa:user/name): "
    :reader deb-packaging-transients--read-ppa
    :always-read t)]
  ["Run"
   ("t" "Run autopkgtest" deb-packaging-commands-autopkgtest)
   ("p" "PPA test report" deb-packaging-ppa-tests-show)])

;;; 5. Upload / PPA

(defun deb-packaging-transients--upload-default-value ()
  "Dynamic default for the upload transient."
  (append (deb-packaging-transients--saved-ppa-arg)
          (list (format "--dist=%s" (deb-packaging-config--effective-distro)))))

(defun deb-packaging-transients--read-ppa (prompt initial-input _history)
  "Read a PPA name, completing against the user's known PPAs."
  (let ((candidates (deb-packaging-infra--list-ppas)))
    (completing-read prompt candidates nil nil initial-input)))

;;;###autoload(autoload 'deb-packaging-upload-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-upload-transient ()
  "Upload to a Launchpad PPA with dput."
  :value #'deb-packaging-transients--upload-default-value
  :environment #'deb-packaging-transients--env
  ["PPA"
   ("-p"  "PPA (required)"
    "--ppa="
    :class transient-option
    :prompt "PPA (e.g. ppa:user/name): "
    :reader deb-packaging-transients--read-ppa
    :always-read t
    :allow-empty nil)]
  ["Options"
   ("-d"  "Distribution"
    "--dist="
    :class transient-option
    :choices deb-packaging-config--distro-choices
    :always-read t
    :allow-empty nil)]
  ["Upload"
   ("p" "Upload with dput" deb-packaging-commands-dput-upload)])

;;; 6. Clean artifacts

;;;###autoload(autoload 'deb-packaging-commands-clean-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-commands-clean-transient ()
  "Remove build artifacts from the output directory."
  :value '("--stale")
  :environment #'deb-packaging-transients--env
  ["What to remove"
   ("-a" "Current-version artifacts" "--artifacts")
   ("-S" "Stale artifacts (other versions)" "--stale")]
  ["Run"
   ("c" "Clean" deb-packaging-commands-clean)])

;;; 7. Reset source tree

;;;###autoload(autoload 'deb-packaging-commands-reset-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-commands-reset-transient ()
  "Reset the source tree to a pristine state."
  :value '("--quilt" "--pc" "--files")
  :environment #'deb-packaging-transients--env
  ["Reset source tree"
   ("-q" "Pop quilt patches"     "--quilt")
   ("-p" "Remove .pc/ directory" "--pc")
   ("-f" "Remove debian/files"   "--files")]
  ["Run"
    ("r" "Reset" deb-packaging-commands-reset)])

;;; 8. Dev shell (LXD)

;;;###autoload(autoload 'deb-packaging-dev-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-dev-transient ()
  "Develop upstream source in an LXD container with LSP."
  :environment #'deb-packaging-transients--env
  ["Dev shell"
   ("e" "Dev shell (C-u=reprovision)" deb-packaging-dev-shell)
   ("o" "Open existing container (dired)" deb-packaging-dev-open)
   ("p" "Open project (find file)" deb-packaging-dev-project)
   ("x" "Shell into container" deb-packaging-dev-exec)
   ("B" "Generate compile_commands.json" deb-packaging-dev-compile-db)
   ("E" "Start eglot" deb-packaging-dev-eglot)]
  ["Manage"
   ("k" "Destroy dev container" deb-packaging-dev-destroy)]
  ["Navigation"
   ("q" "Back" transient-quit-one)])

(provide 'deb-packaging-transients)
;;; deb-packaging-transients.el ends here
