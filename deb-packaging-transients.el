;;; deb-packaging-transients.el --- Tool transients -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karl Smeltzer
;; Author: Karl Smeltzer
;; Keywords: tools, debian, ubuntu, packaging

;;; Commentary:

;; Six per-tool transients:
;;
;;   deb-packaging-source-build-transient  dpkg-buildpackage
;;   deb-packaging-binary-build-transient  sbuild
;;   deb-packaging-lint-transient          lintian + ubuntu-lint
;;   deb-packaging-test-transient          autopkgtest
;;   deb-packaging-upload-transient        ppa tests
;;   deb-packaging-clean-transient         clean artifacts
;;
;; Each lists its own flags and forwards them to the runner in
;; deb-packaging-commands.el.  Flags persist per-prefix via transient.
;; Distro-bearing options seed dynamically from `deb-packaging-target-distro'.

;;; Code:

(require 'transient)
(require 'deb-packaging-detect)
(require 'deb-packaging-config)

;; Forward-declare helpers to silence the byte-compiler.
(declare-function deb-packaging--effective-distro "deb-packaging-config")
(declare-function deb-packaging--runner-choices "deb-packaging-commands")

;; Tool-specific variables live in deb-packaging-commands.el.
(defvar deb-packaging-sbuild-variants)

;; Forward-declare command functions.
(declare-function deb-packaging-source-build "deb-packaging-commands")
(declare-function deb-packaging-sbuild "deb-packaging-commands")
(declare-function deb-packaging-lintian-source "deb-packaging-commands")
(declare-function deb-packaging-lintian-binary "deb-packaging-commands")
(declare-function deb-packaging-lintian-binary-one "deb-packaging-commands")
(declare-function deb-packaging-ubuntu-lint "deb-packaging-commands")
(declare-function deb-packaging-autopkgtest "deb-packaging-commands")
(declare-function deb-packaging-dput-upload "deb-packaging-commands")
(declare-function deb-packaging-ppa-tests "deb-packaging-commands")
(declare-function deb-packaging-clean "deb-packaging-commands")
(declare-function deb-packaging-reset "deb-packaging-commands")
(declare-function deb-packaging-infra--list-ppas "deb-packaging-infra")
(declare-function deb-packaging-dev-shell "deb-packaging-dev")
(declare-function deb-packaging-dev-eglot "deb-packaging-dev")
(declare-function deb-packaging-dev-compile-db "deb-packaging-dev")
(declare-function deb-packaging-dev-destroy "deb-packaging-dev")
(declare-function deb-packaging-dev-open "deb-packaging-dev")
(declare-function deb-packaging-dev-project "deb-packaging-dev")
(declare-function deb-packaging-dev-exec "deb-packaging-dev")

;;; 1. Source build (dpkg-buildpackage)

;;;###autoload(autoload 'deb-packaging-source-build-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-source-build-transient ()
  "Build a Debian source package with dpkg-buildpackage."
  :value '("-S" "-d" "-nc" "-sa" "-I" "-i")
  ["Arguments"
   ("-S" "Source build"            "-S")
   ("-d" "Skip build-dep check"    "-d")
   ("-nc" "No pre-clean"           "-nc")
   ("-sa" "Include orig tarball"   "-sa")
   ("-I"  "Tar ignore pattern"     "-I")
   ("-i"  "Diff ignore pattern"    "-i")]
  ["Build"
   ("s" "Build source" deb-packaging-source-build)])

;;; 2. Binary build (sbuild)

(defun deb-packaging--binary-default-value ()
  "Dynamic default for the binary-build transient.
Seeds the distro from the changelog."
  (list (format "--dist=%s" (deb-packaging--effective-distro))
        "-A"))

(defclass deb-packaging--extra-repo-argument (transient-option) ()
  "Transient option for sbuild --extra-repository= values.
Completes against variant short names and expands to the full repo
string at runtime.")

(cl-defmethod transient-infix-read ((obj deb-packaging--extra-repo-argument))
  "Read a variant short-name and store it."
  (let ((choices (mapcar #'car deb-packaging-sbuild-variants)))
    (completing-read (format "%s: " (oref obj description))
                     choices nil nil (oref obj value))))

(cl-defmethod transient-format-value ((obj deb-packaging--extra-repo-argument))
  "Display the chosen variant short-name."
  (if-let ((v (oref obj value)))
      (propertize v 'face 'transient-value)
    (propertize "none" 'face 'transient-inactive-value)))

;;;###autoload(autoload 'deb-packaging-binary-build-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-binary-build-transient ()
  "Build a Debian binary package with sbuild."
  :value #'deb-packaging--binary-default-value
  ["Arguments"
   ("-d" "Distribution"
    "--dist="
    :class transient-option
    :choices deb-packaging--distro-choices
    :always-read t
    :allow-empty nil)
   ("-A" "Build arch-all packages"  "-A")
   ("-v" "Verbose"                  "-v")
   ("-F" "Shell on build failure"
    "--build-failed-commands=%SBUILD_SHELL")
   ("-e" "Extra repository"
    "--extra-repository="
    :class deb-packaging--extra-repo-argument
    :description "Extra apt repo")]
  ["Build"
   ("b" "Build binary" deb-packaging-sbuild)])

;;; 3. Lint (lintian + ubuntu-lint)

;;;###autoload(autoload 'deb-packaging-lint-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-lint-transient ()
  "Run a linter against the current package.
lintian inspects built artifacts; ubuntu-lint checks Ubuntu policy.
Each action reads only its own flags, so lintian and ubuntu-lint flags
stay separate."
  :value '("-i" "--tag-display-limit=0" "--context=changes" "--all=warn")
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
   ("l" "Lintian source"        deb-packaging-lintian-source)
   ("L" "Lintian binary (all)"  deb-packaging-lintian-binary)
   ("o" "Lintian one binary..." deb-packaging-lintian-binary-one)
   ("u" "Ubuntu-lint"           deb-packaging-ubuntu-lint)])

;;; 4. Autopkgtest

(defun deb-packaging--test-default-value ()
  "Dynamic default for the test transient."
  (list "--apt-upgrade"
        "--runner=lxd"
        (format "--dist=%s" (deb-packaging--effective-distro))))

;;;###autoload(autoload 'deb-packaging-test-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-test-transient ()
  "Run autopkgtest against locally built .debs."
  :value #'deb-packaging--test-default-value
  ["Arguments"
   ("-u"  "Upgrade packages before test"  "--apt-upgrade")
   ("-f"  "Drop to shell on failure"      "--shell-fail")
    ("-r"  "Test runner"
     "--runner="
     :class transient-option
     :choices deb-packaging--runner-choices
     :always-read t
     :allow-empty nil)
   ("-d"  "Distribution (image)"
    "--dist="
    :class transient-option
    :choices deb-packaging--distro-choices
    :always-read t
    :allow-empty nil)]
  ["Run"
   ("t" "Run autopkgtest" deb-packaging-autopkgtest)])

;;; 5. Upload / PPA

(defun deb-packaging--upload-default-value ()
  "Dynamic default for the upload transient."
  (list (format "--dist=%s" (deb-packaging--effective-distro))))

(defun deb-packaging--read-ppa (prompt initial-input _history)
  "Read a PPA name, completing against the user's known PPAs."
  (let ((candidates (when (fboundp 'deb-packaging-infra--list-ppas)
                      (deb-packaging-infra--list-ppas))))
    (completing-read prompt candidates nil nil initial-input)))

;;;###autoload(autoload 'deb-packaging-upload-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-upload-transient ()
  "Upload to a Launchpad PPA with dput, or view test results."
  :value #'deb-packaging--upload-default-value
  ["PPA"
   ("-p"  "PPA (required)"
    "--ppa="
    :class transient-option
    :prompt "PPA (e.g. ppa:user/name): "
    :reader deb-packaging--read-ppa
    :always-read t
    :allow-empty nil)]
  ["Options"
   ("-d"  "Distribution"
    "--dist="
    :class transient-option
    :choices deb-packaging--distro-choices
    :always-read t
    :allow-empty nil)]
  ["Upload"
   ("p" "Upload with dput"     deb-packaging-dput-upload)
   ("r" "PPA test results"     deb-packaging-ppa-tests)])

;;; 6. Clean artifacts

;;;###autoload(autoload 'deb-packaging-clean-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-clean-transient ()
  "Remove build artifacts from the output directory."
  :value '("--stale")
  ["What to remove"
   ("-a" "Current-version artifacts" "--artifacts")
   ("-S" "Stale artifacts (other versions)" "--stale")]
  ["Run"
   ("c" "Clean" deb-packaging-clean)])

;;; 7. Reset source tree

;;;###autoload(autoload 'deb-packaging-reset-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-reset-transient ()
  "Reset the source tree to a pristine state."
  :value '("--quilt" "--pc" "--files")
  ["Reset source tree"
   ("-q" "Pop quilt patches"     "--quilt")
   ("-p" "Remove .pc/ directory" "--pc")
   ("-f" "Remove debian/files"   "--files")]
  ["Run"
    ("r" "Reset" deb-packaging-reset)])

;;; 8. Dev shell (LXD)

;;;###autoload(autoload 'deb-packaging-dev-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-dev-transient ()
  "Develop upstream source in an LXD container with LSP."
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
