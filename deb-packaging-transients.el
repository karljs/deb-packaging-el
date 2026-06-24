;;; deb-packaging-transients.el --- Per-tool transients for deb-packaging -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karl Smeltzer
;; Author: Karl Smeltzer
;; Keywords: tools, debian, ubuntu, packaging
;; Package-Requires: ((emacs "28.1") (transient "0.4.0"))

;;; Commentary:

;; Six per-tool transients, one per major packaging action:
;;
;;   deb-packaging-source-build-transient  — dpkg-buildpackage
;;   deb-packaging-binary-build-transient  — sbuild
;;   deb-packaging-lint-transient          — lintian (source + binary)
;;   deb-packaging-test-transient          — autopkgtest
;;   deb-packaging-upload-transient        — ppa tests
;;   deb-packaging-clean-transient         — clean artifacts
;;
;; Each transient lists its own flags in an ["Arguments"] group and has an
;; action group that reads (transient-args 'PREFIX) and forwards them to the
;; appropriate runner in deb-packaging-commands.el.
;;
;; Flags are persisted per-prefix by transient's native mechanism (C-x C-s
;; saves, C-x s sets for this session), replacing the old global-mode
;; (default/debug/upload) system.  Default values are seeded from sensible
;; defaults at definition time, with distro-bearing options seeded dynamically
;; from `deb-packaging-target-distro' via :default-value functions.

;;; Code:

(require 'transient)
(require 'deb-packaging-detect)
(require 'deb-packaging-presets)

;; Forward-declare command functions to keep the byte-compiler quiet.
(declare-function deb-packaging-source-build "deb-packaging-commands")
(declare-function deb-packaging-sbuild "deb-packaging-commands")
(declare-function deb-packaging-lintian-source "deb-packaging-commands")
(declare-function deb-packaging-lintian-binary "deb-packaging-commands")
(declare-function deb-packaging-lintian-binary-one "deb-packaging-commands")
(declare-function deb-packaging-autopkgtest "deb-packaging-commands")
(declare-function deb-packaging-dput-upload "deb-packaging-commands")
(declare-function deb-packaging-ppa-tests "deb-packaging-commands")
(declare-function deb-packaging-clean "deb-packaging-commands")
(declare-function deb-packaging-infra--list-ppas "deb-packaging-infra")

;;; Known Ubuntu distros (shared completion source)

(defconst deb-packaging-ubuntu-distros
  '("questing" "plucky" "oracular" "noble" "jammy" "focal"
    "resolute" "mantic" "lunar" "kinetic" "impish" "hirsute"
    "groovy" "bionic" "xenial")
  "Known Ubuntu distribution codenames, newest first.")

(defun deb-packaging--distro-choices ()
  "Return distro completion list, prepending the changelog distro if unknown."
  (let ((current deb-packaging-target-distro))
    (if (member current deb-packaging-ubuntu-distros)
        deb-packaging-ubuntu-distros
      (cons current deb-packaging-ubuntu-distros))))

;;; ── 1. Source build (dpkg-buildpackage) ─────────────────────────────────────

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

;;; ── 2. Binary build (sbuild) ─────────────────────────────────────────────────

(defun deb-packaging--binary-default-value ()
  "Dynamic default for the binary-build transient.
Seeds the distro from `deb-packaging-target-distro' so the option reflects
the current package on each invocation."
  (list (format "--dist=%s" deb-packaging-target-distro)
        "-A"))

(defclass deb-packaging--extra-repo-argument (transient-option) ()
  "Transient option class for sbuild --extra-repository= values.
Completes against `deb-packaging-sbuild-variants' short names and
expands the chosen name to the full repository string (with distro
substituted) when the command is actually run.")

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
   ("-e" "Extra repository"
    "--extra-repository="
    :class deb-packaging--extra-repo-argument
    :description "Extra apt repo")]
  ["Build"
   ("b" "Build binary" deb-packaging-sbuild)])

;;; ── 3. Lintian ────────────────────────────────────────────────────────────────

;;;###autoload(autoload 'deb-packaging-lint-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-lint-transient ()
  "Run lintian on the source .dsc or binary .deb files."
  :value '("-i" "--tag-display-limit=0")
  ["Arguments"
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
  ["Run"
   ("l" "Lint source"        deb-packaging-lintian-source)
   ("L" "Lint binary (all)"  deb-packaging-lintian-binary)
   ("o" "Lint one binary..." deb-packaging-lintian-binary-one)])

;;; ── 4. Autopkgtest ────────────────────────────────────────────────────────────

(defun deb-packaging--test-default-value ()
  "Dynamic default for the test transient."
  (list "--apt-upgrade"
        "--runner=lxd"
        (format "--dist=%s" deb-packaging-target-distro)))

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
    :choices ("lxd" "qemu")
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

;;; ── 5. Upload / PPA ───────────────────────────────────────────────────────────

(defun deb-packaging--upload-default-value ()
  "Dynamic default for the upload transient."
  (list (format "--dist=%s" deb-packaging-target-distro)))

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
   ("u" "Upload with dput"     deb-packaging-dput-upload)
   ("p" "Show PPA test results" deb-packaging-ppa-tests)])

;;; ── 6. Clean ──────────────────────────────────────────────────────────────────

;;;###autoload(autoload 'deb-packaging-clean-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-clean-transient ()
  "Clean build artifacts and working-tree state."
  :value '("--quilt" "--sessions" "--artifacts" "--pc" "--files")
  ["What to clean"
   ("-q" "Pop quilt patches"               "--quilt")
   ("-s" "End all schroot sessions"        "--sessions")
   ("-a" "Remove current-version artifacts" "--artifacts")
   ("-S" "Remove stale artifacts (other versions)" "--stale")
   ("-p" "Remove .pc/ directory"           "--pc")
   ("-f" "Remove debian/files"             "--files")]
  ["Run"
   ("c" "Clean" deb-packaging-clean)])

(provide 'deb-packaging-transients)
;;; deb-packaging-transients.el ends here
