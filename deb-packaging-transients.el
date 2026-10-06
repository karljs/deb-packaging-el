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
;; The distro always comes from the changelog (see
;; `deb-packaging-config--effective-distro').

;;; Code:

(require 'transient)
(require 'subr-x)
(require 'deb-packaging-detect)
(require 'deb-packaging-config)
(require 'deb-packaging-ppa)
(require 'deb-packaging-display)
(require 'deb-packaging-resume)

;; Forward-declare helpers to silence the byte-compiler.
(declare-function deb-packaging-commands--runner-choices "deb-packaging-commands")
(declare-function deb-packaging-commands--package-context "deb-packaging-commands")

;; Tool-specific variables live in deb-packaging-commands.el.
(defvar deb-packaging-commands-sbuild-variants)

(defvar deb-packaging-config-extra-ppas)
(declare-function deb-packaging-repos-load "deb-packaging-repos")

;; Single source of truth for the shell-on-failure flag: the transient
;; suffix (below) and the status buffer both read this constant.
(defconst deb-packaging-transients-sbuild-shell-flag
  "--build-failed-commands=%SBUILD_SHELL")

(transient-define-infix deb-packaging-transients--sbuild-shell ()
  "Drop into a chroot shell when the sbuild build fails."
  :argument deb-packaging-transients-sbuild-shell-flag)

(defun deb-packaging-transients--env (fn)
  "Run FN with the package's transient display action bound.
Used as :environment for the prefixes in this package."
  (let ((transient-display-buffer-action
         deb-packaging-display-transient-action)
        (deb-packaging-detect--memo (make-hash-table :test #'equal)))
    (funcall fn)))

(defun deb-packaging-transients--context ()
  "Return the current workspace context with its saved default PPA."
  (when-let ((ctx (deb-packaging-commands--package-context)))
    (plist-put ctx :default-ppa
               (deb-packaging-ppa-load
                (plist-get ctx :name) (plist-get ctx :distro)))))

(defun deb-packaging-transients--titled (header title)
  "Return HEADER's text above group TITLE.
Transient hides groups without suffixes, so a header rides on the first
real group's description."
  (concat (funcall header) "\n\n" (propertize title 'face 'transient-heading)))

(defun deb-packaging-transients--context-header ()
  "Return a compact header for package operation transients."
  (if-let ((ctx (deb-packaging-transients--context)))
      (concat
       (format "%s %s | %s | %s | %s"
               (plist-get ctx :name)
               (plist-get ctx :version)
               (plist-get ctx :distro)
               (let* ((host (plist-get ctx :host-arch))
                      (target (or (plist-get ctx :target-arch) host)))
                 (if (and host (not (equal target host)))
                     (format "%s (host %s)" target host)
                   (or target host "unknown arch")))
               (if (plist-get ctx :git-p)
                   (format "git: %s%s"
                           (or (plist-get ctx :branch) "detached")
                           (if (plist-get ctx :dirty-p) " (modified)" ""))
                 "not a git repository"))
       (when-let ((ppa (plist-get ctx :default-ppa)))
         (format "\nPPA: %s" ppa)))
    "No package context"))

;; Forward-declare command functions.
(declare-function deb-packaging-commands-source-build "deb-packaging-commands")
(declare-function deb-packaging-commands-gbp-export-orig "deb-packaging-commands")
(declare-function deb-packaging-commands-export-orig "deb-packaging-commands")
(declare-function deb-packaging-commands-binary-build "deb-packaging-commands")
(declare-function deb-packaging-infra-create-schroot "deb-packaging-infra")
(declare-function deb-packaging-infra-create-lxd "deb-packaging-infra")
(declare-function deb-packaging-infra-create-qemu "deb-packaging-infra")
(declare-function deb-packaging-commands-lintian-source "deb-packaging-commands")
(declare-function deb-packaging-commands-lintian-binary "deb-packaging-commands")
(declare-function deb-packaging-commands-lintian-binary-one "deb-packaging-commands")
(declare-function deb-packaging-commands-ubuntu-lint "deb-packaging-commands")
(declare-function deb-packaging-commands-autopkgtest "deb-packaging-commands")
(declare-function deb-packaging-commands-dput-upload "deb-packaging-commands")
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
(declare-function deb-packaging-dev--container-exists-p "deb-packaging-dev")
(declare-function deb-packaging-dev--container-name "deb-packaging-dev")

;;; 1. Source build (dpkg-buildpackage)

(defcustom deb-packaging-transients-source-default-args
  '("--builder=dpkg-buildpackage" "-d" "-nc" "-sa" "-I" "-i")
  "Initial dpkg-buildpackage flags for a source build."
  :type '(repeat string)
  :group 'deb-packaging)

(defun deb-packaging-transients--source-default-value ()
  "Return the configured initial source-build arguments."
  deb-packaging-transients-source-default-args)

;;;###autoload(autoload 'deb-packaging-commands-source-build-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-commands-source-build-transient ()
  "Build the Debian source package, or fetch its orig tarball."
  :value #'deb-packaging-transients--source-default-value
  :environment #'deb-packaging-transients--env
  [:description (lambda () (deb-packaging-transients--titled
                              #'deb-packaging-transients--context-header "Builder"))
   ("-b" "Builder" "--builder="
    :class transient-option
    :choices ("dpkg-buildpackage" "gbp")
    :always-read t
    :allow-empty nil)]
  [["dpkg-buildpackage options"
    ("-d" "Skip build-dep check"    "-d")
    ("-nc" "No pre-clean"           "-nc")
    ("-sa" "Include orig tarball"   "-sa")
    ("-I"  "Tar ignore pattern"     "-I")
    ("-i"  "Diff ignore pattern"    "-i")]
   ["gbp options"
    ("-g" "Ignore uncommitted changes" "--git-ignore-new")]]
  [["Build"
    ("s" "Build source package" deb-packaging-commands-source-build)
    ("q" "Quit" transient-quit-one)]
   ["Get orig tarball"
    :if-not deb-packaging-transients--native-p
    ("e" "git ubuntu export-orig" deb-packaging-commands-export-orig)
    ("g" "gbp (pristine-tar)" deb-packaging-commands-gbp-export-orig)]])

(defun deb-packaging-transients--native-p ()
  "Return non-nil when the current package has a native version."
  (when-let ((version (ignore-errors (deb-packaging-detect--package-version))))
    (deb-packaging-detect--native-version-p version)))

;;; 2. Binary build (sbuild)

(defun deb-packaging-transients--effective-repos ()
  "Return the extra-repo entries a binary build would use now.
The saved set for the current package and changelog distro, or nil when
nothing was saved.  No implicit default: the build chroot's own
sources.list already provides the distro's pockets (Ubuntu chroots
include -proposed via mk-sbuild), and a default `proposed' entry would
duplicate it (\"configured multiple times\") and break Debian builds
(sid-proposed does not exist on archive.ubuntu.com).  Chroots without
proposed can add the `proposed' candidate in the -e menu."
  (let* ((distro (deb-packaging-config--effective-distro))
         (pkg-name (deb-packaging-detect--package-name)))
    (when pkg-name
      (deb-packaging-repos-load pkg-name distro))))

(defun deb-packaging-transients--binary-default-value ()
  "Dynamic default for the binary-build transient.
Restores the saved extra-repository set for the current package and
changelog distro; nothing extra when no set was saved (the chroot's own
sources.list provides the distro's pockets)."
  (append (list "--builder=sbuild" "--keep-failed" "-A" (concat "--arch="
                              (deb-packaging-config--effective-architecture)))
          (mapcar (lambda (r) (concat "--extra-repository=" r))
                  (deb-packaging-transients--effective-repos))))

(defun deb-packaging-transients--read-architecture (prompt initial-input _history)
  "Read a Debian architecture with common values as completion candidates."
  (completing-read prompt '("amd64" "arm64" "armhf" "i386" "ppc64el"
                            "riscv64" "s390x")
                   nil nil initial-input))

(defun deb-packaging-transients--seed-from-prefix (obj arg-prefix)
  "Seed OBJ's value from flat ARG-PREFIX args in the prefix value.
Works around upstream repeat-mode init-value, which keeps whole arg
strings and so doubles the argument when the value is re-emitted."
  (oset obj value
        (mapcar (lambda (a) (string-remove-prefix arg-prefix a))
                (seq-filter
                 (lambda (a) (and (stringp a) (string-prefix-p arg-prefix a)))
                 (oref transient--prefix value)))))

(defun deb-packaging-transients--format-list-value (value display-fn)
  "Show VALUE entries, comma-separated, each through DISPLAY-FN."
  (if value
      (mapconcat (lambda (entry)
                   (propertize (funcall display-fn entry)
                               'face 'transient-value))
                 value
                 (propertize "," 'face 'transient-inactive-value))
    (propertize "none" 'face 'transient-inactive-value)))

(defclass deb-packaging-transients--extra-repo-argument (transient-option) ()
  "sbuild --extra-repository= option, expanded to a repo string at build time.")

(defun deb-packaging-transients--extra-repo-read (current)
  "Read one extra-repository entry and toggle it against CURRENT.
CURRENT is the entry list or nil.  Completes against
`deb-packaging-commands-sbuild-variants' names, known PPAs, and
`deb-packaging-config-extra-ppas'.  Selecting an entry already in the
set removes it; empty input keeps the set.  Returns the new list of
entries, or nil when empty.  A variant name or ppa: address expands
at build time; anything else is passed to sbuild verbatim."
  (let* ((variants (mapcar #'car deb-packaging-commands-sbuild-variants))
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
  "Seed OBJ's entries from flat --extra-repository= args in the prefix value."
  (deb-packaging-transients--seed-from-prefix obj "--extra-repository="))

(cl-defmethod transient-format-value ((obj deb-packaging-transients--extra-repo-argument))
  "Show the chosen entries, comma-separated."
  (deb-packaging-transients--format-list-value
   (and (slot-boundp obj 'value) (oref obj value)) #'identity))

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
CURRENT is the path list or nil.  Completes against .deb files in the
build-output directory, falling back to file-name reading.  Selecting
a path already in the set removes it; empty input keeps the set.
Returns absolute paths, or nil when empty."
  (let* ((pkg-dir (deb-packaging-detect--find-package-dir))
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
  "Seed OBJ's paths from flat --extra-package= args in the prefix value."
  (deb-packaging-transients--seed-from-prefix obj "--extra-package="))

(cl-defmethod transient-format-value ((obj deb-packaging-transients--extra-package-argument))
  "Show the selected .deb file(s) by base name."
  (deb-packaging-transients--format-list-value
   (and (slot-boundp obj 'value) (oref obj value)) #'file-name-nondirectory))

;;;###autoload(autoload 'deb-packaging-binary-build-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-binary-build-transient ()
  "Build Debian binary packages.
sbuild builds the .dsc in a chroot; dpkg-buildpackage and gbp build the
working tree on the host."
  :value #'deb-packaging-transients--binary-default-value
  :environment #'deb-packaging-transients--env
  [:description (lambda () (deb-packaging-transients--titled
                              #'deb-packaging-transients--context-header "Builder"))
   ("-b" "Builder" "--builder="
    :class transient-option
    :choices ("sbuild" "dpkg-buildpackage" "gbp")
    :always-read t
    :allow-empty nil)]
  [["sbuild options"
    ("-a" "Target architecture"
     "--arch="
     :class transient-option
     :reader deb-packaging-transients--read-architecture
     :always-read t
     :allow-empty nil)
    ("-A" "Build arch-all packages"  "-A")
    ("-v" "Verbose"                  "-v")
    ("-u" "apt upgrade"              "--apt-upgrade")
    ("-k" "Keep failed builds for resuming" "--keep-failed")
    ("-T" "Skip tests (nocheck)" "--profiles=nocheck")
    ("-F" "Shell on build failure"
     deb-packaging-transients--sbuild-shell)
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
   ["gbp options"
    ("-g" "Ignore uncommitted changes" "--git-ignore-new")]]
  [["Build"
    ("b" "Build binaries" deb-packaging-commands-binary-build)
    ("q" "Quit" transient-quit-one)]
   ["Setup"
    ("c" "Create sbuild chroot" deb-packaging-infra-create-schroot)]]
  ["Failed build kept for resuming"
   :if deb-packaging-resume-current
   ("r" "Resume with the checkout's debian/ changes" deb-packaging-resume-build)
   ("o" "Open the build tree" deb-packaging-resume-open-tree)
   ("x" "Shell in the build session" deb-packaging-resume-shell)
   ("d" "Discard it" deb-packaging-resume-discard)])

;;; Suffix availability
;;
;; Actions whose inputs are missing render inapt with the reason in their
;; label, matching the status buffer's `blocked' rows.

(defconst deb-packaging-transients--artifact-needs
  '((dsc . "needs a source package")
    (source-changes . "needs a source package")
    (debs . "needs binaries"))
  "Why an action is unavailable when an artifact kind is missing.")

(defun deb-packaging-transients--why-not (needs)
  "Return why NEEDS are unmet, or nil.
NEEDS lists tool names (strings) and artifact kinds (symbols)."
  (let ((arts (plist-get (ignore-errors (deb-packaging-commands--package-context))
                         :artifacts)))
    (cl-loop for need in needs
             thereis (if (stringp need)
                         (unless (executable-find need)
                           (format "%s not installed" need))
                       (unless (alist-get need arts)
                         (alist-get need deb-packaging-transients--artifact-needs))))))

(defun deb-packaging-transients--label-why (text why)
  "Return TEXT, suffixed with WHY it is unavailable when non-nil."
  (if why (format "%s (%s)" text why) text))

(defun deb-packaging-transients--label (text needs)
  "Return TEXT, suffixed with why NEEDS are unmet."
  (deb-packaging-transients--label-why
   text (deb-packaging-transients--why-not needs)))

;;; 3. Lint: lintian (Debian policy) and ubuntu-lint (Ubuntu upload rules)

(transient-define-infix deb-packaging-transients--lintian-info ()
  :description "Explain each tag"
  :argument "-i")

(transient-define-infix deb-packaging-transients--lintian-display-info ()
  :description "Show info (I:) tags"
  :argument "-I")

(transient-define-infix deb-packaging-transients--lintian-pedantic ()
  :description "Show pedantic (P:) tags"
  :argument "--pedantic")

(transient-define-infix deb-packaging-transients--lintian-limit ()
  :description "Tags shown per package"
  :class 'transient-option
  :argument "--tag-display-limit="
  :prompt "Limit (0 = unlimited): ")

(transient-define-infix deb-packaging-transients--ubuntu-lint-verbose ()
  :description "Verbose"
  :argument "--verbose")

(transient-define-infix deb-packaging-transients--ubuntu-lint-context ()
  :description "Check against"
  :class 'transient-option
  :argument "--context="
  :choices '("changes" "source-dir" "changelog")
  :always-read t
  :allow-empty nil)

(transient-define-infix deb-packaging-transients--ubuntu-lint-level ()
  :description "Level for all checks"
  :class 'transient-option
  :argument "--all="
  :choices '("auto" "off" "warn" "fail"))

(defconst deb-packaging-transients--lint-default-args
  '("--tag-display-limit=0" "--context=changes" "--all=warn")
  "Initial value shared by the lint menus.")

;;;###autoload(autoload 'deb-packaging-lintian-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-lintian-transient ()
  "Check the built packages against Debian policy with lintian."
  :value deb-packaging-transients--lint-default-args
  :environment #'deb-packaging-transients--env
  [:description (lambda () (deb-packaging-transients--titled
                              #'deb-packaging-transients--context-header "lintian options"))
   ("-i" deb-packaging-transients--lintian-info)
   ("-I" deb-packaging-transients--lintian-display-info)
   ("-P" deb-packaging-transients--lintian-pedantic)
   ("-t" deb-packaging-transients--lintian-limit)]
  ["Check"
   ("s" deb-packaging-commands-lintian-source
    :description (lambda () (deb-packaging-transients--label "Source package (.dsc)" '("lintian" dsc)))
    :inapt-if (lambda () (deb-packaging-transients--why-not '("lintian" dsc))))
   ("b" deb-packaging-commands-lintian-binary
    :description (lambda () (deb-packaging-transients--label "All binaries (.deb)" '("lintian" debs)))
    :inapt-if (lambda () (deb-packaging-transients--why-not '("lintian" debs))))
   ("o" deb-packaging-commands-lintian-binary-one
    :description (lambda () (deb-packaging-transients--label "One binary..." '("lintian" debs)))
    :inapt-if (lambda () (deb-packaging-transients--why-not '("lintian" debs))))
   ("q" "Quit" transient-quit-one)])

;;;###autoload(autoload 'deb-packaging-ubuntu-lint-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-ubuntu-lint-transient ()
  "Check Ubuntu upload rules (changelog, maintainer, bug refs) with ubuntu-lint."
  :value deb-packaging-transients--lint-default-args
  :environment #'deb-packaging-transients--env
  [:description (lambda () (deb-packaging-transients--titled
                              #'deb-packaging-transients--context-header "ubuntu-lint options"))
   ("-v" deb-packaging-transients--ubuntu-lint-verbose)
   ("-C" deb-packaging-transients--ubuntu-lint-context)
   ("-a" deb-packaging-transients--ubuntu-lint-level)]
  ["Check"
   ("u" deb-packaging-commands-ubuntu-lint
    :description (lambda () (deb-packaging-transients--label "Ubuntu upload rules" '("ubuntu-lint")))
    :inapt-if (lambda () (deb-packaging-transients--why-not '("ubuntu-lint"))))
   ("q" "Quit" transient-quit-one)])

;;;###autoload(autoload 'deb-packaging-lint-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-lint-transient ()
  "Run lintian (Debian policy) or ubuntu-lint (Ubuntu upload rules).
Each check reads only its own tool's options."
  :value deb-packaging-transients--lint-default-args
  :environment #'deb-packaging-transients--env
  [:description deb-packaging-transients--context-header
   ["lintian options"
    ("-i" deb-packaging-transients--lintian-info)
    ("-I" deb-packaging-transients--lintian-display-info)
    ("-P" deb-packaging-transients--lintian-pedantic)
    ("-t" deb-packaging-transients--lintian-limit)]
   ["ubuntu-lint options"
    ("-v" deb-packaging-transients--ubuntu-lint-verbose)
    ("-C" deb-packaging-transients--ubuntu-lint-context)
    ("-a" deb-packaging-transients--ubuntu-lint-level)]]
  [["lintian (Debian policy)"
    ("s" deb-packaging-commands-lintian-source
     :description (lambda () (deb-packaging-transients--label "Source package (.dsc)" '("lintian" dsc)))
     :inapt-if (lambda () (deb-packaging-transients--why-not '("lintian" dsc))))
    ("b" deb-packaging-commands-lintian-binary
     :description (lambda () (deb-packaging-transients--label "All binaries (.deb)" '("lintian" debs)))
     :inapt-if (lambda () (deb-packaging-transients--why-not '("lintian" debs))))
    ("o" deb-packaging-commands-lintian-binary-one
     :description (lambda () (deb-packaging-transients--label "One binary..." '("lintian" debs)))
     :inapt-if (lambda () (deb-packaging-transients--why-not '("lintian" debs))))]
   ["ubuntu-lint"
    ("u" deb-packaging-commands-ubuntu-lint
     :description (lambda () (deb-packaging-transients--label "Ubuntu upload rules" '("ubuntu-lint")))
     :inapt-if (lambda () (deb-packaging-transients--why-not '("ubuntu-lint"))))
    ("q" "Quit" transient-quit-one)]])

;;; 4. Autopkgtest

(defun deb-packaging-transients--saved-ppa-arg ()
  "Return a one-element --ppa= list seeded from the saved PPA, or nil."
  (when-let* ((pkg-name (deb-packaging-detect--package-name))
              (ppa (deb-packaging-ppa-load
                    pkg-name (deb-packaging-config--effective-distro))))
    (list (concat "--ppa=" ppa))))

(defun deb-packaging-transients--test-default-value ()
  "Dynamic default for the test transient."
  (list "--apt-upgrade" "--runner=lxd"))

(defun deb-packaging-transients--create-test-image ()
  "Create the image selected in the autopkgtest transient."
  (interactive)
  (if (equal (transient-arg-value "--runner="
                                  (transient-args 'deb-packaging-test-transient))
             "qemu")
      (deb-packaging-infra-create-qemu)
    (deb-packaging-infra-create-lxd)))

;;;###autoload(autoload 'deb-packaging-test-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-test-transient ()
  "Run autopkgtest locally.
The test image's distro comes from the changelog."
  :value #'deb-packaging-transients--test-default-value
  :environment #'deb-packaging-transients--env
  [:description (lambda () (deb-packaging-transients--titled
                              #'deb-packaging-transients--context-header "Options"))
   ("-u"  "Upgrade packages before test"   "--apt-upgrade")
   ("-P"  "Use dependencies from proposed" "--apt-pocket=proposed")
   ("-f"  "Drop to shell on failure"       "--shell-fail")
   ("-r"  "Test runner"
      "--runner="
      :class transient-option
      :choices deb-packaging-commands--runner-choices
      :always-read t
      :allow-empty nil)]
  [["Run"
    ("t" deb-packaging-commands-autopkgtest
     :description (lambda () (deb-packaging-transients--label "Run autopkgtest" '("autopkgtest" debs)))
     :inapt-if (lambda () (deb-packaging-transients--why-not '("autopkgtest" debs))))
    ("q" "Quit" transient-quit-one)]
   ["Setup"
    ("i" "Create test image" deb-packaging-transients--create-test-image)]])

;;; 5. Upload / PPA

(defun deb-packaging-transients--upload-default-value ()
  "Dynamic default for the upload transient."
  (deb-packaging-transients--saved-ppa-arg))

(defun deb-packaging-transients--read-ppa (prompt initial-input _history)
  "Read a PPA name, completing against the user's known PPAs."
  (let ((candidates (deb-packaging-infra--list-ppas)))
    (completing-read prompt candidates nil nil initial-input)))

;;;###autoload(autoload 'deb-packaging-upload-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-upload-transient ()
  "Upload to a Launchpad PPA with dput."
  :value #'deb-packaging-transients--upload-default-value
  :environment #'deb-packaging-transients--env
  [:description (lambda () (deb-packaging-transients--titled
                              #'deb-packaging-transients--context-header "PPA"))
   ("-p"  "PPA (required)"
    "--ppa="
    :class transient-option
    :prompt "PPA (e.g. ppa:user/name): "
    :reader deb-packaging-transients--read-ppa
    :always-read t
    :allow-empty nil)]
  ["Upload"
   ("p" deb-packaging-commands-dput-upload
    :description (lambda () (deb-packaging-transients--label "Upload with dput" '("dput" source-changes)))
    :inapt-if (lambda () (deb-packaging-transients--why-not '("dput" source-changes))))
   ("q" "Quit" transient-quit-one)])

;;; 6. Clean artifacts

;;;###autoload(autoload 'deb-packaging-commands-clean-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-commands-clean-transient ()
  "Remove build artifacts from the output directory."
  :value '("--stale")
  :environment #'deb-packaging-transients--env
  [:description (lambda () (deb-packaging-transients--titled
                              #'deb-packaging-transients--context-header "What to remove"))
   ("-a" "Current-version artifacts" "--artifacts")
   ("-S" "Stale artifacts (other versions)" "--stale")]
  ["Run"
   ("c" "Clean" deb-packaging-commands-clean)
   ("q" "Quit" transient-quit-one)])

;;; 7. Reset source tree

;;;###autoload(autoload 'deb-packaging-commands-reset-transient "deb-packaging-transients" nil t)
(transient-define-prefix deb-packaging-commands-reset-transient ()
  "Reset the source tree to a pristine state."
  :value '("--quilt" "--pc" "--files")
  :environment #'deb-packaging-transients--env
  [:description (lambda () (deb-packaging-transients--titled
                              #'deb-packaging-transients--context-header "Reset source tree"))
   ("-q" "Pop quilt patches"     "--quilt")
   ("-p" "Remove .pc/ directory" "--pc")
   ("-f" "Remove debian/files"   "--files")]
  ["Run"
    ("r" "Reset" deb-packaging-commands-reset)
    ("q" "Quit" transient-quit-one)])

;;; 8. Dev shell (LXD)

;;;###autoload(autoload 'deb-packaging-dev-transient "deb-packaging-transients" nil t)
(defun deb-packaging-transients--no-dev-container-p ()
  "Return non-nil when this package has no dev container yet."
  (not (ignore-errors
         (deb-packaging-dev--container-exists-p
          (deb-packaging-dev--container-name
           (deb-packaging-detect--package-name)
           (deb-packaging-config--effective-distro))))))

(defun deb-packaging-transients--not-in-container-file-p ()
  "Return non-nil unless the current buffer visits a file in a container."
  (not (and buffer-file-name (string-prefix-p "/lxc:" buffer-file-name))))

(transient-define-prefix deb-packaging-dev-transient ()
  "Work on the upstream source in an LXD container with build-deps and LSP."
  :environment #'deb-packaging-transients--env
  [:description deb-packaging-transients--context-header
   ["Container"
    ("e" "Create or update (C-u: reinstall everything)" deb-packaging-dev-shell)
    ("d" "Delete" deb-packaging-dev-destroy
     :inapt-if deb-packaging-transients--no-dev-container-p)]
   ["Work in it"
    ("f" "Find a file" deb-packaging-dev-project
     :inapt-if deb-packaging-transients--no-dev-container-p)
    ("o" "Browse the source (dired)" deb-packaging-dev-open
     :inapt-if deb-packaging-transients--no-dev-container-p)
    ("x" "Shell" deb-packaging-dev-exec
     :inapt-if deb-packaging-transients--no-dev-container-p)]
   ["Language server"
    ("c" "Generate compile_commands.json (C/C++)" deb-packaging-dev-compile-db
     :inapt-if deb-packaging-transients--no-dev-container-p)
    ("l" "Start eglot in this buffer" deb-packaging-dev-eglot
     :inapt-if deb-packaging-transients--not-in-container-file-p)
    ("q" "Quit" transient-quit-one)]])

(provide 'deb-packaging-transients)
;;; deb-packaging-transients.el ends here
