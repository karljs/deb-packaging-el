;;; deb-packaging.el --- Context-aware Debian packaging interface -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/example/deb-packaging
;; Package-Requires: ((emacs "28.1") (transient "0.4.0"))

;;; Commentary:

;; A context-aware transient interface for Debian/Ubuntu packaging.
;; Detects package context, manages presets, and provides workflow hints.
;;
;; Main entry point: `deb-packaging-dispatch'
;; Default keybinding: C-c d

;;; Code:

(require 'transient)
(require 'deb-packaging-detect)
(require 'deb-packaging-presets)
(require 'deb-packaging-commands)
(require 'deb-packaging-infra)

;;; Project Root Detection

(defun deb-packaging--project-root ()
  "Get project root, preferring projectile if available."
  (or (and (bound-and-true-p projectile-mode)
           (projectile-project-root))
      (and (fboundp 'project-current)
           (when-let ((proj (project-current)))
             (project-root proj)))
      default-directory))

;;; Session State

(defvar deb-packaging--session nil
  "Current session state: (pkg-name version distro source-dir artifacts).")

(defun deb-packaging--refresh-session ()
  "Refresh session state from current context."
  (let* ((start-dir (deb-packaging--project-root))
         (pkg-dir (deb-packaging--find-package-dir start-dir))
         (info (when pkg-dir (deb-packaging--parse-changelog pkg-dir)))
         (parent-dir (when pkg-dir
                       (file-name-directory (directory-file-name pkg-dir))))
         (artifacts (when (and info parent-dir)
                      (deb-packaging--scan-artifacts
                       (nth 0 info) (nth 1 info) parent-dir))))
    (setq deb-packaging--session
          (when info
            (list (nth 0 info) (nth 1 info) (nth 2 info) pkg-dir artifacts)))
    ;; Set distro from changelog
    (when (and info (nth 2 info))
      (setq deb-packaging-target-distro (nth 2 info)))))

(defun deb-packaging--ensure-session ()
  "Ensure session exists and matches current directory."
  (let ((pkg-dir (deb-packaging--find-package-dir)))
    ;; Refresh if no session, or if we're in a different package directory
    (when (or (null deb-packaging--session)
              (not (equal pkg-dir (nth 3 deb-packaging--session))))
      (deb-packaging--refresh-session)))
  deb-packaging--session)

;;; Transient Descriptions

(defun deb-packaging--header-with-artifacts ()
  "Format header with package info, settings, and artifacts."
  (deb-packaging--ensure-session)
  (if-let ((s deb-packaging--session))
      (let* ((arts (nth 4 s))
             (src-changes (alist-get 'source-changes arts))
             (bin-changes (car (alist-get 'binary-changes arts)))
             (debs (alist-get 'debs arts)))
        (concat
         (format "Debian Packaging: %s %s\n" (nth 0 s) (nth 1 s))
         (format "Mode: %s  Distro: %s  sbuild: %s  tests: %s\n"
                 deb-packaging-global-mode
                 deb-packaging-target-distro
                 deb-packaging-sbuild-variant
                 deb-packaging-test-runner)
         (format "PPA: %s\n\n"
                 (or deb-packaging--current-ppa "(none)"))
         "Artifacts:\n"
         (if src-changes
             (format "  source: %s\n" (file-name-nondirectory src-changes))
           "  source: (none)\n")
         (if bin-changes
             (format "  binary: %s (%d debs)\n"
                     (file-name-nondirectory bin-changes)
                     (length debs))
           "  binary: (none)\n")))
    "Debian Packaging: [not in package directory]\n"))

(defun deb-packaging--source-desc ()
  "Description for source build action."
  (format "Source build      %s"
          (mapconcat #'identity
                     (deb-packaging--get-mode-args 'dpkg-buildpackage) " ")))

(defun deb-packaging--sbuild-desc ()
  "Description for sbuild action."
  (let* ((s deb-packaging--session)
         (arts (when s (nth 4 s)))
         (dsc (when arts (alist-get 'dsc arts))))
    (if dsc
        (format "Binary build      sbuild -d%s" deb-packaging-target-distro)
      "Binary build      (no .dsc)")))

(defun deb-packaging--test-desc ()
  "Description for autopkgtest action."
  (let* ((s deb-packaging--session)
         (arts (when s (nth 4 s)))
         (debs (when arts (alist-get 'debs arts))))
    (if debs
        (format "Autopkgtest       via %s" deb-packaging-test-runner)
      "Autopkgtest       (no .debs)")))

(defun deb-packaging--lintian-source-desc ()
  "Description for lintian source action."
  (let* ((s deb-packaging--session)
         (arts (when s (nth 4 s)))
         (changes (when arts (alist-get 'source-changes arts))))
    (if changes "Source" "Source (no .changes)")))

(defun deb-packaging--lintian-binary-desc ()
  "Description for lintian binary action."
  (let* ((s deb-packaging--session)
         (arts (when s (nth 4 s)))
         (changes (when arts (car (alist-get 'binary-changes arts)))))
    (if changes "Binary" "Binary (no .changes)")))

(defun deb-packaging--distro-desc ()
  "Description showing current distro."
  (format "Distro [%s]" deb-packaging-target-distro))

(defun deb-packaging--mode-desc ()
  "Description showing current mode."
  (format "Mode [%s]" deb-packaging-global-mode))

(defun deb-packaging--sbuild-variant-desc ()
  "Description showing current sbuild variant."
  (format "sbuild variant [%s]" deb-packaging-sbuild-variant))

(defun deb-packaging--test-runner-desc ()
  "Description showing current test runner."
  (format "Test runner [%s]" deb-packaging-test-runner))

(defun deb-packaging--ppa-desc ()
  "Description showing current PPA."
  (format "PPA [%s]" (or deb-packaging--current-ppa "none")))

;;; Interactive Commands for Transient

(defconst deb-packaging-ubuntu-distros
  '("oracular" "noble" "jammy" "focal" "questing" "plucky" "resolute"
    "mantic" "lunar" "kinetic" "impish" "hirsute" "groovy" "bionic" "xenial")
  "Known Ubuntu distribution codenames.")

(defun deb-packaging-set-distro ()
  "Set target distribution interactively."
  (interactive)
  (let* ((session deb-packaging--session)
         (changelog-distro (when session (nth 2 session)))
         (candidates (if (and changelog-distro
                              (not (member changelog-distro deb-packaging-ubuntu-distros)))
                         (cons changelog-distro deb-packaging-ubuntu-distros)
                       deb-packaging-ubuntu-distros)))
    (setq deb-packaging-target-distro
          (completing-read "Distro: " candidates nil nil deb-packaging-target-distro))
    (message "Target distro: %s" deb-packaging-target-distro)))

(defun deb-packaging-refresh ()
  "Refresh session state."
  (interactive)
  (deb-packaging--refresh-session)
  (message "Session refreshed"))

;;; Transient Suffixes with Dynamic Descriptions

(transient-define-suffix deb-packaging-suffix-source-build ()
  "Build source package."
  :description #'deb-packaging--source-desc
  (interactive)
  (deb-packaging-source-build))

(transient-define-suffix deb-packaging-suffix-sbuild ()
  "Build binary package with sbuild."
  :description #'deb-packaging--sbuild-desc
  (interactive)
  (deb-packaging-sbuild))

(transient-define-suffix deb-packaging-suffix-autopkgtest ()
  "Run autopkgtest."
  :description #'deb-packaging--test-desc
  (interactive)
  (deb-packaging-autopkgtest))

(transient-define-suffix deb-packaging-suffix-lintian-source ()
  "Run lintian on source changes."
  :description #'deb-packaging--lintian-source-desc
  (interactive)
  (deb-packaging-lintian-source))

(transient-define-suffix deb-packaging-suffix-lintian-binary ()
  "Run lintian on binary changes."
  :description #'deb-packaging--lintian-binary-desc
  (interactive)
  (deb-packaging-lintian-binary))

(transient-define-suffix deb-packaging-suffix-set-distro ()
  "Set target distribution."
  :description #'deb-packaging--distro-desc
  (interactive)
  (deb-packaging-set-distro))

(transient-define-suffix deb-packaging-suffix-cycle-mode ()
  "Cycle global mode."
  :description #'deb-packaging--mode-desc
  :transient t
  (interactive)
  (deb-packaging-cycle-mode))

(transient-define-suffix deb-packaging-suffix-cycle-sbuild-variant ()
  "Cycle sbuild variant."
  :description #'deb-packaging--sbuild-variant-desc
  :transient t
  (interactive)
  (deb-packaging-cycle-sbuild-variant))

(transient-define-suffix deb-packaging-suffix-cycle-test-runner ()
  "Cycle test runner."
  :description #'deb-packaging--test-runner-desc
  :transient t
  (interactive)
  (deb-packaging-cycle-test-runner))

(transient-define-suffix deb-packaging-suffix-ppa-tests ()
  "Show autopkgtest results for the current PPA."
  :description "PPA tests"
  (interactive)
  (deb-packaging-ppa-tests))

(transient-define-suffix deb-packaging-suffix-set-ppa ()
  "Set the current Launchpad PPA."
  :description #'deb-packaging--ppa-desc
  :transient t
  (interactive)
  (deb-packaging-set-ppa))

;;; Main Transient

(transient-define-prefix deb-packaging-dispatch ()
  "Context-aware Debian packaging interface."
  :refresh-suffixes t
  [:description deb-packaging--header-with-artifacts]
  ["Actions"
   ("s" deb-packaging-suffix-source-build)
   ("b" deb-packaging-suffix-sbuild)
   ("t" deb-packaging-suffix-autopkgtest)
   ("p" deb-packaging-suffix-ppa-tests)]
  ["Lintian"
   ("l" deb-packaging-suffix-lintian-source)
   ("L" deb-packaging-suffix-lintian-binary)]
  ["Settings"
   ("d" deb-packaging-suffix-set-distro)
   ("m" deb-packaging-suffix-cycle-mode)
   ("v" deb-packaging-suffix-cycle-sbuild-variant)
   ("r" deb-packaging-suffix-cycle-test-runner)
   ("P" deb-packaging-suffix-set-ppa)]
  ["Other"
   ("i" "Infrastructure..." deb-packaging-infra-dispatch)
   ("g" "Refresh" deb-packaging-refresh :transient t)
   ("c" "Clean" deb-packaging-clean)
   ("q" "Quit" transient-quit-one)]
  (interactive)
  (deb-packaging--refresh-session)
  (transient-setup 'deb-packaging-dispatch))

;;; Keybinding

;;;###autoload
(defun deb-packaging-setup-keys ()
  "Set up default keybindings for deb-packaging."
  (global-set-key (kbd "C-c d") #'deb-packaging-dispatch))

;;;###autoload
(autoload 'deb-packaging-dispatch "deb-packaging" nil t)

(provide 'deb-packaging)
;;; deb-packaging.el ends here
