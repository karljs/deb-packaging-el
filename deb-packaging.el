;;; deb-packaging.el --- Context-aware Debian packaging interface -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/example/deb-packaging
;; Package-Requires: ((emacs "28.1") (transient "0.4.0") (magit-section "3.3"))

;;; Commentary:

;; A context-aware interface for Debian/Ubuntu packaging.
;; Detects package context and provides per-tool transients for each action.
;;
;; Primary entry point: `deb-packaging-status' — a Magit-style landing page
;; (see deb-packaging-status.el) that shows the package moving through its
;; phases and opens the appropriate transient on RET.
;;
;; Secondary entry point: `deb-packaging-dispatch' — a top-level hub that
;; lists all per-tool transients, reachable from the status buffer via `?'.
;;
;; Per-tool transients (defined in deb-packaging-transients.el):
;;   deb-packaging-source-build-transient
;;   deb-packaging-binary-build-transient
;;   deb-packaging-lint-transient
;;   deb-packaging-test-transient
;;   deb-packaging-upload-transient
;;   deb-packaging-clean-transient
;;   deb-packaging-reset-transient
;;
;; Default keybinding: C-c d  (opens the status buffer)

;;; Code:

(require 'transient)
(require 'deb-packaging-detect)
(require 'deb-packaging-config)
(require 'deb-packaging-commands)
(require 'deb-packaging-transients)
(require 'deb-packaging-infra)
(require 'deb-packaging-dev)
(require 'deb-packaging-status)

;;; Top-level dispatch hub

(defun deb-packaging--dispatch-header ()
  "Header for the dispatch transient, surfacing the global target distro."
  (format "Debian Packaging\nTarget distro: %s"
          (deb-packaging--effective-distro)))

;;;###autoload
(defun deb-packaging-set-distro (distro)
  "Set the global target distro for deb-packaging to DISTRO.
Interactively, prompt with completion against known distros, defaulting
to the current effective distro.  The chosen value is propagated to every
per-tool transient (build, test, upload) and to the status buffer, since
they all read `deb-packaging-target-distro' as their default."
  (interactive
   (list (completing-read
          "Target distro: "
          (deb-packaging--distro-choices)
          nil t (deb-packaging--effective-distro))))
  (deb-packaging--set-distro distro)
  (message "Target distro set to %s" distro))

(transient-define-prefix deb-packaging-dispatch ()
  "Debian packaging commands.
The target distro is the one piece of genuinely global state; set it
here with `d' and every per-tool transient inherits it."
  [:description deb-packaging--dispatch-header]
  ["Config"
   ("d" "Set target distro..." deb-packaging-set-distro)]
  ["Build"
   ("s" "Source build..."  deb-packaging-source-build-transient)
   ("b" "Binary build..."  deb-packaging-binary-build-transient)]
  ["Check & Test"
   ("l" "Lint..."           deb-packaging-lint-transient)
   ("t" "Autopkgtest..."   deb-packaging-test-transient)]
   ["Patch / Develop"
    ("e" "Edit upstream (dev shell, C-u=reprovision)" deb-packaging-dev-shell)
    ("E" "Start eglot in dev shell" deb-packaging-dev-eglot)
    ("k" "Destroy dev container" deb-packaging-dev-destroy)]
  ["Publish"
   ("p" "PPA upload..."   deb-packaging-upload-transient)]
  ["Cleanup"
   ("c" "Clean artifacts..." deb-packaging-clean-transient)
   ("r" "Reset source tree..." deb-packaging-reset-transient)]
  ["Other"
   ("i" "Infrastructure..."  deb-packaging-infra-dispatch)
   ("q" "Quit"             transient-quit-one)])

;;; Keybinding

;;;###autoload
(defun deb-packaging-setup-keys ()
  "Set up default keybindings for deb-packaging.
Binds C-c d to the status landing page, the primary entry point."
  (global-set-key (kbd "C-c d") #'deb-packaging-status))

;;;###autoload
(autoload 'deb-packaging-status "deb-packaging-status" nil t)

(provide 'deb-packaging)
;;; deb-packaging.el ends here
