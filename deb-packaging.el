;;; deb-packaging.el --- Packaging interface -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/example/deb-packaging
;; Package-Requires: ((emacs "28.1") (transient "0.4.0") (magit-section "3.3"))

;;; Commentary:

;; A context-aware interface for Debian/Ubuntu packaging.
;; Detects package context and provides per-tool transients.
;;
;; Primary entry point: `deb-packaging-status' (Magit-style status buffer).
;; Secondary entry point: `deb-packaging-dispatch' (hub from `?').
;; Per-tool transients are defined in deb-packaging-transients.el.
;;
;; Default keybinding: C-c d

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
  "Header for the dispatch transient, showing the target distro."
  (format "Debian Packaging\nTarget distro: %s"
          (deb-packaging--effective-distro)))

;;;###autoload
(defun deb-packaging-set-distro (distro)
  "Set the global target distro to DISTRO.
Interactively, prompt with completion against known distros.  Propagated
to all per-tool transients and the status buffer."
  (interactive
   (list (completing-read
          "Target distro: "
          (deb-packaging--distro-choices)
          nil t (deb-packaging--effective-distro))))
  (deb-packaging--set-distro distro)
  (message "Target distro set to %s" distro))

(transient-define-prefix deb-packaging-dispatch ()
  "Debian packaging commands.
Set the target distro with `d'; other transients inherit it."
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
    ("d" "Dev shell..." deb-packaging-dev-transient)]
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
Binds C-c d to `deb-packaging-status'."
  (global-set-key (kbd "C-c d") #'deb-packaging-status))

;;;###autoload
(autoload 'deb-packaging-status "deb-packaging-status" nil t)

(provide 'deb-packaging)
;;; deb-packaging.el ends here
