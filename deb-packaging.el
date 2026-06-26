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
(require 'deb-packaging-status)

;;; Top-level dispatch hub

(transient-define-prefix deb-packaging-dispatch ()
  "Debian packaging commands."
  ["Build"
   ("s" "Source build..."  deb-packaging-source-build-transient)
   ("b" "Binary build..."  deb-packaging-binary-build-transient)]
  ["Check & Test"
   ("l" "Lintian..."       deb-packaging-lint-transient)
   ("t" "Autopkgtest..."   deb-packaging-test-transient)]
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
