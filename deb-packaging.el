;;; deb-packaging.el --- Packaging interface -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Context-aware interface for Debian/Ubuntu packaging.
;; Entry points: `deb-packaging-status' (status buffer) and
;; `deb-packaging-dispatch' (transient hub).  Both prompt for a package
;; directory when invoked outside one.

;;; Code:

(require 'transient)
(require 'deb-packaging-detect)
(require 'deb-packaging-config)
(require 'deb-packaging-repos)
(require 'deb-packaging-ppa)
(require 'deb-packaging-commands)
(require 'deb-packaging-ppa-tests)
(require 'deb-packaging-transients)
(require 'deb-packaging-infra)
(require 'deb-packaging-dev)
(require 'deb-packaging-propagate)
(require 'deb-packaging-pq)
(require 'deb-packaging-status)
(require 'deb-packaging-clone)

;;; Top-level dispatch hub

(defun deb-packaging--dispatch-header ()
  "Header for the dispatch transient, showing the target distro."
  (format "Debian Packaging\nTarget distro: %s"
          (deb-packaging-config--effective-distro)))

(transient-define-prefix deb-packaging-dispatch-transient ()
  "Debian packaging commands.
The target distro comes from the changelog; other transients inherit it."
  :environment #'deb-packaging-transients--env
  [:description deb-packaging--dispatch-header]
  ["Build"
   ("s" "Source build..."  deb-packaging-commands-source-build-transient)
   ("b" "Binary build..."  deb-packaging-binary-build-transient)]
  ["Check & Test"
   ("l" "Lint..."           deb-packaging-lint-transient)
   ("t" "Autopkgtest..."   deb-packaging-test-transient)]
  ["Develop & Propagate"
   ("e" "Dev shell..."       deb-packaging-dev-transient)
   ("g" "Regenerate templated files" deb-packaging-commands-regenerate)
   ("u" "Patch queue (gbp pq)..." deb-packaging-pq-transient)
   ("P" "Propagate..."       deb-packaging-propagate-transient)]
  ["Publish"
   ("p" "PPA upload..."   deb-packaging-upload-transient)]
  ["Cleanup"
   ("c" "Clean artifacts..." deb-packaging-commands-clean-transient)
   ("r" "Reset source tree..." deb-packaging-commands-reset-transient)]
   ["Other"
    ("i" "Infrastructure..."  deb-packaging-infra-dispatch)
    ("q" "Quit"             transient-quit-one)])

;;;###autoload
(defun deb-packaging-dispatch ()
  "Open the packaging dispatch transient.
Outside a package tree, go through `deb-packaging-status' first: it
prompts for a package and lands in its status buffer, which becomes the
context the transient's commands run in."
  (interactive)
  (unless (condition-case nil
              (deb-packaging-detect--find-package-dir nil t)
            (user-error nil))
    (deb-packaging-status))
  (deb-packaging-dispatch-transient))

(provide 'deb-packaging)
;;; deb-packaging.el ends here
