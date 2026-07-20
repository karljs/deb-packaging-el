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
;; `deb-packaging-dispatch' (transient hub). Default key: C-c C-d.

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

;;;###autoload
(defun deb-packaging-set-distro (distro)
  "Set the global target distro to DISTRO.
Propagated to all per-tool transients and the status buffer."
  (interactive
   (list (completing-read
          "Target distro: "
          (deb-packaging-config--distro-choices)
          nil t (deb-packaging-config--effective-distro))))
  (deb-packaging-config--set-distro distro)
  (message "Target distro set to %s" distro))

(transient-define-prefix deb-packaging-dispatch ()
  "Debian packaging commands.
Set the target distro with `d'; other transients inherit it."
  [:description deb-packaging--dispatch-header]
  ["Config"
   ("d" "Set target distro..." deb-packaging-set-distro)]
  ["Build"
   ("s" "Source build..."  deb-packaging-commands-source-build-transient)
   ("b" "Binary build..."  deb-packaging-binary-build-transient)]
  ["Check & Test"
   ("l" "Lint..."           deb-packaging-lint-transient)
   ("t" "Autopkgtest..."   deb-packaging-test-transient)]
  ["Develop & Propagate"
   ("e" "Dev shell..."       deb-packaging-dev-transient)
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

;;; Keybinding

;;;###autoload
(defun deb-packaging-setup-keys ()
  "Bind `C-c C-d' to `deb-packaging-status'.
Skips and warns if `C-c C-d' is already bound to another command."
  (let* ((key (kbd "C-c C-d"))
         (cmd (key-binding key t)))
    (if (or (null cmd) (eq cmd #'deb-packaging-status))
        (global-set-key key #'deb-packaging-status)
      (message "deb-packaging: C-c C-d already bound to %s; bind deb-packaging-status manually"
               cmd))))

;;;###autoload
(autoload 'deb-packaging-status "deb-packaging-status" nil t)

(provide 'deb-packaging)
;;; deb-packaging.el ends here
