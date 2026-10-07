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

(defgroup deb-packaging nil
  "Debian and Ubuntu packaging workflows."
  :group 'tools)

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
(require 'deb-packaging-backport)
(require 'deb-packaging-pq)
(require 'deb-packaging-update)
(require 'deb-packaging-develop)
(require 'deb-packaging-status)
(require 'deb-packaging-clone)

;;; Top-level dispatch hub

(defun deb-packaging--dispatch-header ()
  "Header for the dispatch transient, showing workspace context."
  (if-let ((ctx (deb-packaging-status--collect-context)))
      (format "Debian Packaging\n%s %s | %s | %s | %s"
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
    "Debian Packaging"))

(transient-define-prefix deb-packaging-dispatch-transient ()
  "Debian packaging commands.
The target distro comes from the changelog; other transients inherit it."
  :environment #'deb-packaging-transients--env
  [:description deb-packaging--dispatch-header
   ["Develop"
    ("f" "Fix branch..."       deb-packaging-branch-transient)
    ("a" "Patches..."          deb-packaging-patches-transient)
    ("C" "Changelog..."        deb-packaging-changelog-transient)
    ("N" "Upstream version..." deb-packaging-update-transient)
    ("e" "Dev shell..."        deb-packaging-dev-transient)]
   ["Local"
    ("s" "Source package..."   deb-packaging-commands-source-build-transient)
    ("b" "Binaries..."         deb-packaging-binary-build-transient)
    ("l" "Lint..."             deb-packaging-lint-transient)
    ("t" "Autopkgtest..."      deb-packaging-test-transient)]
   ["Launchpad"
    ("U" "Upload..."           deb-packaging-upload-transient)
    ("B" "PPA builds"          deb-packaging-infra-show-ppa-package)
    ("T" "PPA tests"           deb-packaging-ppa-tests-show)]
   ["Submit"
    ("M" "Merge proposal..."   deb-packaging-submit-transient)
    ("P" "Forward to Debian or upstream..." deb-packaging-propagate-transient)]]
  [["Maintain"
    ("c" "Clean artifacts..."        deb-packaging-commands-clean-transient)
    ("K" "Kill build-output buffers" deb-packaging-commands-kill-output-buffers)
    ("r" "Reset source tree..."      deb-packaging-commands-reset-transient)
    ("R" "Regenerate debian/control" deb-packaging-commands-regenerate)]
   ["Other"
    ("G" "Get another package..."    deb-packaging-get-transient)
    ("A" "Target architecture"       deb-packaging-commands-set-architecture)
    ("i" "Infrastructure (chroots, images, PPAs)..." deb-packaging-infra-dispatch)
    ("q" "Quit"                      transient-quit-one)]])

;;;###autoload
(defun deb-packaging-dispatch ()
  "Open the packaging dispatch transient.
Outside a package tree, offer to clone or open a package instead."
  (interactive)
  (if (condition-case nil
          (deb-packaging-detect--find-package-dir nil t)
        (user-error nil))
      (deb-packaging-dispatch-transient)
    (call-interactively #'deb-packaging-get-transient)))

(defconst deb-packaging--optional-tools
  '("autopkgtest" "dput" "gbp" "git-ubuntu" "lintian" "lxc"
    "mk-sbuild" "ppa" "sbuild" "ubuntu-lint")
  "Optional external tools used by some workflows.")

;;;###autoload
(defun deb-packaging-doctor ()
  "Show optional tool availability and open Customize for deb-packaging."
  (interactive)
  (let ((buf (get-buffer-create "*deb-packaging setup*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Optional tools\n\n")
        (dolist (tool deb-packaging--optional-tools)
          (insert (format "%-16s %s\n"
                          tool (if (executable-find tool) "available" "missing"))))
        (special-mode)))
    (deb-packaging-display-buffer buf 'report))
  (customize-group 'deb-packaging))

(provide 'deb-packaging)
;;; deb-packaging.el ends here
