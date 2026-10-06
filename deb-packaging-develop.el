;;; deb-packaging-develop.el --- Develop a change: branch, patches, changelog, submit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; The Ubuntu fix flow, in the order the status buffer shows it:
;; get a package, start a fix branch, add or edit patches, write the
;; changelog, then (after building and testing) submit a merge proposal
;; or forward the fix to Debian/upstream.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magit)
(require 'transient)
(require 'deb-packaging-detect)
(require 'deb-packaging-commands)
(require 'deb-packaging-transients)
(require 'deb-packaging-pq)
(require 'deb-packaging-backport)

(declare-function deb-packaging-status "deb-packaging-status")
(declare-function deb-packaging-clone-git-ubuntu "deb-packaging-clone")
(declare-function deb-packaging-clone-gbp "deb-packaging-clone")

;;; Facts about the change in progress

(defun deb-packaging-develop--git-ubuntu-p ()
  "Return non-nil when the current repository is a git-ubuntu clone."
  (when-let ((url (ignore-errors (magit-get "remote" "pkg" "url"))))
    (string-match-p "/ubuntu/\\+source/" url)))

(defun deb-packaging-develop--maintainer-stale-p (pkg-dir version)
  "Return non-nil when an Ubuntu VERSION still has a Debian Maintainer."
  (and version (string-match-p "ubuntu" version)
       (let ((maintainer (deb-packaging-detect--control-field "Maintainer" pkg-dir)))
         (and maintainer (not (string-match-p "ubuntu" (downcase maintainer)))))))

(defun deb-packaging-develop--facts (ctx)
  "Return a plist describing the change in progress for package CTX.
Keys: :git-ubuntu :branch :upstream :ahead :changelog-changed :bug
:unreleased :maintainer-stale :patches :pq :watch."
  (let* ((pkg-dir (plist-get ctx :pkg-dir))
         (default-directory pkg-dir)
         (git (plist-get ctx :repo-dir))
         (branch (plist-get ctx :branch))
         (upstream (and git branch (ignore-errors (magit-get-upstream-branch branch))))
         (ahead (and upstream
                     (string-to-number
                      (or (magit-git-string "rev-list" "--count"
                                            (concat upstream "..HEAD"))
                          "0")))))
    (list :git-ubuntu (and git (deb-packaging-develop--git-ubuntu-p))
          :branch branch
          :upstream upstream
          :ahead ahead
          :changelog-changed (and upstream
                                  (not (magit-git-success "diff" "--quiet" upstream
                                                          "--" "debian/changelog")))
          :bug (deb-packaging-detect--launchpad-bug pkg-dir)
          :unreleased (equal (plist-get ctx :changelog-distro) "UNRELEASED")
          :maintainer-stale (deb-packaging-develop--maintainer-stale-p
                             pkg-dir (plist-get ctx :version))
          :patches (deb-packaging-detect--list-patches)
          :pq (and git (equal (plist-get ctx :source-format) "3.0 (quilt)")
                   (ignore-errors (deb-packaging-pq--state)))
          :watch (file-readable-p (expand-file-name "debian/watch" pkg-dir)))))

(defun deb-packaging-develop--current-facts ()
  "Return `deb-packaging-develop--facts' for the package at point, or nil."
  (when-let ((ctx (ignore-errors (deb-packaging-commands--package-context))))
    (deb-packaging-develop--facts ctx)))

(defun deb-packaging-develop--pkg-dir ()
  "Return the host package directory or signal a `user-error'."
  (or (deb-packaging-detect--find-package-dir nil t)
      (user-error "Not in a Debian package directory")))

(defun deb-packaging-develop--refresh ()
  "Refresh status buffers after a change to the tree."
  (deb-packaging-commands--notify-status-refresh))

;;; Fix branch

(defun deb-packaging-develop--read-bug (&optional prompt)
  "Read a Launchpad bug number with PROMPT, defaulting to the current one.
Return the number as a string, or nil when left blank."
  (let* ((default (deb-packaging-detect--launchpad-bug))
         (input (string-trim
                 (read-string (format "%s%s: " (or prompt "Launchpad bug (blank for none)")
                                      (if default (format " [%s]" default) ""))
                              nil nil default))))
    (cond ((string-empty-p input) nil)
          ((string-match "\\([0-9]+\\)\\'" input) (match-string 1 input))
          (t (user-error "Not a Launchpad bug number: %s" input)))))

(defun deb-packaging-develop-new-branch (name)
  "Create and switch to fix branch NAME.
Interactively the name defaults to lpNNN for the Launchpad bug asked for.
The new branch tracks what the current branch tracks (for a git-ubuntu
clone, pkg/ubuntu/devel), so the status buffer can count its commits."
  (interactive
   (let* ((bug (deb-packaging-develop--read-bug))
          (default (and bug (format "lp%s" bug))))
     (list (read-string (if default (format "Branch name [%s]: " default)
                          "Branch name: ")
                        nil nil default))))
  (let ((default-directory (deb-packaging-develop--pkg-dir)))
    (when (string-empty-p (or name ""))
      (user-error "No branch name given"))
    (let* ((current (magit-get-current-branch))
           (upstream (or (and current (magit-get-upstream-branch current)) current)))
      (magit-call-git "checkout" "-b" name)
      (when upstream
        (magit-call-git "branch" (concat "--set-upstream-to=" upstream)))
      (deb-packaging-develop--refresh)
      (message "On %s%s" name (if upstream (format ", tracking %s" upstream) "")))))

(defun deb-packaging-develop--lp-user ()
  "Return the configured Launchpad user name, or nil."
  (or (ignore-errors (magit-get "gitubuntu.lpuser"))
      (let ((user (getenv "LP_USER"))) (and user (not (string-empty-p user)) user))))

(defun deb-packaging-develop-add-lp-remote (user)
  "Add Launchpad USER's repository as a remote with `git ubuntu remote add'."
  (interactive
   (list (let ((default (deb-packaging-develop--lp-user)))
           (read-string (if default (format "Launchpad user [%s]: " default)
                          "Launchpad user: ")
                        nil nil default))))
  (when (string-empty-p (or user ""))
    (user-error "No Launchpad user given"))
  (let ((pkg-dir (deb-packaging-develop--pkg-dir)))
    (deb-packaging-commands--run-command
     "lp-remote" (list "git" "ubuntu" "remote" "add" user) pkg-dir 'lp-remote)))

(defun deb-packaging-develop--not-git-p ()
  "Return non-nil outside a Git repository."
  (not (ignore-errors (magit-toplevel))))

(defun deb-packaging-develop--not-git-ubuntu-p ()
  "Return non-nil outside a git-ubuntu clone."
  (not (deb-packaging-develop--git-ubuntu-p)))

(defun deb-packaging-develop--branch-header ()
  "Return the fix-branch menu header."
  (let ((facts (deb-packaging-develop--current-facts)))
    (concat (deb-packaging-transients--context-header)
            "\n\n"
            (cond ((null (plist-get facts :branch)) "Not on a branch")
                  ((plist-get facts :upstream)
                   (format "On %s: %d commit%s ahead of %s"
                           (plist-get facts :branch) (plist-get facts :ahead)
                           (if (eq (plist-get facts :ahead) 1) "" "s")
                           (plist-get facts :upstream)))
                  (t (format "On %s (no upstream branch)" (plist-get facts :branch)))))))

;;;###autoload(autoload 'deb-packaging-branch-transient "deb-packaging-develop" nil t)
(transient-define-prefix deb-packaging-branch-transient ()
  "Start and manage the fix branch."
  :environment #'deb-packaging-transients--env
  [:description deb-packaging-develop--branch-header
   ["Fix branch"
    ("n" "Start a fix branch..." deb-packaging-develop-new-branch
     :inapt-if deb-packaging-develop--not-git-p)
    ("m" "Magit status (commit, diff, log)" magit-status
     :inapt-if deb-packaging-develop--not-git-p)]
   ["Launchpad"
    ("a" "Add your Launchpad remote..." deb-packaging-develop-add-lp-remote
     :inapt-if deb-packaging-develop--not-git-ubuntu-p)
    ("q" "Quit" transient-quit-one)]])

;;; Changelog

(defun deb-packaging-develop--dch (pkg-dir &rest args)
  "Run dch with ARGS in PKG-DIR; signal a `user-error' with its output on failure."
  (unless (executable-find "dch")
    (user-error "dch is not installed (devscripts)"))
  (let ((default-directory pkg-dir)
        (process-environment
         (append (and (not (getenv "DEBFULLNAME")) (> (length user-full-name) 0)
                      (list (concat "DEBFULLNAME=" user-full-name)))
                 (and (not (getenv "DEBEMAIL")) user-mail-address
                      (> (length user-mail-address) 0)
                      (list (concat "DEBEMAIL=" user-mail-address)))
                 process-environment)))
    (with-temp-buffer
      ;; stdin from /dev/null: dch must never wait on an editor or prompt.
      (unless (zerop (apply #'process-file "dch" nil t nil args))
        (user-error "dch failed: %s" (string-trim (buffer-string)))))))

(defun deb-packaging-develop-changelog-add (text bug)
  "Add TEXT (closing Launchpad BUG) to the changelog.
Appends to the top entry while it is UNRELEASED; otherwise opens a new
entry with the next Ubuntu version."
  (interactive
   (list (read-string "Change: ")
         (deb-packaging-develop--read-bug "Closes Launchpad bug (blank for none)")))
  (when (string-empty-p (string-trim text))
    (user-error "No change text given"))
  (let* ((pkg-dir (deb-packaging-develop--pkg-dir))
         (text (if (and bug (not (string-match-p "LP: *#" text)))
                   (format "%s (LP: #%s)" text bug)
                 text))
         (unreleased (equal (nth 3 (deb-packaging-detect--parse-changelog pkg-dir))
                            "UNRELEASED")))
    (deb-packaging-develop--dch pkg-dir (if unreleased "--append" "--increment") text)
    (deb-packaging-develop--refresh)
    (message "%s %s" (if unreleased "Added to" "New entry")
             (nth 1 (deb-packaging-detect--parse-changelog pkg-dir)))))

(defun deb-packaging-develop-changelog-visit ()
  "Visit debian/changelog at the top entry."
  (interactive)
  (find-file (expand-file-name "debian/changelog" (deb-packaging-develop--pkg-dir)))
  (goto-char (point-min)))

(defun deb-packaging-develop-changelog-release ()
  "Replace UNRELEASED in the top entry with the series the package builds for.
That series is the one the status header shows, not dch's own guess."
  (interactive)
  (let* ((pkg-dir (deb-packaging-develop--pkg-dir))
         (series (nth 2 (deb-packaging-detect--parse-changelog pkg-dir))))
    (when (equal series "UNRELEASED")
      (user-error "Cannot tell which series this targets; edit the changelog"))
    (deb-packaging-develop--dch pkg-dir "--release" "--distribution" series "")
    (deb-packaging-develop--refresh)
    (message "Changelog now targets %s"
             (nth 3 (deb-packaging-detect--parse-changelog pkg-dir)))))

(defun deb-packaging-develop-update-maintainer ()
  "Set the Ubuntu Maintainer field, keeping the Debian one as original."
  (interactive)
  (let ((default-directory (deb-packaging-develop--pkg-dir)))
    (unless (executable-find "update-maintainer")
      (user-error "update-maintainer is not installed (ubuntu-dev-tools)"))
    (with-temp-buffer
      (unless (zerop (process-file "update-maintainer" nil t nil))
        (user-error "update-maintainer failed: %s" (string-trim (buffer-string)))))
    (deb-packaging-develop--refresh)
    (message "Maintainer updated for Ubuntu")))

(defun deb-packaging-develop--released-p ()
  "Return non-nil when the top changelog entry is not UNRELEASED."
  (not (equal (nth 3 (deb-packaging-detect--parse-changelog)) "UNRELEASED")))

(defun deb-packaging-develop--maintainer-ok-p ()
  "Return non-nil when the Maintainer field needs no Ubuntu update."
  (not (deb-packaging-develop--maintainer-stale-p
        (deb-packaging-detect--find-package-dir)
        (nth 1 (deb-packaging-detect--parse-changelog)))))

(defun deb-packaging-develop--changelog-header ()
  "Return the changelog menu header."
  (let ((info (deb-packaging-detect--parse-changelog)))
    (format "%s\n\nTop entry: %s %s"
            (deb-packaging-transients--context-header)
            (nth 1 info) (nth 3 info))))

;;;###autoload(autoload 'deb-packaging-changelog-transient "deb-packaging-develop" nil t)
(transient-define-prefix deb-packaging-changelog-transient ()
  "Write the changelog entry for the change."
  :environment #'deb-packaging-transients--env
  [:description deb-packaging-develop--changelog-header
   ["Changelog"
    ("a" "Add a change..." deb-packaging-develop-changelog-add)
    ("e" "Edit debian/changelog" deb-packaging-develop-changelog-visit)
    ("r" deb-packaging-develop-changelog-release
     :description (lambda ()
                    (format "Finalize for %s (dch -r)"
                            (nth 2 (deb-packaging-detect--parse-changelog))))
     :inapt-if deb-packaging-develop--released-p)]
   ["Ubuntu delta"
    ("m" "Update Maintainer field" deb-packaging-develop-update-maintainer
     :inapt-if deb-packaging-develop--maintainer-ok-p)
    ("q" "Quit" transient-quit-one)]])

;;; Patches

(defun deb-packaging-develop--pq-state ()
  "Return the gbp pq state, or nil when gbp pq does not apply here."
  (and (not (deb-packaging-develop--not-git-p))
       (equal (deb-packaging-detect--source-format) "3.0 (quilt)")
       (deb-packaging-pq--state)))

(defun deb-packaging-develop--cannot-start-pq-p ()
  "Return non-nil when starting a gbp pq edit is not possible."
  (let ((state (deb-packaging-develop--pq-state)))
    (or (null state) (plist-get state :on-pq-p))))

(defun deb-packaging-develop--not-on-pq-p ()
  "Return non-nil unless on a patch-queue branch."
  (not (plist-get (deb-packaging-develop--pq-state) :on-pq-p)))

(defun deb-packaging-develop--no-pq-p ()
  "Return non-nil when no patch-queue branch exists."
  (not (plist-get (deb-packaging-develop--pq-state) :exists-p)))

(defun deb-packaging-develop--no-patches-p ()
  "Return non-nil when debian/patches/series lists nothing."
  (null (deb-packaging-detect--list-patches)))

(defun deb-packaging-develop-edit-patches ()
  "Edit the quilt patches as git commits on a gbp patch-queue branch."
  (interactive)
  (if (plist-get (deb-packaging-develop--pq-state) :exists-p)
      (deb-packaging-pq-switch)
    (deb-packaging-pq-import)))

(defun deb-packaging-develop-check-patches ()
  "Check that every patch in debian/patches applies cleanly."
  (interactive)
  (let ((default-directory (deb-packaging-develop--pkg-dir)))
    (deb-packaging-commands--compile
     (deb-packaging-backport--verify-command nil))))

(defun deb-packaging-develop--patches-header ()
  "Return the patches menu header."
  (let ((patches (deb-packaging-detect--list-patches))
        (pq (deb-packaging-develop--pq-state)))
    (format "%s\n\n%d patch%s in debian/patches%s"
            (deb-packaging-transients--context-header)
            (length patches) (if (= (length patches) 1) "" "es")
            (cond ((plist-get pq :on-pq-p)
                   (format "; editing as commits on %s" (plist-get pq :branch)))
                  ((plist-get pq :exists-p)
                   (format "; %s exists" (plist-get pq :pq-branch)))
                  (t "")))))

;;;###autoload(autoload 'deb-packaging-patches-transient "deb-packaging-develop" nil t)
(transient-define-prefix deb-packaging-patches-transient ()
  "Add, edit, and check the package's quilt patches."
  :environment #'deb-packaging-transients--env
  [:description deb-packaging-develop--patches-header
   ["Add"
    ("u" "Patch from upstream (commit/PR URL or file)..." deb-packaging-backport-patch)]
   ["Check"
    ("c" "Check every patch applies" deb-packaging-develop-check-patches
     :inapt-if deb-packaging-develop--no-patches-p)]]
  [["Edit as git commits (gbp pq)"
    ("e" "Start editing" deb-packaging-develop-edit-patches
     :inapt-if deb-packaging-develop--cannot-start-pq-p)
    ("x" "Finish: write commits back to debian/patches" deb-packaging-pq-export
     :inapt-if deb-packaging-develop--not-on-pq-p)
    ("r" "Rebase onto the packaging branch" deb-packaging-pq-rebase
     :inapt-if deb-packaging-develop--not-on-pq-p)
    ("d" "Discard the patch queue" deb-packaging-pq-drop
     :inapt-if deb-packaging-develop--no-pq-p)
    ("q" "Quit" transient-quit-one)]])

;;; Submit

(defun deb-packaging-develop--submit-unmet ()
  "Return why a merge proposal cannot be submitted now, or nil."
  (let ((facts (deb-packaging-develop--current-facts)))
    (cond ((not (executable-find "git-ubuntu")) "git-ubuntu not installed")
          ((not (plist-get facts :git-ubuntu)) "needs a git-ubuntu clone")
          ((not (and (plist-get facts :ahead) (> (plist-get facts :ahead) 0)))
           "needs commits on a fix branch"))))

(defun deb-packaging-develop-submit (&optional args)
  "Push the fix branch and open a Launchpad merge proposal with ARGS."
  (interactive (list (transient-args 'deb-packaging-submit-transient)))
  (when-let ((why (deb-packaging-develop--submit-unmet)))
    (user-error "Cannot submit: %s" why))
  (let ((pkg-dir (deb-packaging-develop--pkg-dir)))
    (deb-packaging-commands--run-command
     "submit" (append '("git" "ubuntu" "submit") args) pkg-dir 'submit)))

;;;###autoload(autoload 'deb-packaging-submit-transient "deb-packaging-develop" nil t)
(transient-define-prefix deb-packaging-submit-transient ()
  "Submit the fix as a Launchpad merge proposal (git ubuntu submit)."
  :environment #'deb-packaging-transients--env
  [:description (lambda () (deb-packaging-transients--titled
                              #'deb-packaging-develop--branch-header "Options"))
   ("-r" "Reviewer (default ubuntu-sponsors)" "--reviewer=")
   ("-t" "Target branch (default: nearest remote)" "--target-branch=")
   ("-n" "Branch already pushed" "--no-push")]
  [["Submit"
    ("s" deb-packaging-develop-submit
     :description (lambda () (deb-packaging-transients--label-why
                              "Push and open merge proposal"
                              (deb-packaging-develop--submit-unmet)))
     :inapt-if deb-packaging-develop--submit-unmet)]
   ["Setup"
    ("a" "Add your Launchpad remote..." deb-packaging-develop-add-lp-remote
     :inapt-if deb-packaging-develop--not-git-ubuntu-p)
    ("q" "Quit" transient-quit-one)]])

;;; Getting a package

(defun deb-packaging-develop-open-checkout (dir)
  "Open the status buffer for the package checkout in DIR."
  (interactive (list (read-directory-name "Package checkout: " nil nil t)))
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (unless (deb-packaging-detect--find-package-dir nil t)
      (user-error "No debian/changelog in %s" dir))
    (deb-packaging-status)))

;;;###autoload(autoload 'deb-packaging-get-transient "deb-packaging-develop" nil t)
(transient-define-prefix deb-packaging-get-transient ()
  "Get a package to work on."
  :environment #'deb-packaging-transients--env
  ["Get a package"
   ("u" "Clone an Ubuntu package (git ubuntu clone)..." deb-packaging-clone-git-ubuntu
    :inapt-if-not (lambda () (executable-find "git-ubuntu")))
   ("d" "Clone a Debian packaging repo (gbp clone)..." deb-packaging-clone-gbp
    :inapt-if-not (lambda () (executable-find "gbp")))
   ("o" "Open a local checkout..." deb-packaging-develop-open-checkout)
   ("q" "Quit" transient-quit-one)])

(provide 'deb-packaging-develop)
;;; deb-packaging-develop.el ends here
