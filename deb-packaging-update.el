;;; deb-packaging-update.el --- New upstream version updates -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Update a package to a new upstream version.
;;   1. `uscan' checks debian/watch and downloads the new .orig.tar.*
;;      into the build-output directory.
;;   2. The method is asked for on every update, defaulting to what the
;;      tree looks like:
;;      - gbp: `gbp import-orig --uscan' imports and merges upstream into
;;        the git repository (needs a gbp layout).
;;      - uupdate: `uupdate --find --upstream-version V' unpacks a fresh
;;        ../name-V tree with debian/ carried over and the changelog
;;        seeded.
;;
;; Entry point: `deb-packaging-update-transient'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit)
(require 'transient)
(require 'deb-packaging-detect)
(require 'deb-packaging-commands)
(require 'deb-packaging-transients)

;;; Preflight

(defun deb-packaging-update--pkg-dir ()
  "Return the current package directory, checking for debian/watch.
Signals `user-error' outside a package tree or without a watch file."
  (let ((pkg-dir (deb-packaging-detect--find-package-dir nil t)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (unless (file-readable-p (expand-file-name "debian/watch" pkg-dir))
      (user-error "No debian/watch file; add one to track upstream versions"))
    pkg-dir))

(defun deb-packaging-update--ensure-binaries (bins)
  "Signal `user-error' unless every program in BINS is in `exec-path'."
  (dolist (bin bins)
    (unless (executable-find bin)
      (user-error "%s not found in `exec-path'" bin))))

;;; Method selection

(defun deb-packaging-update--gbp-signal (pkg-dir)
  "Return the gbp-layout signal found in PKG-DIR's repository, or nil.
Signals: `debian/gbp.conf', a pristine-tar or upstream branch, or
upstream/* tags.  These mark trees where `gbp import-orig' works."
  (let ((default-directory pkg-dir))
    (when (magit-toplevel)
      (cond
       ((file-exists-p (expand-file-name "debian/gbp.conf" pkg-dir))
        "debian/gbp.conf")
       ((magit-ref-p "pristine-tar") "pristine-tar branch")
       ((magit-ref-p "upstream") "upstream branch")
       ((magit-git-lines "tag" "--list" "upstream/*")
        "upstream/* tags")))))

(defun deb-packaging-update--default-method (pkg-dir)
  "Return the update method suggested for PKG-DIR: \\='gbp or \\='uupdate."
  (if (deb-packaging-update--gbp-signal pkg-dir) 'gbp 'uupdate))

(defun deb-packaging-update--read-method (pkg-dir)
  "Read the update method for PKG-DIR, defaulting to the detected one.
Always asked: updating is occasional and layouts differ between
trees, so a remembered choice would bite more than a prompt."
  (let ((default (deb-packaging-update--default-method pkg-dir)))
    (intern (completing-read
             "Update method: "
             '("gbp" "uupdate") nil t nil nil (symbol-name default)))))

;;; Orig tarball scanning

(defun deb-packaging-update--tarballs (name parent-dir)
  "Return sorted orig tarball names for package NAME in PARENT-DIR.
Covers plain and extra-component tarballs (name_V.orig[-comp].tar.*)."
  (sort
   (directory-files parent-dir nil
                    (format "^%s_.*\\.orig\\(-[a-z0-9+]+\\)?\\.tar\\."
                            (regexp-quote name)))
   #'string<))

(defun deb-packaging-update--tarball-version (name tarball)
  "Return the upstream version in TARBALL, an orig tarball name for NAME."
  (when (string-match
         (format "^%s_\\(.+?\\)\\.orig\\(-[a-z0-9+]+\\)?\\.tar\\."
                 (regexp-quote name))
         tarball)
    (match-string 1 tarball)))

(defun deb-packaging-update--target-version (name current tarballs &optional downloaded)
  "Return the upstream version to update to, or nil.
TARBALLS are all orig tarball names in the build directory, DOWNLOADED
the subset added by the uscan run that just finished.  Candidates
include only versions newer than CURRENT.  A single candidate returns
outright; several
prompt, defaulting to a downloaded one.  Stale tarballs therefore
merely cost a prompt, and re-running after a failed uupdate still
finds its version without a fresh download."
  (let* ((versions (lambda (files)
                     (delete-dups
                      (seq-filter
                       (lambda (v)
                         (and v
                              (zerop (call-process
                                      "dpkg" nil nil nil
                                      "--compare-versions" v "gt" current))))
                       (mapcar (lambda (f)
                                 (deb-packaging-update--tarball-version name f))
                               files)))))
         (new (funcall versions downloaded))
         (candidates (funcall versions tarballs)))
    (pcase candidates
      (`nil nil)
      (`(,one) one)
      (_ (let ((default (or (car new) (car (last candidates)))))
           (completing-read (format "Upstream version (default %s): " default)
                            candidates nil t nil nil default))))))

;;; Commands

(defconst deb-packaging-update--check-command
  "uscan --report-status; rc=$?; [ $rc -le 1 ]"
  "Shell command reporting available upstream versions, downloading nothing.
The tail reroutes uscan's exit 1 (\"no newer version\") to success so
compilation-mode does not flag an ordinary outcome as a failure.")

(defconst deb-packaging-update--download-command
  "uscan; rc=$?; [ $rc -le 1 ]"
  "Shell command downloading the newest upstream orig tarball.
Same exit-code rerouting as `deb-packaging-update--check-command'.")

;;;###autoload
(defun deb-packaging-update-check ()
  "Report upstream versions available per debian/watch, without downloading."
  (interactive)
  (let ((pkg-dir (deb-packaging-update--pkg-dir)))
    (deb-packaging-update--ensure-binaries '("uscan"))
    (let ((default-directory pkg-dir))
      (deb-packaging-commands--compile deb-packaging-update--check-command))))

(defun deb-packaging-update--pristine-tar-p (pkg-dir)
  "Return non-nil when PKG-DIR's repository has a pristine-tar branch."
  (let ((default-directory pkg-dir))
    (and (magit-toplevel) (magit-ref-p "pristine-tar"))))

(defun deb-packaging-update--gbp-command (pkg-dir)
  "Return the `gbp import-orig --uscan' command line for PKG-DIR.
--pristine-tar joins only when a pristine-tar branch already exists;
creating that branch on a first import is left to the command line."
  (if (deb-packaging-update--pristine-tar-p pkg-dir)
      "gbp import-orig --uscan --pristine-tar"
    "gbp import-orig --uscan"))

(defun deb-packaging-update--run-gbp (pkg-dir)
  "Import the new upstream into PKG-DIR's repository with gbp."
  (let ((default-directory pkg-dir))
    (unless (magit-toplevel)
      (user-error "gbp import-orig requires a git repository"))
    (deb-packaging-commands--after-compile
     (deb-packaging-commands--compile
      (deb-packaging-update--gbp-command pkg-dir))
     (lambda ()
       (deb-packaging-commands--notify-status-refresh)
       (let ((default-directory pkg-dir))
         (message
          (if (deb-packaging-detect--list-patches)
              "Upstream imported; refresh patches via the patch-queue transient (gbp pq rebase) if they no longer apply"
            "Upstream import complete")))))))

(defun deb-packaging-update--uupdate-step (pkg-dir version)
  "Run `uupdate --find --upstream-version VERSION' in PKG-DIR.
On success, open the new ../name-VERSION tree in Dired."
  (let ((default-directory pkg-dir)
        (name (deb-packaging-detect--package-name pkg-dir))
        (parent-dir (deb-packaging-detect--parent-dir pkg-dir)))
    (deb-packaging-commands--after-compile
     (deb-packaging-commands--compile
      (format "uupdate --find --upstream-version %s"
              (shell-quote-argument version)))
     (lambda ()
       (deb-packaging-commands--notify-status-refresh)
       (let ((new-tree (expand-file-name (concat name "-" version) parent-dir)))
         (if (file-directory-p new-tree)
             (progn
               (dired new-tree)
               (message "New tree: %s (changelog seeded; review, bump, build)"
                        new-tree))
           (message "uupdate finished; see its output for the new tree location")))))))

(defun deb-packaging-update--run-uupdate (pkg-dir)
  "Update PKG-DIR with uscan and uupdate.
uscan downloads the tarball into the build directory; the target
version comes from the tarball name (no output parsing); uupdate then
unpacks the new tree."
  (let* ((name (deb-packaging-detect--package-name pkg-dir))
         (current (deb-packaging-detect--upstream-version
                   (deb-packaging-detect--package-version pkg-dir)))
         (parent-dir (deb-packaging-detect--parent-dir pkg-dir)))
    (unless name
      (user-error "Cannot parse debian/changelog"))
    (let ((before (deb-packaging-update--tarballs name parent-dir))
          (default-directory pkg-dir))
      (deb-packaging-commands--after-compile
       (deb-packaging-commands--compile
        deb-packaging-update--download-command)
       (lambda ()
         (let* ((after (deb-packaging-update--tarballs name parent-dir))
                (version (deb-packaging-update--target-version
                          name current after
                          (seq-difference after before))))
           (if (null version)
               (message "No new upstream version available")
             (deb-packaging-update--uupdate-step pkg-dir version))))))))

;;;###autoload
(defun deb-packaging-update-new-upstream ()
  "Update the package to a new upstream version.
Runs `uscan' to download the new orig tarball, then applies it with the
method chosen at the prompt (default detected from the tree):
  gbp     `gbp import-orig --uscan' (git trees with a gbp layout)
  uupdate `uupdate --find' (unpacks a new ../name-VERSION tree)"
  (interactive)
  (let* ((pkg-dir (deb-packaging-update--pkg-dir))
         (method (deb-packaging-update--read-method pkg-dir)))
    (deb-packaging-update--ensure-binaries
     (pcase method
       ('gbp '("uscan" "gbp"))
       (_ '("uscan" "uupdate"))))
    (if (eq method 'gbp)
        (deb-packaging-update--run-gbp pkg-dir)
      (deb-packaging-update--run-uupdate pkg-dir))))

;;; Transient

(defun deb-packaging-update--transient-header ()
  "Header for the update transient: package, version, suggested method."
  (if-let* ((pkg-dir (deb-packaging-detect--find-package-dir))
            (info (deb-packaging-detect--parse-changelog pkg-dir)))
      (let* ((signal (deb-packaging-update--gbp-signal pkg-dir))
             (method (if signal "gbp" "uupdate")))
        (format "New upstream version\n%s %s (%s)\nDefault method: %s%s"
                (nth 0 info) (nth 1 info) (nth 2 info) method
                (if signal (format " (%s)" signal) "")))
    "New upstream version"))

;;;###autoload(autoload 'deb-packaging-update-transient "deb-packaging-update" nil t)
(transient-define-prefix deb-packaging-update-transient ()
  "Update the package to a new upstream version.
The method (gbp import-orig or uupdate) is asked for on every update;
the header shows what will be suggested."
  :environment #'deb-packaging-transients--env
  [:description deb-packaging-update--transient-header]
  ["Check"
   ("c" "Check for new upstream (no download)" deb-packaging-update-check)]
  ["Update"
   ("u" "Update to new upstream version..." deb-packaging-update-new-upstream)
   ("q" "Quit" transient-quit-one)])

(provide 'deb-packaging-update)
;;; deb-packaging-update.el ends here
