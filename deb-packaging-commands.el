;;; deb-packaging-commands.el --- Command execution for deb-packaging -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/example/deb-packaging
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Build and execute commands for Debian packaging tools.

;;; Code:

(require 'comint)
(require 'ansi-color)
(require 'deb-packaging-detect)
(require 'deb-packaging-presets)

;;; Core Execution

(defun deb-packaging--filter-osc-sequences (string)
  "Filter OSC and terminal query sequences from STRING for comint."
  ;; Remove OSC sequences: ESC ] ... (BEL | ESC \)
  (setq string (replace-regexp-in-string "\e\\][^\a\e]*\\(\a\\|\e\\\\\\)" "" string))
  ;; Remove CSI ? sequences (cursor show/hide, etc)
  (setq string (replace-regexp-in-string "\e\\[\\?[0-9;]*[a-zA-Z]" "" string))
  string)

(defun deb-packaging--run-command (name args &optional dir)
  "Run command in comint buffer. NAME for buffer, ARGS is command list."
  (let* ((timestamp (format-time-string "%H:%M:%S"))
         (buf-name (format "*deb-%s-%s*" name timestamp))
         (default-directory (or dir default-directory))
         (cmd (mapconcat #'shell-quote-argument args " ")))
    (make-comint-in-buffer name buf-name shell-file-name nil
                           shell-command-switch cmd)
    (with-current-buffer buf-name
      (add-hook 'comint-preoutput-filter-functions #'deb-packaging--filter-osc-sequences nil t)
      (add-hook 'comint-output-filter-functions #'ansi-color-process-output nil t))
    (pop-to-buffer buf-name)))

;;; dpkg-buildpackage

(defun deb-packaging--build-source-command ()
  "Build dpkg-buildpackage command from current presets."
  (cons "dpkg-buildpackage" (deb-packaging--get-mode-args 'dpkg-buildpackage)))

(defun deb-packaging-source-build ()
  "Run source build with current presets."
  (interactive)
  (let ((pkg-dir (deb-packaging--find-package-dir)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (deb-packaging--run-command "source-build"
                                (deb-packaging--build-source-command)
                                pkg-dir)))

;;; lintian

(defun deb-packaging--run-lintian (target)
  "Run lintian on TARGET file."
  (let ((pkg-dir (deb-packaging--find-package-dir)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let ((parent-dir (file-name-directory (directory-file-name pkg-dir))))
      (deb-packaging--run-command "lintian"
                                  (append (cons "lintian"
                                                (deb-packaging--get-mode-args 'lintian))
                                          (list target))
                                  parent-dir))))

(defun deb-packaging-lintian-source ()
  "Run lintian on source .changes file."
  (interactive)
  (let* ((pkg-dir (deb-packaging--find-package-dir))
         (info (deb-packaging--parse-changelog pkg-dir))
         (parent-dir (file-name-directory (directory-file-name pkg-dir)))
         (artifacts (deb-packaging--scan-artifacts (nth 0 info) (nth 1 info) parent-dir))
         (changes (alist-get 'source-changes artifacts)))
    (unless changes
      (user-error "No source .changes file found"))
    (deb-packaging--run-lintian changes)))

(defun deb-packaging-lintian-binary ()
  "Run lintian on binary .changes file."
  (interactive)
  (let* ((pkg-dir (deb-packaging--find-package-dir))
         (info (deb-packaging--parse-changelog pkg-dir))
         (parent-dir (file-name-directory (directory-file-name pkg-dir)))
         (artifacts (deb-packaging--scan-artifacts (nth 0 info) (nth 1 info) parent-dir))
         (changes (car (alist-get 'binary-changes artifacts))))
    (unless changes
      (user-error "No binary .changes file found"))
    (deb-packaging--run-lintian changes)))

;;; sbuild

(defun deb-packaging--build-sbuild-command (dsc-file)
  "Build sbuild command for DSC-FILE."
  (let* ((distro deb-packaging-target-distro)
         (base-args (deb-packaging--get-mode-args 'sbuild))
         (variant-args (deb-packaging--get-sbuild-variant-args distro))
         (distro-arg (format "-d%s" distro)))
    (append (list "sbuild")
            base-args
            variant-args
            (list distro-arg dsc-file))))

(defun deb-packaging-sbuild ()
  "Run sbuild on current package."
  (interactive)
  (let ((pkg-dir (deb-packaging--find-package-dir)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let* ((info (deb-packaging--parse-changelog pkg-dir))
           (parent-dir (file-name-directory (directory-file-name pkg-dir)))
           (artifacts (when info
                        (deb-packaging--scan-artifacts (nth 0 info)
                                                       (nth 1 info)
                                                       parent-dir)))
           (dsc-file (alist-get 'dsc artifacts)))
      (unless dsc-file
        (user-error "No .dsc file found. Run source build first"))
      (deb-packaging--run-command "sbuild"
                                  (deb-packaging--build-sbuild-command dsc-file)
                                  parent-dir))))

;;; autopkgtest

(defun deb-packaging--lxd-image-exists-p (image)
  "Check if LXD IMAGE exists locally."
  (zerop (call-process "lxc" nil nil nil "image" "info" image)))

(defun deb-packaging--build-autopkgtest-command (debs source-dir)
  "Build autopkgtest command for DEBS list and SOURCE-DIR."
  (let* ((config (deb-packaging--get-test-runner-config deb-packaging-target-distro))
         (runner (symbol-name (alist-get 'runner config)))
         (image (alist-get 'image config))
         (base-args (deb-packaging--get-mode-args 'autopkgtest)))
    (append (list "autopkgtest")
            base-args
            debs
            (list source-dir "--" runner)
            (when image (list image)))))

(defun deb-packaging-autopkgtest ()
  "Run autopkgtest with built .debs."
  (interactive)
  (let ((pkg-dir (deb-packaging--find-package-dir)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let* ((info (deb-packaging--parse-changelog pkg-dir))
           (parent-dir (file-name-directory (directory-file-name pkg-dir)))
           (artifacts (when info
                        (deb-packaging--scan-artifacts (nth 0 info)
                                                       (nth 1 info)
                                                       parent-dir)))
           (debs (alist-get 'debs artifacts))
           (config (deb-packaging--get-test-runner-config deb-packaging-target-distro))
           (runner (alist-get 'runner config))
           (image (alist-get 'image config)))
      (unless debs
        (user-error "No .deb files found. Run sbuild first"))
      ;; Check LXD image exists
      (when (and (eq runner 'lxd) image)
        (unless (deb-packaging--lxd-image-exists-p image)
          (user-error "LXD image '%s' not found. Build it with:\n  autopkgtest-build-lxd ubuntu-daily:%s\nOr switch to qemu runner (press 'r')"
                      image deb-packaging-target-distro)))
      ;; Check QEMU image exists
      (when (and (eq runner 'qemu) image)
        (unless (file-exists-p image)
          (user-error "QEMU image '%s' not found. Build it with:\n  autopkgtest-buildvm-ubuntu-cloud -r %s"
                      image deb-packaging-target-distro)))
      (deb-packaging--run-command "autopkgtest"
                                  (deb-packaging--build-autopkgtest-command debs ".")
                                  pkg-dir))))

;;; PPA (Launchpad) testing

(defun deb-packaging-ppa-tests ()
  "Show autopkgtest results / trigger URLs for the current PPA.
Scoped to the current package and target distro by default.  Prompts for
the PPA via `deb-packaging-set-ppa' if none is set for the session."
  (interactive)
  (unless deb-packaging--current-ppa
    (deb-packaging-set-ppa))
  (when (or (null deb-packaging--current-ppa)
            (string-empty-p deb-packaging--current-ppa))
    (user-error "No PPA selected"))
  (let* ((pkg-dir (deb-packaging--find-package-dir))
         (info (when pkg-dir (deb-packaging--parse-changelog pkg-dir)))
         (name (nth 0 info))
         (args (append (list "ppa" "tests" deb-packaging--current-ppa)
                       (when name (list "-p" name))
                       (when deb-packaging-target-distro
                         (list "-r" deb-packaging-target-distro)))))
    (deb-packaging--run-command "ppa-tests" args
                                (or pkg-dir default-directory))))

;;; Clean

(defun deb-packaging-clean ()
  "Clean build artifacts and schroot sessions."
  (interactive)
  (let ((pkg-dir (deb-packaging--find-package-dir)))
    (unless pkg-dir
      (user-error "Not in a Debian package directory"))
    (let* ((info (deb-packaging--parse-changelog pkg-dir))
           (name (nth 0 info))
           (version (nth 1 info))
           (parent-dir (file-name-directory (directory-file-name pkg-dir)))
           (file-version (deb-packaging--version-to-filename version))
           (pattern (format "%s[_-]*%s*" name file-version)))
      ;; Build cleanup command
      (deb-packaging--run-command
       "clean"
       (list "sh" "-c"
             (format "cd %s && quilt pop -a 2>/dev/null; \
schroot -e --all-sessions 2>/dev/null; \
rm -f %s/%s; \
rm -f debian/files; \
rm -rf .pc/; \
echo 'Clean complete'"
                     (shell-quote-argument pkg-dir)
                     (shell-quote-argument parent-dir)
                     pattern))
       pkg-dir))))

(provide 'deb-packaging-commands)
;;; deb-packaging-commands.el ends here
