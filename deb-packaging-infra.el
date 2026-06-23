;;; deb-packaging-infra.el --- Infrastructure management for deb-packaging -*- lexical-binding: t; -*-

;;; Commentary:

;; Manage schroots, LXD containers, and QEMU images for Debian packaging.
;; Provides listing, creation, update, and deletion of build/test environments.

;;; Code:

(require 'transient)
(require 'deb-packaging-presets)

;;; Schroot Management

(defun deb-packaging-infra--list-schroots ()
  "Return alist of schroots: ((name . config-file) ...)."
  (let ((config-dir "/etc/schroot/chroot.d/")
        result)
    (when (file-directory-p config-dir)
      (dolist (file (directory-files config-dir t "^[^.]"))
        (when (file-regular-p file)
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (re-search-forward "^\\[\\([^]]+\\)\\]" nil t)
              (push (cons (match-string 1) file) result))))))
    (nreverse result)))

(defun deb-packaging-infra--schroot-directory (config-file name)
  "Get directory path for schroot NAME from CONFIG-FILE."
  (with-temp-buffer
    (insert-file-contents config-file)
    (goto-char (point-min))
    ;; Find the section for this schroot
    (when (re-search-forward (format "^\\[%s\\]" (regexp-quote name)) nil t)
      (let ((section-end (save-excursion
                           (if (re-search-forward "^\\[" nil t)
                               (match-beginning 0)
                             (point-max)))))
        (when (re-search-forward "^directory=\\(.+\\)$" section-end t)
          (match-string 1))))))

(defun deb-packaging-infra--format-schroot (entry)
  "Format schroot ENTRY for display."
  (car entry))

(defun deb-packaging-infra-create-schroot ()
  "Create a new schroot using mk-sbuild."
  (interactive)
  (let* ((distro (read-string "Distro: " deb-packaging-target-distro))
         (arch (completing-read "Arch: " '("amd64" "i386" "arm64" "armhf") nil t "amd64"))
         (cmd (format "mk-sbuild --arch=%s %s" arch distro)))
    (when (yes-or-no-p (format "Run: %s? " cmd))
      (compile cmd))))

(defun deb-packaging-infra-update-schroot ()
  "Update a schroot using sbuild-update."
  (interactive)
  (let* ((schroots (deb-packaging-infra--list-schroots))
         (names (mapcar #'car schroots))
         (name (completing-read "Schroot to update: " names nil t)))
    (compile (format "sbuild-update -udcar %s" name))))

(defun deb-packaging-infra-delete-schroot ()
  "Delete a schroot (config and directory)."
  (interactive)
  (let* ((schroots (deb-packaging-infra--list-schroots))
         (names (mapcar #'car schroots))
         (name (completing-read "Schroot to delete: " names nil t))
         (config-file (cdr (assoc name schroots)))
         (directory (deb-packaging-infra--schroot-directory config-file name)))
    (if (not directory)
        (message "Could not find directory for schroot %s" name)
      ;; Show what will be deleted
      (let ((msg (format "Will delete:\n  Config: %s\n  Directory: %s\n\nProceed?"
                         config-file directory)))
        (when (yes-or-no-p msg)
          (let ((cmd (format "sudo rm -rf %s && sudo rm %s"
                             (shell-quote-argument directory)
                             (shell-quote-argument config-file))))
            (compile cmd)))))))

(defun deb-packaging-infra-list-schroots ()
  "List available schroots."
  (interactive)
  (let ((schroots (deb-packaging-infra--list-schroots)))
    (if schroots
        (message "Schroots: %s"
                 (mapconcat #'deb-packaging-infra--format-schroot schroots ", "))
      (message "No schroots found"))))

;;; LXD Management

(defun deb-packaging-infra--list-lxd-images ()
  "Return list of autopkgtest LXD images."
  (let ((output (shell-command-to-string "lxc image list --format=csv 2>/dev/null")))
    (when (and output (not (string-empty-p output)))
      (let (result)
        (dolist (line (split-string output "\n" t))
          (let ((fields (split-string line ",")))
            (when (>= (length fields) 2)
              (let ((alias (nth 1 fields)))
                (when (string-match "autopkgtest" alias)
                  (push alias result))))))
        (nreverse result)))))

(defun deb-packaging-infra-create-lxd ()
  "Create an LXD image for autopkgtest."
  (interactive)
  (let* ((distro (read-string "Distro: " deb-packaging-target-distro))
         (arch (completing-read "Arch: " '("amd64" "arm64") nil t "amd64"))
         (cmd (format "autopkgtest-build-lxd ubuntu-daily:%s/%s" distro arch)))
    (when (yes-or-no-p (format "Run: %s? " cmd))
      (compile cmd))))

(defun deb-packaging-infra-delete-lxd ()
  "Delete an LXD autopkgtest image."
  (interactive)
  (let* ((images (deb-packaging-infra--list-lxd-images))
         (image (completing-read "Image to delete: " images nil t)))
    (when (yes-or-no-p (format "Delete LXD image %s?" image))
      (compile (format "lxc image delete %s" (shell-quote-argument image))))))

(defun deb-packaging-infra-list-lxd ()
  "List available LXD autopkgtest images."
  (interactive)
  (let ((images (deb-packaging-infra--list-lxd-images)))
    (if images
        (message "LXD images: %s" (mapconcat #'identity images ", "))
      (message "No autopkgtest LXD images found"))))

;;; QEMU Management

(defconst deb-packaging-infra-qemu-dir "/var/lib/adt-images/"
  "Directory where QEMU autopkgtest images are stored.")

(defun deb-packaging-infra--list-qemu-images ()
  "Return list of QEMU autopkgtest images."
  (when (file-directory-p deb-packaging-infra-qemu-dir)
    (let (result)
      (dolist (file (directory-files deb-packaging-infra-qemu-dir nil "\\.img$"))
        (push file result))
      (nreverse result))))

(defun deb-packaging-infra-create-qemu ()
  "Create a QEMU image for autopkgtest."
  (interactive)
  (let* ((distro (read-string "Distro: " deb-packaging-target-distro))
         (arch (completing-read "Arch: " '("amd64" "arm64" "i386") nil t "amd64"))
         (cmd (format "autopkgtest-buildvm-ubuntu-cloud -r %s -a %s -o %s"
                      distro arch deb-packaging-infra-qemu-dir)))
    (when (yes-or-no-p (format "Run: %s? " cmd))
      (compile cmd))))

(defun deb-packaging-infra-delete-qemu ()
  "Delete a QEMU autopkgtest image."
  (interactive)
  (let* ((images (deb-packaging-infra--list-qemu-images))
         (image (completing-read "Image to delete: " images nil t))
         (path (expand-file-name image deb-packaging-infra-qemu-dir)))
    (when (yes-or-no-p (format "Delete %s?" path))
      (compile (format "sudo rm %s" (shell-quote-argument path))))))

(defun deb-packaging-infra-list-qemu ()
  "List available QEMU autopkgtest images."
  (interactive)
  (let ((images (deb-packaging-infra--list-qemu-images)))
    (if images
        (message "QEMU images: %s" (mapconcat #'identity images ", "))
      (message "No QEMU images found in %s" deb-packaging-infra-qemu-dir))))

;;; PPA (Launchpad) Management

(defun deb-packaging-infra--list-ppas ()
  "Return list of the current user's PPA names via the `ppa' tool.
Each entry is of the form \"ppa:owner/name\"."
  (let ((output (shell-command-to-string "ppa list 2>/dev/null"))
        result)
    (dolist (line (split-string output "\n" t))
      (when (string-match "\\(ppa:[^ \t]+/[^ \t]+\\)" line)
        (push (match-string 1 line) result)))
    (nreverse result)))

(defun deb-packaging-infra-create-ppa ()
  "Create a new Launchpad PPA via `ppa create'."
  (interactive)
  (let* ((name (read-string "PPA name to create: "))
         (cmd (format "ppa create %s" (shell-quote-argument name))))
    (when (and (not (string-empty-p name))
               (yes-or-no-p (format "Run: %s? " cmd)))
      (compile cmd))))

(defun deb-packaging-infra-destroy-ppa ()
  "Destroy a Launchpad PPA via `ppa destroy'."
  (interactive)
  (let* ((ppas (deb-packaging-infra--list-ppas))
         (name (completing-read "PPA to destroy: " ppas nil nil)))
    (when (and (not (string-empty-p name))
               (yes-or-no-p (format "Really destroy PPA %s? " name)))
      (compile (format "ppa destroy %s" (shell-quote-argument name))))))

(defun deb-packaging-infra-set-ppa-config ()
  "Apply configuration to a Launchpad PPA via `ppa set'.
Prompts for an optional display name and description."
  (interactive)
  (let* ((ppas (deb-packaging-infra--list-ppas))
         (name (completing-read "PPA to configure: " ppas nil nil))
         (displayname (read-string "Display name (blank to skip): "))
         (description (read-string "Description (blank to skip): "))
         (args (append (list "ppa" "set" name)
                       (unless (string-empty-p displayname)
                         (list "--displayname" displayname))
                       (unless (string-empty-p description)
                         (list "--description" description))))
         (cmd (mapconcat #'shell-quote-argument args " ")))
    (if (= (length args) 3)
        (message "No configuration changes specified")
      (when (yes-or-no-p (format "Run: %s? " cmd))
        (compile cmd)))))

(defun deb-packaging-infra-show-ppa ()
  "Show configuration info for a Launchpad PPA via `ppa show'."
  (interactive)
  (let* ((ppas (deb-packaging-infra--list-ppas))
         (name (completing-read "PPA to show: " ppas nil nil)))
    (unless (string-empty-p name)
      (compile (format "ppa show %s" (shell-quote-argument name))))))

(defun deb-packaging-infra-list-ppas ()
  "List the current user's PPAs."
  (interactive)
  (let ((ppas (deb-packaging-infra--list-ppas)))
    (if ppas
        (message "PPAs: %s" (mapconcat #'identity ppas ", "))
      (message "No PPAs found"))))

;;; Infrastructure Transient

(defun deb-packaging-infra--header ()
  "Header for infrastructure transient."
  (format "Infrastructure Management\nDistro: %s" deb-packaging-target-distro))

(transient-define-prefix deb-packaging-infra-dispatch ()
  "Manage build and test infrastructure."
  [:description deb-packaging-infra--header]
  ["Schroots (sbuild)"
   ("sc" "Create schroot" deb-packaging-infra-create-schroot)
   ("su" "Update schroot" deb-packaging-infra-update-schroot)
   ("sd" "Delete schroot" deb-packaging-infra-delete-schroot)
   ("ss" "List schroots" deb-packaging-infra-list-schroots :transient t)]
  ["LXD (autopkgtest)"
   ("lc" "Create LXD image" deb-packaging-infra-create-lxd)
   ("ld" "Delete LXD image" deb-packaging-infra-delete-lxd)
   ("ll" "List LXD images" deb-packaging-infra-list-lxd :transient t)]
  ["QEMU (autopkgtest)"
   ("vc" "Create QEMU image" deb-packaging-infra-create-qemu)
   ("vd" "Delete QEMU image" deb-packaging-infra-delete-qemu)
   ("vv" "List QEMU images" deb-packaging-infra-list-qemu :transient t)]
  ["PPA (Launchpad)"
   ("pc" "Create PPA" deb-packaging-infra-create-ppa)
   ("pd" "Destroy PPA" deb-packaging-infra-destroy-ppa)
   ("ps" "Set PPA config" deb-packaging-infra-set-ppa-config)
   ("pw" "Show PPA" deb-packaging-infra-show-ppa :transient t)
   ("pl" "List PPAs" deb-packaging-infra-list-ppas :transient t)]
  ["Navigation"
   ("q" "Back" transient-quit-one)])

(provide 'deb-packaging-infra)
;;; deb-packaging-infra.el ends here
