;;; deb-packaging-infra.el --- Infrastructure management for deb-packaging -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karl Smeltzer
;; Author: Karl Smeltzer
;; Keywords: tools, debian, ubuntu, packaging
;; Package-Requires: ((emacs "28.1") (transient "0.4.0") (magit-section "3.3"))

;;; Commentary:

;; Manage schroots, LXD containers, QEMU images, and Launchpad PPAs.
;;
;; Each infrastructure type has its own dedicated magit-section list buffer
;; where entries can be navigated (n/p), acted on at point (u/d/s/e), and
;; created (c).  The hub transient (`deb-packaging-infra-dispatch') opens
;; from the status buffer via `i' and dispatches to the per-type buffers.

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'transient)
(require 'deb-packaging-presets)

;;; Schroot Management

(defun deb-packaging-infra--list-schroots ()
  "Return list of schroot plists.
Each plist has keys: :name, :config-file, :description, :directory."
  (let ((config-dir "/etc/schroot/chroot.d/")
        result)
    (when (file-directory-p config-dir)
      (dolist (file (directory-files config-dir t "^[^.]"))
        (when (file-regular-p file)
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (re-search-forward "^\\[\\([^]]+\\)\\]" nil t)
              (let ((name (match-string 1))
                    (section-start (point))
                    (section-end (save-excursion
                                   (if (re-search-forward "^\\[" nil t)
                                       (match-beginning 0)
                                     (point-max))))
                    (description nil)
                    (directory nil))
                (save-excursion
                  (save-restriction
                    (narrow-to-region section-start section-end)
                    (goto-char section-start)
                    (when (re-search-forward "^description=\\(.+\\)$" nil t)
                      (setq description (match-string 1)))
                    (goto-char section-start)
                    (when (re-search-forward "^directory=\\(.+\\)$" nil t)
                      (setq directory (match-string 1)))))
                (push (list :name name
                            :config-file file
                            :description description
                            :directory directory)
                      result))))))
    (nreverse result))))

(defun deb-packaging-infra-create-schroot ()
  "Create a new schroot using mk-sbuild."
  (interactive)
  (let* ((distro (read-string "Distro: " deb-packaging-target-distro))
         (arch (completing-read "Arch: " '("amd64" "i386" "arm64" "armhf") nil t "amd64"))
         (cmd (format "mk-sbuild --arch=%s %s" arch distro)))
    (when (yes-or-no-p (format "Run: %s? " cmd))
      (compile cmd))))

(defun deb-packaging-infra-update-schroot (&optional name)
  "Update a schroot using sbuild-update.
Acts on the schroot at point when in a schroots list buffer; otherwise
prompts for one."
  (interactive
   (list (or (plist-get (deb-packaging-infra--schroot-at-point) :name)
             (let* ((schroots (deb-packaging-infra--list-schroots))
                    (names (mapcar (lambda (s) (plist-get s :name)) schroots)))
               (completing-read "Schroot to update: " names nil t)))))
  (compile (format "sbuild-update -udcar %s" (shell-quote-argument name))))

(defun deb-packaging-infra-delete-schroot (&optional name)
  "Delete a schroot (config and directory).
Acts on the schroot at point when in a schroots list buffer; otherwise
prompts for one."
  (interactive
   (list (or (plist-get (deb-packaging-infra--schroot-at-point) :name)
             (let* ((schroots (deb-packaging-infra--list-schroots))
                    (names (mapcar (lambda (s) (plist-get s :name)) schroots)))
               (completing-read "Schroot to delete: " names nil t)))))
  (let* ((schroots (deb-packaging-infra--list-schroots))
         (sc (cl-find name schroots
                      :key (lambda (s) (plist-get s :name)) :test #'equal))
         (config-file (plist-get sc :config-file))
         (directory (plist-get sc :directory)))
    (if (not directory)
        (message "Could not find directory for schroot %s" name)
      (let ((msg (format "Will delete:\n  Config: %s\n  Directory: %s\n\nProceed?"
                         config-file directory)))
        (when (yes-or-no-p msg)
          (let ((cmd (format "sudo rm -rf %s && sudo rm %s"
                             (shell-quote-argument directory)
                             (shell-quote-argument config-file))))
            (compile cmd)))))))

;;; Schroot list buffer

(defvar-keymap deb-packaging-infra-schroots-mode-map
  :doc "Keymap for the schroots list buffer.
\\{deb-packaging-infra-schroots-mode-map}"
  :parent magit-section-mode-map
  "u" #'deb-packaging-infra-update-schroot
  "d" #'deb-packaging-infra-delete-schroot
  "c" #'deb-packaging-infra-create-schroot
  "g" #'deb-packaging-infra-refresh-schroots
  "q" #'quit-window)

(define-derived-mode deb-packaging-infra-schroots-mode magit-section-mode
  "Infra-Schroots"
  "Major mode for listing and managing schroots."
  :group 'deb-packaging)

(defun deb-packaging-infra--schroot-at-point ()
  "Return the schroot plist at point, or nil."
  (when-let ((section (magit-current-section)))
    (when (eq (oref section type) 'deb-packaging-infra-schroot)
      (oref section value))))

(defun deb-packaging-infra--pad (text width)
  "Left-justify TEXT to WIDTH columns."
  (format (format "%%-%ds" width) (or text "")))

(defun deb-packaging-infra-refresh-schroots ()
  "Refresh the schroots list buffer."
  (interactive)
  (when (derived-mode-p 'deb-packaging-infra-schroots-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((schroots (deb-packaging-infra--list-schroots)))
        (magit-insert-section (deb-packaging-infra-root)
          (insert (propertize (format "Schroots (%d)\n\n" (length schroots))
                              'font-lock-face 'magit-section-heading))
          (if (null schroots)
              (insert (propertize
                       "No schroots found in /etc/schroot/chroot.d/\n\
Create one with 'c'.\n"
                       'font-lock-face 'shadow))
            (insert (propertize
                     (format "%s  %s  %s\n"
                             (deb-packaging-infra--pad "Name" 20)
                             (deb-packaging-infra--pad "Description" 20)
                             "Directory")
                     'font-lock-face 'shadow))
            (insert (propertize (make-string 78 ?-) 'font-lock-face 'shadow)
                    "\n")
            (dolist (sc schroots)
              (magit-insert-section (deb-packaging-infra-schroot sc)
                (magit-insert-heading
                  (concat (deb-packaging-infra--pad (plist-get sc :name) 20)
                          "  "
                          (deb-packaging-infra--pad
                           (plist-get sc :description) 20)
                          "  "
                          (propertize (or (plist-get sc :directory) "")
                                      'font-lock-face 'shadow)))))))))
    (when magit-root-section
      (magit-section-show magit-root-section))))

(defun deb-packaging-infra-schroots ()
  "Open a buffer listing all schroots."
  (interactive)
  (let ((buf (get-buffer-create "*deb-packaging infra: Schroots*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'deb-packaging-infra-schroots-mode)
        (deb-packaging-infra-schroots-mode))
      (deb-packaging-infra-refresh-schroots))
    (pop-to-buffer buf)))

;;; LXD Management

(defun deb-packaging-infra--list-lxd-images ()
  "Return list of autopkgtest LXD image plists.
Each plist has keys: :alias, :fingerprint, :description, :arch, :size.
Parses `lxc image list --format=csv' output, which is:
  ALIAS,FINGERPRINT,PUBLIC,DESCRIPTION,ARCH,TYPE,SIZE,UPLOAD_DATE"
  (let ((output (shell-command-to-string "lxc image list --format=csv 2>/dev/null")))
    (when (and output (not (string-empty-p output)))
      (let (result)
        (dolist (line (split-string output "\n" t))
          (let ((fields (split-string line ",")))
            ;; CSV: ALIAS,FINGERPRINT,PUBLIC,DESCRIPTION,ARCH,TYPE,SIZE,...
            ;; The last field (upload date) may contain a comma inside quotes,
            ;; but we only read indices 0-6 which are before it.
            (when (>= (length fields) 2)
              (let ((alias (nth 0 fields)))
                (when (and alias (string-match-p "autopkgtest" alias))
                  (push (list :alias alias
                              :fingerprint (nth 1 fields)
                              :description (nth 3 fields)
                              :arch (nth 4 fields)
                              :size (nth 6 fields))
                        result))))))
        (nreverse result)))))

(defun deb-packaging-infra-create-lxd ()
  "Create an LXD image for autopkgtest."
  (interactive)
  (let* ((distro (read-string "Distro: " deb-packaging-target-distro))
         (arch (completing-read "Arch: " '("amd64" "arm64") nil t "amd64"))
         (cmd (format "autopkgtest-build-lxd ubuntu-daily:%s/%s" distro arch)))
    (when (yes-or-no-p (format "Run: %s? " cmd))
      (compile cmd))))

(defun deb-packaging-infra-delete-lxd (&optional alias)
  "Delete an LXD autopkgtest image.
Acts on the image at point when in an LXD images list buffer; otherwise
prompts for one."
  (interactive
   (list (or (plist-get (deb-packaging-infra--lxd-image-at-point) :alias)
             (let* ((images (deb-packaging-infra--list-lxd-images))
                    (aliases (mapcar (lambda (i) (plist-get i :alias)) images)))
               (completing-read "Image to delete: " aliases nil t)))))
  (when (yes-or-no-p (format "Delete LXD image %s?" alias))
    (compile (format "lxc image delete %s" (shell-quote-argument alias)))))

;;; LXD list buffer

(defvar-keymap deb-packaging-infra-lxd-images-mode-map
  :doc "Keymap for the LXD images list buffer.
\\{deb-packaging-infra-lxd-images-mode-map}"
  :parent magit-section-mode-map
  "d" #'deb-packaging-infra-delete-lxd
  "c" #'deb-packaging-infra-create-lxd
  "g" #'deb-packaging-infra-refresh-lxd-images
  "q" #'quit-window)

(define-derived-mode deb-packaging-infra-lxd-images-mode magit-section-mode
  "Infra-LXD"
  "Major mode for listing and managing LXD autopkgtest images."
  :group 'deb-packaging)

(defun deb-packaging-infra--lxd-image-at-point ()
  "Return the LXD image plist at point, or nil."
  (when-let ((section (magit-current-section)))
    (when (eq (oref section type) 'deb-packaging-infra-lxd-image)
      (oref section value))))

(defun deb-packaging-infra-refresh-lxd-images ()
  "Refresh the LXD images list buffer."
  (interactive)
  (when (derived-mode-p 'deb-packaging-infra-lxd-images-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((images (deb-packaging-infra--list-lxd-images)))
        (magit-insert-section (deb-packaging-infra-root)
          (insert (propertize
                   (format "LXD autopkgtest images (%d)\n\n" (length images))
                   'font-lock-face 'magit-section-heading))
          (if (null images)
              (insert (propertize
                       "No autopkgtest LXD images found.\n\
Create one with 'c'.\n"
                       'font-lock-face 'shadow))
            (insert (propertize
                     (format "%s  %s  %s  %s\n"
                             (deb-packaging-infra--pad "Alias" 40)
                             (deb-packaging-infra--pad "Arch" 8)
                             (deb-packaging-infra--pad "Size" 12)
                             "Fingerprint")
                     'font-lock-face 'shadow))
            (insert (propertize (make-string 78 ?-) 'font-lock-face 'shadow)
                    "\n")
            (dolist (img images)
              (magit-insert-section (deb-packaging-infra-lxd-image img)
                (magit-insert-heading
                  (concat (deb-packaging-infra--pad (plist-get img :alias) 40)
                          "  "
                          (deb-packaging-infra--pad (plist-get img :arch) 8)
                          "  "
                          (deb-packaging-infra--pad (plist-get img :size) 12)
                          "  "
                          (propertize (or (plist-get img :fingerprint) "")
                                      'font-lock-face 'shadow))))))))
    (when magit-root-section
      (magit-section-show magit-root-section)))))

(defun deb-packaging-infra-lxd-images ()
  "Open a buffer listing all LXD autopkgtest images."
  (interactive)
  (let ((buf (get-buffer-create "*deb-packaging infra: LXD images*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'deb-packaging-infra-lxd-images-mode)
        (deb-packaging-infra-lxd-images-mode))
      (deb-packaging-infra-refresh-lxd-images))
    (pop-to-buffer buf)))

;;; QEMU Management

(defconst deb-packaging-infra-qemu-dir "/var/lib/adt-images/"
  "Directory where QEMU autopkgtest images are stored.")

(defun deb-packaging-infra--list-qemu-images ()
  "Return list of QEMU image plists.
Each plist has keys: :name, :path, :size."
  (when (file-directory-p deb-packaging-infra-qemu-dir)
    (let (result)
      (dolist (file (directory-files deb-packaging-infra-qemu-dir nil "\\.img$"))
        (let ((path (expand-file-name file deb-packaging-infra-qemu-dir)))
          (push (list :name file
                      :path path
                      :size (file-attribute-size
                             (file-attributes path)))
                result)))
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

(defun deb-packaging-infra-delete-qemu (&optional name)
  "Delete a QEMU autopkgtest image.
Acts on the image at point when in a QEMU images list buffer; otherwise
prompts for one."
  (interactive
   (list (or (plist-get (deb-packaging-infra--qemu-image-at-point) :name)
             (let* ((images (deb-packaging-infra--list-qemu-images))
                    (names (mapcar (lambda (i) (plist-get i :name)) images)))
               (completing-read "Image to delete: " names nil t)))))
  (let* ((images (deb-packaging-infra--list-qemu-images))
         (img (cl-find name images
                       :key (lambda (i) (plist-get i :name)) :test #'equal))
         (path (plist-get img :path)))
    (when (yes-or-no-p (format "Delete %s?" path))
      (compile (format "sudo rm %s" (shell-quote-argument path))))))

;;; QEMU list buffer

(defvar-keymap deb-packaging-infra-qemu-images-mode-map
  :doc "Keymap for the QEMU images list buffer.
\\{deb-packaging-infra-qemu-images-mode-map}"
  :parent magit-section-mode-map
  "d" #'deb-packaging-infra-delete-qemu
  "c" #'deb-packaging-infra-create-qemu
  "g" #'deb-packaging-infra-refresh-qemu-images
  "q" #'quit-window)

(define-derived-mode deb-packaging-infra-qemu-images-mode magit-section-mode
  "Infra-QEMU"
  "Major mode for listing and managing QEMU autopkgtest images."
  :group 'deb-packaging)

(defun deb-packaging-infra--qemu-image-at-point ()
  "Return the QEMU image plist at point, or nil."
  (when-let ((section (magit-current-section)))
    (when (eq (oref section type) 'deb-packaging-infra-qemu-image)
      (oref section value))))

(defun deb-packaging-infra-refresh-qemu-images ()
  "Refresh the QEMU images list buffer."
  (interactive)
  (when (derived-mode-p 'deb-packaging-infra-qemu-images-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((images (deb-packaging-infra--list-qemu-images)))
        (magit-insert-section (deb-packaging-infra-root)
          (insert (propertize
                   (format "QEMU autopkgtest images (%d)\n\n" (length images))
                   'font-lock-face 'magit-section-heading))
          (if (null images)
              (insert (propertize
                       (format "No QEMU images found in %s\n\
Create one with 'c'.\n"
                               deb-packaging-infra-qemu-dir)
                       'font-lock-face 'shadow))
            (insert (propertize
                     (format "%s  %s  %s\n"
                             (deb-packaging-infra--pad "Name" 50)
                             (deb-packaging-infra--pad "Size" 12)
                             "Path")
                     'font-lock-face 'shadow))
            (insert (propertize (make-string 78 ?-) 'font-lock-face 'shadow)
                    "\n")
            (dolist (img images)
              (magit-insert-section (deb-packaging-infra-qemu-image img)
                (magit-insert-heading
                  (concat (deb-packaging-infra--pad (plist-get img :name) 50)
                          "  "
                          (deb-packaging-infra--pad
                           (if (plist-get img :size)
                               (file-size-human-readable
                                (plist-get img :size))
                             "unknown")
                           12)
                          "  "
                          (propertize (or (plist-get img :path) "")
                                      'font-lock-face 'shadow))))))))
      (when magit-root-section
        (magit-section-show magit-root-section)))))

(defun deb-packaging-infra-qemu-images ()
  "Open a buffer listing all QEMU autopkgtest images."
  (interactive)
  (let ((buf (get-buffer-create "*deb-packaging infra: QEMU images*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'deb-packaging-infra-qemu-images-mode)
        (deb-packaging-infra-qemu-images-mode))
      (deb-packaging-infra-refresh-qemu-images))
    (pop-to-buffer buf)))

;;; PPA (Launchpad) Management

(defun deb-packaging-infra--list-ppas ()
  "Return list of the current user's PPA names via the `ppa' tool.
Each entry is a string of the form \"ppa:owner/name\".
Kept as strings (not plists) for compatibility with PPA completion
in `deb-packaging--read-ppa' (deb-packaging-transients.el)."
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

(defun deb-packaging-infra-destroy-ppa (&optional name)
  "Destroy a Launchpad PPA via `ppa destroy'.
Acts on the PPA at point when in a PPAs list buffer; otherwise prompts."
  (interactive
   (list (or (deb-packaging-infra--ppa-at-point)
             (let ((ppas (deb-packaging-infra--list-ppas)))
               (completing-read "PPA to destroy: " ppas nil nil)))))
  (when (and (not (string-empty-p name))
             (yes-or-no-p (format "Really destroy PPA %s? " name)))
    (compile (format "ppa destroy %s" (shell-quote-argument name)))))

(defun deb-packaging-infra-set-ppa-config (&optional name)
  "Apply configuration to a Launchpad PPA via `ppa set'.
Acts on the PPA at point when in a PPAs list buffer; otherwise prompts.
Prompts for an optional display name and description."
  (interactive
   (list (or (deb-packaging-infra--ppa-at-point)
             (let ((ppas (deb-packaging-infra--list-ppas)))
               (completing-read "PPA to configure: " ppas nil nil)))))
  (let ((displayname (read-string "Display name (blank to skip): "))
        (description (read-string "Description (blank to skip): ")))
    (let* ((args (append (list "ppa" "set" name)
                        (unless (string-empty-p displayname)
                          (list "--displayname" displayname))
                        (unless (string-empty-p description)
                          (list "--description" description))))
           (cmd (mapconcat #'shell-quote-argument args " ")))
      (if (= (length args) 3)
          (message "No configuration changes specified")
        (when (yes-or-no-p (format "Run: %s? " cmd))
          (compile cmd))))))

(defun deb-packaging-infra-show-ppa (&optional name)
  "Show configuration info for a Launchpad PPA via `ppa show'.
Acts on the PPA at point when in a PPAs list buffer; otherwise prompts."
  (interactive
   (list (or (deb-packaging-infra--ppa-at-point)
             (let ((ppas (deb-packaging-infra--list-ppas)))
               (completing-read "PPA to show: " ppas nil nil)))))
  (unless (string-empty-p name)
    (compile (format "ppa show %s" (shell-quote-argument name)))))

;;; PPA list buffer

(defvar-keymap deb-packaging-infra-ppas-mode-map
  :doc "Keymap for the PPAs list buffer.
\\{deb-packaging-infra-ppas-mode-map}"
  :parent magit-section-mode-map
  "s" #'deb-packaging-infra-show-ppa
  "d" #'deb-packaging-infra-destroy-ppa
  "e" #'deb-packaging-infra-set-ppa-config
  "c" #'deb-packaging-infra-create-ppa
  "g" #'deb-packaging-infra-refresh-ppas
  "q" #'quit-window)

(define-derived-mode deb-packaging-infra-ppas-mode magit-section-mode
  "Infra-PPAs"
  "Major mode for listing and managing Launchpad PPAs."
  :group 'deb-packaging)

(defun deb-packaging-infra--ppa-at-point ()
  "Return the PPA name at point, or nil."
  (when-let ((section (magit-current-section)))
    (when (eq (oref section type) 'deb-packaging-infra-ppa)
      (oref section value))))

(defun deb-packaging-infra-refresh-ppas ()
  "Refresh the PPAs list buffer."
  (interactive)
  (when (derived-mode-p 'deb-packaging-infra-ppas-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((ppas (deb-packaging-infra--list-ppas)))
        (magit-insert-section (deb-packaging-infra-root)
          (insert (propertize (format "Launchpad PPAs (%d)\n\n" (length ppas))
                              'font-lock-face 'magit-section-heading))
          (if (null ppas)
              (insert (propertize
                       "No PPAs found.\nCreate one with 'c'.\n"
                       'font-lock-face 'shadow))
            (insert (propertize "PPA\n" 'font-lock-face 'shadow))
            (insert (propertize (make-string 40 ?-) 'font-lock-face 'shadow)
                    "\n")
            (dolist (ppa ppas)
              (magit-insert-section (deb-packaging-infra-ppa ppa)
                (magit-insert-heading
                  (propertize ppa
                              'font-lock-face 'magit-section-heading))))))))
    (when magit-root-section
      (magit-section-show magit-root-section))))

(defun deb-packaging-infra-ppas ()
  "Open a buffer listing all Launchpad PPAs."
  (interactive)
  (let ((buf (get-buffer-create "*deb-packaging infra: PPAs*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'deb-packaging-infra-ppas-mode)
        (deb-packaging-infra-ppas-mode))
      (deb-packaging-infra-refresh-ppas))
    (pop-to-buffer buf)))

;;; Infrastructure dispatch

(defun deb-packaging-infra--header ()
  "Header for infrastructure transient."
  (format "Infrastructure Management\nDistro: %s" deb-packaging-target-distro))

(transient-define-prefix deb-packaging-infra-dispatch ()
  "Manage build and test infrastructure."
  [:description deb-packaging-infra--header]
  ["Infrastructure"
   ("s" "Schroots (sbuild)..."          deb-packaging-infra-schroots)
   ("l" "LXD images (autopkgtest)..."   deb-packaging-infra-lxd-images)
   ("v" "QEMU images (autopkgtest)..."  deb-packaging-infra-qemu-images)
   ("p" "PPAs (Launchpad)..."           deb-packaging-infra-ppas)]
  ["Navigation"
   ("q" "Back" transient-quit-one)])

(provide 'deb-packaging-infra)
;;; deb-packaging-infra.el ends here
