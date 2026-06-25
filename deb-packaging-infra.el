;;; deb-packaging-infra.el --- Infrastructure management for deb-packaging -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karl Smeltzer
;; Author: Karl Smeltzer
;; Keywords: tools, debian, ubuntu, packaging
;; Package-Requires: ((emacs "28.1") (transient "0.4.0"))

;;; Commentary:

;; Manage schroots, LXD containers, QEMU images, and Launchpad PPAs.
;;
;; Each infrastructure type has its own dedicated `tabulated-list-mode'
;; buffer where entries are shown as sortable, aligned columns.  Each
;; buffer gives a consistent set of keys (c/d/g/q) plus a few
;; type-specific actions.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'transient)
(require 'deb-packaging-presets)

;;; Shared table helpers

(defun deb-packaging-infra--format-cell (value width &optional align face help-echo)
  "Format VALUE as a table cell of display WIDTH.
ALIGN is `left' or `right' (default `left').  FACE is applied to the
visible text.  If HELP-ECHO is non-nil and VALUE must be truncated,
attach HELP-ECHO as `help-echo'.  If HELP-ECHO is t, use VALUE itself
as the tooltip."
  (let* ((str (if value (format "%s" value) ""))
         (sw (string-width str))
         (cell (if (eq align 'right)
                   (if (> sw width)
                       (truncate-string-to-width str width nil nil "…")
                     (concat (make-string (max 0 (- width sw)) ?\s) str))
                 (truncate-string-to-width str width nil ?\s "…")))
         (tip (when (and help-echo (> sw width))
                (if (eq help-echo t) str help-echo))))
    (when face
      (put-text-property 0 (length cell) 'face face cell))
    (when tip
      (put-text-property 0 (length cell) 'help-echo tip cell))
    cell))

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
   (list (or (plist-get (tabulated-list-get-id) :name)
             (let* ((schroots (deb-packaging-infra--list-schroots))
                    (names (mapcar (lambda (s) (plist-get s :name)) schroots)))
               (completing-read "Schroot to update: " names nil t)))))
  (compile (format "sbuild-update -udcar %s" (shell-quote-argument name))))

(defun deb-packaging-infra-delete-schroot (&optional name)
  "Delete a schroot (config and directory).
Acts on the schroot at point when in a schroots list buffer; otherwise
prompts for one."
  (interactive
   (list (or (plist-get (tabulated-list-get-id) :name)
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
  :doc "Keymap for the schroots list buffer."
  :parent tabulated-list-mode-map
  "u" #'deb-packaging-infra-update-schroot
  "d" #'deb-packaging-infra-delete-schroot
  "c" #'deb-packaging-infra-create-schroot
  "g" #'deb-packaging-infra-refresh-schroots
  "q" #'quit-window)

(define-derived-mode deb-packaging-infra-schroots-mode tabulated-list-mode "Infra-Schroots"
  "Major mode for listing and managing schroots."
  :group 'deb-packaging
  (setq tabulated-list-format
        [("Name" 25 t)
         ("Description" 25 t)
         ("Directory" 50 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil))

(defun deb-packaging-infra-refresh-schroots ()
  "Refresh the schroots list buffer."
  (interactive)
  (when (derived-mode-p 'deb-packaging-infra-schroots-mode)
    (setq tabulated-list-entries
          (mapcar (lambda (schroot)
                    (list schroot
                          (vector
                           (deb-packaging-infra--format-cell
                            (plist-get schroot :name) 25 'left
                            'magit-section-heading)
                           (deb-packaging-infra--format-cell
                            (plist-get schroot :description) 25)
                           (deb-packaging-infra--format-cell
                            (plist-get schroot :directory) 50 nil 'shadow t))))
                  (deb-packaging-infra--list-schroots)))
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (when (null tabulated-list-entries)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize "\nNo schroots found.\nCreate one with 'c'."
                            'face 'shadow))))))

(defun deb-packaging-infra-schroots ()
  "Open a buffer listing all schroots."
  (interactive)
  (let ((buf (get-buffer-create "*deb-packaging infra: Schroots*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'deb-packaging-infra-schroots-mode)
        (deb-packaging-infra-schroots-mode))
      (deb-packaging-infra-refresh-schroots))
    (switch-to-buffer buf)))

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
   (list (or (plist-get (tabulated-list-get-id) :alias)
             (let* ((images (deb-packaging-infra--list-lxd-images))
                    (aliases (mapcar (lambda (i) (plist-get i :alias)) images)))
               (completing-read "Image to delete: " aliases nil t)))))
  (when (yes-or-no-p (format "Delete LXD image %s?" alias))
    (compile (format "lxc image delete %s" (shell-quote-argument alias)))))

;;; LXD list buffer

(defvar-keymap deb-packaging-infra-lxd-images-mode-map
  :doc "Keymap for the LXD images list buffer."
  :parent tabulated-list-mode-map
  "d" #'deb-packaging-infra-delete-lxd
  "c" #'deb-packaging-infra-create-lxd
  "g" #'deb-packaging-infra-refresh-lxd-images
  "q" #'quit-window)

(define-derived-mode deb-packaging-infra-lxd-images-mode tabulated-list-mode "Infra-LXD"
  "Major mode for listing and managing LXD autopkgtest images."
  :group 'deb-packaging
  (setq tabulated-list-format
        [("Alias" 40 t)
         ("Arch" 8 t)
         ("Size" 12 t :right-align t)
         ("Fingerprint" 30 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil))

(defun deb-packaging-infra-refresh-lxd-images ()
  "Refresh the LXD images list buffer."
  (interactive)
  (when (derived-mode-p 'deb-packaging-infra-lxd-images-mode)
    (setq tabulated-list-entries
          (mapcar (lambda (img)
                    (list img
                          (vector
                           (deb-packaging-infra--format-cell
                            (plist-get img :alias) 40 'left
                            'magit-section-heading)
                           (deb-packaging-infra--format-cell
                            (plist-get img :arch) 8)
                           (deb-packaging-infra--format-cell
                            (plist-get img :size) 12 'right)
                           (deb-packaging-infra--format-cell
                            (plist-get img :fingerprint) 30 nil 'shadow t))))
                  (deb-packaging-infra--list-lxd-images)))
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (when (null tabulated-list-entries)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize "\nNo autopkgtest LXD images found.\nCreate one with 'c'."
                            'face 'shadow))))))

(defun deb-packaging-infra-lxd-images ()
  "Open a buffer listing all LXD autopkgtest images."
  (interactive)
  (let ((buf (get-buffer-create "*deb-packaging infra: LXD images*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'deb-packaging-infra-lxd-images-mode)
        (deb-packaging-infra-lxd-images-mode))
      (deb-packaging-infra-refresh-lxd-images))
    (switch-to-buffer buf)))

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
   (list (or (plist-get (tabulated-list-get-id) :name)
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
  :doc "Keymap for the QEMU images list buffer."
  :parent tabulated-list-mode-map
  "d" #'deb-packaging-infra-delete-qemu
  "c" #'deb-packaging-infra-create-qemu
  "g" #'deb-packaging-infra-refresh-qemu-images
  "q" #'quit-window)

(define-derived-mode deb-packaging-infra-qemu-images-mode tabulated-list-mode "Infra-QEMU"
  "Major mode for listing and managing QEMU autopkgtest images."
  :group 'deb-packaging
  (setq tabulated-list-format
        [("Name" 45 t)
         ("Size" 12 t :right-align t)
         ("Path" 40 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil))

(defun deb-packaging-infra-refresh-qemu-images ()
  "Refresh the QEMU images list buffer."
  (interactive)
  (when (derived-mode-p 'deb-packaging-infra-qemu-images-mode)
    (setq tabulated-list-entries
          (mapcar (lambda (img)
                    (list img
                          (vector
                           (deb-packaging-infra--format-cell
                            (plist-get img :name) 45 'left
                            'magit-section-heading)
                           (deb-packaging-infra--format-cell
                            (file-size-human-readable (or (plist-get img :size) 0))
                            12 'right)
                           (deb-packaging-infra--format-cell
                            (plist-get img :path) 40 nil 'shadow t))))
                  (deb-packaging-infra--list-qemu-images)))
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (when (null tabulated-list-entries)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize (format "\nNo QEMU images found in %s.\nCreate one with 'c'."
                                    deb-packaging-infra-qemu-dir)
                            'face 'shadow))))))

(defun deb-packaging-infra-qemu-images ()
  "Open a buffer listing all QEMU autopkgtest images."
  (interactive)
  (let ((buf (get-buffer-create "*deb-packaging infra: QEMU images*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'deb-packaging-infra-qemu-images-mode)
        (deb-packaging-infra-qemu-images-mode))
      (deb-packaging-infra-refresh-qemu-images))
    (switch-to-buffer buf)))

;;; PPA (Launchpad) Management

(defun deb-packaging-infra--list-ppas ()
  "Return list of the current user's PPA names via the `ppa' tool.
Each entry is a string of the form \"ppa:owner/name\"."
  (let ((output (shell-command-to-string "ppa list 2>/dev/null"))
        result)
    (dolist (line (split-string output "\n" t))
      (when (string-match "\\(ppa:[^ \t]+/[^ \t]+\\)" line)
        (push (match-string 1 line) result)))
    (nreverse result)))

(defun deb-packaging-infra--ppa-owner (ppa)
  "Return the owner portion of PPA string PPA."
  (when (string-match "\\`ppa:\\([^/]+\\)" ppa)
    (match-string 1 ppa)))

(defun deb-packaging-infra--ppa-name (ppa)
  "Return the name portion of PPA string PPA."
  (when (string-match "\\`ppa:[^/]+/\\(.+\\)" ppa)
    (match-string 1 ppa)))

(defun deb-packaging-infra-create-ppa ()
  "Create a new Launchpad PPA via `ppa create'."
  (interactive)
  (let* ((name (read-string "PPA name to create: "))
         (cmd (format "ppa create %s" (shell-quote-argument name))))
    (when (and (not (string-empty-p name))
               (yes-or-no-p (format "Run: %s? " cmd)))
      (compile cmd))))

(defun deb-packaging-infra-delete-ppa (&optional name)
  "Delete a Launchpad PPA via `ppa destroy'.
Acts on the PPA at point when in a PPAs list buffer; otherwise prompts."
  (interactive
   (list (or (tabulated-list-get-id)
             (let ((ppas (deb-packaging-infra--list-ppas)))
               (completing-read "PPA to delete: " ppas nil nil)))))
  (when (and (not (string-empty-p name))
             (yes-or-no-p (format "Really delete PPA %s? " name)))
    (compile (format "ppa destroy %s" (shell-quote-argument name)))))

(defun deb-packaging-infra-set-ppa-config (&optional name)
  "Apply configuration to a Launchpad PPA via `ppa set'.
Acts on the PPA at point when in a PPAs list buffer; otherwise prompts.
Prompts for an optional display name and description."
  (interactive
   (list (or (tabulated-list-get-id)
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
   (list (or (tabulated-list-get-id)
             (let ((ppas (deb-packaging-infra--list-ppas)))
               (completing-read "PPA to show: " ppas nil nil)))))
  (unless (string-empty-p name)
    (compile (format "ppa show %s" (shell-quote-argument name)))))

;;; PPA list buffer

(defvar-keymap deb-packaging-infra-ppas-mode-map
  :doc "Keymap for the PPAs list buffer."
  :parent tabulated-list-mode-map
  "s" #'deb-packaging-infra-show-ppa
  "d" #'deb-packaging-infra-delete-ppa
  "e" #'deb-packaging-infra-set-ppa-config
  "c" #'deb-packaging-infra-create-ppa
  "g" #'deb-packaging-infra-refresh-ppas
  "q" #'quit-window)

(define-derived-mode deb-packaging-infra-ppas-mode tabulated-list-mode "Infra-PPAs"
  "Major mode for listing and managing Launchpad PPAs."
  :group 'deb-packaging
  (setq tabulated-list-format
        [("Owner" 25 t)
         ("Name" 40 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil))

(defun deb-packaging-infra-refresh-ppas ()
  "Refresh the PPAs list buffer."
  (interactive)
  (when (derived-mode-p 'deb-packaging-infra-ppas-mode)
    (setq tabulated-list-entries
          (mapcar (lambda (ppa)
                    (list ppa
                          (vector
                           (deb-packaging-infra--format-cell
                            (deb-packaging-infra--ppa-owner ppa) 25 'left
                            'magit-section-heading ppa)
                           (deb-packaging-infra--format-cell
                            (deb-packaging-infra--ppa-name ppa) 40 nil nil ppa))))
                  (deb-packaging-infra--list-ppas)))
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (when (null tabulated-list-entries)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize "\nNo PPAs found.\nCreate one with 'c'."
                            'face 'shadow))))))

(defun deb-packaging-infra-ppas ()
  "Open a buffer listing all Launchpad PPAs."
  (interactive)
  (let ((buf (get-buffer-create "*deb-packaging infra: PPAs*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'deb-packaging-infra-ppas-mode)
        (deb-packaging-infra-ppas-mode))
      (deb-packaging-infra-refresh-ppas))
    (switch-to-buffer buf)))

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
