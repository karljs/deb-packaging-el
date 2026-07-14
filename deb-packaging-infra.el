;;; deb-packaging-infra.el --- Infrastructure management for deb-packaging -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging

;;; Commentary:

;; Manage schroots, LXD containers, QEMU images, and Launchpad PPAs.
;; Each type has its own `tabulated-list-mode' buffer with sortable
;; columns and a common key set (c/d/g/q).

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'transient)
(require 'deb-packaging-config)
(require 'deb-packaging-dev)

;;; Shared table helpers

(defun deb-packaging-infra--format-cell (value width &optional align face help-echo)
  "Format VALUE as a table cell of display WIDTH.
ALIGN is `left' or `right' (default `left').  FACE styles the text.
When VALUE is truncated and HELP-ECHO is non-nil, use it as `help-echo'
(t means use VALUE)."
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

(defun deb-packaging-infra--read-name (prompt list-fn)
  "Return the name at point, or prompt with PROMPT from (LIST-FN).
LIST-FN returns a list of plists with :name keys."
  (or (plist-get (tabulated-list-get-id) :name)
      (let* ((items (funcall list-fn))
             (names (mapcar (lambda (s) (plist-get s :name)) items)))
        (completing-read prompt names nil t))))

(defun deb-packaging-infra--read-entry (prompt &optional type-filter)
  "Return the LXD entry (tabulated-list id plist) at point, or prompt with PROMPT.
TYPE-FILTER (e.g. `container') restricts completion to that type and
signals `user-error' if none exist."
  (or (tabulated-list-get-id)
      (let* ((all (deb-packaging-infra--list-lxd-all))
             (candidates (if type-filter
                             (cl-remove-if-not
                              (lambda (e) (eq (plist-get e :type) type-filter))
                              all)
                           all))
             (names (mapcar (lambda (e) (plist-get e :name)) candidates)))
        (when (null names)
          (user-error (if (eq type-filter 'container)
                          "No dev containers found"
                        "No LXD images or containers found")))
        (let ((name (completing-read prompt names nil t)))
          (cl-find name all
                   :key (lambda (e) (plist-get e :name))
                   :test #'equal)))))

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
  "Create a schroot with mk-sbuild."
  (interactive)
  (let* ((distro (read-string "Distro: " deb-packaging-target-distro))
         (arch (completing-read "Arch: " '("amd64" "i386" "arm64" "armhf") nil t "amd64"))
         (cmd (format "mk-sbuild --arch=%s %s" arch distro)))
    (when (yes-or-no-p (format "Run: %s? " cmd))
      (compile cmd))))

(defun deb-packaging-infra-update-schroot (&optional name)
  "Update a schroot with sbuild-update.
Use schroot at point, or prompt."
  (interactive
   (list (deb-packaging-infra--read-name
          "Schroot to update: " #'deb-packaging-infra--list-schroots)))
  (compile (format "sbuild-update -udcar %s" (shell-quote-argument name))))

(defun deb-packaging-infra-end-sessions ()
  "End all active schroot sessions."
  (interactive)
  (compile "schroot -e --all-sessions"))

(defun deb-packaging-infra-delete-schroot (&optional name)
  "Delete a schroot (config and directory).
Use schroot at point, or prompt."
  (interactive
   (list (deb-packaging-infra--read-name
          "Schroot to delete: " #'deb-packaging-infra--list-schroots)))
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
  "e" #'deb-packaging-infra-end-sessions
  "d" #'deb-packaging-infra-delete-schroot
  "c" #'deb-packaging-infra-create-schroot
  "g" #'deb-packaging-infra-refresh-schroots
  "q" #'quit-window)

(define-derived-mode deb-packaging-infra-schroots-mode tabulated-list-mode "Infra-Schroots"
  "Major mode for listing and managing schroots."
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
    (pop-to-buffer buf)))

;;; LXD Management (autopkgtest images and dev containers)

(defun deb-packaging-infra--list-lxd-images ()
  "Return autopkgtest LXD image plists from `lxc image list'.
Keys: :alias, :fingerprint, :description, :arch, :size."
  (require 'json)
  (let ((output (deb-packaging--call-process-string
                 "lxc" "image" "list" "--format=json")))
    (when (and output (not (string-empty-p output)))
      (let ((data (json-read-from-string output)))
        (cl-remove-if-not
         #'identity
         (mapcar
          (lambda (img)
            (let* ((aliases (cdr (assoc-string "aliases" img)))
                   (alias (and (arrayp aliases) (> (length aliases) 0)
                               (cdr (assoc-string "name" (aref aliases 0))))))
              (when (and alias (string-match-p "autopkgtest" alias))
                (list :alias alias
                      :fingerprint (cdr (assoc-string "fingerprint" img))
                      :description (cdr (assoc-string "description" img))
                      :arch (cdr (assoc-string "architecture" img))
                      :size (cdr (assoc-string "size" img))))))
          data))))))

(defun deb-packaging-infra--list-lxd-all ()
  "Return LXD images and dev containers as plists.
Each plist has :name, :type, :status, and type-specific keys."
  (append
   (mapcar (lambda (img)
             (list :name (plist-get img :alias)
                   :type 'image
                   :status (plist-get img :arch)
                   :detail (plist-get img :size)
                   :raw img))
           (deb-packaging-infra--list-lxd-images))
   (mapcar (lambda (c)
             (let* ((name (plist-get c :name))
                    (rest (replace-regexp-in-string "^deb-dev-" "" name))
                    (parts (split-string rest "-"))
                    (release (car (last parts)))
                    (pkg (mapconcat #'identity (butlast parts) "-")))
               (list :name name
                     :type 'container
                     :status (plist-get c :status)
                     :detail (format "%s / %s" pkg release)
                     :raw c)))
           (deb-packaging-dev--list-containers))))

(defun deb-packaging-infra-create-lxd ()
  "Create an autopkgtest LXD image."
  (interactive)
  (let* ((distro (read-string "Distro: " deb-packaging-target-distro))
         (arch (completing-read "Arch: " '("amd64" "arm64") nil t "amd64"))
         (cmd (format "autopkgtest-build-lxd ubuntu-daily:%s/%s" distro arch)))
    (when (yes-or-no-p (format "Run: %s? " cmd))
      (compile cmd))))

(defun deb-packaging-infra-delete-lxd-entry (&optional entry)
  "Delete the LXD image or container at point.
ENTRY is a plist from `deb-packaging-infra--list-lxd-all'."
  (interactive
   (list (deb-packaging-infra--read-entry "Delete: ")))
  (let ((name (plist-get entry :name))
        (type (plist-get entry :type)))
    (when (yes-or-no-p
           (format "Delete %s %s? "
                   (if (eq type 'image) "image" "container") name))
      (compile
       (if (eq type 'image)
           (format "lxc image delete %s" (shell-quote-argument name))
         (format "lxc delete --force %s" (shell-quote-argument name)))))))

(defun deb-packaging-infra-visit-lxd-entry (&optional entry)
  "Open dired for the LXD container at point.
Images are ignored."
  (interactive
   (list (deb-packaging-infra--read-entry "Visit container: " 'container)))
  (if (not (eq (plist-get entry :type) 'container))
      (message "Only dev containers can be visited")
    (let* ((raw (plist-get entry :raw))
           (source (plist-get raw :source))
           (name (plist-get entry :name))
           (mount (or source "/root/work"))
           (tramp-path (format "/lxc:%s:%s" name mount)))
      (deb-packaging-dev--ensure-tramp-method)
      (dired tramp-path))))

(defun deb-packaging-infra-stop-lxd-entry (&optional entry)
  "Stop the LXD container at point.
No-op for images."
  (interactive
   (list (deb-packaging-infra--read-entry "Stop container: " 'container)))
  (if (not (eq (plist-get entry :type) 'container))
      (message "Cannot stop an image")
    (let ((name (plist-get entry :name)))
      (message "Stopping %s..." name)
      (call-process "lxc" nil nil nil "stop" name)
      (message "Stopped %s" name)
      (deb-packaging-infra-refresh-lxd))))

(defun deb-packaging-infra-start-lxd-entry (&optional entry)
  "Start the LXD container at point.
No-op for images."
  (interactive
   (list (deb-packaging-infra--read-entry "Start container: " 'container)))
  (if (not (eq (plist-get entry :type) 'container))
      (message "Cannot start an image")
    (let ((name (plist-get entry :name)))
      (message "Starting %s..." name)
      (call-process "lxc" nil nil nil "start" name)
      (message "Started %s" name)
      (deb-packaging-infra-refresh-lxd))))

(defun deb-packaging-infra-shell-lxd-entry (&optional entry)
  "Open a shell in the LXD container at point.
Runs `lxc exec NAME -- bash -l' in a comint buffer."
  (interactive
   (list (deb-packaging-infra--read-entry "Shell into container: " 'container)))
  (if (not (eq (plist-get entry :type) 'container))
      (message "Cannot shell into an image")
    (let ((name (plist-get entry :name)))
      (call-process "lxc" nil nil nil "start" name)
      (deb-packaging-dev--ensure-tramp-method)
      (let ((buf (make-comint (format "lxc:%s" name) "lxc" nil
                              "exec" name "--" "bash" "-l")))
        (pop-to-buffer buf)))))

;;; LXD list buffer

(defvar-keymap deb-packaging-infra-lxd-mode-map
  :doc "Keymap for the LXD list buffer."
  :parent tabulated-list-mode-map
  "d" #'deb-packaging-infra-delete-lxd-entry
  "s" #'deb-packaging-infra-stop-lxd-entry
  "S" #'deb-packaging-infra-start-lxd-entry
  "x" #'deb-packaging-infra-shell-lxd-entry
  "RET" #'deb-packaging-infra-visit-lxd-entry
  "c" #'deb-packaging-infra-create-lxd
  "g" #'deb-packaging-infra-refresh-lxd
  "q" #'quit-window)

(define-derived-mode deb-packaging-infra-lxd-mode tabulated-list-mode "Infra-LXD"
  "Major mode for listing LXD images and dev containers."
  (setq tabulated-list-format
        [("Name" 35 t)
         ("Type" 12 t)
         ("Status" 10 t)
         ("Details" 25 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil))

(defun deb-packaging-infra-refresh-lxd ()
  "Refresh the LXD list buffer."
  (interactive)
  (when (derived-mode-p 'deb-packaging-infra-lxd-mode)
    (setq tabulated-list-entries
          (mapcar (lambda (e)
                    (let ((type-str (if (eq (plist-get e :type) 'image)
                                        "Image"
                                      "Container")))
                      (list e
                            (vector
                             (deb-packaging-infra--format-cell
                              (plist-get e :name) 35 'left
                              'magit-section-heading)
                             (deb-packaging-infra--format-cell
                              type-str 12)
                             (deb-packaging-infra--format-cell
                              (or (plist-get e :status) "") 10)
                             (deb-packaging-infra--format-cell
                              (or (plist-get e :detail) "") 25 nil 'shadow t)))))
                  (deb-packaging-infra--list-lxd-all)))
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (when (null tabulated-list-entries)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize
                 "\nNo LXD images or dev containers found.\nCreate an image with 'c'."
                 'face 'shadow))))))

(defun deb-packaging-infra-lxd ()
  "Open a buffer listing all LXD images and dev containers."
  (interactive)
  (let ((buf (get-buffer-create "*deb-packaging infra: LXD*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'deb-packaging-infra-lxd-mode)
        (deb-packaging-infra-lxd-mode))
      (deb-packaging-infra-refresh-lxd))
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
Use image at point, or prompt."
  (interactive
   (list (deb-packaging-infra--read-name
          "Image to delete: " #'deb-packaging-infra--list-qemu-images)))
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
  "Major mode for listing QEMU autopkgtest images."
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
    (pop-to-buffer buf)))

;;; PPA (Launchpad) Management

(defvar deb-packaging-infra-ppa-team-config-dir
  "~/.config/ppa-dev-tools/teams"
  "Directory of per-team `ppa' config files.
YAML files (`.yml'/`.yaml') with a `list' section containing
`owner_name'.  Used via `ppa list -C <file>'.  Set in your init file.")

(defun deb-packaging-infra--team-config-files ()
  "Return per-team `ppa' config files.
Files in `deb-packaging-infra-ppa-team-config-dir' matching `.yml' or
`.yaml'.  Nil if the directory is missing or empty."
  (let ((dir (expand-file-name deb-packaging-infra-ppa-team-config-dir)))
    (when (file-directory-p dir)
      (directory-files dir 'full "\\.ya?ml\\'"))))

(defun deb-packaging-infra--parse-ppa-lines (output)
  "Extract \"ppa:owner/name\" entries from `ppa' OUTPUT.
Return PPA address strings in order."
  (let (result)
    (dolist (line (split-string output "\n" t))
      (when (string-match "\\(ppa:[^ \t]+/[^ \t]+\\)" line)
        (push (match-string 1 line) result)))
    (nreverse result)))

(defvar deb-packaging-infra--ppa-cache nil
  "Session cache for `deb-packaging-infra--list-ppas'.
Cons of (PPAS . FETCHED-AT-FLOAT-TIME), or nil.")

(defvar deb-packaging-infra--ppa-cache-ttl 300
  "Seconds before the PPA cache is stale and refetched synchronously.")

(defun deb-packaging-infra--invalidate-ppa-cache ()
  "Clear the PPA cache.  Call after creating or deleting a PPA."
  (setq deb-packaging-infra--ppa-cache nil))

(defun deb-packaging-infra--fetch-ppas-sync ()
  "Fetch and cache PPA names for the user and configured teams synchronously.
Personal PPAs first, then team-config PPAs.  Blocks on `ppa list'; team
config failures are warned and skipped."
  (let ((result (deb-packaging-infra--parse-ppa-lines
                 (shell-command-to-string "ppa list 2>/dev/null"))))
    (dolist (cfg (deb-packaging-infra--team-config-files))
      (condition-case err
          (let ((output (shell-command-to-string
                         (format "ppa list -C %s 2>/dev/null"
                                 (shell-quote-argument cfg)))))
            (dolist (line (deb-packaging-infra--parse-ppa-lines output))
              (cl-pushnew line result :test #'string=)))
        (error (message "ppa team config %s failed: %s" cfg err))))
    (setq result (nreverse result))
    (setq deb-packaging-infra--ppa-cache (cons result (float-time)))
    result))

(defun deb-packaging-infra--list-ppas ()
  "Return PPA names for the user and configured teams.
Cached for `deb-packaging-infra--ppa-cache-ttl' seconds; blocks on the
first call and after the TTL expires."
  (if (and deb-packaging-infra--ppa-cache
           (< (- (float-time) (cdr deb-packaging-infra--ppa-cache))
              deb-packaging-infra--ppa-cache-ttl))
      (car deb-packaging-infra--ppa-cache)
    (deb-packaging-infra--fetch-ppas-sync)))

(defun deb-packaging-infra--ppa-owner (ppa)
  "Return the owner part of PPA string PPA."
  (when (string-match "\\`ppa:\\([^/]+\\)" ppa)
    (match-string 1 ppa)))

(defun deb-packaging-infra--ppa-name (ppa)
  "Return the name part of PPA string PPA."
  (when (string-match "\\`ppa:[^/]+/\\(.+\\)" ppa)
    (match-string 1 ppa)))

(defun deb-packaging-infra-create-ppa ()
  "Create a Launchpad PPA via `ppa create'."
  (interactive)
  (let* ((name (read-string "PPA name to create: "))
         (cmd (format "ppa create %s" (shell-quote-argument name))))
    (when (and (not (string-empty-p name))
               (yes-or-no-p (format "Run: %s? " cmd)))
      (deb-packaging-infra--invalidate-ppa-cache)
      (compile cmd))))

(defun deb-packaging-infra-delete-ppa (&optional name)
  "Delete a Launchpad PPA via `ppa destroy'.
Use PPA at point, or prompt."
  (interactive
   (list (or (tabulated-list-get-id)
             (let ((ppas (deb-packaging-infra--list-ppas)))
               (completing-read "PPA to delete: " ppas nil nil)))))
  (when (and (not (string-empty-p name))
             (yes-or-no-p (format "Really delete PPA %s? " name)))
    (deb-packaging-infra--invalidate-ppa-cache)
    (compile (format "ppa destroy %s" (shell-quote-argument name)))))

(defun deb-packaging-infra-set-ppa-config (&optional name)
  "Configure a Launchpad PPA via `ppa set'.
Use PPA at point, or prompt.  Prompts for display name and description."
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
  "Show Launchpad PPA info via `ppa show'.
Use PPA at point, or prompt."
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
  "Major mode for listing Launchpad PPAs."
  (setq tabulated-list-format
        [("Owner" 25 t)
         ("Name" 40 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil))

(defun deb-packaging-infra--make-ppa-entry (ppa)
  "Build a tabulated-list entry for PPA address PPA."
  (list ppa
        (vector
         (deb-packaging-infra--format-cell
          (deb-packaging-infra--ppa-owner ppa) 25 'left
          'magit-section-heading ppa)
         (deb-packaging-infra--format-cell
          (deb-packaging-infra--ppa-name ppa) 40 nil nil ppa))))

(defvar-local deb-packaging-infra--ppa-processes nil
  "In-flight async `ppa list' processes for the PPAs buffer.")

(defun deb-packaging-infra--cancel-ppa-processes ()
  "Cancel in-flight async PPA listing processes."
  (dolist (proc deb-packaging-infra--ppa-processes)
    (when (process-live-p proc)
      (delete-process proc)))
  (setq deb-packaging-infra--ppa-processes nil))

(defun deb-packaging-infra--append-ppa (ppa)
  "Append PPA to `tabulated-list-entries' if not already present."
  (unless (assoc ppa tabulated-list-entries)
    (setq tabulated-list-entries
          (append tabulated-list-entries
                  (list (deb-packaging-infra--make-ppa-entry ppa))))))

(defun deb-packaging-infra--finalize-ppas (buf)
  "Reprint the PPAs table in BUF, showing empty-state once all fetches finish."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (tabulated-list-init-header)
      (tabulated-list-print t)
      (when (and (null deb-packaging-infra--ppa-processes)
                 (null tabulated-list-entries))
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (propertize "\nNo PPAs found.\nCreate one with 'c'."
                              'face 'shadow)))))))

(defun deb-packaging-infra--ppa-list-sentinel (buf temp-buf)
  "Return a sentinel for an async `ppa list' process.
BUF is the PPAs list buffer; TEMP-BUF holds output."
  (lambda (proc _event)
    (let ((status (process-status proc)))
      (when (memq status '(exit failed))
        (unwind-protect
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (setq deb-packaging-infra--ppa-processes
                      (delq proc deb-packaging-infra--ppa-processes))
                (when (eq status 'exit)
                  (let ((output (with-current-buffer temp-buf
                                  (buffer-string))))
                    (dolist (ppa (deb-packaging-infra--parse-ppa-lines output))
                      (deb-packaging-infra--append-ppa ppa))))
                (deb-packaging-infra--finalize-ppas buf)))
          (when (buffer-live-p temp-buf)
            (kill-buffer temp-buf)))))))

(defun deb-packaging-infra--show-ppas-loading-message ()
  "Show a loading message in the PPAs list buffer while async fetches run."
  (setq tabulated-list-entries nil)
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize "\nLoading PPAs..." 'face 'shadow))))

(defun deb-packaging-infra-refresh-ppas ()
  "Refresh the PPAs list buffer asynchronously so Emacs does not block."
  (interactive)
  (when (derived-mode-p 'deb-packaging-infra-ppas-mode)
    (deb-packaging-infra--cancel-ppa-processes)
    (deb-packaging-infra--show-ppas-loading-message)
    (let ((buf (current-buffer)))
      (dolist (cfg (cons nil (deb-packaging-infra--team-config-files)))
        (let* ((args (if cfg
                         (list "ppa" "list" "-C" cfg)
                       (list "ppa" "list")))
               (temp-buf (generate-new-buffer " *ppa-list*"))
               (proc (make-process
                      :name "ppa-list"
                      :buffer temp-buf
                      :command args
                      :noquery t
                      :sentinel (deb-packaging-infra--ppa-list-sentinel
                                 buf temp-buf))))
          (push proc deb-packaging-infra--ppa-processes))))))

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
   ("l" "LXD (images + dev containers)..." deb-packaging-infra-lxd)
   ("v" "QEMU images (autopkgtest)..."  deb-packaging-infra-qemu-images)
   ("p" "PPAs (Launchpad)..."           deb-packaging-infra-ppas)]
  ["Navigation"
   ("q" "Back" transient-quit-one)])

(provide 'deb-packaging-infra)
;;; deb-packaging-infra.el ends here
