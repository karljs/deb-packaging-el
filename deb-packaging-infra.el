;;; deb-packaging-infra.el --- Infrastructure management for deb-packaging -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Manage schroots, LXD containers, QEMU images, and Launchpad PPAs.
;; Schroots and their active sessions live in a `magit-section-mode'
;; buffer; the other types use `tabulated-list-mode' buffers with
;; sortable columns and a common key set (c/d/g/q).

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'tabulated-list)
(require 'transient)
(require 'deb-packaging-commands)
(require 'deb-packaging-config)
(require 'deb-packaging-dev)
(require 'deb-packaging-transients)
(require 'deb-packaging-display)

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

(defun deb-packaging-infra--list-sessions ()
  "Return names of active schroot sessions."
  (when-let ((output (deb-packaging-detect--call-process-string
                      "schroot" "--list" "--all-sessions")))
    (delq nil
          (mapcar (lambda (line)
                    (when (string-prefix-p "session:" line)
                      (string-remove-prefix "session:" line)))
                  (split-string output "\n" t)))))

(defun deb-packaging-infra--session-chroot (session chroots)
  "Return the name of the chroot SESSION belongs to, or nil.
CHROOTS is a list of plists from `deb-packaging-infra--list-schroots'.
The parent is the longest chroot name that is a prefix of SESSION."
  (let (match)
    (dolist (schroot chroots)
      (let ((name (plist-get schroot :name)))
        (when (and (string-prefix-p name session)
                   (or (null match) (> (length name) (length match))))
          (setq match name))))
    match))

(defun deb-packaging-infra--section-value-at-point (type)
  "Return the value of the section of TYPE at point, or nil."
  (when-let ((section (magit-current-section)))
    (when (eq (oref section type) type)
      (oref section value))))

(defun deb-packaging-infra--session-targets ()
  "Return session names in the active region, at point, or under the heading."
  (or (magit-region-values 'deb-packaging-infra-session)
      (when-let ((section (magit-current-section)))
        (pcase (oref section type)
          ('deb-packaging-infra-session (list (oref section value)))
          ('deb-packaging-infra-sessions
           (mapcar (lambda (s) (oref s value)) (oref section children)))))))

(defun deb-packaging-infra--chroot-targets ()
  "Return names of chroots in the active region or at point."
  (or (magit-region-values 'deb-packaging-infra-chroot)
      (when-let ((name (deb-packaging-infra--section-value-at-point
                        'deb-packaging-infra-chroot)))
        (list name))))

(defun deb-packaging-infra-create-schroot ()
  "Create a schroot with mk-sbuild.
mk-sbuild self-sudos; the comint buffer's pty carries its prompt."
  (interactive)
  (let* ((distro (read-string
                  (format "Distro (default %s): "
                          (deb-packaging-config--effective-distro))
                  nil nil (deb-packaging-config--effective-distro)))
         (arch (completing-read "Arch (default amd64): "
                                '("amd64" "i386" "arm64" "armhf")
                                nil t nil nil "amd64")))
    (when (yes-or-no-p (format "Run: mk-sbuild --arch=%s %s? " arch distro))
      (deb-packaging-infra--run-privileged
       "mk-sbuild"
       (list "mk-sbuild" (format "--arch=%s" arch) distro)
       'deb-packaging-infra-schroots-mode
       #'deb-packaging-infra-refresh-schroots))))

(defun deb-packaging-infra--update-command (names)
  "Return a shell command updating schroots NAMES sequentially.
Chained with \";\" so one failing schroot does not block the rest."
  (mapconcat (lambda (name)
               (format "sbuild-update -udcar %s" (shell-quote-argument name)))
             names "; "))

(defun deb-packaging-infra-update-schroots (&optional names)
  "Update schroots with sbuild-update.
Schroots in the active region, the schroot at point, or prompt for one.
NAMES, when given, is a list of schroot names to update."
  (interactive)
  (let ((targets (or names
                     (deb-packaging-infra--chroot-targets)
                     (let ((schroots (mapcar (lambda (s) (plist-get s :name))
                                             (deb-packaging-infra--list-schroots))))
                       (unless schroots
                         (user-error "No schroots found"))
                       (list (completing-read
                              "Schroot to update: " schroots nil t))))))
    (when (y-or-n-p (if (= (length targets) 1)
                        (format "Update schroot %s? " (car targets))
                      (format "Update %d schroots? " (length targets))))
      (deb-packaging-commands--compile
       (deb-packaging-infra--update-command targets)))))

(defun deb-packaging-infra-update-all-schroots ()
  "Update all schroots with sbuild-update."
  (interactive)
  (let ((names (mapcar (lambda (s) (plist-get s :name))
                       (deb-packaging-infra--list-schroots))))
    (if (null names)
        (message "No schroots found")
      (deb-packaging-infra-update-schroots names))))

(defun deb-packaging-infra--end-session (name)
  "End schroot session NAME, messaging the outcome."
  (if (zerop (call-process "schroot" nil nil nil "-e" "-c" name))
      (message "Ended session %s" name)
    (message "Failed to end session %s" name)))

(defun deb-packaging-infra--end-session-list (names)
  "End schroot sessions NAMES after confirmation, then refresh."
  (if (null names)
      (message "No schroot sessions to end")
    (when (y-or-n-p (if (= (length names) 1)
                        (format "End schroot session %s? " (car names))
                      (format "End %d schroot sessions? " (length names))))
      (dolist (name names)
        (deb-packaging-infra--end-session name))
      (deb-packaging-commands--refresh-buffer
       'deb-packaging-infra-schroots-mode
       #'deb-packaging-infra-refresh-schroots))))

(defun deb-packaging-infra-end-sessions ()
  "End schroot sessions in the active region, at point, or under the heading."
  (interactive)
  (deb-packaging-infra--end-session-list (deb-packaging-infra--session-targets)))

(defun deb-packaging-infra-end-all-sessions ()
  "End all active schroot sessions."
  (interactive)
  (deb-packaging-infra--end-session-list (deb-packaging-infra--list-sessions)))

(defun deb-packaging-infra--compile-then-refresh (cmd mode refresh-fn)
  "Run CMD via the compile wrapper; on success refresh the MODE list buffer.
Keeps the row of a deleted item from lingering until a manual `g'."
  (when-let ((buf (deb-packaging-commands--compile cmd)))
    (deb-packaging-commands--after-compile
     buf
     (lambda ()
       (deb-packaging-commands--refresh-buffer mode refresh-fn)))))

(defun deb-packaging-infra--run-privileged (name args mode refresh-fn)
  "Run a privileged command through the comint runner; refresh on success.
ARGS is the command list; interactive sudo and mk-sbuild need a pty for
their password prompts (authd included), which the comint output buffer
provides.  MODE and REFRESH-FN refresh the affected list buffer when the
command exits 0, mirroring `deb-packaging-infra--compile-then-refresh'."
  (let ((buf (deb-packaging-commands--run-command name args)))
    (when-let ((proc (get-buffer-process buf)))
      (deb-packaging-commands--wrap-sentinel
       proc
       (lambda (p _event)
         (when (and (eq (process-status p) 'exit)
                    (zerop (process-exit-status p)))
           (deb-packaging-commands--refresh-buffer mode refresh-fn)))))))

(defun deb-packaging-infra-visit-schroot ()
  "Visit the chroot or session at point.
Chroot row: dired its directory.  Session row: `schroot --info' in an
output buffer.  RET elsewhere is refused rather than silently dead."
  (interactive)
  (let ((section (magit-current-section)))
    (pcase (and section (oref section type))
      ('deb-packaging-infra-chroot
       (let* ((name (oref section value))
              (sc (cl-find name (deb-packaging-infra--list-schroots)
                           :key (lambda (s) (plist-get s :name))
                           :test #'equal))
              (directory (and sc (plist-get sc :directory))))
         (if (and directory (file-directory-p directory))
             (dired directory)
           (message "No directory for chroot %s" name))))
      ('deb-packaging-infra-session
       (deb-packaging-commands--run-command
        "schroot-info" (list "schroot" "--info" "-c" (oref section value))))
      (_ (user-error "Nothing to visit on this line")))))

(defun deb-packaging-infra-delete-schroot (&optional name)
  "Delete a schroot (config and directory).
Use schroot at point, or prompt."
  (interactive)
  (let* ((name (or name
                   (deb-packaging-infra--section-value-at-point
                    'deb-packaging-infra-chroot)
                   (let ((schroots (mapcar (lambda (s) (plist-get s :name))
                                           (deb-packaging-infra--list-schroots))))
                     (unless schroots
                       (user-error "No schroots found"))
                     (completing-read
                      "Schroot to delete: " schroots nil t))))
         (schroots (deb-packaging-infra--list-schroots))
         (sc (cl-find name schroots
                      :key (lambda (s) (plist-get s :name)) :test #'equal))
         (config-file (plist-get sc :config-file))
         (directory (plist-get sc :directory)))
    (if (not directory)
        (message "Could not find directory for schroot %s" name)
      (let ((msg (format "Will delete:\n  Config: %s\n  Directory: %s\n\nProceed?"
                         config-file directory)))
        (when (yes-or-no-p msg)
          ;; One sh -c so both sudo calls share the pty (and its cached
          ;; credential); --run-command shell-quotes per-arg, so && only
          ;; survives inside a single -c string.
          (deb-packaging-infra--run-privileged
           "schroot-delete"
           (list "sh" "-c"
                 (format "sudo rm -rf %s && sudo rm %s"
                         (shell-quote-argument directory)
                         (shell-quote-argument config-file)))
           'deb-packaging-infra-schroots-mode
           #'deb-packaging-infra-refresh-schroots))))))

;;; Schroots buffer

(defvar-keymap deb-packaging-infra-schroots-mode-map
  :doc "Keymap for the schroots buffer.
RET visits a chroot's directory or shows session info; ? lists this
buffer's commands."
  :parent magit-section-mode-map
  "RET" #'deb-packaging-infra-visit-schroot
  "u" #'deb-packaging-infra-update-schroots
  "U" #'deb-packaging-infra-update-all-schroots
  "e" #'deb-packaging-infra-end-sessions
  "E" #'deb-packaging-infra-end-all-sessions
  "d" #'deb-packaging-infra-delete-schroot
  "c" #'deb-packaging-infra-create-schroot
  "g" #'deb-packaging-infra-refresh-schroots
  "?" #'deb-packaging-infra-schroots-dispatch
  "q" #'quit-window)

(define-derived-mode deb-packaging-infra-schroots-mode magit-section-mode "Infra-Schroots"
  "Major mode for listing and managing schroots and their sessions.")

(defun deb-packaging-infra--insert-session-row (session schroots)
  "Insert a row for schroot SESSION, dimming its parent chroot from SCHROOTS."
  (magit-insert-section (deb-packaging-infra-session session)
    (insert "  "
            (deb-packaging-infra--format-cell
             session 32 'left 'magit-section-heading)
            (deb-packaging-infra--format-cell
             (or (deb-packaging-infra--session-chroot session schroots) "")
             32 nil 'shadow)
            "\n")))

(defun deb-packaging-infra--insert-chroot-row (schroot)
  "Insert a row for schroot plist SCHROOT."
  (magit-insert-section (deb-packaging-infra-chroot (plist-get schroot :name))
    (insert "  "
            (deb-packaging-infra--format-cell
             (plist-get schroot :name) 25 'left 'magit-section-heading)
            (deb-packaging-infra--format-cell
             (plist-get schroot :description) 25)
            (deb-packaging-infra--format-cell
             (plist-get schroot :directory) 50 nil 'shadow t)
            "\n")))

(defun deb-packaging-infra-refresh-schroots ()
  "Refresh the schroots buffer."
  (interactive)
  (unless (derived-mode-p 'deb-packaging-infra-schroots-mode)
    (user-error "Not in a schroots buffer"))
  (let ((inhibit-read-only t)
        (schroots (deb-packaging-infra--list-schroots))
        (sessions (deb-packaging-infra--list-sessions))
        (pos (point)))
    (erase-buffer)
    (magit-insert-section (deb-packaging-infra-root)
      (when sessions
        (magit-insert-section (deb-packaging-infra-sessions)
          (magit-insert-heading (format "Sessions (%d)" (length sessions)))
          (dolist (session sessions)
            (deb-packaging-infra--insert-session-row session schroots)))
        (insert "\n"))
      (magit-insert-section (deb-packaging-infra-chroots)
        (magit-insert-heading (format "Chroots (%d)" (length schroots)))
        (dolist (schroot schroots)
          (deb-packaging-infra--insert-chroot-row schroot)))
      (when (and (null schroots) (null sessions))
        (insert (propertize "\nNo schroots found.\nCreate one with 'c'."
                            'face 'shadow))))
    (goto-char (min pos (point-max)))))

(defun deb-packaging-infra-schroots ()
  "Open a buffer listing all schroots."
  (interactive)
  (let ((buf (get-buffer-create "*deb-packaging infra: Schroots*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'deb-packaging-infra-schroots-mode)
        (deb-packaging-infra-schroots-mode))
      (deb-packaging-infra-refresh-schroots))
    (deb-packaging-display-buffer buf 'list)))

;;; LXD Management (autopkgtest images and dev containers)

(defun deb-packaging-infra--list-lxd-images ()
  "Return autopkgtest LXD image plists from `lxc image list'.
Keys: :alias, :fingerprint, :description, :arch, :size."
  (require 'json)
  (let ((output (deb-packaging-detect--call-process-string
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
                    (parts (split-string
                            (string-remove-prefix "deb-dev-" name) "-"))
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
  (let* ((distro (read-string
                  (format "Distro (default %s): "
                          (deb-packaging-config--effective-distro))
                  nil nil (deb-packaging-config--effective-distro)))
         (arch (completing-read "Arch (default amd64): "
                                '("amd64" "arm64") nil t nil nil "amd64"))
         (cmd (format "autopkgtest-build-lxd ubuntu-daily:%s/%s" distro arch)))
    (when (yes-or-no-p (format "Run: %s? " cmd))
      (deb-packaging-commands--compile cmd))))

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
      (deb-packaging-infra--compile-then-refresh
       (if (eq type 'image)
           (format "lxc image delete %s" (shell-quote-argument name))
         (format "lxc delete --force %s" (shell-quote-argument name)))
       'deb-packaging-infra-lxd-mode
       #'deb-packaging-infra-refresh-lxd))))

(defun deb-packaging-infra--container-package (name)
  "Return the package name encoded in a dev container NAME, or nil.
Names look like deb-dev-PKG-DISTRO; PKG itself may contain hyphens."
  (when (string-prefix-p "deb-dev-" name)
    (let ((parts (split-string (string-remove-prefix "deb-dev-" name) "-")))
      (when (cdr parts)
        (mapconcat #'identity (butlast parts) "-")))))

(defun deb-packaging-infra-visit-lxd-entry (&optional entry)
  "Open dired for the LXD container at point.
Images are ignored.  Dev containers mount the source at
`deb-packaging-dev--mount-path'; the device source is the host path,
which does not exist inside the container."
  (interactive
   (list (deb-packaging-infra--read-entry "Visit container: " 'container)))
  (if (not (eq (plist-get entry :type) 'container))
      (message "Only dev containers can be visited")
    (let* ((name (plist-get entry :name))
           (pkg (deb-packaging-infra--container-package name))
           (mount (if pkg
                      (deb-packaging-dev--mount-path pkg)
                    "/root/work"))
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
      (unless (zerop (call-process "lxc" nil nil nil "stop" name))
        (user-error "Failed to stop %s" name))
      (message "Stopped %s" name)
      (deb-packaging-commands--refresh-buffer 'deb-packaging-infra-lxd-mode
                                              #'deb-packaging-infra-refresh-lxd))))

(defun deb-packaging-infra-start-lxd-entry (&optional entry)
  "Start the LXD container at point.
No-op for images."
  (interactive
   (list (deb-packaging-infra--read-entry "Start container: " 'container)))
  (if (not (eq (plist-get entry :type) 'container))
      (message "Cannot start an image")
    (let ((name (plist-get entry :name)))
      (message "Starting %s..." name)
      (unless (zerop (call-process "lxc" nil nil nil "start" name))
        (user-error "Failed to start %s" name))
      (message "Started %s" name)
      (deb-packaging-commands--refresh-buffer 'deb-packaging-infra-lxd-mode
                                              #'deb-packaging-infra-refresh-lxd))))

(defun deb-packaging-infra-shell-lxd-entry (&optional entry)
  "Open a shell in the LXD container at point.
Runs `lxc exec NAME -- bash -l' in a comint buffer."
  (interactive
   (list (deb-packaging-infra--read-entry "Shell into container: " 'container)))
  (if (not (eq (plist-get entry :type) 'container))
      (message "Cannot shell into an image")
    (let ((name (plist-get entry :name)))
      (unless (zerop (call-process "lxc" nil nil nil "start" name))
        (user-error "Failed to start %s" name))
      (deb-packaging-dev--ensure-tramp-method)
      (let ((buf (make-comint (format "lxc:%s" name) "lxc" nil
                              "exec" name "--" "bash" "-l")))
        (with-current-buffer buf
          (setq deb-packaging-display-category 'shell))
        (deb-packaging-display-buffer buf 'shell)))))

;;; LXD list buffer

(defvar-keymap deb-packaging-infra-lxd-mode-map
  :doc "Keymap for the LXD list buffer."
  :parent tabulated-list-mode-map
  "d" #'deb-packaging-infra-delete-lxd-entry
  "k" #'deb-packaging-infra-stop-lxd-entry
  "s" #'deb-packaging-infra-start-lxd-entry
  "x" #'deb-packaging-infra-shell-lxd-entry
  "c" #'deb-packaging-infra-create-lxd
  "g" #'deb-packaging-infra-refresh-lxd
  "?" #'deb-packaging-infra-lxd-dispatch
  "q" #'quit-window)

(defvar deb-packaging-infra-lxd-row-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'deb-packaging-infra-visit-lxd-entry)
    map)
  "Keymap on LXD container rows so RET visits only containers.
Image rows get no binding rather than a pretend action.")

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
  (unless (derived-mode-p 'deb-packaging-infra-lxd-mode)
    (user-error "Not in an LXD buffer"))
  (setq tabulated-list-entries
        (mapcar (lambda (e)
                  (let* ((containerp (eq (plist-get e :type) 'container))
                         (row-cell
                          (lambda (cell)
                            (if containerp
                                (propertize cell 'keymap
                                            deb-packaging-infra-lxd-row-map)
                              cell)))
                         (type-str (if containerp "Container" "Image")))
                    (list e
                          (vector
                           (funcall row-cell
                                    (deb-packaging-infra--format-cell
                                     (plist-get e :name) 35 'left
                                     'magit-section-heading))
                           (funcall row-cell
                                    (deb-packaging-infra--format-cell
                                     type-str 12))
                           (funcall row-cell
                                    (deb-packaging-infra--format-cell
                                     (or (plist-get e :status) "") 10))
                           (funcall row-cell
                                    (deb-packaging-infra--format-cell
                                     (or (plist-get e :detail) "") 25
                                     nil 'shadow t))))))
                (deb-packaging-infra--list-lxd-all)))
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (when (null tabulated-list-entries)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (propertize
               "\nNo LXD images or dev containers found.\nCreate an image with 'c'."
               'face 'shadow)))))

(defun deb-packaging-infra-lxd ()
  "Open a buffer listing all LXD images and dev containers."
  (interactive)
  (let ((buf (get-buffer-create "*deb-packaging infra: LXD*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'deb-packaging-infra-lxd-mode)
        (deb-packaging-infra-lxd-mode))
      (deb-packaging-infra-refresh-lxd))
    (deb-packaging-display-buffer buf 'list)))

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
  "Create a QEMU image for autopkgtest.
The image dir is often root-owned; sudo (with its prompt in the comint
buffer) is used only when it is not user-writable."
  (interactive)
  (let* ((distro (read-string
                  (format "Distro (default %s): "
                          (deb-packaging-config--effective-distro))
                  nil nil (deb-packaging-config--effective-distro)))
         (arch (completing-read "Arch (default amd64): "
                                '("amd64" "arm64" "i386") nil t nil nil "amd64"))
         (sudo-p (not (file-writable-p deb-packaging-infra-qemu-dir))))
    (when (yes-or-no-p
           (format "Run: %sautopkgtest-buildvm-ubuntu-cloud -r %s -a %s -o %s? "
                   (if sudo-p "sudo " "") distro arch
                   deb-packaging-infra-qemu-dir))
      (deb-packaging-infra--run-privileged
       "qemu-create"
       (let ((args (list "autopkgtest-buildvm-ubuntu-cloud"
                         "-r" distro "-a" arch "-o"
                         deb-packaging-infra-qemu-dir)))
         (if sudo-p (cons "sudo" args) args))
       'deb-packaging-infra-qemu-images-mode
       #'deb-packaging-infra-refresh-qemu-images))))

(defun deb-packaging-infra-delete-qemu (&optional name)
  "Delete a QEMU autopkgtest image.
Use image at point, or prompt.  Sudo (prompt answered in the comint
buffer) only when the image is not user-writable."
  (interactive
   (list (or (plist-get (tabulated-list-get-id) :name)
             (completing-read
              "Image to delete: "
              (mapcar (lambda (s) (plist-get s :name))
                      (deb-packaging-infra--list-qemu-images))
              nil t))))
  (let* ((images (deb-packaging-infra--list-qemu-images))
         (img (cl-find name images
                       :key (lambda (i) (plist-get i :name)) :test #'equal))
         (path (plist-get img :path)))
    (when (yes-or-no-p (format "Delete %s?" path))
      (deb-packaging-infra--run-privileged
       "qemu-delete"
       (if (file-writable-p path)
           (list "rm" path)
         (list "sudo" "rm" path))
       'deb-packaging-infra-qemu-images-mode
       #'deb-packaging-infra-refresh-qemu-images))))

;;; QEMU list buffer

(defun deb-packaging-infra-visit-qemu-dir ()
  "Open dired on the QEMU autopkgtest image directory."
  (interactive)
  (if (file-directory-p deb-packaging-infra-qemu-dir)
      (dired deb-packaging-infra-qemu-dir)
    (message "No image directory %s" deb-packaging-infra-qemu-dir)))

(defvar-keymap deb-packaging-infra-qemu-images-mode-map
  :doc "Keymap for the QEMU images list buffer."
  :parent tabulated-list-mode-map
  "RET" #'deb-packaging-infra-visit-qemu-dir
  "d" #'deb-packaging-infra-delete-qemu
  "c" #'deb-packaging-infra-create-qemu
  "g" #'deb-packaging-infra-refresh-qemu-images
  "?" #'deb-packaging-infra-qemu-dispatch
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
  (unless (derived-mode-p 'deb-packaging-infra-qemu-images-mode)
    (user-error "Not in a QEMU images buffer"))
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
                          'face 'shadow)))))

(defun deb-packaging-infra-qemu-images ()
  "Open a buffer listing all QEMU autopkgtest images."
  (interactive)
  (let ((buf (get-buffer-create "*deb-packaging infra: QEMU images*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'deb-packaging-infra-qemu-images-mode)
        (deb-packaging-infra-qemu-images-mode))
      (deb-packaging-infra-refresh-qemu-images))
    (deb-packaging-display-buffer buf 'list)))

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
  "Seconds before the PPA cache is stale and refreshed in the background.")

(defvar deb-packaging-infra--ppa-warm-proc nil
  "In-flight background `ppa list' refresh, or nil.")

(defvar deb-packaging-infra--ppa-tool-missing-warned nil
  "Non-nil once the missing-`ppa'-tool warning has been shown.")

(defun deb-packaging-infra--invalidate-ppa-cache ()
  "Clear the PPA cache.  Call after creating or deleting a PPA."
  (setq deb-packaging-infra--ppa-cache nil))

(defun deb-packaging-infra--warm-ppa-cache-async ()
  "Refresh the PPA cache in the background; never blocks the caller.
No-op while the cache is fresh or a refresh is already in flight, so it
is safe to call speculatively (e.g. when opening the status buffer).
No-op in batch Emacs: background network fetches do not belong in
batch runs.  A missing `ppa' binary is reported once and caches an
empty list; fetch failures just yield whatever the tool managed to
print."
  (unless noninteractive
    (cond
     ((not (executable-find "ppa"))
      (unless deb-packaging-infra--ppa-tool-missing-warned
        (setq deb-packaging-infra--ppa-tool-missing-warned t)
        (message "ppa tool not installed; PPA completion unavailable"))
      ;; Timestamp it so the warning and the empty list hold for the TTL.
      (setq deb-packaging-infra--ppa-cache (cons nil (float-time))))
     ((and deb-packaging-infra--ppa-cache
           (< (- (float-time) (cdr deb-packaging-infra--ppa-cache))
              deb-packaging-infra--ppa-cache-ttl))
      nil)                                ; fresh
     ((process-live-p deb-packaging-infra--ppa-warm-proc)
      nil)                                ; already warming
     (t
      (let* ((cfgs (deb-packaging-infra--team-config-files))
             (script (mapconcat
                      #'identity
                      (cons "ppa list 2>/dev/null"
                            (mapcar (lambda (cfg)
                                      (format "ppa list -C %s 2>/dev/null"
                                              (shell-quote-argument cfg)))
                                    cfgs))
                      "; "))
             (temp-buf (generate-new-buffer " *ppa-warm*")))
        (setq deb-packaging-infra--ppa-warm-proc
              (make-process
               :name "ppa-warm"
               :buffer temp-buf
               :command (list "sh" "-c" script)
               :noquery t
               :sentinel
               (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (unwind-protect
                       (when (eq (process-status proc) 'exit)
                         (setq deb-packaging-infra--ppa-cache
                               (cons (deb-packaging-infra--parse-ppa-lines
                                      (with-current-buffer temp-buf
                                        (buffer-string)))
                                     (float-time))))
                     (kill-buffer temp-buf)))))))))))

(defun deb-packaging-infra--list-ppas ()
  "Return PPA names for the user and configured teams, without blocking.
A fresh cache is returned as-is; a stale or cold one returns whatever is
cached (possibly nil) while a background refresh warms the next prompt.
Free-text input at the callers works regardless of candidates."
  (unless (and deb-packaging-infra--ppa-cache
               (< (- (float-time) (cdr deb-packaging-infra--ppa-cache))
                  deb-packaging-infra--ppa-cache-ttl))
    (deb-packaging-infra--warm-ppa-cache-async))
  (car deb-packaging-infra--ppa-cache))

(defun deb-packaging-infra--ppa-owner (ppa)
  "Return the owner part of PPA string PPA."
  (when (string-match "\\`ppa:\\([^/]+\\)" ppa)
    (match-string 1 ppa)))

(defun deb-packaging-infra--ppa-name (ppa)
  "Return the name part of PPA string PPA."
  (when (string-match "\\`ppa:[^/]+/\\(.+\\)" ppa)
    (match-string 1 ppa)))

(defun deb-packaging-infra--read-ppa (prompt)
  "Read a PPA name with PROMPT, or use the row at point.
Inside the PPAs list buffer, candidates are the buffer's own rows.
Elsewhere the cached list is used; with no candidates (cold cache, the
background refresh still running, or `ppa' not installed) the prompt
falls back to free text rather than erroring."
  (or (tabulated-list-get-id)
      (let ((ppas (if (derived-mode-p 'deb-packaging-infra-ppas-mode)
                      (mapcar #'car tabulated-list-entries)
                    (deb-packaging-infra--list-ppas))))
        (if ppas
            (completing-read prompt ppas nil nil)
          (read-string prompt)))))

(defun deb-packaging-infra-create-ppa ()
  "Create a Launchpad PPA via `ppa create'."
  (interactive)
  (let* ((name (read-string "PPA name to create: "))
         (cmd (format "ppa create %s" (shell-quote-argument name))))
    (when (string-empty-p name)
      (user-error "No PPA name given"))
    (when (yes-or-no-p (format "Run: %s? " cmd))
      (deb-packaging-infra--invalidate-ppa-cache)
      (deb-packaging-commands--compile cmd))))

(defun deb-packaging-infra-delete-ppa (&optional name)
  "Delete a Launchpad PPA via `ppa destroy'.
Use PPA at point, or prompt."
  (interactive
   (list (deb-packaging-infra--read-ppa "PPA to delete: ")))
  (when (string-empty-p name)
    (user-error "No PPA name given"))
  (when (yes-or-no-p (format "Really delete PPA %s? " name))
    (deb-packaging-infra--invalidate-ppa-cache)
    (deb-packaging-infra--compile-then-refresh
     (format "ppa destroy %s" (shell-quote-argument name))
     'deb-packaging-infra-ppas-mode
     #'deb-packaging-infra-refresh-ppas)))

(defun deb-packaging-infra-set-ppa-config (&optional name)
  "Configure a Launchpad PPA via `ppa set'.
Use PPA at point, or prompt.  Prompts for display name and description."
  (interactive
   (list (deb-packaging-infra--read-ppa "PPA to configure: ")))
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
          (deb-packaging-commands--compile cmd))))))

(defun deb-packaging-infra-show-ppa (&optional name)
  "Show Launchpad PPA info via `ppa show'.
Use PPA at point, or prompt.  Runs asynchronously (a synchronous call
would freeze Emacs on the Launchpad round trip) and fills a read-only
`special-mode' buffer when done; a compilation buffer would error-parse
the text and send RET to bogus locations."
  (interactive
   (list (deb-packaging-infra--read-ppa "PPA to show: ")))
  (when (string-empty-p name)
    (user-error "No PPA name given"))
  (let ((buf (get-buffer-create (format "*deb-ppa: %s*" name)))
        (out-buf (generate-new-buffer " *deb-ppa-show*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Fetching ppa show %s...\n" name)
                            'font-lock-face 'shadow))))
    (make-process
     :name "deb-ppa-show"
     :buffer out-buf
     :command (list "ppa" "show" name)
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (unwind-protect
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (if (and (eq (process-status proc) 'exit)
                            (zerop (process-exit-status proc)))
                       (insert-buffer-substring out-buf)
                     (insert (propertize
                              (format "ppa show %s failed (exit %s)\n\n"
                                      name
                                      (if (eq (process-status proc) 'exit)
                                          (process-exit-status proc)
                                        "killed"))
                              'font-lock-face 'error))
                     (insert-buffer-substring out-buf))
                   (goto-char (point-min))
                   (special-mode)
                   (setq deb-packaging-display-category 'report)
                   (deb-packaging-display-buffer buf 'report))))
           (when (buffer-live-p out-buf)
             (kill-buffer out-buf))))))
    (deb-packaging-display-buffer buf 'report)))

;;; PPA list buffer

(defun deb-packaging-infra-visit-ppa ()
  "Show the PPA at point (`ppa show')."
  (interactive)
  (if-let ((ppa (tabulated-list-get-id)))
      (deb-packaging-infra-show-ppa ppa)
    (user-error "No PPA on this line")))

(defvar-keymap deb-packaging-infra-ppas-mode-map
  :doc "Keymap for the PPAs list buffer."
  :parent tabulated-list-mode-map
  "RET" #'deb-packaging-infra-visit-ppa
  "s" #'deb-packaging-infra-show-ppa
  "d" #'deb-packaging-infra-delete-ppa
  "e" #'deb-packaging-infra-set-ppa-config
  "c" #'deb-packaging-infra-create-ppa
  "g" #'deb-packaging-infra-refresh-ppas
  "?" #'deb-packaging-infra-ppas-dispatch
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

(defvar-local deb-packaging-infra--ppa-fetch-failed nil
  "Non-nil when the last `ppa list' fetch exited non-zero.")

(defvar-local deb-packaging-infra--ppa-pending-entries nil
  "PPA rows collected by the current refresh.")

(defvar-local deb-packaging-infra--ppa-fetch-succeeded nil
  "Non-nil when any process in the current PPA refresh succeeded.")

(defun deb-packaging-infra--cancel-ppa-processes ()
  "Cancel in-flight async PPA listing processes."
  (dolist (proc deb-packaging-infra--ppa-processes)
    (when (process-live-p proc)
      (delete-process proc)))
  (setq deb-packaging-infra--ppa-processes nil))

(defun deb-packaging-infra--finalize-ppas (buf)
  "Reprint the PPAs table in BUF, showing empty-state once all fetches finish."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (null deb-packaging-infra--ppa-processes)
        (when deb-packaging-infra--ppa-fetch-succeeded
          (if deb-packaging-infra--ppa-fetch-failed
              (dolist (entry deb-packaging-infra--ppa-pending-entries)
                (unless (assoc (car entry) tabulated-list-entries)
                  (setq tabulated-list-entries
                        (append tabulated-list-entries (list entry)))))
            (setq tabulated-list-entries deb-packaging-infra--ppa-pending-entries)))
        (let ((inhibit-read-only t))
          (erase-buffer))
        (tabulated-list-init-header)
        (tabulated-list-print t)
        (when (and deb-packaging-infra--ppa-fetch-failed
                   tabulated-list-entries)
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (propertize
                     "\nSome PPA lists failed; showing cached results."
                     'face 'warning))))
        (when (null tabulated-list-entries)
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (propertize
                     (if deb-packaging-infra--ppa-fetch-failed
                         "\nppa list failed; press g to retry."
                       "\nNo PPAs found.\nCreate one with 'c'.")
                     'face 'shadow))))))))

(defun deb-packaging-infra--ppa-list-sentinel (buf temp-buf)
  "Return a sentinel for an async `ppa list' process.
BUF is the PPAs list buffer; TEMP-BUF holds output.  A non-zero exit
reports the failure instead of masquerading as an empty list.  Killed
processes (refresh cancels them) only clean up."
  (lambda (proc _event)
    (let ((status (process-status proc)))
      (when (memq status '(exit failed signal))
        (unwind-protect
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (setq deb-packaging-infra--ppa-processes
                      (delq proc deb-packaging-infra--ppa-processes))
                (when (eq status 'exit)
                  (if (zerop (process-exit-status proc))
                      (let ((output (with-current-buffer temp-buf
                                      (buffer-string))))
                        (dolist (ppa (deb-packaging-infra--parse-ppa-lines output))
                          (unless (assoc ppa deb-packaging-infra--ppa-pending-entries)
                            (setq deb-packaging-infra--ppa-pending-entries
                                  (append deb-packaging-infra--ppa-pending-entries
                                          (list (deb-packaging-infra--make-ppa-entry
                                                 ppa))))))
                        (setq deb-packaging-infra--ppa-fetch-succeeded t))
                    (setq deb-packaging-infra--ppa-fetch-failed t)
                    (message "ppa list failed (exit %d); keeping previous list"
                             (process-exit-status proc))))
                (deb-packaging-infra--finalize-ppas buf)))
          (when (buffer-live-p temp-buf)
            (kill-buffer temp-buf)))))))

(defun deb-packaging-infra--show-ppas-loading-message ()
  "Show a loading message in the PPAs list buffer while async fetches run."
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize "\nLoading PPAs..." 'face 'shadow))))

(defun deb-packaging-infra-refresh-ppas ()
  "Refresh the PPAs list buffer asynchronously so Emacs does not block."
  (interactive)
  (unless (derived-mode-p 'deb-packaging-infra-ppas-mode)
    (user-error "Not in a PPAs buffer"))
  (deb-packaging-infra--cancel-ppa-processes)
  (setq deb-packaging-infra--ppa-pending-entries nil
        deb-packaging-infra--ppa-fetch-succeeded nil
        deb-packaging-infra--ppa-fetch-failed nil)
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
        (push proc deb-packaging-infra--ppa-processes)))))

(defun deb-packaging-infra-ppas ()
  "Open a buffer listing all Launchpad PPAs."
  (interactive)
  (let ((buf (get-buffer-create "*deb-packaging infra: PPAs*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'deb-packaging-infra-ppas-mode)
        (deb-packaging-infra-ppas-mode))
      (deb-packaging-infra-refresh-ppas))
    (deb-packaging-display-buffer buf 'list)))

;;; Infrastructure dispatch

(defun deb-packaging-infra--header ()
  "Header for infrastructure transient."
  (format "Infrastructure Management\nDistro: %s" (deb-packaging-config--effective-distro)))

(transient-define-prefix deb-packaging-infra-schroots-dispatch ()
  "Manage schroots and their sessions in the schroots buffer."
  :environment #'deb-packaging-transients--env
  ["Schroots"
   ("u" "Update schroot(s)"      deb-packaging-infra-update-schroots)
   ("U" "Update all schroots"    deb-packaging-infra-update-all-schroots)
   ("d" "Delete schroot"         deb-packaging-infra-delete-schroot)
   ("c" "Create schroot"         deb-packaging-infra-create-schroot)]
  ["Sessions"
   ("e" "End session(s)"         deb-packaging-infra-end-sessions)
   ("E" "End all sessions"       deb-packaging-infra-end-all-sessions)]
  ["Other lists"
   ("l" "LXD (images + dev containers)..." deb-packaging-infra-lxd)
   ("v" "QEMU images (autopkgtest)..."     deb-packaging-infra-qemu-images)
   ("p" "PPAs (Launchpad)..."              deb-packaging-infra-ppas)]
  ["Navigation"
   ("g" "Refresh" deb-packaging-infra-refresh-schroots)
   ("q" "Quit"    transient-quit-one)])

(transient-define-prefix deb-packaging-infra-lxd-dispatch ()
  "Manage LXD images and dev containers in the LXD buffer."
  :environment #'deb-packaging-transients--env
  ["LXD"
   ("s" "Start container"          deb-packaging-infra-start-lxd-entry)
   ("k" "Stop container"           deb-packaging-infra-stop-lxd-entry)
   ("x" "Shell into container"     deb-packaging-infra-shell-lxd-entry)
   ("d" "Delete image/container"   deb-packaging-infra-delete-lxd-entry)
   ("c" "Create autopkgtest image" deb-packaging-infra-create-lxd)]
  ["Navigation"
   ("g" "Refresh" deb-packaging-infra-refresh-lxd)
   ("q" "Quit"    transient-quit-one)])

(transient-define-prefix deb-packaging-infra-qemu-dispatch ()
  "Manage QEMU autopkgtest images in the QEMU buffer."
  :environment #'deb-packaging-transients--env
  ["QEMU images"
   ("d" "Delete image" deb-packaging-infra-delete-qemu)
   ("c" "Create image" deb-packaging-infra-create-qemu)]
  ["Navigation"
   ("g" "Refresh" deb-packaging-infra-refresh-qemu-images)
   ("q" "Quit"    transient-quit-one)])

(transient-define-prefix deb-packaging-infra-ppas-dispatch ()
  "Manage Launchpad PPAs in the PPAs buffer."
  :environment #'deb-packaging-transients--env
  ["PPAs"
   ("s" "Show PPA"      deb-packaging-infra-show-ppa)
   ("e" "Configure PPA" deb-packaging-infra-set-ppa-config)
   ("d" "Delete PPA"    deb-packaging-infra-delete-ppa)
   ("c" "Create PPA"    deb-packaging-infra-create-ppa)]
  ["Navigation"
   ("g" "Refresh" deb-packaging-infra-refresh-ppas)
   ("q" "Quit"    transient-quit-one)])

(transient-define-prefix deb-packaging-infra-dispatch ()
  "Manage build and test infrastructure."
  :environment #'deb-packaging-transients--env
  [:description deb-packaging-infra--header]
  ["Infrastructure"
   ("s" "Schroots (sbuild)..."          deb-packaging-infra-schroots)
   ("l" "LXD (images + dev containers)..." deb-packaging-infra-lxd)
   ("v" "QEMU images (autopkgtest)..."  deb-packaging-infra-qemu-images)
   ("p" "PPAs (Launchpad)..."           deb-packaging-infra-ppas)]
  ["Navigation"
   ("q" "Quit" transient-quit-one)])

(provide 'deb-packaging-infra)
;;; deb-packaging-infra.el ends here
