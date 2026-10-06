;;; deb-packaging-resume.el --- Resume failed sbuild builds -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; A failed sbuild run keeps its schroot session (build-deps installed)
;; and build tree.  Resuming copies the checkout's changed debian/ files
;; into that tree, unapplies patches that changed so they re-apply, and
;; continues with dpkg-buildpackage -nc in the session: make or ninja then
;; rebuilds only what the fix touched.
;;
;; Assumes schroot mode, where the build runs as the invoking user and
;; /build is a host bind mount (see /etc/schroot/sbuild/fstab).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'magit)
(require 'deb-packaging-detect)

(declare-function deb-packaging-commands--package-context "deb-packaging-commands")
(declare-function deb-packaging-commands--run-command "deb-packaging-commands")
(declare-function deb-packaging-commands--wrap-sentinel "deb-packaging-commands")
(declare-function deb-packaging-commands--notify-status-refresh "deb-packaging-commands")

;;; Kept build records

(defun deb-packaging-resume--record-file (name series arch)
  "Return the kept-build record file for NAME on SERIES/ARCH."
  (expand-file-name (format "deb-packaging/kept-builds/%s.%s.%s" name series arch)
                    (deb-packaging-detect--cache-dir)))

(defun deb-packaging-resume--ctx-file (ctx)
  "Return the kept-build record file for package CTX."
  (deb-packaging-resume--record-file
   (plist-get ctx :name) (plist-get ctx :distro) (plist-get ctx :target-arch)))

(defun deb-packaging-resume--host-build-root ()
  "Return the host directory bind-mounted as /build in sbuild sessions."
  (or (ignore-errors
        (with-temp-buffer
          (insert-file-contents "/etc/schroot/sbuild/fstab")
          (when (re-search-forward "^\\(/\\S-+\\)\\s-+/build\\s-" nil t)
            (match-string 1))))
      "/var/lib/sbuild/build"))

(defun deb-packaging-resume--host-path (chroot-path)
  "Map CHROOT-PATH under /build to its host path."
  (concat (deb-packaging-resume--host-build-root)
          (string-remove-prefix "/build" chroot-path)))

(defun deb-packaging-resume--session-alive-p (session)
  "Return non-nil when schroot SESSION still exists."
  (member (concat "session:" session)
          (split-string (or (deb-packaging-detect--call-process-string
                             "schroot" "--list" "--all-sessions")
                            ""))))

(defun deb-packaging-resume--load (ctx)
  "Return the kept-build record for CTX when its session and tree still exist."
  (let ((file (deb-packaging-resume--ctx-file ctx)))
    (when-let* (((file-readable-p file))
                (record (ignore-errors
                          (with-temp-buffer
                            (insert-file-contents file)
                            (read (current-buffer))))))
      (if (and (equal (plist-get record :version) (plist-get ctx :version))
               (file-directory-p (deb-packaging-resume--host-path
                                  (plist-get record :tree)))
               (deb-packaging-resume--session-alive-p (plist-get record :session)))
          record
        (delete-file file)
        nil))))

(defun deb-packaging-resume--save (ctx record)
  "Save RECORD as CTX's kept build."
  (let ((file (deb-packaging-resume--ctx-file ctx)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (prin1 record (current-buffer)))))

(defun deb-packaging-resume--forget (ctx)
  "Delete CTX's kept-build record."
  (let ((file (deb-packaging-resume--ctx-file ctx)))
    (when (file-exists-p file) (delete-file file))))

(defun deb-packaging-resume-current ()
  "Return the current package's kept build, or nil."
  (when-let ((ctx (ignore-errors (deb-packaging-commands--package-context))))
    (deb-packaging-resume--load ctx)))

;;; Capturing a failed build

(defun deb-packaging-resume--parse-sbuild (buf)
  "Return the kept-build facts sbuild printed in BUF, or nil.
Keys: :session, :tree (chroot path), :command, :dbo."
  (when (buffer-live-p (get-buffer buf))
    (with-current-buffer buf
      (save-excursion
        (cl-flet ((grab (re) (goto-char (point-min))
                        (and (re-search-forward re nil t) (match-string-no-properties 1))))
          (let ((session (grab "^Keeping session: \\(\\S-+\\)"))
                (tree (grab "extracting \\S-+ in \\(/build/\\S-+\\)")))
            (when (and session tree)
              (list :session session
                    :tree tree
                    :command (grab "^Command: \\(dpkg-buildpackage .*\\)$")
                    :dbo (grab "^DEB_BUILD_OPTIONS=\\(.*\\)$")))))))))

(defun deb-packaging-resume--watch-sbuild (buffer ctx)
  "Record a kept build when the sbuild run in BUFFER fails for CTX."
  (when-let ((proc (get-buffer-process buffer)))
    (deb-packaging-commands--wrap-sentinel
     proc
     (lambda (p _event)
       (if (and (eq (process-status p) 'exit) (zerop (process-exit-status p)))
           (deb-packaging-resume--forget ctx)
         (when-let ((facts (deb-packaging-resume--parse-sbuild buffer)))
           (deb-packaging-resume--save
            ctx (append (list :version (plist-get ctx :version)) facts))
           (deb-packaging-commands--notify-status-refresh)))))))

;;; Syncing the fix into the kept tree

(defun deb-packaging-resume--read-lines (file)
  "Return FILE's non-empty, non-comment lines, or nil."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      ;; Series lines may carry options ("fix.patch -p1"): keep the name.
      (cl-loop for line in (split-string (buffer-string) "\n" t "[ \t]+")
               unless (string-prefix-p "#" line)
               collect (car (split-string line))))))

(defun deb-packaging-resume--same-file-p (a b)
  "Return non-nil when files A and B exist with identical contents."
  (and (file-regular-p a) (file-regular-p b)
       (= (file-attribute-size (file-attributes a))
          (file-attribute-size (file-attributes b)))
       (equal (with-temp-buffer (set-buffer-multibyte nil)
                                (insert-file-contents-literally a) (buffer-string))
              (with-temp-buffer (set-buffer-multibyte nil)
                                (insert-file-contents-literally b) (buffer-string)))))

(defun deb-packaging-resume--stale-patches (applied series patch-file tree-patch)
  "Return the tail of APPLIED that must be unapplied before re-patching.
SERIES is the checkout's series.  PATCH-FILE and TREE-PATCH map a patch
name to its checkout and kept-tree paths.  The first applied patch that
moved, was dropped, or changed, and everything applied after it, go."
  (let ((i 0))
    (while (and (< i (length applied))
                (equal (nth i applied) (nth i series))
                (deb-packaging-resume--same-file-p
                 (funcall patch-file (nth i applied))
                 (funcall tree-patch (nth i applied))))
      (setq i (1+ i)))
    (nthcdr i applied)))

(defun deb-packaging-resume--checkout-debian-files (pkg-dir)
  "Return debian/ files of PKG-DIR relative to it, without build products."
  (let ((default-directory pkg-dir))
    (or (and (ignore-errors (magit-toplevel))
             (magit-git-lines "ls-files" "--" "debian"))
        (mapcar (lambda (f) (file-relative-name f pkg-dir))
                (directory-files-recursively (expand-file-name "debian" pkg-dir) "")))))

(defun deb-packaging-resume--unapply (tree patches)
  "Unapply PATCHES (latest first) in TREE and drop them from .pc."
  (let ((applied-file (expand-file-name ".pc/applied-patches" tree)))
    (dolist (patch (reverse patches))
      (with-temp-buffer
        (unless (zerop (call-process "patch" nil t nil "-R" "-p1" "-s" "-f"
                                     "--no-backup-if-mismatch" "-d" tree "-i"
                                     (expand-file-name (concat "debian/patches/" patch) tree)))
          (user-error "Cannot unapply %s in the kept tree (%s); discard it and rebuild"
                      patch (string-trim (buffer-string)))))
      (delete-directory (expand-file-name (concat ".pc/" patch) tree) t))
    (when (file-exists-p applied-file)
      (with-temp-file applied-file
        (dolist (p (deb-packaging-resume--read-lines applied-file))
          (unless (member p patches) (insert p "\n")))))))

(defun deb-packaging-resume--sync (pkg-dir tree)
  "Bring TREE's debian/ up to date with PKG-DIR's; return files copied.
Copies get a fresh modification time so make sees them as changed;
identical files are left alone."
  (let* ((applied (deb-packaging-resume--read-lines
                   (expand-file-name ".pc/applied-patches" tree)))
         (series (deb-packaging-resume--read-lines
                  (expand-file-name "debian/patches/series" pkg-dir)))
         (stale (deb-packaging-resume--stale-patches
                 applied series
                 (lambda (p) (expand-file-name (concat "debian/patches/" p) pkg-dir))
                 (lambda (p) (expand-file-name (concat "debian/patches/" p) tree))))
         (copied nil))
    ;; Unapply with the old patch text, before the sync overwrites it.
    (deb-packaging-resume--unapply tree stale)
    (dolist (rel (deb-packaging-resume--checkout-debian-files pkg-dir))
      (let ((from (expand-file-name rel pkg-dir))
            (to (expand-file-name rel tree)))
        (when (and (file-regular-p from)
                   (not (deb-packaging-resume--same-file-p from to)))
          (make-directory (file-name-directory to) t)
          (copy-file from to t)
          (push rel copied))))
    (list :copied (nreverse copied) :reapplied stale)))

;;; Commands

(defun deb-packaging-resume--require ()
  "Return (CTX . RECORD) for the current package or signal a `user-error'."
  (let* ((ctx (or (ignore-errors (deb-packaging-commands--package-context))
                  (user-error "Not in a Debian package directory")))
         (record (or (deb-packaging-resume--load ctx)
                     (user-error "No kept failed build for %s %s"
                                 (plist-get ctx :name) (plist-get ctx :version)))))
    (cons ctx record)))

(defun deb-packaging-resume--command (record)
  "Return the shell command continuing RECORD's build."
  (let ((command (or (plist-get record :command)
                     "dpkg-buildpackage --sanitize-env -us -uc -b")))
    (format "export LC_ALL=C.UTF-8 DEB_BUILD_OPTIONS=%s; %s -nc"
            (shell-quote-argument
             (or (plist-get record :dbo)
                 (format "parallel=%d" (num-processors))))
            command)))

(defun deb-packaging-resume--collect-artifacts (ctx record)
  "Copy the resumed build's packages next to CTX's other artifacts."
  (let* ((from (file-name-directory
                (directory-file-name
                 (deb-packaging-resume--host-path (plist-get record :tree)))))
         ;; A per-build random parent holds only this build; the shared
         ;; reproducible-path parent needs the package name to filter.
         (prefix (if (string-suffix-p "/reproducible-path/" from)
                     (regexp-quote (concat (plist-get ctx :name) "_"))
                   ""))
         (to (plist-get ctx :artifact-dir)))
    (dolist (f (directory-files
                from t (concat "\\`" prefix ".*\\.\\(u?deb\\|ddeb\\|changes\\|buildinfo\\)\\'")))
      (copy-file f (expand-file-name (file-name-nondirectory f) to) t))))

(defun deb-packaging-resume-build ()
  "Bring the fix into the kept failed build and continue it."
  (interactive)
  (pcase-let* ((`(,ctx . ,record) (deb-packaging-resume--require))
               (tree (deb-packaging-resume--host-path (plist-get record :tree)))
               (sync (deb-packaging-resume--sync (plist-get ctx :pkg-dir) tree))
               (buffer (deb-packaging-commands--run-command
                        "resume"
                        (list "schroot" "--run-session" "-c" (plist-get record :session)
                              "-d" (plist-get record :tree) "--"
                              "sh" "-c" (deb-packaging-resume--command record))
                        (plist-get ctx :pkg-dir) 'binary-build (plist-get ctx :pkg-dir))))
    (message "Resuming: %d file(s) synced, %d patch(es) to re-apply"
             (length (plist-get sync :copied)) (length (plist-get sync :reapplied)))
    (when-let ((proc (get-buffer-process buffer)))
      (deb-packaging-commands--wrap-sentinel
       proc
       (lambda (p _event)
         (when (and (eq (process-status p) 'exit) (zerop (process-exit-status p)))
           (deb-packaging-resume--collect-artifacts ctx record)
           (deb-packaging-resume--discard ctx record)
           (deb-packaging-commands--notify-status-refresh)))))
    buffer))

(defun deb-packaging-resume--discard (ctx record)
  "End RECORD's session, delete its tree, and forget it for CTX."
  (let* ((session (plist-get record :session))
         (tree (plist-get record :tree))
         (parent (file-name-directory (directory-file-name tree)))
         ;; sbuild's default shared parent holds other builds too.
         (top (if (string-match-p "\\`/build/[^/]+/\\'" parent)
                  (if (string-suffix-p "/reproducible-path/" parent) tree parent)
                tree)))
    ;; As root in the session: the host may not let you remove /build entries.
    (when (deb-packaging-resume--session-alive-p session)
      (call-process "schroot" nil nil nil "--run-session" "-c" session "-u" "root"
                    "-d" "/" "--" "rm" "-rf" (directory-file-name top))
      (call-process "schroot" nil nil nil "--end-session" "-c" session))
    (ignore-errors (delete-directory (deb-packaging-resume--host-path top) t))
    (deb-packaging-resume--forget ctx)))

(defun deb-packaging-resume-discard ()
  "Delete the kept failed build: end its session and remove its tree."
  (interactive)
  (pcase-let ((`(,ctx . ,record) (deb-packaging-resume--require)))
    (when (y-or-n-p "Delete the kept build tree and its session? ")
      (deb-packaging-resume--discard ctx record)
      (deb-packaging-commands--notify-status-refresh)
      (message "Kept build discarded"))))

(defun deb-packaging-resume-open-tree ()
  "Open the kept build tree in Dired; edits there take effect on resume."
  (interactive)
  (pcase-let ((`(,_ctx . ,record) (deb-packaging-resume--require)))
    (dired (deb-packaging-resume--host-path (plist-get record :tree)))))

(defun deb-packaging-resume-shell ()
  "Open a shell in the kept session, in the build tree."
  (interactive)
  (pcase-let ((`(,_ctx . ,record) (deb-packaging-resume--require)))
    (pop-to-buffer
     (make-comint (format "kept-build %s" (plist-get record :session))
                  "schroot" nil "--run-session" "-c" (plist-get record :session)
                  "-d" (plist-get record :tree)))))

(provide 'deb-packaging-resume)
;;; deb-packaging-resume.el ends here
