;;; deb-packaging-display.el --- Window display policy -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Magit-style window display policy for the package's own buffers.
;;
;; `deb-packaging-display-buffer' is the single entry point; callers
;; pass a category:
;;
;;   status, list, report  displayed in the selected window
;;   output, shell         reuse a visible window already showing the
;;                         same category, else the selected window
;;                         (quit-window restores the previous buffer)
;;
;; The policy overrides user rules by default; Customize can disable the
;; override or change actions by category. No side windows are used.

;;; Code:

(require 'seq)
(require 'subr-x)

(defcustom deb-packaging-display-category-actions
  '((status display-buffer-same-window display-buffer-pop-up-window)
    (list display-buffer-same-window display-buffer-pop-up-window)
    (report display-buffer-same-window display-buffer-pop-up-window)
    (output display-buffer-reuse-window
            deb-packaging-display--reuse-category-window
            display-buffer-same-window display-buffer-pop-up-window)
    (shell display-buffer-reuse-window
           deb-packaging-display--reuse-category-window
           display-buffer-same-window display-buffer-pop-up-window))
  "Display action functions for each deb-packaging buffer category."
  :type '(alist :key-type (choice (const status) (const list) (const report)
                                  (const output) (const shell))
                :value-type (repeat function))
  :group 'deb-packaging)

(defcustom deb-packaging-display-override-user-rules t
  "Whether package buffer rules take precedence over `display-buffer-alist'."
  :type 'boolean
  :group 'deb-packaging)

(defcustom deb-packaging-display-transient-action
  '(deb-packaging-display--transient-window (inhibit-same-window . t))
  "Display action for package transients."
  :type 'sexp
  :group 'deb-packaging)

(defvar-local deb-packaging-display-category nil
  "Display category of this buffer, or nil.
Set on output and shell buffers at creation so a visible window already
showing the same category can be reused for new buffers of that
category.")

(defun deb-packaging-display--reuse-category-window (buffer _alist)
  "Display BUFFER in a window showing a buffer of the same display category.
Skips dedicated and side windows.  ALIST is the display action alist.
Return the window used, or nil when no category window is visible."
  (when-let* ((buffer (get-buffer buffer))
              (category (buffer-local-value 'deb-packaging-display-category
                                            buffer))
              (window (seq-find
                       (lambda (w)
                         (and (not (window-dedicated-p w))
                              (not (window-parameter w 'window-side))
                              (eq (buffer-local-value
                                   'deb-packaging-display-category
                                   (window-buffer w))
                                  category)))
                       (window-list nil 'nomini))))
     (set-window-buffer window buffer)
     window))

(defun deb-packaging-display--transient-window (buffer alist)
  "Display the transient menu BUFFER without adding a split.
Reuse a visible, non-dedicated, non-side window showing an output or
shell buffer; the menu replaces the process buffer and transient
restores it on exit.  Otherwise open a new dedicated window below the
selected one."
  (if-let ((window (seq-find
                    (lambda (w)
                      (and (not (window-dedicated-p w))
                           (not (window-parameter w 'window-side))
                           (memq (buffer-local-value
                                  'deb-packaging-display-category
                                  (window-buffer w))
                                 '(output shell))))
                     (window-list nil 'nomini))))
       (progn
         (set-window-buffer window buffer)
         window)
    (when-let ((window (display-buffer-below-selected buffer alist)))
      (set-window-dedicated-p window t)
      window)))

(defun deb-packaging-display--action (category)
  "Return the `display-buffer' action for CATEGORY.
CATEGORY is one of status, list, report, output, or shell."
  (if-let ((functions (alist-get category deb-packaging-display-category-actions)))
      (list functions)
    (error "Unknown deb-packaging display category: %S" category)))

(defun deb-packaging-display-buffer (buffer category)
  "Display BUFFER according to CATEGORY and select its window.
Uses the package policy unless `deb-packaging-display-override-user-rules'
is nil.
CATEGORY is one of status, list, report, output, or shell."
  (select-window
   (if deb-packaging-display-override-user-rules
       (let ((display-buffer-overriding-action
              (deb-packaging-display--action category)))
         (display-buffer buffer))
     (display-buffer buffer))))

(provide 'deb-packaging-display)
;;; deb-packaging-display.el ends here
