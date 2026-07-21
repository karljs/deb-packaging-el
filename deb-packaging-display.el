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
;;                         same category, else a new window below the
;;                         selected one
;;
;; The policy is authoritative for package buffers: it is applied via
;; `display-buffer-overriding-action', which outranks user
;; `display-buffer-alist' rules (e.g. comint side windows).  The
;; override is bound dynamically per call, so user rules still govern
;; all other buffers.  No side windows are used anywhere.

;;; Code:

(require 'seq)
(require 'subr-x)

(defvar-local deb-packaging-display-category nil
  "Display category of this buffer, or nil.
Set on output and shell buffers at creation so a visible window already
showing the same category can be reused for new buffers of that
category.")

(defun deb-packaging-display--reuse-category-window (buffer alist)
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
    (window--display-buffer buffer window 'reuse alist)))

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
      (window--display-buffer buffer window 'reuse alist)
    (when-let ((window (display-buffer-below-selected buffer alist)))
      (set-window-dedicated-p window t)
      window)))

(defun deb-packaging-display--action (category)
  "Return the `display-buffer' action for CATEGORY.
CATEGORY is one of status, list, report, output, or shell."
  (pcase category
    ((or 'status 'list 'report)
     '((display-buffer-same-window
        display-buffer-pop-up-window)))
    ((or 'output 'shell)
     '((display-buffer-reuse-window
        deb-packaging-display--reuse-category-window
        display-buffer-below-selected
        display-buffer-pop-up-window)
       (inhibit-same-window . t)))
    (_ (error "Unknown deb-packaging display category: %S" category))))

(defun deb-packaging-display-buffer-default (buffer category)
  "Display BUFFER per the CATEGORY policy and return the window used.
Binds `display-buffer-overriding-action' so the package policy wins
over user `display-buffer-alist' rules for package buffers."
  (let ((display-buffer-overriding-action
         (deb-packaging-display--action category)))
    (display-buffer buffer)))

(defvar deb-packaging-display-buffer-function
  #'deb-packaging-display-buffer-default
  "Function used to display package buffers.
Called with (BUFFER CATEGORY) and must return the window used.")

(defun deb-packaging-display-buffer (buffer category)
  "Display BUFFER according to CATEGORY and select its window.
CATEGORY is one of status, list, report, output, or shell."
  (select-window
   (funcall deb-packaging-display-buffer-function buffer category)))

(provide 'deb-packaging-display)
;;; deb-packaging-display.el ends here
