;;; deb-packaging-test-display.el --- Tests for the display policy -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Keywords: tools, debian, ubuntu, packaging

;;; Commentary:

;; Tests for deb-packaging-display.el and the package's window policy.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'deb-packaging-test)
(require 'deb-packaging-display)

;;; Action mapping

(ert-deftest deb-packaging-test-display/action-per-category ()
  "status/list/report use same-window; output/shell reuse-or-below."
  (dolist (cat '(status list report))
    (should (memq 'display-buffer-same-window
                  (car (deb-packaging-display--action cat)))))
  (dolist (cat '(output shell))
    (let ((fns (car (deb-packaging-display--action cat))))
      (should (memq 'deb-packaging-display--reuse-category-window fns))
      (should (memq 'display-buffer-below-selected fns)))))

(ert-deftest deb-packaging-test-display/action-unknown-category-errors ()
  (should-error (deb-packaging-display--action 'bogus)
                :type 'error))

;;; Category-window reuse

(defmacro deb-packaging-test-display--with-marked-buffers (specs &rest body)
  "Create temp buffers per SPECS, a list of (VAR CATEGORY), then run BODY.
Each VAR is bound to a buffer whose `deb-packaging-display-category' is
CATEGORY.  Buffers are killed afterwards."
  (declare (indent 1) (debug (form body)))
  (let ((bufs (mapcar #'car specs)))
    `(let ,(mapcar (lambda (v) `(,v (get-buffer-create
                                     ,(format "*dp-test-%s*" v))))
                   bufs)
       (unwind-protect
           (progn
             ,@(mapcar (lambda (s)
                         `(with-current-buffer ,(car s)
                            (setq deb-packaging-display-category ,(cadr s))))
                       specs)
             ,@body)
         ,@(mapcar (lambda (v) `(kill-buffer ,v)) bufs)))))

(ert-deftest deb-packaging-test-display/reuse-finds-category-window ()
  "A visible window showing a same-category buffer is reused."
  (save-window-excursion
    (deb-packaging-test-display--with-marked-buffers
        ((old-buf 'output) (new-buf 'output))
      (let ((win (split-window (selected-window) nil 'below)))
        (set-window-buffer win old-buf)
        (should (eq (deb-packaging-display--reuse-category-window new-buf nil)
                    win))
        (should (eq (window-buffer win) new-buf))))))

(ert-deftest deb-packaging-test-display/reuse-skips-dedicated-window ()
  "Dedicated windows are not reused."
  (save-window-excursion
    (deb-packaging-test-display--with-marked-buffers
        ((old-buf 'output) (new-buf 'output))
      (let ((win (split-window (selected-window) nil 'below)))
        (set-window-buffer win old-buf)
        (set-window-dedicated-p win t)
        (should-not (deb-packaging-display--reuse-category-window new-buf nil))
        (set-window-dedicated-p win nil)))))

(ert-deftest deb-packaging-test-display/reuse-skips-side-window ()
  "Side windows are not reused."
  (save-window-excursion
    (deb-packaging-test-display--with-marked-buffers
        ((old-buf 'output) (new-buf 'output))
      (let ((win (split-window (selected-window) nil 'below)))
        (set-window-buffer win old-buf)
        (set-window-parameter win 'window-side 'bottom)
        (should-not (deb-packaging-display--reuse-category-window new-buf nil))
        (set-window-parameter win 'window-side nil)))))

(ert-deftest deb-packaging-test-display/reuse-ignores-other-category ()
  "A window showing a different category is not reused."
  (save-window-excursion
    (deb-packaging-test-display--with-marked-buffers
        ((old-buf 'shell) (new-buf 'output))
      (let ((win (split-window (selected-window) nil 'below)))
        (set-window-buffer win old-buf)
        (should-not (deb-packaging-display--reuse-category-window new-buf nil))))))

;;; End-to-end display

(ert-deftest deb-packaging-test-display/status-takes-over-window ()
  "status displays in the selected window and keeps it selected."
  (save-window-excursion
    (let ((buf (get-buffer-create "*dp-test-status*"))
          (start (selected-window)))
      (unwind-protect
          (progn
            (deb-packaging-display-buffer buf 'status)
            (should (eq (selected-window) start))
            (should (eq (window-buffer start) buf)))
        (kill-buffer buf)))))

(ert-deftest deb-packaging-test-display/output-opens-below-and-selects ()
  "output opens a regular window below the invoking one and selects it."
  (save-window-excursion
    (deb-packaging-test-display--with-marked-buffers ((buf 'output))
      (let ((start (selected-window)))
        (deb-packaging-display-buffer buf 'output)
        (should-not (eq (selected-window) start))
        (should (eq (window-buffer (selected-window)) buf))
        (should-not (window-parameter (selected-window) 'window-side))))))

(ert-deftest deb-packaging-test-display/overrides-user-display-buffer-alist ()
  "User alist side-window rules must not grab package buffers."
  (save-window-excursion
    (let ((display-buffer-alist
           '((".*" (display-buffer-in-side-window (side . bottom) (slot . 0))))))
      (deb-packaging-test-display--with-marked-buffers ((buf 'output))
        (deb-packaging-display-buffer buf 'output)
        (let ((win (get-buffer-window buf)))
          (should win)
          (should-not (window-parameter win 'window-side)))))))

(provide 'deb-packaging-test-display)
;;; deb-packaging-test-display.el ends here
