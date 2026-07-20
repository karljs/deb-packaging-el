;;; deb-packaging-ppa-tests.el --- Parsed PPA autopkgtest report -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer
;; Author: Karl Smeltzer
;; Version: 0.1.0
;; Keywords: tools, debian, ubuntu, packaging
;; URL: https://github.com/karljs/deb-packaging-el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0") (magit "3.3") (magit-section "3.3"))

;;; Commentary:

;; Runs `ppa tests -L' asynchronously, parses the output, and renders a
;; magit-section report with trigger actions.  -L makes the tool print
;; trigger URLs as plain text; the default OSC 8 hyperlinks do not survive
;; comint's OSC filtering.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; Parsing

;; In the regexps below, `\S-' is "non-whitespace".  `\S+' would eat the
;; `+' as a syntax-class designator and match any single character.

(defconst deb-packaging-ppa-tests--osc8-re
  "\e\\]8;;[^\e]*\e\\\\\\([^\e]*\\)\e\\]8;;\e\\\\"
  "Regexp matching an OSC 8 hyperlink; group 1 is the visible text.")

(defun deb-packaging-ppa-tests--strip-osc8 (text)
  "Replace OSC 8 hyperlinks in TEXT with their visible text."
  (replace-regexp-in-string deb-packaging-ppa-tests--osc8-re "\\1" text))

(defun deb-packaging-ppa-tests--parse-trigger-line (line pub)
  "Fold trigger LINE into publication plist PUB (mutated)."
  (when (string-match "^    \\+ @\\([^ :]+\\): \\(\\S-+\\)" line)
    (let* ((arch (match-string 1 line))
           (url (match-string 2 line))
           (entry (assoc arch (plist-get pub :arches))))
      (unless entry
        (setq entry (cons arch (list :basic nil :all-proposed nil)))
        (setf (plist-get pub :arches)
              (append (plist-get pub :arches) (list entry))))
      (if (string-match-p "all-proposed=1" url)
          (setf (plist-get (cdr entry) :all-proposed) url)
        (setf (plist-get (cdr entry) :basic) url)))))

(defun deb-packaging-ppa-tests--parse (output)
  "Parse `ppa tests -L' OUTPUT.
Return a plist with :triggers, :results, :running, :waiting."
  (let ((triggers nil)
        (results nil)
        (running nil)
        (waiting nil)
        (section nil)
        (pub nil)
        (result nil))
    (dolist (raw (split-string
                  (deb-packaging-ppa-tests--strip-osc8 output) "\n"))
      (let ((line (string-trim-right raw)))
        (cond
         ((string-prefix-p "* Triggers:" line) (setq section 'triggers))
         ((string-prefix-p "* Results:" line)  (setq section 'results))
         ((string-prefix-p "* Running:" line)  (setq section 'running))
         ((string-prefix-p "* Waiting:" line)  (setq section 'waiting))
         (t
          (pcase section
            ('triggers
             (if (string-match "^  - Source \\(.+\\): \\(\\S.+\\)$" line)
                 (let ((parts (save-match-data
                               (split-string (match-string 1 line) "/"))))
                   (when (= (length parts) 3)
                     (setq pub (list :series (nth 0 parts)
                                     :package (nth 1 parts)
                                     :version (nth 2 parts)
                                     :status (match-string 2 line)
                                     :arches nil))
                     (push pub triggers)))
               (when pub
                 (deb-packaging-ppa-tests--parse-trigger-line line pub))))
            ('results
             (cond
              ((string-match
                "^  - \\([^ ]+\\): \\([^ /]+\\)/\\([^ /]+\\)/\\([^ ]+\\) \\[\\([^]]+\\)\\]$"
                line)
               ;; Group header; stray bullets must not attach to the old entry.
               (setq result nil))
              ((string-match
                "^    \\+ \\([✅❌⛔]\\) \\(.+\\) on \\([^ ]+\\) for \\([^ ]+\\) +@ \\([^ ]+ [^ ]+\\) *$"
                line)
               (setq result
                     (list :source (match-string 2 line)
                           :series (match-string 3 line)
                           :arch (match-string 4 line)
                           :status (pcase (match-string 1 line)
                                     ("✅" 'pass)
                                     ("❌" 'fail)
                                     ("⛔" 'bad))
                           :timestamp (match-string 5 line)
                           :log-url nil
                           :subtests nil))
               (push result results))
              ((and result
                    (string-match "^      • Log: \\(\\S-+\\)" line))
               (setf (plist-get result :log-url) (match-string 1 line)))
              ((and result
                    (string-match
                     "^      • \\(.+?\\) +\\(PASS\\|FAIL\\|BAD\\|SKIP\\|FLAKY\\) +\\S-+ *$"
                     line))
               (setf (plist-get result :subtests)
                     (append (plist-get result :subtests)
                             (list (cons (match-string 1 line)
                                         (match-string 2 line))))))))
            ('running
             (when (string-prefix-p "  - " line)
               (push (string-trim (substring line 2)) running)))
            ('waiting
             (when (string-prefix-p "  - " line)
               (push (string-trim (substring line 2)) waiting))))))))
    (list :triggers (nreverse triggers)
          :results (nreverse results)
          :running (nreverse running)
          :waiting (nreverse waiting))))

(defun deb-packaging-ppa-tests--summary (parsed)
  "Return a :pass/:fail/:bad/:running/:waiting count plist for PARSED."
  (let ((pass 0) (fail 0) (bad 0))
    (dolist (r (plist-get parsed :results))
      (pcase (plist-get r :status)
        ('pass (cl-incf pass))
        ('fail (cl-incf fail))
        ('bad (cl-incf bad))))
    (list :pass pass
          :fail fail
          :bad bad
          :running (length (plist-get parsed :running))
          :waiting (length (plist-get parsed :waiting)))))

(provide 'deb-packaging-ppa-tests)
;;; deb-packaging-ppa-tests.el ends here
