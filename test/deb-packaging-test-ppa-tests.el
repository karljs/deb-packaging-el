;;; deb-packaging-test-ppa-tests.el --- PPA test report tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Karl Smeltzer

;;; Commentary:

;; ERT tests for deb-packaging-ppa-tests.el.  The main fixture is condensed
;; from live `ppa tests -L' output (PASS, FAIL with subtests, BAD testbed,
;; OSC 8 hyperlink on the Source line, empty queues).  The queue fixture is
;; synthesized from ppa-dev-tools' job.py column format.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'deb-packaging-ppa-tests)

(defconst deb-packaging-test-ppa-tests--fixture
  (concat
   "* Triggers:\n"
   "  - Source \e]8;;https://launchpad.net/ubuntu/+source/llvm-toolchain-19/1:19.1.7-21ubuntu1~24.04.1\e\\noble/llvm-toolchain-19/1:19.1.7-21ubuntu1~24.04.1\e]8;;\e\\: Published\n"
   "    + @amd64: https://autopkgtest.ubuntu.com/request.cgi?release=noble&package=llvm-toolchain-19&arch=amd64&trigger=llvm-toolchain-19%2F1%3A19.1.7-21ubuntu1~24.04.1&ppa=karljs%2Fsru-llvm-19-noble ♻️ \n"
   "    + @arm64: https://autopkgtest.ubuntu.com/request.cgi?release=noble&package=llvm-toolchain-19&arch=arm64&trigger=llvm-toolchain-19%2F1%3A19.1.7-21ubuntu1~24.04.1&ppa=karljs%2Fsru-llvm-19-noble ♻️ \n"
   "    + @amd64: https://autopkgtest.ubuntu.com/request.cgi?release=noble&package=llvm-toolchain-19&arch=amd64&trigger=llvm-toolchain-19%2F1%3A19.1.7-21ubuntu1~24.04.1&ppa=karljs%2Fsru-llvm-19-noble&all-proposed=1 💍\n"
   "    + @arm64: https://autopkgtest.ubuntu.com/request.cgi?release=noble&package=llvm-toolchain-19&arch=arm64&trigger=llvm-toolchain-19%2F1%3A19.1.7-21ubuntu1~24.04.1&ppa=karljs%2Fsru-llvm-19-noble&all-proposed=1 💍\n"
   "* Results:\n"
   "  - llvm-toolchain-19: noble/llvm-toolchain-19/1:19.1.7-21ubuntu1~24.04.1 [amd64]\n"
   "    + ❌ llvm-toolchain-19 on noble for amd64   @ 09.02.26 23:21:05\n"
   "      • Log: https://autopkgtest.ubuntu.com/results/autopkgtest-noble-karljs-sru-llvm-19-noble/noble/amd64/l/llvm-toolchain-19/20260209_232105_b52c9@/log.gz\n"
    "      • Status: FAIL\n"
    "      • command1                  FAIL   🟥\n"
    "      • cmake-llvm-test           PASS   🟩\n"
    "      • flaky-test                FLAKY  🟨\n"
    "      • skipped-test              SKIP   ⏭️\n"
   "  - llvm-toolchain-19: noble/llvm-toolchain-19/1:19.1.7-21ubuntu1~24.04.1 [armhf]\n"
   "    + ✅ llvm-toolchain-19 on noble for armhf   @ 09.02.26 23:21:00\n"
   "      • Log: https://autopkgtest.ubuntu.com/results/autopkgtest-noble-karljs-sru-llvm-19-noble/noble/armhf/l/llvm-toolchain-19/20260209_232100_73c7d@/log.gz\n"
   "  - llvm-toolchain-19: noble/llvm-toolchain-19/1:19.1.7-21ubuntu1~24.04.1 [riscv64]\n"
   "    + ⛔ llvm-toolchain-19 on noble for riscv64 @ 09.02.26 23:35:24\n"
   "      • Log: https://autopkgtest.ubuntu.com/results/autopkgtest-noble-karljs-sru-llvm-19-noble/noble/riscv64/l/llvm-toolchain-19/20260209_233524_1b683@/log.gz\n"
   "      • Status: BAD\n"
   "      • testbed                   BAD    ⛔\n"
   "* Running: (none)\n"
   "* Waiting: (none)\n")
  "Condensed live `ppa tests -L' output.")

(defconst deb-packaging-test-ppa-tests--queue-fixture
  (concat
   "* Triggers:\n"
   "* Results: (none)\n"
   "* Running:\n"
   "  # time     pkg                  release  arch     ppa                       trigger\n"
   "  - 2026-07-20 09:12:34 mypkg    noble    amd64    ppa:me/x                  mypkg/1.0-1\n"
   "* Waiting:\n"
   "  # Q-num    pkg                  release  arch     ppa                       trigger\n"
   "  - 3        mypkg                noble    arm64    ppa:me/x                  mypkg/1.0-1\n")
  "Running/Waiting rows per ppa-dev-tools job.py format.")

(ert-deftest deb-packaging-test-ppa-tests/strip-osc8 ()
  (should (equal (deb-packaging-ppa-tests--strip-osc8
                  "\e]8;;https://example.com\e\\visible\e]8;;\e\\")
                 "visible")))

(ert-deftest deb-packaging-test-ppa-tests/parse-triggers ()
  (let* ((parsed (deb-packaging-ppa-tests--parse
                  deb-packaging-test-ppa-tests--fixture))
         (triggers (plist-get parsed :triggers)))
    (should (= (length triggers) 1))
    (let ((pub (car triggers)))
      (should (equal (plist-get pub :series) "noble"))
      (should (equal (plist-get pub :package) "llvm-toolchain-19"))
      (should (equal (plist-get pub :version) "1:19.1.7-21ubuntu1~24.04.1"))
      (should (equal (plist-get pub :status) "Published"))
      (let ((arches (plist-get pub :arches)))
        (should (= (length arches) 2))
        (let ((amd64 (cdr (assoc "amd64" arches))))
          (should (string-match-p "arch=amd64" (plist-get amd64 :basic)))
          (should-not (string-match-p "all-proposed" (plist-get amd64 :basic)))
          (should (string-match-p "all-proposed=1"
                                  (plist-get amd64 :all-proposed))))))))

(ert-deftest deb-packaging-test-ppa-tests/parse-results ()
  (let* ((parsed (deb-packaging-ppa-tests--parse
                  deb-packaging-test-ppa-tests--fixture))
         (results (plist-get parsed :results)))
    (should (= (length results) 3))
    (let ((fail (nth 0 results)))
      (should (eq (plist-get fail :status) 'fail))
      (should (equal (plist-get fail :source) "llvm-toolchain-19"))
      (should (equal (plist-get fail :series) "noble"))
      (should (equal (plist-get fail :arch) "amd64"))
      (should (equal (plist-get fail :timestamp) "09.02.26 23:21:05"))
      (should (string-match-p "log.gz" (plist-get fail :log-url)))
      (should (equal (plist-get fail :subtests)
                     '(("command1" . "FAIL") ("cmake-llvm-test" . "PASS")
                       ("flaky-test" . "FLAKY") ("skipped-test" . "SKIP")))))
    (let ((pass (nth 1 results)))
      (should (eq (plist-get pass :status) 'pass))
      (should (null (plist-get pass :subtests))))
    (let ((bad (nth 2 results)))
      (should (eq (plist-get bad :status) 'bad))
      (should (equal (plist-get bad :subtests) '(("testbed" . "BAD")))))))

(ert-deftest deb-packaging-test-ppa-tests/parse-empty-queues ()
  (let ((parsed (deb-packaging-ppa-tests--parse
                 deb-packaging-test-ppa-tests--fixture)))
    (should (null (plist-get parsed :running)))
    (should (null (plist-get parsed :waiting)))))

(ert-deftest deb-packaging-test-ppa-tests/parse-queues ()
  (let ((parsed (deb-packaging-ppa-tests--parse
                 deb-packaging-test-ppa-tests--queue-fixture)))
    (should (= (length (plist-get parsed :running)) 1))
    (should (= (length (plist-get parsed :waiting)) 1))
    (should (string-match-p "mypkg" (car (plist-get parsed :running))))
    (should (null (plist-get parsed :results)))
    (should (null (plist-get parsed :triggers)))))

(ert-deftest deb-packaging-test-ppa-tests/summary-counts ()
  (let* ((parsed (deb-packaging-ppa-tests--parse
                  deb-packaging-test-ppa-tests--fixture))
         (summary (deb-packaging-ppa-tests--summary parsed)))
    (should (equal (plist-get summary :pass) 1))
    (should (equal (plist-get summary :fail) 1))
    (should (equal (plist-get summary :bad) 1))
    (should (equal (plist-get summary :running) 0))
    (should (equal (plist-get summary :waiting) 0))))

;;; Report rendering and triggering

;; `text-property-any' compares values with `eq', which cannot match a
;; parsed string against a literal, so compare with `equal' instead.
(defun deb-packaging-test-ppa-tests--has-prop-value-p (prop value)
  "Return non-nil if any character in the buffer has PROP `equal' to VALUE."
  (cl-loop for pos from (point-min) below (point-max)
           thereis (equal (get-text-property pos prop) value)))

(ert-deftest deb-packaging-test-ppa-tests/render-smoke ()
  "The rendered report contains the key lines and text properties."
  (let ((parsed (deb-packaging-ppa-tests--parse
                  deb-packaging-test-ppa-tests--fixture)))
    (with-temp-buffer
      (deb-packaging-ppa-tests-mode)
      (deb-packaging-ppa-tests--render parsed "ppa:me/x")
      (let ((text (buffer-string)))
        (should (string-match-p "PPA tests: ppa:me/x" text))
        (should (string-match-p "llvm-toolchain-19 on noble for amd64" text))
        (should (string-match-p "command1" text))
        (should (string-match-p "t: trigger basic" text))
        (should (string-match-p "Log:" text)))
      (should (deb-packaging-test-ppa-tests--has-prop-value-p
               'deb-packaging-ppa-tests-log-url
               "https://autopkgtest.ubuntu.com/results/autopkgtest-noble-karljs-sru-llvm-19-noble/noble/amd64/l/llvm-toolchain-19/20260209_232105_b52c9@/log.gz"))
      (should (deb-packaging-test-ppa-tests--has-prop-value-p
               'deb-packaging-ppa-tests-desc
               "llvm-toolchain-19 on noble/amd64")))))

(ert-deftest deb-packaging-test-ppa-tests/subtest-faces-distinct ()
  "PASS green, SKIP dim, FLAKY yellow, FAIL red; the report uses the
package's named status faces, not raw success/error."
  (should (eq (deb-packaging-ppa-tests--subtest-face "PASS")
              'deb-packaging-status-done))
  (should (eq (deb-packaging-ppa-tests--subtest-face "SKIP") 'shadow))
  (should (eq (deb-packaging-ppa-tests--subtest-face "FLAKY")
              'deb-packaging-status-running))
  (should (eq (deb-packaging-ppa-tests--subtest-face "FAIL")
              'deb-packaging-status-failed))
  (should (eq (deb-packaging-ppa-tests--subtest-face "BAD")
              'deb-packaging-status-failed))
  ;; The rendered lines actually carry the faces.
  (with-temp-buffer
    (deb-packaging-ppa-tests-mode)
    (deb-packaging-ppa-tests--render
     (deb-packaging-ppa-tests--parse deb-packaging-test-ppa-tests--fixture)
     "ppa:me/x")
    (dolist (cell '(("command1" "FAIL" deb-packaging-status-failed)
                    ("cmake-llvm-test" "PASS" deb-packaging-status-done)
                    ("flaky-test" "FLAKY" deb-packaging-status-running)
                    ("skipped-test" "SKIP" shadow)))
      (goto-char (point-min))
      ;; Find the subtest's line, then its state word; point ends just
      ;; past the state, so the face check targets the last matched
      ;; char (the face covers the state string only).
      (search-forward (concat (car cell) " "))
      (search-forward (cadr cell))
      (should (eq (get-text-property (1- (point)) 'font-lock-face)
                  (nth 2 cell))))
    ;; Result headings use the named faces too.
    (goto-char (point-min))
    (search-forward "llvm-toolchain-19 on noble for armhf")
    (should (eq (get-text-property (point) 'font-lock-face)
                'deb-packaging-status-done))))

(ert-deftest deb-packaging-test-ppa-tests/open-log-from-result-heading ()
  (let (opened)
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url &rest _) (setq opened url))))
      (with-temp-buffer
        (deb-packaging-ppa-tests-mode)
        (deb-packaging-ppa-tests--render
         (deb-packaging-ppa-tests--parse deb-packaging-test-ppa-tests--fixture)
         "ppa:me/x")
        (goto-char (point-min))
        (search-forward "llvm-toolchain-19 on noble for amd64")
        (deb-packaging-ppa-tests-open-log)
        (should (string-match-p "amd64.*log.gz\\'" opened))))))

(ert-deftest deb-packaging-test-ppa-tests/open-log-from-subtest-line ()
  (let (opened)
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url &rest _) (setq opened url))))
      (with-temp-buffer
        (deb-packaging-ppa-tests-mode)
        (deb-packaging-ppa-tests--render
         (deb-packaging-ppa-tests--parse deb-packaging-test-ppa-tests--fixture)
         "ppa:me/x")
        (goto-char (point-min))
        (search-forward "command1")
        (deb-packaging-ppa-tests-open-log)
        (should (string-match-p "amd64.*log.gz\\'" opened))))))

(ert-deftest deb-packaging-test-ppa-tests/open-log-outside-result-errors ()
  (cl-letf (((symbol-function 'browse-url)
             (lambda (&rest _) (error "must not browse"))))
    (with-temp-buffer
      (deb-packaging-ppa-tests-mode)
      (deb-packaging-ppa-tests--render
       (deb-packaging-ppa-tests--parse deb-packaging-test-ppa-tests--fixture)
       "ppa:me/x")
      (goto-char (point-min))
      (search-forward "Triggers")
      (should-error (deb-packaging-ppa-tests-open-log) :type 'user-error))))

(ert-deftest deb-packaging-test-ppa-tests/trigger-basic-confirmed ()
  (let (requested)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url &rest _) (setq requested url)))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (with-temp-buffer
        (insert (propertize "amd64   t: trigger basic"
                            'deb-packaging-ppa-tests-basic-url
                            "https://example.com/basic"
                            'deb-packaging-ppa-tests-desc
                            "mypkg on noble/amd64"))
        (goto-char (point-min))
        (deb-packaging-ppa-tests-trigger-basic)))
    (should (equal requested "https://example.com/basic"))))

(ert-deftest deb-packaging-test-ppa-tests/trigger-declined ()
  (let (requested)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url &rest _) (setq requested url)))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (with-temp-buffer
        (insert (propertize "x"
                            'deb-packaging-ppa-tests-basic-url
                            "https://example.com/basic"))
        (goto-char (point-min))
        (deb-packaging-ppa-tests-trigger-basic)))
    (should (null requested))))

(ert-deftest deb-packaging-test-ppa-tests/trigger-no-url-errors ()
  (with-temp-buffer
    (insert "plain line")
    (goto-char (point-min))
    (should-error (deb-packaging-ppa-tests-trigger-basic) :type 'user-error)))

(ert-deftest deb-packaging-test-ppa-tests/fetch-command ()
  "The fetch runs ppa tests -L with package and release filters."
  (let (captured)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest props) (setq captured props) nil)))
      (deb-packaging-ppa-tests--fetch "ppa:me/x" "mypkg" "noble"))
    (should (equal (plist-get captured :command)
                   '("ppa" "tests" "-L" "ppa:me/x" "-p" "mypkg"
                     "-r" "noble")))))

(ert-deftest deb-packaging-test-ppa-tests/fetch-done-failure-raw-dump ()
  "A failed fetch dumps the raw output and records :status 'failure."
  (let ((deb-packaging-commands--run-history nil)
        (proc (make-symbol "proc")))
    (cl-letf (((symbol-function 'process-status) (lambda (_) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_) 1)))
      (let ((out-buf (generate-new-buffer " *test-ppa-tests-out*"))
            (report-buf (generate-new-buffer " *test-ppa-tests-report*")))
        (unwind-protect
            (progn
              (with-current-buffer out-buf
                (insert "boom output"))
              (deb-packaging-ppa-tests--fetch-done
               proc out-buf report-buf "ppa:me/x")
              (with-current-buffer report-buf
                (should (string-match-p "boom output" (buffer-string)))
                (should (string-match-p "ppa tests failed for ppa:me/x"
                                        (buffer-string))))
              (should (eq (plist-get
                           (deb-packaging-commands-run-record 'ppa-tests)
                           :status)
                          'failure)))
          (kill-buffer out-buf)
          (kill-buffer report-buf))))))

;;; Mode-map conventions

(ert-deftest deb-packaging-test-ppa-tests/map-question-opens-test-transient ()
  (should (eq (lookup-key deb-packaging-ppa-tests-mode-map "?")
              #'deb-packaging-test-transient)))

;;; Fetch races and PPA saving

(ert-deftest deb-packaging-test-ppa-tests/refresh-kills-in-flight-fetch ()
  "A second fetch cancels the first so two sentinels cannot race."
  (let ((killed nil)
        (procs nil)
        (buf nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _)
                 (let ((p (list 'fake (length procs))))
                   (push p procs)
                   p)))
              ((symbol-function 'process-live-p) (lambda (p) (and p t)))
              ((symbol-function 'delete-process) (lambda (p) (push p killed)))
              ((symbol-function 'deb-packaging-commands--record-run)
               (lambda (&rest _) nil))
              ((symbol-function 'deb-packaging-commands--notify-status-refresh)
               (lambda () nil)))
      (deb-packaging-ppa-tests--fetch "ppa:me/x" "pkg" "noble")
      (deb-packaging-ppa-tests--fetch "ppa:me/x" "pkg" "noble")
      (setq buf (get-buffer (deb-packaging-ppa-tests--buffer-name
                             "ppa:me/x" "pkg" "noble")))
      (should (equal (length killed) 1))
      (should (eq (car killed) (cadr procs)))
      (when buf (kill-buffer buf)))))

(ert-deftest deb-packaging-test-ppa-tests/show-does-not-save-ppa ()
  "A one-off report lookup must not clobber the saved default PPA."
  (deb-packaging-test--with-package-tree '(:name "mypkg" :version "1.0-1")
    (let (saved)
      (cl-letf (((symbol-function 'deb-packaging-commands--resolve-ppa)
                 (lambda (_) "ppa:someone/else"))
                ((symbol-function 'deb-packaging-ppa-save)
                 (lambda (&rest args) (setq saved args)))
                ((symbol-function 'deb-packaging-ppa-tests--fetch) #'ignore)
                ((symbol-function 'deb-packaging-display-buffer)
                 (lambda (&rest _) (selected-window))))
        (deb-packaging-ppa-tests-show '("--ppa=ppa:someone/else"))
        (should (null saved))))))

;;; Report buffer naming

(ert-deftest deb-packaging-test-ppa-tests/buffer-name-keys-on-all-params ()
  "Reports for different packages/distros in the same PPA must not clobber."
  (should (not (equal
                (deb-packaging-ppa-tests--buffer-name "ppa:me/x" "a" "noble")
                (deb-packaging-ppa-tests--buffer-name "ppa:me/x" "b" "noble"))))
  (should (not (equal
                (deb-packaging-ppa-tests--buffer-name "ppa:me/x" "a" "noble")
                (deb-packaging-ppa-tests--buffer-name "ppa:me/x" "a" "jammy"))))
  (should (string-match-p "ppa:me/x"
                          (deb-packaging-ppa-tests--buffer-name
                           "ppa:me/x" "a" "noble"))))

(ert-deftest deb-packaging-test-ppa-tests/render-shows-fetched-at ()
  "The report header shows when the data was fetched."
  (let ((parsed (deb-packaging-ppa-tests--parse
                 deb-packaging-test-ppa-tests--fixture)))
    (with-temp-buffer
      (deb-packaging-ppa-tests-mode)
      (deb-packaging-ppa-tests--render parsed "ppa:me/x")
      (should (string-match-p "Fetched:" (buffer-string))))))

(provide 'deb-packaging-test-ppa-tests)
;;; deb-packaging-test-ppa-tests.el ends here
