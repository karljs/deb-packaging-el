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
                     '(("command1" . "FAIL") ("cmake-llvm-test" . "PASS")))))
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

(provide 'deb-packaging-test-ppa-tests)
;;; deb-packaging-test-ppa-tests.el ends here
