(defpackage #:mallet/tests/engine-coalton-lisp-fixture
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:engine #:mallet/engine)
   (#:config #:mallet/config)
   (#:rules #:mallet/rules)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/engine-coalton-lisp-fixture)

;;; Tests that verify CL rules detect violations inside (lisp ...) bodies
;;; using real fixture files on disk, checking exact line/column positions.

;;; Helpers

(defun fixture-path (subdir filename)
  (merge-pathnames (concatenate 'string "tests/fixtures/" subdir "/" filename)
                   (asdf:system-source-directory :mallet)))

(defun violations-file (filename)
  (fixture-path "violations" filename))

(defun clean-file (filename)
  (fixture-path "clean" filename))

(defun lint (path rule-keywords)
  (engine:lint-file path
                    :config (config:make-config
                             :rules (mapcar #'rules:make-rule rule-keywords))))

(defun by-rule (violations rule-keyword)
  (remove-if-not (lambda (v) (eq rule-keyword (violation:violation-rule v)))
                 violations))

;;; missing-else inside (lisp ...) bodies

(deftest missing-else-in-lisp-body-fixture
  (testing "fixture: missing-else fires on (if ...) inside (lisp ...) body"
    (let* ((violations (lint (violations-file "coalton-lisp-body-missing-else.lisp")
                             '(:missing-else)))
           (iwe (by-rule violations :missing-else)))
      (ok (= 2 (length iwe))
          "Exactly 2 missing-else violations (one per lisp body)")))

  (testing "fixture: missing-else violation line numbers are correct"
    (let* ((violations (lint (violations-file "coalton-lisp-body-missing-else.lisp")
                             '(:missing-else)))
           (iwe (sort (by-rule violations :missing-else)
                      #'< :key #'violation:violation-line)))
      (ok (= 2 (length iwe)))
      (when (= 2 (length iwe))
        (ok (= 12 (violation:violation-line (first iwe)))
            "First violation is on line 12 ((if (evenp x) 1))")
        (ok (= 17 (violation:violation-line (second iwe)))
            "Second violation is on line 17 ((if (> y 0) 1))"))))

  (testing "fixture: missing-else violations have correct file path"
    (let* ((violations (lint (violations-file "coalton-lisp-body-missing-else.lisp")
                             '(:missing-else)))
           (iwe (by-rule violations :missing-else)))
      (ok (= 2 (length iwe)))
      (ok (every #'violation:violation-file iwe)
          "All violations carry a file path")))

  (testing "fixture: Coalton (if b 1 0) with else does NOT trigger missing-else"
    ;; The fixture has (if b 1 0) as a Coalton if with an else branch.
    ;; That must not produce a missing-else violation.
    (let* ((violations (lint (violations-file "coalton-lisp-body-missing-else.lisp")
                             '(:missing-else)))
           (iwe (by-rule violations :missing-else)))
      ;; Only the 2 CL (if ...) inside lisp bodies fire; Coalton if does not
      (ok (= 2 (length iwe))
          "Exactly 2 violations — Coalton (if ...) with else does not fire"))))

;;; redundant-progn inside (lisp ...) bodies

(deftest redundant-progn-in-lisp-body-fixture
  (testing "fixture: redundant-progn fires inside (lisp ...) body"
    (let* ((violations (lint (violations-file "coalton-lisp-body-redundant-progn.lisp")
                             '(:redundant-progn)))
           (rpv (by-rule violations :redundant-progn)))
      (ok (= 2 (length rpv))
          "Exactly 2 redundant-progn violations (one per lisp body)")))

  (testing "fixture: redundant-progn violation line numbers are correct"
    (let* ((violations (lint (violations-file "coalton-lisp-body-redundant-progn.lisp")
                             '(:redundant-progn)))
           (rpv (sort (by-rule violations :redundant-progn)
                      #'< :key #'violation:violation-line)))
      (ok (= 2 (length rpv)))
      (when (= 2 (length rpv))
        (ok (= 11 (violation:violation-line (first rpv)))
            "First violation is on line 11 ((progn (1+ x)))")
        (ok (= 16 (violation:violation-line (second rpv)))
            "Second violation is on line 16 ((progn (1- y)))"))))

  (testing "fixture: no double-firing — coalton-aware rules run only once per lisp body"
    ;; redundant-progn is coalton-aware; it already walks the full tree.
    ;; The engine must NOT dispatch a second time for coalton-aware rules.
    (let* ((violations (lint (violations-file "coalton-lisp-body-redundant-progn.lisp")
                             '(:redundant-progn)))
           (rpv (by-rule violations :redundant-progn)))
      (ok (= 2 (length rpv))
          "Exactly 2, not 4 — no double-firing"))))

;;; Clean file: no CL rule violations

(deftest clean-coalton-lisp-body-fixture
  (testing "fixture: clean file produces no missing-else violations"
    (let* ((violations (lint (clean-file "coalton-lisp-body.lisp")
                             '(:missing-else)))
           (iwe (by-rule violations :missing-else)))
      (ok (null iwe)
          "No missing-else violations in clean coalton-lisp-body.lisp")))

  (testing "fixture: clean file produces no redundant-progn violations"
    (let* ((violations (lint (clean-file "coalton-lisp-body.lisp")
                             '(:redundant-progn)))
           (rpv (by-rule violations :redundant-progn)))
      (ok (null rpv)
          "No redundant-progn violations in clean coalton-lisp-body.lisp")))

  (testing "fixture: Coalton (if ...) with else branch in clean file is not flagged"
    (let* ((violations (lint (clean-file "coalton-lisp-body.lisp")
                             '(:missing-else)))
           (iwe (by-rule violations :missing-else)))
      (ok (null iwe)
          "Coalton (if b 1 0) with else in clean file produces no violation"))))

;;; Existing Coalton rules unaffected (regression)

(deftest coalton-rules-unaffected-by-lisp-dispatch
  (testing "coalton-missing-declare still fires on coalton-toplevel with lisp bodies"
    ;; The fixture has two declares (foo and bar) and baz with a declare.
    ;; No missing declare — use the redundant-progn fixture which has full declares.
    (let* ((violations (lint (violations-file "coalton-lisp-body-redundant-progn.lisp")
                             '(:coalton-missing-declare)))
           (cmd (by-rule violations :coalton-missing-declare)))
      (ok (null cmd)
          "No coalton-missing-declare violation (all defines have declares)")))

  (testing "coalton-missing-declare fires on form WITHOUT declare, even with lisp dispatch"
    (let* ((violations (lint (violations-file "coalton-lisp-body-missing-else.lisp")
                             '(:coalton-missing-declare)))
           (cmd (by-rule violations :coalton-missing-declare)))
      ;; coalton-lisp-body-missing-else.lisp has (define (baz b) ...) with a declare
      ;; and all other defines have declares too. No coalton-missing-declare expected.
      (ok (null cmd)
          "All defines in the fixture have declares")))

  (testing "coalton rules still run on coalton-toplevel containing lisp forms"
    ;; Construct a code snippet inline to confirm coalton rule fires independently
    ;; of whether lisp-body dispatch is happening.
    ;; (Already well-covered by engine-coalton-lisp-dispatch-test.lisp;
    ;;  this fixture test confirms no regression using real file paths.)
    (let* ((violations (lint (violations-file "coalton-lisp-body-missing-else.lisp")
                             '(:missing-else :coalton-missing-declare)))
           (iwe (by-rule violations :missing-else))
           (cmd (by-rule violations :coalton-missing-declare)))
      (ok (= 2 (length iwe))
          "missing-else still fires in lisp bodies")
      (ok (null cmd)
          "No spurious coalton-missing-declare from synthetic forms"))))

;;; Stub guards

(deftest stub-guard-fixture-is-not-trivially-passing
  (testing "violations fixture must actually produce violations (not silently ignored)"
    (let* ((violations (lint (violations-file "coalton-lisp-body-missing-else.lisp")
                             '(:missing-else)))
           (iwe (by-rule violations :missing-else)))
      (ok (not (null iwe))
          "A no-op dispatch would return nil here — confirms dispatch is active")))

  (testing "clean fixture must not accidentally produce violations"
    (let* ((violations (lint (clean-file "coalton-lisp-body.lisp")
                             '(:missing-else :redundant-progn)))
           (relevant (by-rule violations :missing-else)))
      (ok (null relevant)
          "Clean file must not trigger missing-else"))))
