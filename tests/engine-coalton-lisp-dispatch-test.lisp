(defpackage #:mallet/tests/engine-coalton-lisp-dispatch
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:engine #:mallet/engine)
   (#:config #:mallet/config)
   (#:rules #:mallet/rules)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/engine-coalton-lisp-dispatch)

;;; Tests for synthetic form dispatch:
;;; CL rules should fire inside (lisp Type (vars) body...) forms
;;; that appear inside coalton-toplevel.

;;; Helpers

(defun lint-string (code rule-keywords)
  "Write CODE to a temp file, lint it with RULE-KEYWORDS, return violations."
  (let ((path (uiop:tmpize-pathname
               (merge-pathnames "mallet-test-XXXXXXXX.lisp"
                                (uiop:temporary-directory)))))
    (unwind-protect
         (progn
           (uiop:with-output-file (s path :if-exists :supersede)
             (write-string code s))
           (let ((cfg (config:make-config
                       :rules (mapcar #'rules:make-rule rule-keywords))))
             (engine:lint-file path :config cfg)))
      (uiop:delete-file-if-exists path))))

(defun violations-for-rule (violations rule-keyword)
  "Filter VIOLATIONS to only those for RULE-KEYWORD."
  (remove-if-not (lambda (v) (eq rule-keyword (violation:violation-rule v)))
                 violations))

;;; Core dispatch: CL rule fires inside (lisp ...) body

(deftest cl-rule-fires-inside-lisp-body
  (testing "missing-else fires on (if ...) inside (lisp ...) body"
    (let* ((violations (lint-string
                        "(coalton-toplevel
  (define (foo x)
    (lisp Integer (x)
      (if (evenp x) 1))))"
                        '(:missing-else)))
           (iwe (violations-for-rule violations :missing-else)))
      (ok (= 1 (length iwe))
          "Exactly one missing-else violation inside lisp body")
      (ok (eq :missing-else (violation:violation-rule (first iwe))))))

  (testing "no violation when if has else branch"
    (let* ((violations (lint-string
                        "(coalton-toplevel
  (define (foo x)
    (lisp Integer (x)
      (if (evenp x) 1 0))))"
                        '(:missing-else)))
           (iwe (violations-for-rule violations :missing-else)))
      (ok (null iwe)
          "No missing-else when if has else clause"))))

;;; Line number accuracy

(deftest cl-violation-line-number
  (testing "violation line matches position of the if form inside lisp body"
    (let* ((violations (lint-string
                        "(coalton-toplevel
  (define (foo x)
    (lisp Integer (x)
      (if (evenp x) 1))))"
                        '(:missing-else)))
           (iwe (violations-for-rule violations :missing-else)))
      (ok (= 1 (length iwe)))
      (when (= 1 (length iwe))
        ;; The (if ...) is on line 4
        (ok (= 4 (violation:violation-line (first iwe)))
            "Violation line is 4 (line of the if form)")))))

;;; Multiple lisp forms

(deftest multiple-lisp-forms-all-checked
  (testing "violations from multiple lisp forms are all collected"
    (let* ((violations (lint-string
                        "(coalton-toplevel
  (define (foo x)
    (lisp Integer (x)
      (if (evenp x) 1)))
  (define (bar y)
    (lisp Integer (y)
      (if (> y 0) 1))))"
                        '(:missing-else)))
           (iwe (violations-for-rule violations :missing-else)))
      (ok (= 2 (length iwe))
          "Both missing-else violations (one per lisp form) are reported"))))

;;; Clean lisp body

(deftest clean-lisp-body-no-violation
  (testing "no extra violations for a lisp body with no CL issues"
    (let* ((violations (lint-string
                        "(coalton-toplevel
  (define (foo x)
    (lisp Integer (x)
      (1+ x))))"
                        '(:missing-else)))
           (iwe (violations-for-rule violations :missing-else)))
      (ok (null iwe)
          "No violations for a clean lisp body"))))

;;; Non-coalton forms unaffected

(deftest non-coalton-forms-still-processed
  (testing "CL rules still run on non-coalton forms"
    (let* ((violations (lint-string
                        "(defun foo (x)
  (if (evenp x) 1))"
                        '(:missing-else)))
           (iwe (violations-for-rule violations :missing-else)))
      (ok (= 1 (length iwe))
          "missing-else fires on plain defun (not coalton) as before")))

  (testing "coalton-toplevel without lisp forms produces no CL rule violations"
    (let* ((violations (lint-string
                        "(coalton-toplevel
  (define (foo x)
    (match x
      ((Some y) y)
      (None 0))))"
                        '(:missing-else)))
           (iwe (violations-for-rule violations :missing-else)))
      (ok (null iwe)
          "No missing-else violations for coalton-toplevel without any lisp form"))))

;;; Coalton rules still run on coalton-toplevel (regression)

(deftest coalton-rules-still-run
  (testing "coalton-missing-declare still fires on coalton-toplevel (not broken by dispatch)"
    (let* ((violations (lint-string
                        "(coalton-toplevel
  (define (foo x)
    (lisp Integer (x) (1+ x))))"
                        '(:coalton-missing-declare)))
           (cmd (violations-for-rule violations :coalton-missing-declare)))
      (ok (= 1 (length cmd))
          "coalton-missing-declare still fires despite lisp dispatch"))))

;;; Nested lisp form (deep in tree)

(deftest nested-lisp-form-violations-found
  (testing "violation in a lisp form nested inside match is found"
    (let* ((violations (lint-string
                        "(coalton-toplevel
  (define (foo x)
    (match x
      ((Some y)
       (lisp Integer (y)
         (if (> y 0) 1))))))"
                        '(:missing-else)))
           (iwe (violations-for-rule violations :missing-else)))
      (ok (= 1 (length iwe))
          "missing-else found in lisp body nested inside match"))))

;;; Atom-only lisp body → no dispatch

(deftest atom-only-lisp-body-no-dispatch
  (testing "lisp body with only atoms generates no CL rule violations"
    ;; (lisp Integer (x) some-symbol) — atom body, not dispatched
    (let* ((violations (lint-string
                        "(coalton-toplevel
  (define x
    (lisp Integer () 42)))"
                        '(:missing-else)))
           (iwe (violations-for-rule violations :missing-else)))
      (ok (null iwe)
          "No dispatch for atom-only lisp body"))))

;;; Nested lisp inside Coalton let

(deftest nested-lisp-inside-let
  (testing "violation in a lisp form nested inside let binding is found"
    (let* ((violations (lint-string
                        "(coalton-toplevel
  (define (foo x)
    (let ((y (lisp Integer (x)
               (if (evenp x) 1))))
      y)))"
                        '(:missing-else)))
           (iwe (violations-for-rule violations :missing-else)))
      (ok (= 1 (length iwe))
          "missing-else found in lisp body nested inside let"))))

;;; Nested lisp inside Coalton if branch

(deftest nested-lisp-inside-if
  (testing "violation in a lisp form nested inside Coalton if branch is found"
    (let* ((violations (lint-string
                        "(coalton-toplevel
  (declare foo (Integer -> Integer))
  (define (foo x)
    (if (== x 0)
        (lisp Integer (x)
          (if (evenp x) 1))
        0)))"
                        '(:missing-else)))
           (iwe (violations-for-rule violations :missing-else)))
      (ok (= 1 (length iwe))
          "missing-else found in lisp body nested inside Coalton if"))))

;;; Second CL rule: redundant-progn also fires

(deftest redundant-progn-in-lisp-body
  (testing "redundant-progn fires inside (lisp ...) body"
    (let* ((violations (lint-string
                        "(coalton-toplevel
  (define (foo x)
    (lisp Integer (x)
      (progn (1+ x)))))"
                        '(:redundant-progn)))
           (rpv (violations-for-rule violations :redundant-progn)))
      ;; redundant-progn fires on both the coalton-toplevel form (Coalton-aware rule)
      ;; and the synthetic lisp body form, so expect >= 1
      (ok (<= 1 (length rpv))
          "At least one redundant-progn violation from lisp body"))))

;;; Multiple CL rules simultaneously

(deftest multiple-cl-rules-on-lisp-body
  (testing "multiple CL rules fire independently on the same lisp body"
    (let* ((violations (lint-string
                        "(coalton-toplevel
  (define (foo x)
    (lisp Integer (x)
      (progn
        (if (evenp x) 1)))))"
                        '(:missing-else :redundant-progn)))
           (iwe (violations-for-rule violations :missing-else))
           (rpv (violations-for-rule violations :redundant-progn)))
      (ok (<= 1 (length iwe))
          "missing-else fires")
      (ok (<= 1 (length rpv))
          "redundant-progn fires"))))

;;; Synthetic form inherits position-map from parent

(deftest synthetic-form-position-map-inherited
  (testing "violation file matches the original coalton source file"
    (let* ((violations (lint-string
                        "(coalton-toplevel
  (define (foo x)
    (lisp Integer (x)
      (if (evenp x) 1))))"
                        '(:missing-else)))
           (iwe (violations-for-rule violations :missing-else)))
      (ok (= 1 (length iwe)))
      ;; The violation should have a file (not NIL) — inherited from the parent form
      (when (= 1 (length iwe))
        (ok (violation:violation-file (first iwe))
            "Violation has a file path (inherited from parent form)")))))

;;; Coalton rules do not fire on synthetic CL forms
;;; Note: coalton rules CAN fire on the parent coalton-toplevel form, which
;;; recursively walks the tree including lisp bodies. The key invariant is that
;;; coalton rules do NOT run on synthetic forms (where coalton-form-p is nil).
;;; We verify this indirectly: a coalton-only rule with no matching pattern in
;;; the lisp body should produce zero violations from the synthetic dispatch.

(deftest coalton-rules-skip-synthetic-forms
  (testing "coalton-missing-declare fires only on the coalton-toplevel form, not on synthetic forms"
    ;; coalton-missing-declare fires on the parent coalton-toplevel because there
    ;; is no (declare ...) for foo. It should NOT fire again from the synthetic form.
    (let* ((violations (lint-string
                        "(coalton-toplevel
  (define (foo x)
    (lisp Integer (x) (1+ x))))"
                        '(:coalton-missing-declare)))
           (cmd (violations-for-rule violations :coalton-missing-declare)))
      (ok (= 1 (length cmd))
          "Exactly one coalton-missing-declare (from parent form, not duplicated by synthetic)"))))

;;; Stub guards (prevent trivially-passing implementations)

(deftest stub-guard-dispatch-is-not-no-op
  (testing "a real missing-else violation in lisp body must be reported (not ignored)"
    (let* ((violations (lint-string
                        "(coalton-toplevel
  (define (foo x)
    (lisp Integer (x)
      (if (evenp x) 1))))"
                        '(:missing-else)))
           (iwe (violations-for-rule violations :missing-else)))
      (ok (not (null iwe))
          "A stub that ignores lisp bodies would return nil here — fails if no dispatch"))))

(deftest stub-guard-dispatch-uses-correct-rule
  (testing "violation rule keyword is :missing-else, not some synthetic rule"
    (let* ((violations (lint-string
                        "(coalton-toplevel
  (define (foo x)
    (lisp Integer (x)
      (if (evenp x) 1))))"
                        '(:missing-else)))
           (iwe (violations-for-rule violations :missing-else)))
      (ok (= 1 (length iwe)))
      (when (= 1 (length iwe))
        (ok (eq :missing-else (violation:violation-rule (first iwe)))
            "Violation rule is :missing-else, as reported by the CL rule")))))

(deftest stub-guard-no-false-positives-on-pure-coalton
  (testing "pure Coalton code (no lisp forms) generates zero CL violations"
    (let* ((violations (lint-string
                        "(coalton-toplevel
  (declare foo (Integer -> Boolean))
  (define (foo x)
    (if (== x 0) True False)))"
                        '(:missing-else)))
           (iwe (violations-for-rule violations :missing-else)))
      (ok (null iwe)
          "Coalton (if ...) should not trigger CL missing-else rule"))))
