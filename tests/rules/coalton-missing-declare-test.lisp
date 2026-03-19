(defpackage #:mallet/tests/rules/coalton-missing-declare
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/coalton-missing-declare)

;;; Helper

(defun check-missing-declare (code)
  (let ((forms (parser:parse-forms code #p"test.lisp"))
        (rule (make-instance 'rules:coalton-missing-declare-rule)))
    (mapcan (lambda (form)
              (base:check-form rule form #p"test.lisp"))
            forms)))

;;; Valid cases — no violations expected

(deftest valid-cases
  (testing "function define preceded by declare — no violation"
    (ok (null (check-missing-declare
               "(coalton-toplevel
                  (declare (add-one Integer -> Integer))
                  (define (add-one x) (+ x 1)))"))))

  (testing "value define preceded by declare — no violation"
    (ok (null (check-missing-declare
               "(coalton-toplevel
                  (declare (pi Float))
                  (define pi 3.14))"))))

  (testing "multiple defines each with a preceding declare — no violation"
    (ok (null (check-missing-declare
               "(coalton-toplevel
                  (declare (foo Integer -> Integer))
                  (define (foo x) x)
                  (declare (bar String -> Integer))
                  (define (bar s) (length s)))"))))

  (testing "empty coalton-toplevel — no violation"
    (ok (null (check-missing-declare "(coalton-toplevel)"))))

  (testing "only declare forms — no violation"
    (ok (null (check-missing-declare
               "(coalton-toplevel
                  (declare (foo Integer -> Integer)))"))))

  (testing "declare before function define is matched case-insensitively"
    (ok (null (check-missing-declare
               "(coalton-toplevel
                  (declare (ADD-ONE Integer -> Integer))
                  (define (add-one x) x))"))))

  (testing "non-coalton forms do not trigger the rule"
    (ok (null (check-missing-declare
               "(defun foo (x) (+ x 1))"))))

  (testing "define inside function body is not checked at toplevel"
    (ok (null (check-missing-declare
               "(coalton-toplevel
                  (declare (outer Integer -> Integer))
                  (define (outer x)
                    (define inner (+ x 1))
                    inner))")))))

;;; Invalid cases — violations expected

(deftest function-define-without-declare
  (testing "function define with no declare at all — one violation"
    (let ((violations (check-missing-declare
                       "(coalton-toplevel
                          (define (add-one x) (+ x 1)))")))
      (ok (= 1 (length violations)))
      (ok (eq :coalton-missing-declare (violation:violation-rule (first violations))))))

  (testing "violation message includes the function name"
    (let ((violations (check-missing-declare
                       "(coalton-toplevel
                          (define (my-fn x) x))")))
      (ok (search "my-fn" (violation:violation-message (first violations))
                  :test #'char-equal))))

  (testing "function define where declare is for a different name — one violation"
    (let ((violations (check-missing-declare
                       "(coalton-toplevel
                          (declare (other-fn Integer -> Integer))
                          (define (add-one x) (+ x 1)))")))
      (ok (= 1 (length violations)))
      (ok (eq :coalton-missing-declare (violation:violation-rule (first violations))))))

  (testing "declare appears AFTER define — violation (declare too late)"
    (let ((violations (check-missing-declare
                       "(coalton-toplevel
                          (define (add-one x) (+ x 1))
                          (declare (add-one Integer -> Integer)))")))
      (ok (= 1 (length violations)))))

  (testing "multiple defines, only some have declares — partial violations"
    (let ((violations (check-missing-declare
                       "(coalton-toplevel
                          (declare (foo Integer -> Integer))
                          (define (foo x) x)
                          (define (bar s) (length s)))")))
      (ok (= 1 (length violations)))
      (ok (search "bar" (violation:violation-message (first violations))
                  :test #'char-equal))))

  (testing "multiple defines all missing declares — multiple violations"
    (let ((violations (check-missing-declare
                       "(coalton-toplevel
                          (define (foo x) x)
                          (define (bar y) y))")))
      (ok (= 2 (length violations))))))

(deftest value-define-not-flagged
  (testing "value define without declare — NOT flagged (only function defines are checked)"
    (ok (null (check-missing-declare
               "(coalton-toplevel
                  (define pi 3.14))"))))

  (testing "define-type without declare — NOT flagged"
    (ok (null (check-missing-declare
               "(coalton-toplevel
                  (define-type Color Red Green Blue))"))))

  (testing "define-instance without declare — NOT flagged"
    (ok (null (check-missing-declare
               "(coalton-toplevel
                  (define-instance (Eq MyType)
                    (define (== a b) (equal a b))))"))))

  (testing "define with fn body (lambda value) — NOT flagged (syntactically a value define)"
    (ok (null (check-missing-declare
               "(coalton-toplevel
                  (define foo (fn (x) x)))"))))

  (testing "define-struct without declare — NOT flagged"
    (ok (null (check-missing-declare
               "(coalton-toplevel
                  (define-struct Point (x Integer) (y Integer)))")))))

(deftest package-qualified-symbols
  (testing "package-qualified coalton:define without declare — flagged"
    (let ((violations (check-missing-declare
                       "(coalton-toplevel
                          (coalton:define (add-one x) (+ x 1)))")))
      (ok (= 1 (length violations)))))

  (testing "package-qualified coalton:declare satisfies requirement"
    (ok (null (check-missing-declare
               "(coalton-toplevel
                  (coalton:declare (add-one Integer -> Integer))
                  (coalton:define (add-one x) (+ x 1)))"))))

  (testing "mixed qualified and unqualified — declare matches define"
    (ok (null (check-missing-declare
               "(coalton-toplevel
                  (declare (add-one Integer -> Integer))
                  (coalton:define (add-one x) (+ x 1)))"))))

  (testing "package-qualified coalton:coalton-toplevel is handled"
    (let ((violations (check-missing-declare
                       "(coalton:coalton-toplevel
                          (define (foo x) x))")))
      (ok (= 1 (length violations))))))

(deftest multiple-coalton-toplevel-blocks
  (testing "each coalton-toplevel block is independent — declare in first does not cover second"
    (let ((violations (check-missing-declare
                       "(coalton-toplevel
                          (declare (foo Integer -> Integer))
                          (define (foo x) x))
                        (coalton-toplevel
                          (define (foo x) x))")))
      (ok (= 1 (length violations)))
      (ok (search "FOO" (violation:violation-message (first violations))
                  :test #'char-equal))))

  (testing "two blocks both with missing declares — two violations"
    (let ((violations (check-missing-declare
                       "(coalton-toplevel
                          (define (foo x) x))
                        (coalton-toplevel
                          (define (bar y) y))")))
      (ok (= 2 (length violations)))))

  (testing "two blocks both fully declared — no violations"
    (ok (null (check-missing-declare
               "(coalton-toplevel
                  (declare (foo Integer -> Integer))
                  (define (foo x) x))
                (coalton-toplevel
                  (declare (bar String -> String))
                  (define (bar s) s))")))))

(deftest robustness-edge-cases
  (testing "non-list second element in define is a value define — not flagged"
    (ok (null (check-missing-declare
               "(coalton-toplevel
                  (define my-const 42))"))))

  (testing "define with string body is a value define — not flagged"
    (ok (null (check-missing-declare
               "(coalton-toplevel
                  (define greeting \"hello\"))"))))

  (testing "coalton-toplevel with only define-type and define-instance — no violations"
    (ok (null (check-missing-declare
               "(coalton-toplevel
                  (define-type Color Red Green Blue)
                  (define-instance (Eq Color)
                    (define (== a b) (equal a b))))"))))

  (testing "declare with matching name but define is a value — no violation"
    (ok (null (check-missing-declare
               "(coalton-toplevel
                  (declare (x Integer))
                  (define x 42))"))))

  (testing "function define where declare is for SAME name later — still a violation"
    (let ((violations (check-missing-declare
                       "(coalton-toplevel
                          (define (foo x) x)
                          (declare (foo Integer -> Integer)))")))
      (ok (= 1 (length violations))))))

(deftest violation-severity
  (testing "violation has :warning severity"
    (let ((violations (check-missing-declare
                       "(coalton-toplevel
                          (define (foo x) x))")))
      (ok (eq :warning (violation:violation-severity (first violations))))))

  (testing "violation has :practice category"
    (let ((violations (check-missing-declare
                       "(coalton-toplevel
                          (define (foo x) x))")))
      (ok (eq :practice (violation:violation-category (first violations)))))))

(deftest registration
  (testing "coalton-missing-declare-rule can be instantiated via make-rule"
    (ok (rules:make-rule :coalton-missing-declare)))

  (testing "coalton-missing-declare-rule has :practice category"
    (let ((rule (rules:make-rule :coalton-missing-declare)))
      (ok (eq :practice (base:rule-category rule))))))
