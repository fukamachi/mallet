;;; Coalton-specific test cases for progn-in-conditional rule
;;; Violations: (if cond (progn ...) ...) inside coalton-toplevel

(defpackage #:test-pkg
  (:use #:cl))
(in-package #:test-pkg)

(coalton-toplevel
  (declare foo (Boolean -> Integer))
  (define (foo x)
    (if x
        (progn (bar x) (baz x))
        (qux x)))

  (declare also-bad (Boolean -> Integer))
  (define (also-bad x)
    (if x
        (quux x)
        (progn (bar x) (baz x))))

  ;; Valid: if without progn is fine
  (declare ok (Boolean -> Integer))
  (define (ok x)
    (if x (bar x) (baz x))))
