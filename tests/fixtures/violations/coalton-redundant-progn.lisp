;;; Coalton-specific test cases for redundant-progn rule
;;; Violations: (progn single-form) inside coalton-toplevel

(defpackage #:test-pkg
  (:use #:cl))
(in-package #:test-pkg)

(coalton-toplevel
  (declare foo (Integer -> Integer))
  (define (foo x)
    (progn (bar x)))

  (declare baz (Integer -> Integer))
  (define (baz x)
    (progn (qux x)))

  ;; Valid: progn with two forms is not a violation
  (declare ok (Integer -> Integer))
  (define (ok x)
    (progn (a x) (b x))))
