;;; Clean Coalton code: no CL rule violations.
;;; - Coalton (if ...) with else — no missing-else
;;; - (lisp ...) bodies with no violating CL patterns

(defpackage #:test-pkg
  (:use #:cl))
(in-package #:test-pkg)

(coalton-toplevel
  (declare classify (Integer -> Integer))
  (define (classify x)
    (lisp Integer (x)
      (if (evenp x) 1 0)))

  (declare add-one (Integer -> Integer))
  (define (add-one x)
    (lisp Integer (x)
      (1+ x)))

  ;; Coalton if with else branches — not a CL missing-else
  (declare baz (Boolean -> Integer))
  (define (baz b)
    (if b 1 0))

  ;; Pure Coalton: no lisp escape at all
  (declare double (Integer -> Integer))
  (define (double x)
    (* x 2)))
