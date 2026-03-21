;;; CL missing-else fires inside (lisp ...) bodies within coalton-toplevel.
;;; Uses cl: prefix on CL symbols as in real Coalton code — Coalton packages
;;; do not :use CL, so CL symbols must be package-qualified.

(defpackage #:test-coalton-pkg
  (:use))
(in-package #:test-coalton-pkg)

(coalton-toplevel
  (declare foo (Integer -> Integer))
  (define (foo x)
    (lisp Integer (x)
      (cl:if (cl:evenp x) 1)))

  (declare bar (Integer -> Integer))
  (define (bar y)
    (lisp Integer (y)
      (cl:if (cl:> y 0) 1)))

  ;; Coalton if — must NOT trigger missing-else
  (declare baz (Boolean -> Integer))
  (define (baz b)
    (if b 1 0)))
