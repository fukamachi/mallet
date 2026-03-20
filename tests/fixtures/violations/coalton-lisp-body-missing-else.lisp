;;; CL missing-else fires inside (lisp ...) bodies within coalton-toplevel.
;;; Coalton (if ...) forms must NOT trigger CL missing-else.

(defpackage #:test-pkg
  (:use #:cl))
(in-package #:test-pkg)

(coalton-toplevel
  (declare foo (Integer -> Integer))
  (define (foo x)
    (lisp Integer (x)
      (if (evenp x) 1)))

  (declare bar (Integer -> Integer))
  (define (bar y)
    (lisp Integer (y)
      (if (> y 0) 1)))

  ;; Coalton if — must NOT trigger missing-else
  (declare baz (Boolean -> Integer))
  (define (baz b)
    (if b 1 0)))
