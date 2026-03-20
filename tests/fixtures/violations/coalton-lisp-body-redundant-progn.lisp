;;; CL redundant-progn fires inside (lisp ...) bodies within coalton-toplevel.

(defpackage #:test-pkg
  (:use #:cl))
(in-package #:test-pkg)

(coalton-toplevel
  (declare foo (Integer -> Integer))
  (define (foo x)
    (lisp Integer (x)
      (progn (1+ x))))

  (declare bar (Integer -> Integer))
  (define (bar y)
    (lisp Integer (y)
      (progn (1- y)))))
