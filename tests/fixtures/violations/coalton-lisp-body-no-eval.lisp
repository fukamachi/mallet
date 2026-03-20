;;; CL no-eval fires inside (lisp ...) bodies within coalton-toplevel.
;;; no-eval is a CL-only rule (not coalton-aware), so it only fires
;;; via synthetic form dispatch.

(defpackage #:test-pkg
  (:use #:cl))
(in-package #:test-pkg)

(coalton-toplevel
  (declare foo (Integer -> Integer))
  (define (foo x)
    (lisp Integer (x)
      (eval (list '+ x 1))))

  (declare bar (Integer -> Integer))
  (define (bar y)
    (lisp Integer (y)
      (eval (list '* y 2)))))
