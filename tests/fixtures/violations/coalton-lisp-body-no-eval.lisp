;;; CL no-eval fires inside (lisp ...) bodies within coalton-toplevel.
;;; no-eval is a CL-only rule (not coalton-aware), so it only fires
;;; via synthetic form dispatch.
;;; Uses cl: prefix as in real Coalton code.

(defpackage #:test-coalton-pkg
  (:use))
(in-package #:test-coalton-pkg)

(coalton-toplevel
  (declare foo (Integer -> Integer))
  (define (foo x)
    (lisp Integer (x)
      (cl:eval (cl:list 'cl:+ x 1))))

  (declare bar (Integer -> Integer))
  (define (bar y)
    (lisp Integer (y)
      (cl:eval (cl:list 'cl:* y 2)))))
