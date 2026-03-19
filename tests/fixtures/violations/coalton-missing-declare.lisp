(defpackage #:test-pkg
  (:use #:cl))
(in-package #:test-pkg)

(coalton-toplevel
  (define (add-one x) (+ x 1))

  (declare subtract-one (Integer -> Integer))
  (define (subtract-one x) (- x 1))

  (define pi 314)

  (define-type Color Red Green Blue)

  (define-instance (Eq Color)
    (define (== a b) (equal a b)))

  (define foo (fn (x) x))

  (define (double x) (* x 2)))
