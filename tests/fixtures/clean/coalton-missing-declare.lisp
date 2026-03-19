(defpackage #:test-pkg
  (:use #:cl))
(in-package #:test-pkg)

(coalton-toplevel
  (declare (add-one Integer -> Integer))
  (define (add-one x) (+ x 1))

  (declare (pi Float))
  (define pi 3.14)

  (define-type Color Red Green Blue)

  (define foo (fn (x) x)))

;; Second block with package-qualified symbols
(coalton:coalton-toplevel
  (coalton:declare (bar String -> String))
  (coalton:define (bar s) s))
