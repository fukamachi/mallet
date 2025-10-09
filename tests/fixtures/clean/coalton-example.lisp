;; This file demonstrates Coalton code that should not trigger Common Lisp linting violations

(defpackage #:example/coalton
  (:use #:cl)
  (:local-nicknames
   (#:coal #:coalton-library/builtin)))
(in-package #:example/coalton)

;; Coalton code has different semantics - should not trigger CL linting
(coalton-toplevel
  ;; These would be violations in Common Lisp but are fine in Coalton
  (define (if-example x)
    (if (> x 0)
        "positive"
        ;; No else clause - this is fine in Coalton, would be violation in CL
        ))

  (define (case-example x)
    (match x
      ((Some y) y)
      ;; No catch-all - Coalton has exhaustiveness checking, unlike CL case
      ))

  (define (unused-var-example x)
    ;; 'y' is unused - but Coalton has different semantics
    (let ((y 10))
      x)))

;; Qualified coalton-toplevel should also work
(coalton:coalton-toplevel
  (define (another-example n)
    (if (< n 10)
        n)))
