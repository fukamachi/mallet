;; Fixture: suppress comment appears AFTER the violating form.
;; A suppress placed below a violation should not retroactively suppress it.
;; Expected: 1 needless-let* violation (the let* before the suppress).

(defpackage #:test-comment-suppress-after-violation
  (:use #:cl))
(in-package #:test-comment-suppress-after-violation)

(defun foo ()
  (let* ((x 1)
         (y 2))
    (+ x y))
  ; mallet:suppress :needless-let*
  (let ((z 3))
    z))
