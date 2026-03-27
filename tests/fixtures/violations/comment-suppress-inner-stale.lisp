;; Fixture: stale inner-form inline suppression.
;; The suppression targets needless-let* but there is no let* — one stale violation expected.

(defpackage #:test-comment-suppress-inner-stale
  (:use #:cl))
(in-package #:test-comment-suppress-inner-stale)

(defun foo ()
  ; mallet:suppress :needless-let*
  (let ((x 1)
        (y 2))
    (+ x y)))
