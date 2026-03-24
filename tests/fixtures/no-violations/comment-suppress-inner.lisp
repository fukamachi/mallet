;; Fixture: inner-form inline suppression inside a function body.
;; The suppress comment appears before the nested let* — no violations expected.

(defpackage #:test-comment-suppress-inner
  (:use #:cl))
(in-package #:test-comment-suppress-inner)

(defun foo ()
  ; mallet:suppress :needless-let*
  (let* ((x 1)
         (y 2))
    (+ x y)))
