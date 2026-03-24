;; Fixture: inner-form suppress-next declaim inside a function body.
;; The declaim appears before the nested let* — no violations expected.

(defpackage #:test-declaim-suppress-inner
  (:use #:cl))
(in-package #:test-declaim-suppress-inner)

(defun foo ()
  #+mallet (declaim (mallet:suppress-next :needless-let*))
  (let* ((x 1)
         (y 2))
    (+ x y)))
