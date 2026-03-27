;; Fixture: suppress-next declaim is the last sub-form in a defun body.
;; The state must not leak into the next top-level form.
;; Expected: 1 needless-let* violation in bar (not suppressed by foo's leaked state).

(defpackage #:test-suppress-next-last-subform
  (:use #:cl))
(in-package #:test-suppress-next-last-subform)

(defun foo ()
  (let ((x 1))
    x)
  #+mallet (declaim (mallet:suppress-next :needless-let*)))

(defun bar ()
  (let* ((x 1)
         (y 2))
    (+ x y)))
