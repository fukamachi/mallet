(defpackage #:test-disable-form-level
  (:use #:cl))
(in-package #:test-disable-form-level)

; mallet:disable :needless-let*
(defun foo ()
  (let* ((x 1) (y 2))
    (+ x y)))
; mallet:enable :needless-let*
