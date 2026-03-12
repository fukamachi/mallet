;; Test fixture for declaim suppress-next stale tracking

(defpackage #:test-declaim-stale
  (:use #:cl))
(in-package #:test-declaim-stale)

;; This suppress-next has no violation following — should be stale
#+mallet
(declaim (mallet:suppress-next :needless-let*))
(defun clean-function (x)
  ;; No needless-let* here: single binding, no independent bindings
  (let* ((a (+ x 1))
         (b (* a 2)))
    (+ a b)))  ; b depends on a, so let* is valid

;; This suppress-next IS used — should NOT be stale
#+mallet
(declaim (mallet:suppress-next :needless-let*))
(defun suppressed-function ()
  ;; Independent bindings: needless-let* would fire, but suppressed
  (let* ((a 1)
         (b 2))
    (+ a b)))
