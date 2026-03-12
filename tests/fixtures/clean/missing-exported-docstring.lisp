;;; Clean examples for missing-exported-docstring rule

(defpackage #:mallet-fixture/missing-exported-docstring-clean
  (:export #:add
           #:widget))
(in-package #:mallet-fixture/missing-exported-docstring-clean)

(defun add (x y)
  "Add X and Y."
  (+ x y))

(defclass widget ()
  ((name :initarg :name))
  (:documentation "A UI widget."))

;; Non-exported function — no missing-exported-docstring violation
(defun internal-helper (x)
  "Double X."
  (* x 2))
