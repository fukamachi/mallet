(defpackage #:clean-example
  (:use #:cl))
(in-package #:clean-example)

;;; This file has no violations

(defun add (a b)
  "Add two numbers."
  (+ a b))

(defun greet (name)
  "Greet a person by name."
  (when name
    (format nil "Hello, ~A!" name)))

(defun process-values (values)
  "Process a list of values."
  (loop for value in values
        collect (* value 2)))
