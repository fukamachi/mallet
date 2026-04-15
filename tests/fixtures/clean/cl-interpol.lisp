(defpackage #:cl-interpol-clean-example
  (:use #:cl))
(in-package #:cl-interpol-clean-example)

;;; This file demonstrates cl-interpol string interpolation with no violations.
;;; Variables used inside #?"..." interpolation are considered referenced.

(defun greet (name)
  "Greet by name using cl-interpol string interpolation."
  #?"Hello, ${name}!")

(defun format-point (x y)
  "Format a 2D point using cl-interpol string interpolation."
  #?"Point: (${x}, ${y})")

(defun describe-range (start end)
  "Describe a range using cl-interpol string interpolation."
  (let ((length (- end start)))
    #?"Range from ${start} to ${end}, length ${length}"))

(defun summarize (items)
  "Summarize a list using cl-interpol expression interpolation."
  (let ((count (length items)))
    #?"${count} item${(if (= count 1) \"\" \"s\")}"))

(defun summarize-unescaped (items)
  "Issue #48 pattern: variable referenced ONLY inside ${(or x \"\")} with
unescaped quotes inside the interpolation. Must not produce a false-positive
unused-variables warning for COUNT — that's the exact bug being fixed."
  (let ((count (length items)))
    #?"${count} item${(if (= count 1) "" "s")}"))
