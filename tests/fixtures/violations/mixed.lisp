(defpackage #:mixed-example
  (:use #:cl))
(in-package #:mixed-example)

;; Bad: top-level comment should have 3+ semicolons

;; This is an extremely long line that definitely exceeds the maximum allowed line length of 80 characters

(defun problematic-function (x)
  ; Bad: should have 2 semicolons
  (if (> x 0) ; Line length violation here as well - this comment makes the line too long
      (progn
        (print "positive")
        (print x))))

(defun another-problem (month)
  ;; This function has multiple issues
  (case month
    ((1 2 3) "Q1")
    ((4 5 6) "Q2")
    ((7 8 9) "Q3")))
