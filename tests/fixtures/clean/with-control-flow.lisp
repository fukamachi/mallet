(defpackage #:clean-control-flow
  (:use #:cl))
(in-package #:clean-control-flow)

;;; This file demonstrates correct control flow patterns

;;; Correct: when instead of if without else
(defun check-positive (x)
  "Check if x is positive."
  (when (> x 0)
    (print "positive")))

;;; Correct: cond instead of if with progn
(defun handle-value (x)
  "Handle different value ranges."
  (cond
    ((> x 0)
     (print "positive")
     (print x))
    (t
     (print "not positive")
     (print x))))

;;; Correct: case with otherwise
(defun days-in-month (month leap-year-p)
  "Get number of days in a month."
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (2 (if leap-year-p 29 28))
    (otherwise (error "Invalid month"))))

;;; Correct: ecase without otherwise (exhaustive)
(defun classify-priority (priority)
  "Classify priority level."
  (ecase priority
    (:high 1)
    (:medium 2)
    (:low 3)))

;;; Correct: if with proper else clause
(defun absolute-value (x)
  "Compute absolute value."
  (if (>= x 0)
      x
      (- x)))
