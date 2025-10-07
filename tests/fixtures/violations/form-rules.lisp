(defpackage #:form-rules-example
  (:use #:cl))
(in-package #:form-rules-example)

;;; This file tests form-level rules

;;; if-without-else rule violations

(defun check-positive (x)
  "Bad: if without else should use when."
  (if (> x 0)
      (print "positive")))

;;; bare-progn-in-if rule violations

(defun handle-value (x)
  "Bad: bare progn in if should use cond."
  (if (> x 0)
      (progn
        (print "positive")
        (print x))
      (progn
        (print "not positive")
        (print x))))

;;; missing-otherwise rule violations

(defun days-in-month (month)
  "Bad: case without otherwise clause."
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (2 28)))

(defun process-type (value)
  "Bad: typecase without otherwise clause."
  (typecase value
    (integer (handle-integer value))
    (string (handle-string value))))

(defun classify-with-t (value)
  "Bad: case with t instead of otherwise."
  (case value
    (:small 1)
    (:large 2)
    (t 0)))

;;; wrong-otherwise rule violations (ERROR severity)

(defun strict-check (value)
  "Bad: ecase should not have otherwise."
  (ecase value
    (:a 1)
    (:b 2)
    (otherwise 0)))

(defun strict-type-check (value)
  "Bad: etypecase should not have otherwise."
  (etypecase value
    (integer 1)
    (string 2)
    (otherwise 0)))

;;; Helper functions (no violations)

(defun handle-integer (x) x)
(defun handle-string (s) s)
