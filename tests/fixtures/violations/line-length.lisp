(defpackage #:text-rules-example
  (:use #:cl))
(in-package #:text-rules-example)

;;; This file tests text-level rules (line-length)

;; This line is exactly 80 characters long and should not trigger a violation!
;; This line is 81 characters long and should trigger a line-length violation!!
;; This is an extremely long line that definitely exceeds the maximum allowed line length of 80 characters and should be reported as a violation

(defun very-long-function-with-many-parameters (parameter1 parameter2 parameter3 parameter4 parameter5)
  "A function with a very long signature that exceeds line length."
  (list parameter1 parameter2 parameter3 parameter4 parameter5))
