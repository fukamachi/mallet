;; Test file for suppress-next declarations

(defpackage #:test-suppress-next
  (:use #:cl))
(in-package #:test-suppress-next)

;; This function has if-without-else but is suppressed
#+mallet
(declaim (mallet:suppress-next :if-without-else))
(defun foo (x)
  (if x
      (print "yes")))  ; No else clause - but suppressed!

;; This should trigger if-without-else violation (no suppression)
(defun bar (x)
  (if x
      (print "also yes")))  ; No else clause - should be flagged
