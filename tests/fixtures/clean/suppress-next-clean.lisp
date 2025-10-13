;; Test file demonstrating working suppress-next feature
;; All violations in this file are suppressed, so it should be "clean"

(defpackage #:test-suppress-next-clean
  (:use #:cl))
(in-package #:test-suppress-next-clean)

;; Suppress if-without-else violation
#+mallet
(declaim (mallet:suppress-next :if-without-else))
(defun conditional-print (x)
  (if x
      (print "yes")))  ; No else - suppressed!

;; Suppress multiple violations at once
#+mallet
(declaim (mallet:suppress-next :if-without-else :bare-progn-in-if))
(defun complex-conditional (x y)
  (if (and x y)
      (progn
        (print "both true")
        (+ x y))))  ; Both violations suppressed!

;; Multiple suppress-next declarations in sequence
#+mallet
(declaim (mallet:suppress-next :if-without-else))
#+mallet
(declaim (mallet:suppress-next :bare-progn-in-if))
(defun another-conditional (x)
  (if x
      (progn
        (print "yes")
        x)))  ; Both violations suppressed!
