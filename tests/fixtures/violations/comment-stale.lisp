;; Test fixture for stale suppression detection

(defpackage #:test-comment-stale
  (:use #:cl))
(in-package #:test-comment-stale)

;; This suppress has no matching violation — should produce stale-suppression violation
; mallet:suppress :missing-else
(defun clean-function (x)
  (if x
      (print "yes")
      (print "no")))  ; Has else — no violation, so suppress is stale
