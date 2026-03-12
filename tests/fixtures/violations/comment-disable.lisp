;; Test fixture for inline comment disable/enable integration

(defpackage #:test-comment-disable
  (:use #:cl))
(in-package #:test-comment-disable)

;; This violation is NOT disabled yet — should be flagged
(defun before-disable (x)
  (if x
      (print "yes")))

; mallet:disable :if-without-else
;; This violation is inside the disabled region — should be suppressed
(defun during-disable (x)
  (if x
      (print "also yes")))

; mallet:enable :if-without-else
;; This violation is after re-enable — should be flagged again
(defun after-enable (x)
  (if x
      (print "still yes")))
