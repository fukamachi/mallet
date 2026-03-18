;; Fixture: disable/enable region suppression
;; Expects 2 violations: before-disable and after-enable.
;; The form during-disable is inside the disabled region and should be suppressed.

(defpackage #:test-comment-disable-enable
  (:use #:cl))
(in-package #:test-comment-disable-enable)

;; This violation is NOT disabled yet — should be flagged
(defun before-disable (x)
  (if x
      (print "yes")))

; mallet:disable :missing-else
;; This violation is inside the disabled region — should be suppressed
(defun during-disable (x)
  (if x
      (print "also yes")))

; mallet:enable :missing-else
;; This violation is after re-enable — should be flagged again
(defun after-enable (x)
  (if x
      (print "still yes")))
