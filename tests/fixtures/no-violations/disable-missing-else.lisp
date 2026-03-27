(defpackage #:test-disable-missing-else
  (:use #:cl))
(in-package #:test-disable-missing-else)

;; :missing-else is a form-level rule. A :disable directive for it must not be
;; registered in the text-token-suppression-state, so no stale warning fires.

; mallet:disable :missing-else
(defun foo (x)
  (if x
      (print "yes")))
; mallet:enable :missing-else
