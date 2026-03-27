(defpackage #:test-disable-all-form-level
  (:use #:cl))
(in-package #:test-disable-all-form-level)

;; :ALL is a special keyword that disables all rules. It is not a registered rule
;; object, so filter-text-token-violations must not attempt to look it up and register
;; it — doing so would find no rule-obj and silently skip it anyway, but this test
;; verifies there is no stale-suppression false positive when :ALL wraps a form-level
;; violation (here: a missing-else form).

; mallet:disable :ALL
(defun foo (x)
  (if x
      (print "yes")))
; mallet:enable :ALL
