;; Fixture: stale :disable region for text/token rule (line-length)
;; The :disable/:enable region below contains no long lines,
;; so the disable directive is stale — should trigger :stale-suppression.

(defpackage #:test-stale-text-disable
  (:use #:cl))
(in-package #:test-stale-text-disable)

; mallet:disable :line-length
;; Short line inside disabled region — no line-length violation here
(defun example (x) x)
; mallet:enable :line-length
