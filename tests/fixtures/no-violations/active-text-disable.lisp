(defpackage #:test-active-text-disable
  (:use #:cl))
(in-package #:test-active-text-disable)

;; A :disable region for a text-level rule (:line-length) that actually suppresses
;; a violation. The disable directive must be registered AND marked used, so no
;; stale-suppression violation is emitted.

; mallet:disable :line-length
;; This line is inside the disabled region and deliberately exceeds the 100-character line-length limit by being quite long indeed.
; mallet:enable :line-length
