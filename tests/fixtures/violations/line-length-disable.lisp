(defpackage #:line-length-disable-example
  (:use #:cl))
(in-package #:line-length-disable-example)

;;; Tests that mallet:disable/enable suppresses line-length violations in region

;; This line is outside the disabled region and exceeds the 100-char limit — it SHOULD trigger a violation!

; mallet:disable line-length
;; This extremely long line is inside the disabled region and should NOT trigger a line-length violation at all
;; Another very long line inside the disabled region that should also NOT produce any kind of line-length violation
; mallet:enable line-length

;; This line is after the disabled region and also exceeds the 100-char limit — it SHOULD trigger a violation!
