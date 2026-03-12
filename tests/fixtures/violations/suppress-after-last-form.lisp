;; Fixture: stale suppress comment that has no following form to apply to.
;; A mallet:suppress directive after the last top-level form should be
;; reported as a stale suppression.

(defpackage #:test-suppress-after-last-form
  (:use #:cl))
(in-package #:test-suppress-after-last-form)

(defun example (x) x)

; mallet:suppress :needless-let*
