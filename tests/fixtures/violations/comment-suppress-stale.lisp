;; Fixture: stale suppress comment (no matching violation)
;; The suppress comment below targets :needless-let* but the form has no such violation.
;; This should trigger a :stale-suppression warning.

(defpackage #:test-comment-suppress-stale
  (:use #:cl))
(in-package #:test-comment-suppress-stale)

; mallet:suppress :needless-let*
(defun clean-function (x)
  ;; Uses let (not let*) — no needless-let* violation here
  (let ((a x))
    a))
