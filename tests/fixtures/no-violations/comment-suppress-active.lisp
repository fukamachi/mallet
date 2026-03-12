;; Fixture: active suppress comment (matches a real violation — no output expected)
;; The suppress comment before the defun suppresses the needless-let* violation.
;; With :needless-let* rule enabled, this file should produce zero violations.

(defpackage #:test-comment-suppress-active
  (:use #:cl))
(in-package #:test-comment-suppress-active)

; mallet:suppress :needless-let*
(defun suppressed-function (x)
  (let* ((a x))
    a))
