;; Clean fixture: defun with a docstring that contains directive-like text.
;; The text on the continuation line of the docstring looks like a real directive
;; (no opening quote on that line), but is inside the string.
;; This must produce ZERO stale-suppression violations.

(defpackage #:test-docstring-with-directive-text
  (:use #:cl))
(in-package #:test-docstring-with-directive-text)

(defun example-function (x)
  "This is a multi-line docstring.
; mallet:suppress needless-let*
  The line above is inside the string and must not be treated as a directive."
  x)

(defun another-function (y)
  "Another docstring with directive-like text on a new line.
; mallet:disable line-length
  Still inside the string here."
  (+ y 1))
