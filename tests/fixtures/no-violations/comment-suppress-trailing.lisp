;; Fixture: trailing same-line suppress comment
;; The suppress comment appears on the same line as the form, after the closing paren.
;; With :needless-let* rule enabled, this file should produce zero violations.

(defpackage #:test-comment-suppress-trailing
  (:use #:cl))
(in-package #:test-comment-suppress-trailing)

(let* ((x (some-function))) ; mallet:suppress needless-let*
  x)
