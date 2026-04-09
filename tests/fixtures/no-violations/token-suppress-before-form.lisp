;; Fixture: ; mallet:suppress BEFORE the form (standalone line) suppresses token-level violations
;; within the next form. Semantics should match form-level rules.

(defpackage #:test-token-suppress-before-form
  (:use #:cl))
(in-package #:test-token-suppress-before-form)

; mallet:suppress double-colon-access
(defun test-fn (x)
  (let ((a some-pkg::*first-internal*)
        (b some-pkg::*second-internal*))
    (list a b)))
