;; Fixture: active suppress comment for a token-level rule.
;; The suppress comment on the same line as the :: access should suppress the violation.
;; With :double-colon-access + :stale-suppression enabled, this file should produce
;; zero violations.

(defpackage #:test-token-suppress-active
  (:use #:cl))
(in-package #:test-token-suppress-active)

(defun test-fn (x)
  (let ((y some-pkg::*internal-var*)) ; mallet:suppress double-colon-access
    y))
