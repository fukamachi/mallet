;; Test fixture for inline comment suppression integration

(defpackage #:test-comment-suppress
  (:use #:cl))
(in-package #:test-comment-suppress)

;; This function's if-without-else is suppressed by inline comment
; mallet:suppress :missing-else
(defun suppressed-foo (x)
  (if x
      (print "yes")))   ; No else — but suppressed

;; This function's if-without-else is NOT suppressed — should be flagged
(defun unsuppressed-bar (x)
  (if x
      (print "also yes")))  ; No else — should be flagged

;; needless-let* suppression test
; mallet:suppress :needless-let*
(defun suppressed-let* (x)
  (let* ((a x))
    a))

;; needless-let* without suppression — should be flagged
(defun unsuppressed-let* (x)
  (let* ((a x))
    a))
