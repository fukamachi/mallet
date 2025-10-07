(defpackage #:token-rules-example
  (:use #:cl))
(in-package #:token-rules-example)

;;; This file tests token-level rules (comment-level)

;; Bad: top-level comment should have 3 or 4 semicolons
(defun example1 ()
  ; Bad: inside function should have 2 semicolons
  (let ((x 1))
    (+ x 2)))

;;;; Good: file-level comment
;;; Good: section-level comment

(defun example2 ()
  ;; Good: line comment inside function
  (let ((y 2)) ; Good: inline comment
    (* y 3)))

; Bad: top-level should be 3 or 4 semicolons
(defvar *global* 42)
