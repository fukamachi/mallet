(defpackage #:malvolio-test-formatting
  (:use #:cl))
(in-package #:malvolio-test-formatting)

;; This file has various text-level formatting violations

;; Trailing whitespace on this line
(defun foo ()
  (+ 1 2))

;; Tab character on next line
	(defun bar ()
  (+ 3 4))

;; Too many consecutive blank lines follow




(defun baz ()
  (+ 5 6))

;; File ends without newline - this is the last line