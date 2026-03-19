;;; Clean examples for missing-variable-docstring rule

;; defvar with init value and docstring
(defvar *offset* 10 "Starting offset value.")

;; defvar without init value (not checkable)
(defvar *state*)

;; defparameter with docstring
(defparameter *timeout* 30 "Request timeout in seconds.")
