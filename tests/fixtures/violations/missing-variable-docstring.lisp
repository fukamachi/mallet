;; Test fixtures for missing-variable-docstring rule

;; Bad: defvar with init value but no docstring
(defvar *counter* 0)

;; Good: defvar with docstring
(defvar *offset* 10 "Starting offset value.")

;; Good: defvar without init value (not checkable)
(defvar *state*)

;; Bad: defparameter without docstring
(defparameter *max-retries* 5)

;; Good: defparameter with docstring
(defparameter *timeout* 30 "Request timeout in seconds.")

;; Bad: defvar with nil value but no docstring
(defvar *cache* nil)
