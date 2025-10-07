(defpackage #:malvolio/parser/reader
  (:use #:cl)
  (:local-nicknames
   (#:parser #:malvolio/parser))
  (:export #:parse-forms))
(in-package #:malvolio/parser/reader)

(defun parse-forms (text file)
  "Parse forms from TEXT in FILE using SBCL reader with source tracking.
Returns a list of FORM objects."
  (declare (ignore text file))
  ;; Stub implementation
  '())
