(defpackage #:mallet/violation
  (:use #:cl)
  (:export #:violation
           #:violation-rule
           #:violation-file
           #:violation-line
           #:violation-column
           #:violation-severity
           #:violation-message
           #:violation-fix))
(in-package #:mallet/violation)

(defclass violation ()
  ((rule
    :initarg :rule
    :reader violation-rule
    :type keyword
    :documentation "Rule that was violated (keyword)")
   (file
    :initarg :file
    :reader violation-file
    :type pathname
    :documentation "File containing violation")
   (line
    :initarg :line
    :reader violation-line
    :type (integer 1)
    :documentation "Line number where violation occurs")
   (column
    :initarg :column
    :reader violation-column
    :type (integer 0)
    :documentation "Column number (0-based)")
   (severity
    :initarg :severity
    :reader violation-severity
    :type (member :error :warning :convention :format :info)
    :documentation "Severity level of the violation")
   (message
    :initarg :message
    :reader violation-message
    :type string
    :documentation "Human-readable description of the violation")
   (fix
    :initarg :fix
    :initform nil
    :reader violation-fix
    :type (or null string)
    :documentation "Suggested fix (if auto-fixable)"))
  (:documentation
   "Represents a linting violation found in source code."))
