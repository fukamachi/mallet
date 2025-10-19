(defpackage #:mallet/violation
  (:use #:cl)
  (:export #:violation
           #:violation-rule
           #:violation-file
           #:violation-line
           #:violation-column
           #:violation-severity
           #:violation-message
           #:violation-fix
           #:make-violation-fix
           #:violation-fix-type
           #:violation-fix-line-number
           #:violation-fix-replacement-content
           #:violation-fix-start-line
           #:violation-fix-end-line
           #:violation-fix-appended-content
           #:violation-fix-original-form
           #:violation-fix-replacement-form
           #:violation-fix-form-end-line
           #:violation-fix-form-end-column))
(in-package #:mallet/violation)

;;; Fix metadata structure

(defstruct (violation-fix (:copier nil) (:predicate nil))
  "Metadata for auto-fixing a violation.

Fix types:
  :replace-line - Replace a single line with new content
    Required: line-number, replacement-content
  :append-to-file - Append content to end of file
    Required: appended-content
  :delete-lines - Delete a range of lines
    Required: start-line, end-line
  :replace-form - Replace a form expression with new expression
    Required: original-form, replacement-form"
  (type
   nil
   :type (member :replace-line :append-to-file :delete-lines :replace-form nil)
   :read-only t)
  ;; For :replace-line
  (line-number
   nil
   :type (or null (integer 1))
   :read-only t)
  (replacement-content
   nil
   :type (or null string)
   :read-only t)
  ;; For :delete-lines
  (start-line
   nil
   :type (or null (integer 1))
   :read-only t)
  (end-line
   nil
   :type (or null (integer 1))
   :read-only t)
  ;; For :append-to-file
  (appended-content
   nil
   :type (or null string)
   :read-only t)
  ;; For :replace-form
  (original-form
   nil
   :read-only t)
  (replacement-form
   nil
   :read-only t)
  (form-end-line
   nil
   :type (or null (integer 1))
   :read-only t)
  (form-end-column
   nil
   :type (or null (integer 0))
   :read-only t))

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
    :type (or null violation-fix)
    :documentation "Fix metadata (if auto-fixable)"))
  (:documentation
   "Represents a linting violation found in source code."))
