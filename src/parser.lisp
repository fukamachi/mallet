(defpackage #:mallet/parser
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:ppcre #:cl-ppcre)
   (#:utils #:mallet/utils))
  (:export #:analyze-text
           ;; Token
           #:token
           #:token-type
           #:token-value
           #:token-line
           #:token-column
           #:token-file
           #:token-raw
           ;; Tokenizer
           #:tokenize
           ;; Form
           #:form
           #:form-expr
           #:form-line
           #:form-column
           #:form-end-line
           #:form-end-column
           #:form-file
           #:form-source
           #:form-position-map
           ;; Reader
           #:parse-forms
           #:find-position
           ;; Parse errors
           #:parse-error-info
           #:parse-error-info-message
           #:parse-error-info-file
           #:parse-error-info-line
           #:parse-error-info-column
           ;; Form utilities
           #:walk-form
           #:if-form-p
           #:if-without-else-p
           #:case-form-p
           #:defpackage-form-p
           #:defun-form-p
           ;; Eclector utilities
           #:eclector-unquote-p
           #:extract-from-eclector-unquote))
(in-package #:mallet/parser)

;;; Token data structure

(defclass token ()
  ((type
    :initarg :type
    :reader token-type
    :type keyword
    :documentation "Token type (e.g., :symbol, :string, :comment-line)")
   (value
    :initarg :value
    :reader token-value
    :documentation "Token value (string or parsed)")
   (file
    :initarg :file
    :reader token-file
    :type pathname
    :documentation "Source file")
   (line
    :initarg :line
    :reader token-line
    :type (integer 1)
    :documentation "Line number where token appears")
   (column
    :initarg :column
    :reader token-column
    :type (integer 0)
    :documentation "Column number (0-based)")
   (raw
    :initarg :raw
    :reader token-raw
    :type string
    :documentation "Raw source text for this token"))
  (:documentation
   "Represents a token in the source code with position information."))

;;; Form data structure

(defclass form ()
  ((expr
    :initarg :expr
    :reader form-expr
    :documentation "The actual s-expression")
   (file
    :initarg :file
    :reader form-file
    :type pathname
    :documentation "Source file")
   (line
    :initarg :line
    :reader form-line
    :type (integer 1)
    :documentation "Starting line number")
   (column
    :initarg :column
    :reader form-column
    :type (integer 0)
    :documentation "Starting column number (0-based)")
   (end-line
    :initarg :end-line
    :reader form-end-line
    :type (integer 1)
    :documentation "Ending line number")
   (end-column
    :initarg :end-column
    :reader form-end-column
    :type (integer 0)
    :documentation "Ending column number (0-based)")
   (source
    :initarg :source
    :reader form-source
    :type string
    :documentation "Raw source text for this form")
   (position-map
    :initarg :position-map
    :initform nil
    :reader form-position-map
    :documentation "Hash table mapping expressions to (line . column) positions"))
  (:documentation
   "Represents a parsed form (s-expression) with source location information."))

;;; Parse error data structure

(defclass parse-error-info ()
  ((message
    :initarg :message
    :reader parse-error-info-message
    :type string
    :documentation "Error message")
   (file
    :initarg :file
    :reader parse-error-info-file
    :type pathname
    :documentation "Source file")
   (line
    :initarg :line
    :reader parse-error-info-line
    :type (integer 1)
    :documentation "Line number where error occurred")
   (column
    :initarg :column
    :reader parse-error-info-column
    :type (integer 0)
    :documentation "Column number where error occurred (0-based)"))
  (:documentation
   "Represents a parse error with position information."))

;;; Eclector Utilities

(defun eclector-unquote-p (obj)
  "Check if OBJ is an ECLECTOR.READER:UNQUOTE object.
Eclector creates these for nested backquote/unquote expressions.
Returns T if OBJ is a cons whose car is ECLECTOR.READER:UNQUOTE."
  (and (consp obj)
       (symbolp (first obj))
       (or (eq (first obj) 'eclector.reader:unquote)
           (and (eq (symbol-package (first obj))
                    (find-package "ECLECTOR.READER"))
                (string-equal (symbol-name (first obj)) "UNQUOTE")))))

(defun extract-from-eclector-unquote (unquote-obj)
  "Extract the value from an ECLECTOR.READER:UNQUOTE object.
Returns the unquoted value, or NIL if OBJ is not a valid UNQUOTE object.

Example:
  (ECLECTOR.READER:UNQUOTE \"foo\") → \"foo\"
  (ECLECTOR.READER:UNQUOTE (ECLECTOR.READER:UNQUOTE \"bar\")) → (ECLECTOR.READER:UNQUOTE \"bar\")"
  (when (and (eclector-unquote-p unquote-obj)
             (rest unquote-obj))
    (second unquote-obj)))
