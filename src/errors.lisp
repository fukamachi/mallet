(defpackage #:malo/errors
  (:use #:cl)
  (:export #:malo-error
           #:malo-simple-error
           #:cli-error
           #:unknown-option
           #:unknown-option-option
           #:missing-option-value
           #:missing-option-value-option
           #:invalid-option-value
           #:invalid-option-value-option
           #:invalid-option-value-value
           #:invalid-option-value-expected
           #:file-not-found
           #:file-not-found-path
           #:no-files-specified
           #:invalid-format
           #:invalid-preset
           #:config-not-found
           #:config-not-found-path
           #:config-parse-failed
           #:config-parse-failed-path
           #:config-parse-failed-cause))
(in-package #:malo/errors)

;;; Base error conditions

(define-condition malo-error (error)
  ()
  (:documentation "Base error condition for all Malo errors."))

(define-condition malo-simple-error (malo-error simple-error)
  ()
  (:documentation "Simple error with format control and args."))

;;; CLI-specific error conditions

(define-condition cli-error (malo-error)
  ()
  (:documentation "Base condition for CLI-related errors."))

(define-condition unknown-option (cli-error)
  ((option :initarg :option
           :reader unknown-option-option))
  (:report (lambda (condition stream)
             (format stream "Unknown option: ~A~%~
                            Run 'malo --help' to see available options."
                     (unknown-option-option condition))))
  (:documentation "Signaled when an unknown command-line option is encountered."))

(define-condition missing-option-value (cli-error)
  ((option :initarg :option
           :reader missing-option-value-option))
  (:report (lambda (condition stream)
             (format stream "Missing value for option: ~A"
                     (missing-option-value-option condition))))
  (:documentation "Signaled when a command-line option requires a value but none is provided."))

(define-condition invalid-option-value (cli-error)
  ((option :initarg :option
           :reader invalid-option-value-option)
   (value :initarg :value
          :reader invalid-option-value-value)
   (expected :initarg :expected
             :reader invalid-option-value-expected))
  (:report (lambda (condition stream)
             (format stream "Invalid value '~A' for option ~A~%~
                            Expected: ~A"
                     (invalid-option-value-value condition)
                     (invalid-option-value-option condition)
                     (invalid-option-value-expected condition))))
  (:documentation "Signaled when a command-line option has an invalid value."))

(define-condition file-not-found (cli-error)
  ((path :initarg :path
         :reader file-not-found-path))
  (:report (lambda (condition stream)
             (format stream "File not found: ~A"
                     (file-not-found-path condition))))
  (:documentation "Signaled when a specified file does not exist."))

(define-condition no-files-specified (cli-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "No files specified~%~
                            Run 'malo --help' for usage information.")))
  (:documentation "Signaled when no files are specified for linting."))

(define-condition invalid-format (invalid-option-value)
  ()
  (:report (lambda (condition stream)
             (format stream "Invalid format: ~A~%~
                            Expected: text or json"
                     (invalid-option-value-value condition))))
  (:documentation "Signaled when an invalid output format is specified."))

(define-condition invalid-preset (invalid-option-value)
  ()
  (:report (lambda (condition stream)
             (format stream "Invalid preset: ~A~%~
                            Expected: default or all"
                     (invalid-option-value-value condition))))
  (:documentation "Signaled when an invalid preset is specified."))

(define-condition config-not-found (cli-error)
  ((path :initarg :path
         :reader config-not-found-path))
  (:report (lambda (condition stream)
             (format stream "Config file not found: ~A"
                     (config-not-found-path condition))))
  (:documentation "Signaled when a specified config file does not exist."))

(define-condition config-parse-failed (cli-error)
  ((path :initarg :path
         :reader config-parse-failed-path)
   (cause :initarg :cause
          :initform nil
          :reader config-parse-failed-cause))
  (:report (lambda (condition stream)
             (format stream "Failed to parse config file: ~A"
                     (config-parse-failed-path condition))
             (when (config-parse-failed-cause condition)
               (format stream "~%Cause: ~A"
                       (config-parse-failed-cause condition)))))
  (:documentation "Signaled when a config file cannot be parsed."))
