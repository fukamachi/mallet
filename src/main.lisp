(defpackage #:malvolio
  (:use #:cl
        #:malvolio/violation
        #:malvolio/parser
        #:malvolio/rules)
  (:local-nicknames
   (#:a #:alexandria)
   (#:engine #:malvolio/engine)
   (#:formatter #:malvolio/formatter))
  (:export #:main
           #:lint-file
           #:lint-files
           ;; Public API for library use
           #:*rule-registry*
           #:make-registry
           #:register-rule
           #:find-rule
           #:list-rules
           #:enable-rule
           #:disable-rule
           ;; Configuration
           #:load-config
           #:make-config
           ;; Core classes (for extensibility)
           #:rule
           #:rule-name
           #:rule-description
           #:rule-severity
           #:rule-type
           #:rule-enabled-p
           #:config
           ;; Re-exported from malvolio/violation
           #:violation
           #:violation-rule
           #:violation-file
           #:violation-line
           #:violation-column
           #:violation-severity
           #:violation-message
           #:violation-fix
           ;; Re-exported from malvolio/parser
           #:token
           #:token-type
           #:token-value
           #:token-line
           #:token-column
           #:token-file
           #:token-raw
           #:tokenize
           #:form
           #:form-expr
           #:form-line
           #:form-column
           #:form-end-line
           #:form-end-column
           #:form-file
           #:form-source
           #:parse-forms
           #:analyze-text))
(in-package #:malvolio)

;;; CLI Implementation

(defun parse-args (args)
  "Parse command-line ARGS into options and files.
Returns (values format files)."
  (let ((format :text)
        (files '()))
    (loop while args do
      (let ((arg (pop args)))
        (cond
          ((string= arg "--format")
           (let ((fmt (pop args)))
             (unless fmt
               (error "Missing value for --format"))
             (setf format (cond
                            ((string= fmt "text") :text)
                            ((string= fmt "json") :json)
                            (t (error "Unknown format: ~A (must be 'text' or 'json')" fmt))))))
          ((string= arg "--help")
           (print-help)
           (uiop:quit 0))
          ((string= arg "--version")
           (format t "Malvolio version 0.1.0~%")
           (uiop:quit 0))
          ((and (> (length arg) 0) (char= (char arg 0) #\-))
           (error "Unknown option: ~A" arg))
          (t
           (push arg files)))))
    (values format (nreverse files))))

(defun print-help ()
  "Print CLI usage information."
  (format t "~
Malvolio - A relentless guardian of code integrity for Common Lisp

Usage: malvolio [options] <file>...

Options:
  --format <format>   Output format (text or json, default: text)
  --help              Show this help message
  --version           Show version information

Examples:
  malvolio src/main.lisp
  malvolio --format json src/*.lisp
  malvolio src/
"))

(defun expand-file-args (file-args)
  "Expand FILE-ARGS into a list of Lisp file pathnames.
Handles wildcards and directories."
  (let ((files '()))
    (dolist (arg file-args)
      (let ((path (uiop:parse-native-namestring arg)))
        (cond
          ;; Directory - recursively find .lisp files
          ((uiop:directory-exists-p path)
           (setf files (nconc files
                              (uiop:directory-files path "**/*.lisp"))))
          ;; Wildcard pattern - expand using directory
          ((or (find #\* arg) (find #\? arg))
           (setf files (nconc files (directory path))))
          ;; Regular file
          ((probe-file path)
           (push path files))
          (t
           (error "File not found: ~A" arg)))))
    (nreverse files)))

(defun has-errors-p (results)
  "Check if RESULTS contain any :error severity violations."
  (loop for (file . violations) in results
        thereis (some (lambda (v)
                        (eq (violation-severity v) :error))
                      violations)))

(defun has-violations-p (results)
  "Check if RESULTS contain any violations at all."
  (loop for (file . violations) in results
        thereis (not (null violations))))

(defun main (&optional (args (uiop:command-line-arguments)))
  "Main entry point for the Malvolio CLI.
Lints files specified in ARGS and exits with appropriate status code."
  (handler-case
      (multiple-value-bind (format file-args)
          (parse-args args)

        ;; Validate we have files to lint
        (when (null file-args)
          (format *error-output* "Error: No files specified~%~%")
          (print-help)
          (uiop:quit 3))

        ;; Expand file arguments
        (let* ((files (expand-file-args file-args))
               (registry (engine:make-default-registry))
               (results (engine:lint-files files :registry registry)))

          ;; Format output
          (ecase format
            (:text (formatter:format-text results))
            (:json (formatter:format-json results)))

          ;; Exit with appropriate status
          (cond
            ((has-errors-p results) (uiop:quit 2))
            ((has-violations-p results) (uiop:quit 1))
            (t (uiop:quit 0)))))

    (error (e)
      (format *error-output* "Fatal error: ~A~%" e)
      (uiop:quit 3))))
