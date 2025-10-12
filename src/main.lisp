(defpackage #:malo
  (:use #:cl
        #:malo/violation
        #:malo/parser
        #:malo/rules)
  (:local-nicknames
   (#:a #:alexandria)
   (#:engine #:malo/engine)
   (#:config #:malo/config)
   (#:formatter #:malo/formatter)
   (#:errors #:malo/errors))
  (:export #:main
           #:lint-file
           #:lint-files
           ;; Debug mode
           #:*debug-mode*
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
           ;; Re-exported from malo/violation
           #:violation
           #:violation-rule
           #:violation-file
           #:violation-line
           #:violation-column
           #:violation-severity
           #:violation-message
           #:violation-fix
           ;; Re-exported from malo/parser
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
(in-package #:malo)

;;; Special variables

(defvar *debug-mode* nil
  "When T, enable detailed diagnostic output.")

;;; CLI Implementation

(defun parse-args (args)
  "Parse command-line ARGS into options and files.
Returns (values format config-path preset debug files).
Signals specific error conditions for invalid input."
  (let ((format :text)
        (config-path nil)
        (preset nil)
        (debug nil)
        (files '()))
    (loop while args do
      (let ((arg (pop args)))
        (cond
          ((string= arg "--format")
           (let ((fmt (pop args)))
             (unless fmt
               (error 'errors:missing-option-value :option "--format"))
             (setf format (cond
                            ((string= fmt "text") :text)
                            ((string= fmt "json") :json)
                            (t (error 'errors:invalid-format
                                      :option "--format"
                                      :value fmt
                                      :expected "text or json"))))))
          ((string= arg "--config")
           (let ((path (pop args)))
             (unless path
               (error 'errors:missing-option-value :option "--config"))
             (setf config-path path)))
          ((string= arg "--preset")
           (let ((preset-name (pop args)))
             (unless preset-name
               (error 'errors:missing-option-value :option "--preset"))
             (setf preset (cond
                            ((string= preset-name "default") :default)
                            ((string= preset-name "all") :all)
                            (t (error 'errors:invalid-preset
                                      :option "--preset"
                                      :value preset-name
                                      :expected "default or all"))))))
          ((string= arg "--all")
           (setf preset :all))
          ((string= arg "--debug")
           (setf debug t))
          ((string= arg "--help")
           (print-help)
           (uiop:quit 0))
          ((string= arg "--version")
           (format t "Malo version 0.1.0~%")
           (uiop:quit 0))
          ((and (> (length arg) 0) (char= (char arg 0) #\-))
           (error 'errors:unknown-option :option arg))
          (t
           (push arg files)))))
    (values format config-path preset debug (nreverse files))))

(defun print-help ()
  "Print CLI usage information."
  (format t "~
Malo - A relentless guardian of code integrity for Common Lisp

Usage: malo [options] <file>...

Options:
  --format <format>   Output format (text or json, default: text)
  --config <path>     Path to config file (default: auto-discover .malo.lisp)
  --preset <name>     Use built-in preset (default or all)
  --all               Alias for --preset all
  --debug             Enable debug mode with detailed diagnostics
  --help              Show this help message
  --version           Show version information

Presets:
  default             Only universally-accepted rules (quiet, recommended)
  all                 All rules enabled (useful for exploration)

Examples:
  malo src/main.lisp
  malo --all src/*.lisp
  malo --format json src/*.lisp
  malo --config .malo.lisp src/
  malo --debug src/
"))

(defun expand-file-args (file-args)
  "Expand FILE-ARGS into a list of Lisp file pathnames.
Handles wildcards and directories, excluding common non-source directories."
  (let ((files '())
        (excluded-dirs '(".qlot" ".bundle-libs" ".git" ".svn" ".hg" "node_modules" "_build")))
    (labels ((should-exclude-p (path)
               "Check if PATH is in an excluded directory."
               (let ((path-string (namestring path)))
                 (some (lambda (excluded)
                         (search (concatenate 'string "/" excluded "/") path-string))
                       excluded-dirs))))
      (dolist (arg file-args)
        (let ((path (uiop:parse-native-namestring arg)))
          (cond
            ;; Directory - recursively find .lisp files, excluding common directories
            ((uiop:directory-exists-p path)
             (let* ((dir-path (uiop:ensure-directory-pathname path))
                    (all-files (uiop:directory-files dir-path "**/*.lisp"))
                    (filtered-files (remove-if #'should-exclude-p all-files)))
               (setf files (nconc files filtered-files))))
            ;; Wildcard pattern - expand using directory
            ((or (find #\* arg) (find #\? arg))
             (setf files (nconc files (directory path))))
            ;; Regular file
            ((probe-file path)
             (push path files))
            (t
             (error 'errors:file-not-found :path arg))))))
    (nreverse files)))

(defun has-errors-p (results)
  "Check if RESULTS contain any :error severity violations."
  (loop for (file . violations) in results
        thereis (some (lambda (v)
                        (eq (violation-severity v) :error))
                      violations)))

(defun has-warnings-p (results)
  "Check if RESULTS contain any :warning severity violations."
  (loop for (file . violations) in results
        thereis (some (lambda (v)
                        (eq (violation-severity v) :warning))
                      violations)))

(defun has-violations-p (results)
  "Check if RESULTS contain any violations at all."
  (loop for (file . violations) in results
        thereis (not (null violations))))

(defun main ()
  "Main entry point for the Malo CLI.
Lints files specified in ARGS and exits with appropriate status code."
  (let* ((args (uiop:command-line-arguments))
         (args (if (equal (first args) "--")
                   (rest args)
                   args)))
    (handler-case
        (handler-bind
            ;; Handle CLI errors with nice messages, no stacktrace
            ((errors:cli-error
              (lambda (e)
                (format *error-output* "Error: ~A~%" e)
                (uiop:quit 3)))
             ;; Handle unexpected errors with stacktrace
             (error
              (lambda (e)
                (format *error-output* "Fatal error: ~A~%" e)
                (when *debug-mode*
                  (uiop:print-condition-backtrace e))
                (uiop:quit 3))))

          (multiple-value-bind (format config-path preset debug file-args)
              (parse-args args)

            ;; Enable debug mode if requested
            (setf *debug-mode* debug)

            ;; Validate we have files to lint
            (when (null file-args)
              (print-help)
              (uiop:quit 1))

            (let* ((files (expand-file-args file-args))
                   (config:*default-preset* (or preset :default))
                   (results (engine:lint-files files
                                               :config (and config-path
                                                            (config:load-config config-path)))))

              ;; Format output
              (ecase format
                (:text (formatter:format-text results))
                (:json (formatter:format-json results)))

              ;; Exit with appropriate status
              ;; Exit 2: ERROR severity (objectively wrong)
              ;; Exit 1: Any other violations (WARNING/CONVENTION/FORMAT/INFO)
              ;; Exit 0: No violations
              ;; Teams control strictness via config (enable/disable rules)
              (cond
                ((has-errors-p results) (uiop:quit 2))
                ((has-violations-p results) (uiop:quit 1))
                (t (uiop:quit 0))))))
      ;; Catch explicit exit to ensure we don't suppress intentional quits
      (#+sbcl sb-sys:interactive-interrupt
       #-sbcl error ()
       (format *error-output* "~&Interrupted.~%")
       (uiop:quit 130)))))
