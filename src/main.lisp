(defpackage #:malvolio
  (:use #:cl
        #:malvolio/violation
        #:malvolio/parser
        #:malvolio/rules)
  (:local-nicknames
   (#:a #:alexandria)
   (#:engine #:malvolio/engine)
   (#:config #:malvolio/config)
   (#:formatter #:malvolio/formatter))
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

;;; Special variables

(defvar *debug-mode* nil
  "When T, enable detailed diagnostic output.")

;;; CLI Implementation

(defun parse-args (args)
  "Parse command-line ARGS into options and files.
Returns (values format config-path debug files)."
  (let ((format :text)
        (config-path nil)
        (debug nil)
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
          ((string= arg "--config")
           (let ((path (pop args)))
             (unless path
               (error "Missing value for --config"))
             (setf config-path path)))
          ((string= arg "--debug")
           (setf debug t))
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
    (values format config-path debug (nreverse files))))

(defun print-help ()
  "Print CLI usage information."
  (format t "~
Malvolio - A relentless guardian of code integrity for Common Lisp

Usage: malvolio [options] <file>...

Options:
  --format <format>   Output format (text or json, default: text)
  --config <path>     Path to config file (default: auto-discover .malvolio.lisp)
  --debug             Enable debug mode with detailed diagnostics
  --help              Show this help message
  --version           Show version information

Examples:
  malvolio src/main.lisp
  malvolio --format json src/*.lisp
  malvolio --config .malvolio.lisp src/
  malvolio --debug src/
"))

(defun expand-file-args (file-args)
  "Expand FILE-ARGS into a list of Lisp file pathnames.
Handles wildcards and directories, excluding common non-source directories."
  (let ((files '())
        (excluded-dirs '(".qlot" ".git" ".svn" ".hg" "node_modules" "_build")))
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
             (error "File not found: ~A" arg))))))
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

(defun main (&optional (args (uiop:command-line-arguments)))
  "Main entry point for the Malvolio CLI.
Lints files specified in ARGS and exits with appropriate status code."
  (handler-bind ((error (lambda (e)
                          (format *error-output* "Fatal error: ~A~%" e)
                          (uiop:print-condition-backtrace e)
                          (uiop:quit 3))))
    (multiple-value-bind (format config-path debug file-args)
        (parse-args args)

      ;; Enable debug mode if requested
      (setf *debug-mode* debug)

      ;; Validate we have files to lint
      (when (null file-args)
        (format *error-output* "Error: No files specified~%~%")
        (print-help)
        (uiop:quit 3))

      ;; Load or discover config
      (let* ((cfg (cond
                    ;; Explicit config path provided
                    (config-path
                     (config:load-config config-path))
                    ;; Auto-discover from current directory
                    (t
                     (let ((found-config (config:find-config-file (uiop:getcwd))))
                       (if found-config
                           (config:load-config found-config)
                           ;; No config found, use recommended defaults
                           (config:get-built-in-config :recommended))))))
             ;; Expand file arguments
             (files (expand-file-args file-args))
             ;; Create registry from config
             (registry (engine:make-registry-from-config cfg))
             ;; Lint files
             (results (engine:lint-files files :registry registry :config cfg)))

        ;; Format output
        (ecase format
          (:text (formatter:format-text results))
          (:json (formatter:format-json results)))

        ;; Exit with appropriate status
        ;; Exit 2: ERROR severity (objectively wrong)
        ;; Exit 1: WARNING severity (likely bugs)
        ;; Exit 0: CONVENTION/FORMAT/INFO (style/preferences) or no violations
        (cond
          ((has-errors-p results) (uiop:quit 2))
          ((has-warnings-p results) (uiop:quit 1))
          (t (uiop:quit 0)))))))
