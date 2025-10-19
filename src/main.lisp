(defpackage #:mallet
  (:use #:cl
        #:mallet/violation
        #:mallet/parser
        #:mallet/rules)
  (:local-nicknames
   (#:a #:alexandria)
   (#:engine #:mallet/engine)
   (#:config #:mallet/config)
   (#:formatter #:mallet/formatter)
   (#:fixer #:mallet/fixer)
   (#:errors #:mallet/errors))
  (:export #:main
           #:lint-file
           #:lint-files
           ;; Debug mode
           #:*debug-mode*
           ;; Public API for library use
           #:make-rule
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
           ;; Re-exported from mallet/violation
           #:violation
           #:violation-rule
           #:violation-file
           #:violation-line
           #:violation-column
           #:violation-severity
           #:violation-message
           #:violation-fix
           ;; Re-exported from mallet/parser
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
(in-package #:mallet)

;;; Special variables

(defvar *debug-mode* nil
  "When T, enable detailed diagnostic output.")

;;; CLI Implementation

(defun parse-args (args)
  "Parse command-line ARGS into options and files.
Returns (values format config-path preset debug fix-mode files).
Signals specific error conditions for invalid input."
  (let ((format :text)
        (config-path nil)
        (preset nil)
        (debug nil)
        (fix-mode nil)  ; nil, :fix, or :fix-dry-run
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
          ((or (string= arg "--all") (string= arg "-a"))
           (setf preset :all))
          ((string= arg "--debug")
           (setf debug t))
          ((string= arg "--fix")
           (setf fix-mode :fix))
          ((string= arg "--fix-dry-run")
           (setf fix-mode :fix-dry-run))
          ((string= arg "--help")
           (print-help)
           (uiop:quit 0))
          ((string= arg "--version")
           (format t "Mallet version 0.1.0~%")
           (uiop:quit 0))
          ((and (> (length arg) 0) (char= (char arg 0) #\-))
           (error 'errors:unknown-option :option arg))
          (t
           (push arg files)))))
    (values format config-path preset debug fix-mode (nreverse files))))

(defun print-help ()
  "Print CLI usage information."
  (format t "~
Mallet - A sensible Common Lisp linter that catches mistakes, not style

Usage: mallet [options] <file>...

Options:
  --format <format>   Output format (text or json, default: text)
  --config <path>     Path to config file (default: auto-discover .mallet.lisp)
  --preset <name>     Use built-in preset (default or all)
  --all, -a           Alias for --preset all
  --fix               Auto-fix violations and write files
  --fix-dry-run       Show what would be fixed without writing files
  --debug             Enable debug mode with detailed diagnostics
  --help              Show this help message
  --version           Show version information

Presets:
  default             Only universally-accepted rules (quiet, recommended)
  all                 All rules enabled (useful for exploration)

Examples:
  mallet src
  mallet src/main.lisp
  mallet -a src/*.lisp
  mallet --format json src/*.lisp
  mallet --config .mallet.lisp src/
  mallet --fix src/                  # Auto-fix violations
  mallet --fix-dry-run src/          # Preview fixes without changing files
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

(defun main ()
  "Main entry point for the Mallet CLI.
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

          (multiple-value-bind (format config-path preset debug fix-mode file-args)
              (parse-args args)

            ;; Enable debug mode if requested
            (setf *debug-mode* debug)

            ;; Validate we have files to lint
            (when (null file-args)
              (print-help)
              (uiop:quit 1))

            (let* ((files (expand-file-args file-args))
                   (config:*default-preset* (or preset :default))
                   (config (and config-path (config:load-config config-path)))
                   (severity-counts '())  ; Accumulated counts as plist
                   (has-errors nil)
                   (has-violations nil)
                   (first-file-with-violations t)  ; For JSON comma handling
                   (all-violations '()))  ; Collect all violations for fix mode

              ;; For JSON, print opening bracket
              (when (eq format :json)
                (formatter:format-json-start))

              ;; Process files one at a time
              (dolist (file files)
                (multiple-value-bind (violations ignored-p)
                    (engine:lint-file file :config config)
                  (unless ignored-p
                    (cond
                      ;; Fix mode: collect violations, apply fixes later
                      (fix-mode
                       (setf all-violations (nconc all-violations violations)))

                      ;; Normal mode: format and output immediately
                      (t
                       (ecase format
                         (:text
                          ;; Output violations and accumulate counts
                          (let ((file-counts (formatter:format-text-file file violations)))
                            ;; Merge counts into accumulated counts
                            (loop for (severity count) on file-counts by #'cddr
                                  do (setf (getf severity-counts severity 0)
                                           (+ (getf severity-counts severity 0) count)))))
                         (:json
                          ;; Output JSON for this file
                          (when (formatter:format-json-file file violations
                                                            first-file-with-violations)
                            (setf first-file-with-violations nil))))))

                    ;; Track violations for exit code
                    (when violations
                      (setf has-violations t)
                      (when (some (lambda (v)
                                    (eq (violation-severity v) :error))
                                  violations)
                        (setf has-errors t))))))

              ;; Apply fixes if in fix mode
              (when fix-mode
                (multiple-value-bind (fixed-count fixed-violations unfixed-violations)
                    (fixer:apply-fixes all-violations :dry-run (eq fix-mode :fix-dry-run))

                  ;; Output results (only text format supported for --fix for now)
                  (when (eq format :text)
                    ;; Group violations by file for output
                    (let ((by-file (make-hash-table :test 'equal)))
                      (dolist (v (append fixed-violations unfixed-violations))
                        (push v (gethash (violation-file v) by-file)))

                      ;; Output each file's violations
                      (maphash (lambda (file file-violations)
                                 (formatter:format-text-file
                                  file
                                  (nreverse file-violations)
                                  :fixed-violations fixed-violations))
                               by-file))

                    ;; Print fix summary
                    (cond
                      ((zerop fixed-count)
                       (format t "~%No auto-fixable violations found.~%"))
                      ((eq fix-mode :fix-dry-run)
                       (format t "~%Would fix ~D violation~:P (dry run - no files changed).~%"
                               fixed-count))
                      (t
                       (format t "~%Fixed ~D violation~:P.~%" fixed-count)))

                    (when (< 0 (length unfixed-violations))
                      (format t "~D violation~:P cannot be auto-fixed.~%"
                              (length unfixed-violations))))

                  ;; Update exit code tracking based on unfixed violations
                  (setf has-violations (< 0 (length unfixed-violations)))
                  (setf has-errors
                        (some (lambda (v) (eq (violation-severity v) :error))
                              unfixed-violations))))

              ;; Print summary/closing (only for normal mode)
              (unless fix-mode
                (ecase format
                  (:text
                   ;; Print summary with accumulated counts
                   (formatter:format-text-summary severity-counts))
                  (:json
                   ;; Print closing bracket
                   (formatter:format-json-end))))

              ;; Exit with appropriate status
              ;; Exit 2: ERROR severity (objectively wrong)
              ;; Exit 1: Any other violations (WARNING/CONVENTION/FORMAT/INFO)
              ;; Exit 0: No violations
              ;; Teams control strictness via config (enable/disable rules)
              (cond
                (has-errors (uiop:quit 2))
                (has-violations (uiop:quit 1))
                (t (uiop:quit 0))))))
      ;; Catch explicit exit to ensure we don't suppress intentional quits
      (#+sbcl sb-sys:interactive-interrupt
       #-sbcl error ()
       (format *error-output* "~&Interrupted.~%")
       (uiop:quit 130)))))
