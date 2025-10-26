(defpackage #:mallet
  (:use #:cl
        #:mallet/violation
        #:mallet/parser
        #:mallet/rules)
  (:local-nicknames
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

;;; CLI parsing helpers for rule options

(defun parse-option-value (val-str)
  "Parse option value: number, keyword, or string."
  (check-type val-str string)
  (cond
    ;; Try parsing as integer
    ((and (plusp (length val-str))
          (every #'digit-char-p val-str))
     (parse-integer val-str))
    ;; Try parsing as keyword (for variant=modified)
    ((and (plusp (length val-str))
          (alpha-char-p (char val-str 0)))
     (intern (string-upcase val-str) :keyword))
    ;; Otherwise keep as string
    (t val-str)))

(defun parse-rule-options (options-str)
  "Parse 'max=15,variant=modified' into plist (:max 15 :variant :modified)."
  (check-type options-str string)
  (let ((pairs (uiop:split-string options-str :separator ","))
        (result '()))
    (dolist (pair pairs)
      (let ((eq-pos (position #\= pair)))
        (unless eq-pos
          (error 'errors:invalid-rule-option :value pair))
        (let* ((key-str (subseq pair 0 eq-pos))
               (val-str (subseq pair (1+ eq-pos)))
               (key (intern (string-upcase key-str) :keyword))
               (val (parse-option-value val-str)))
          (setf result (list* val key result)))))
    (nreverse result)))

(defun parse-rule-name (name-str)
  "Parse rule name string to keyword. Validates it exists."
  (check-type name-str string)
  (let ((keyword (intern (string-upcase name-str) :keyword)))
    ;; Validate rule exists (check against make-rule)
    (handler-case
        (progn
          (make-rule keyword)
          keyword)
      (error ()
        (error 'errors:unknown-rule :value name-str)))))

(defun parse-rule-spec (spec)
  "Parse rule spec like 'cyclomatic-complexity' or 'cyclomatic-complexity:max=15,variant=modified'.
Returns (rule-name . options-plist)."
  (check-type spec string)
  (let ((colon-pos (position #\: spec)))
    (if colon-pos
        ;; Has options: split and parse
        (let* ((rule-name (parse-rule-name (subseq spec 0 colon-pos)))
               (options-str (subseq spec (1+ colon-pos)))
               (options (parse-rule-options options-str)))
          (cons rule-name options))
        ;; No options: just rule name
        (cons (parse-rule-name spec) '()))))

(defun parse-group-name (group-str)
  "Parse group name and validate it's a valid severity."
  (check-type group-str string)
  (let ((keyword (intern (string-upcase group-str) :keyword)))
    (unless (member keyword '(:error :warning :convention :format :info :metrics))
      (error 'errors:invalid-group :value group-str))
    keyword))

(defun handle-format-option (args)
  "Handle --format option. Returns (values format remaining-args)."
  (let ((fmt (pop args)))
    (unless fmt
      (error 'errors:missing-option-value :option "--format"))
    (values
     (cond
       ((string= fmt "text") :text)
       ((string= fmt "json") :json)
       ((string= fmt "line") :line)
       (t (error 'errors:invalid-format
                 :option "--format"
                 :value fmt
                 :expected "text, json, or line")))
     args)))

(defun handle-config-option (args)
  "Handle --config option. Returns (values config-path remaining-args)."
  (let ((path (pop args)))
    (unless path
      (error 'errors:missing-option-value :option "--config"))
    (values path args)))

(defun handle-preset-option (args)
  "Handle --preset option. Returns (values preset remaining-args)."
  (let ((preset-name (pop args)))
    (unless preset-name
      (error 'errors:missing-option-value :option "--preset"))
    (values
     (cond
       ((string= preset-name "default") :default)
       ((string= preset-name "all") :all)
       (t (error 'errors:invalid-preset
                 :option "--preset"
                 :value preset-name
                 :expected "default or all")))
     args)))

(defun handle-enable-option (args)
  "Handle --enable option. Returns (values rule-spec remaining-args)."
  (let ((spec (pop args)))
    (unless spec
      (error 'errors:missing-option-value :option "--enable"))
    (values (parse-rule-spec spec) args)))

(defun handle-disable-option (args)
  "Handle --disable option. Returns (values rule-name remaining-args)."
  (let ((rule-name-str (pop args)))
    (unless rule-name-str
      (error 'errors:missing-option-value :option "--disable"))
    (values (parse-rule-name rule-name-str) args)))

(defun handle-enable-group-option (args)
  "Handle --enable-group option. Returns (values group-name remaining-args)."
  (let ((group-str (pop args)))
    (unless group-str
      (error 'errors:missing-option-value :option "--enable-group"))
    (values (parse-group-name group-str) args)))

(defun handle-disable-group-option (args)
  "Handle --disable-group option. Returns (values group-name remaining-args)."
  (let ((group-str (pop args)))
    (unless group-str
      (error 'errors:missing-option-value :option "--disable-group"))
    (values (parse-group-name group-str) args)))


(defun parse-args (args)
  "Parse command-line ARGS into options and files.
Returns (values format config-path preset debug fix-mode cli-rules files).
Signals specific error conditions for invalid input."
  (let ((format :text)
        (config-path nil)
        (preset nil)
        (debug nil)
        (fix-mode nil)
        (enable-rules '())
        (disable-rules '())
        (enable-groups '())
        (disable-groups '())
        (files '()))
    (loop while args do
      (let ((arg (pop args)))
        (cond
          ((string= arg "--format")
           (multiple-value-setq (format args)
             (handle-format-option args)))
          ((string= arg "--config")
           (multiple-value-setq (config-path args)
             (handle-config-option args)))
          ((string= arg "--preset")
           (multiple-value-setq (preset args)
             (handle-preset-option args)))
          ((or (string= arg "--all") (string= arg "-a"))
           (setf preset :all))
          ((string= arg "--debug")
           (setf debug t))
          ((string= arg "--fix")
           (setf fix-mode :fix))
          ((string= arg "--fix-dry-run")
           (setf fix-mode :fix-dry-run))
          ((string= arg "--enable")
           (let (rule-spec)
             (multiple-value-setq (rule-spec args)
               (handle-enable-option args))
             (push rule-spec enable-rules)))
          ((string= arg "--disable")
           (let (rule-name)
             (multiple-value-setq (rule-name args)
               (handle-disable-option args))
             (push rule-name disable-rules)))
          ((string= arg "--enable-group")
           (let (group-name)
             (multiple-value-setq (group-name args)
               (handle-enable-group-option args))
             (push group-name enable-groups)))
          ((string= arg "--disable-group")
           (let (group-name)
             (multiple-value-setq (group-name args)
               (handle-disable-group-option args))
             (push group-name disable-groups)))
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
    (let ((cli-rules (list :enable-rules (nreverse enable-rules)
                           :disable-rules (nreverse disable-rules)
                           :enable-groups (nreverse enable-groups)
                           :disable-groups (nreverse disable-groups))))
      (values format config-path preset debug fix-mode cli-rules (nreverse files)))))


(defun print-help ()
  "Print CLI usage information."
  (format t "~
Mallet - A sensible Common Lisp linter that catches mistakes, not style

Usage: mallet [options] <file>...

Options:
  --format <format>   Output format (text, line, or json; default: text)
  --config <path>     Path to config file (default: auto-discover .mallet.lisp)
  --preset <name>     Use built-in preset (default or all)
  --all, -a           Alias for --preset all

  --enable <rule>     Enable specific rule (e.g., --enable cyclomatic-complexity)
  --enable <rule:opts> Enable rule with options (e.g., --enable line-length:max=120)
  --disable <rule>    Disable specific rule (e.g., --disable trailing-whitespace)
  --enable-group <group>  Enable all rules in severity group
  --disable-group <group> Disable all rules in severity group

  --fix               Auto-fix violations and write files
  --fix-dry-run       Show what would be fixed without writing files
  --debug             Enable debug mode with detailed diagnostics
  --help              Show this help message
  --version           Show version information

Output Formats:
  text                Human-readable grouped by file (default)
  line                One violation per line (file:line:col: severity: message)
  json                Machine-readable JSON format

Presets:
  default             Only universally-accepted rules (quiet, recommended)
  all                 All rules enabled (useful for exploration)

Rule Groups (by severity):
  error               Objectively wrong code (causes runtime errors)
  warning             Likely bugs or dangerous patterns
  convention          Idiom suggestions (not wrong, but not idiomatic)
  format              Consensus formatting (Emacs/SLIME standards)
  info                Subjective preferences (style choices)
  metrics             Code quality measurements (complexity, length)

Examples:
  mallet src
  mallet src/main.lisp
  mallet -a src/*.lisp
  mallet --format line src/*.lisp     # GCC/GNU format for editor integration
  mallet --format json src/*.lisp
  mallet --config .mallet.lisp src/
  mallet --fix src/                   # Auto-fix violations
  mallet --fix-dry-run src/           # Preview fixes without changing files

  # Enable metrics rules without config file
  mallet --enable-group metrics src/

  # Enable specific rule with custom options
  mallet --enable cyclomatic-complexity:max=15 src/

  # Mix preset with CLI overrides
  mallet --preset default --enable-group metrics src/

  # Override multiple rules
  mallet --enable cyclomatic-complexity:max=10 --enable function-length:max=30 src/
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
            ;; Regular file - use probe-file result to get absolute path
            ((probe-file path)
             (push (probe-file path) files))
            (t
             (error 'errors:file-not-found :path arg))))))
    (nreverse files)))

(defun load-configuration (config-path preset)
  "Load configuration from file or use built-in preset.
Returns the final config with CLI preset override applied."
  (let ((config:*default-preset* (or preset :default)))
    (or (and config-path
             (config:load-config config-path :preset-override preset))
        (let ((discovered (config:find-config-file (uiop:getcwd))))
          (when discovered
            (config:load-config discovered :preset-override preset)))
        (config:get-built-in-config (or preset :default)))))

(defun has-cli-rules-p (cli-rules)
  "Check if cli-rules has any actual overrides."
  (or (getf cli-rules :enable-rules)
      (getf cli-rules :disable-rules)
      (getf cli-rules :enable-groups)
      (getf cli-rules :disable-groups)))

(defun track-violation-severity (violations)
  "Check violations for errors and warnings.
Returns (values has-errors-p has-warnings-p)."
  (values
   (some (lambda (v) (eq (violation-severity v) :error)) violations)
   (some (lambda (v) (eq (violation-severity v) :warning)) violations)))

(defun process-fix-mode (all-violations fix-mode format)
  "Apply fixes to violations and output results.
Returns (values has-errors-p has-warnings-p)."
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

    ;; Return updated exit code tracking based on unfixed violations
    (track-violation-severity unfixed-violations)))

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

          (multiple-value-bind (format config-path preset debug fix-mode cli-rules file-args)
              (parse-args args)

            ;; Enable debug mode if requested
            (setf *debug-mode* debug)

            ;; Validate we have files to lint
            (when (null file-args)
              (print-help)
              (uiop:quit 1))

            (let* ((files (expand-file-args file-args))
                   (base-config (load-configuration config-path preset))
                   ;; Apply CLI overrides if any
                   (config (if (has-cli-rules-p cli-rules)
                               (config:apply-cli-overrides base-config cli-rules)
                               base-config))
                   (severity-counts '())
                   (has-errors nil)
                   (has-warnings nil)
                   (first-file-with-violations t)
                   (all-violations '()))

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
                         (:line
                          ;; Output violations in line format and accumulate counts
                          (let ((file-counts (formatter:format-line-file file violations)))
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
                      (multiple-value-bind (errors warnings)
                          (track-violation-severity violations)
                        (when errors (setf has-errors t))
                        (when warnings (setf has-warnings t)))))))

              ;; Apply fixes if in fix mode
              (when fix-mode
                (multiple-value-bind (errors warnings)
                    (process-fix-mode all-violations fix-mode format)
                  (setf has-errors errors)
                  (setf has-warnings warnings)))

              ;; Print summary/closing (only for normal mode)
              (unless fix-mode
                (ecase format
                  ((:text :line)
                   (formatter:format-text-summary severity-counts))
                  (:json
                   (formatter:format-json-end))))

              ;; Exit with appropriate status
              (cond
                (has-errors (uiop:quit 2))
                (has-warnings (uiop:quit 1))
                (t (uiop:quit 0))))))
      ;; Catch explicit exit to ensure we don't suppress intentional quits
      (#+sbcl sb-sys:interactive-interrupt
       #-sbcl error ()
        (format *error-output* "~&Interrupted.~%")
        (uiop:quit 130)))))


