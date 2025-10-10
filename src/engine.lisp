(defpackage #:malo/engine
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:parser #:malo/parser)
   (#:rules #:malo/rules)
   (#:config #:malo/config)
   (#:violation #:malo/violation))
  (:export #:lint-file
           #:lint-files
           #:make-registry-from-config
           #:make-default-registry))
(in-package #:malo/engine)

(defun make-default-registry ()
  "Create a registry with all default rules enabled."
  (make-registry-from-config (config:get-built-in-config :default)))

(defun make-registry-from-config (cfg)
  "Create a registry based on CONFIG."
  (check-type cfg config:config)

  (let ((registry (rules:make-registry)))
    ;; Define all available rules with their metadata
    (let ((rule-definitions
           '(;; ERROR: Objectively wrong code
             (:wrong-otherwise
              :description "ecase/etypecase should not have 'otherwise' or 't'"
              :default-severity :error
              :type :form)
             ;; WARNING: Likely bugs or dangerous patterns
             (:unused-variables
              :description "Variables should be used or explicitly ignored"
              :default-severity :warning
              :type :form)
             (:missing-otherwise
              :description "case/typecase should have 'otherwise' clause"
              :default-severity :warning
              :type :form)
             (:mixed-optional-and-key
              :description "Don't mix &optional and &key in lambda lists"
              :default-severity :warning
              :type :form)
             ;; INFO: Code quality suggestions
             (:unused-local-nicknames
              :description "Local nicknames should be used"
              :default-severity :info
              :type :form)
             (:unused-imported-symbols
              :description "Imported symbols should be used or re-exported"
              :default-severity :info
              :type :form)
             (:constant-naming
              :description "Constants should be named +foo+"
              :default-severity :info
              :type :form)
             (:unused-loop-variables
              :description "LOOP variables should be used or explicitly ignored"
              :default-severity :info
              :type :form)
             ;; CONVENTION: Style/idiom suggestions
             (:if-without-else
              :description "Use 'when' or 'unless' instead of 'if' without else"
              :default-severity :convention
              :type :form)
             (:bare-progn-in-if
              :description "Use 'cond' instead of 'if' with bare 'progn'"
              :default-severity :convention
              :type :form)
             (:special-variable-naming
              :description "Special variables should be named *foo*"
              :default-severity :convention
              :type :form)
             (:asdf-component-strings
              :description "ASDF components, systems, and dependencies should use strings not symbols"
              :default-severity :convention
              :type :form
              :file-types (:asd))
             ;; FORMAT: Consensus formatting (Emacs/SLIME standards)
             (:no-tabs
              :description "Use spaces instead of tab characters"
              :default-severity :format
              :type :text)
             (:trailing-whitespace
              :description "Lines should not have trailing whitespace"
              :default-severity :format
              :type :text)
             (:final-newline
              :description "Files must end with a newline"
              :default-severity :format
              :type :text)
             ;; INFO: Subjective preferences
             (:line-length
              :description "Lines should not exceed maximum length"
              :default-severity :info
              :type :text)
             (:consecutive-blank-lines
              :description "Limit consecutive blank lines"
              :default-severity :info
              :type :text))))

      ;; Register each rule with config-based settings
      (dolist (rule-def rule-definitions)
        (let* ((rule-name (first rule-def))
               (description (getf (rest rule-def) :description))
               (default-severity (getf (rest rule-def) :default-severity))
               (type (getf (rest rule-def) :type))
               (file-types (getf (rest rule-def) :file-types '(:lisp)))
               (enabled (config:rule-enabled-p cfg rule-name))
               (severity (or (config:get-rule-option cfg rule-name :severity)
                             default-severity)))
          (rules:register-rule registry rule-name
                              :description description
                              :severity severity
                              :type type
                              :enabled enabled
                              :file-types file-types))))

    registry))

(defun lint-file (file &key registry config)
  "Lint a single FILE using REGISTRY rules and CONFIG.
Returns a list of VIOLATION objects."
  (check-type file pathname)

  ;; If neither registry nor config provided, use default
  (when (and (not registry) (not config))
    (setf config (config:get-built-in-config :default)))

  ;; Apply path-based overrides if config is provided
  (when config
    (setf config (config:apply-overrides-for-file config file)))

  ;; Create registry from config if not provided
  (when (and config (not registry))
    (setf registry (make-registry-from-config config)))

  ;; If no config but have registry, create default config for options
  (when (and registry (not config))
    (setf config (config:get-built-in-config :default)))

  (check-type registry rules:registry)
  (check-type config config:config)

  (unless (probe-file file)
    (error "File not found: ~A" file))

  (let ((text (uiop:read-file-string file))
        (violations '())
        ;; Extract file extension and convert to keyword (e.g., "lisp" -> :LISP, "asd" -> :ASD)
        (file-type (let ((type-string (pathname-type file)))
                     (when type-string
                       (intern (string-upcase type-string) :keyword)))))

    ;; Run text-level rules
    (dolist (rule (rules:list-rules registry))
      (when (and (rules:rule-enabled-p rule)
                 (eq (rules:rule-type rule) :text)
                 ;; Only run rule if file has extension AND it matches rule's file-types
                 ;; Files without extensions (LICENSE, README, etc.) are skipped
                 (and file-type
                      (member file-type (rules:rule-file-types rule))))
        (let* ((rule-name (rules:rule-name rule))
               (severity (rules:rule-severity rule))
               (rule-impl (case rule-name
                            (:line-length
                             (let ((max-length (or (config:get-rule-option config :line-length :max-length)
                                                   80)))
                               (make-instance 'rules:line-length-rule :max-length max-length :severity severity)))
                            (:trailing-whitespace
                             (make-instance 'rules:trailing-whitespace-rule :severity severity))
                            (:no-tabs
                             (make-instance 'rules:no-tabs-rule :severity severity))
                            (:final-newline
                             (make-instance 'rules:final-newline-rule :severity severity))
                            (:consecutive-blank-lines
                             (let ((max-consecutive (or (config:get-rule-option config :consecutive-blank-lines :max-consecutive)
                                                        2)))
                               (make-instance 'rules:consecutive-blank-lines-rule :max-consecutive max-consecutive :severity severity)))
                            (t nil))))
          (when rule-impl
            (setf violations
                  (nconc violations (rules:check-text rule-impl text file)))))))

    ;; Run form-level rules
    (multiple-value-bind (forms parse-errors)
        (parser:parse-forms text file)

      ;; Convert parse errors to violations (always reported - not configurable)
      ;; Parse errors are :error severity since they prevent compilation
      (dolist (parse-error parse-errors)
        (push (make-instance 'violation:violation
                             :rule :parse-error
                             :file (parser:parse-error-info-file parse-error)
                             :line (parser:parse-error-info-line parse-error)
                             :column (parser:parse-error-info-column parse-error)
                             :severity :error
                             :message (parser:parse-error-info-message parse-error)
                             :fix nil)
              violations))

      (dolist (form forms)
        (dolist (rule (rules:list-rules registry))
          (when (and (rules:rule-enabled-p rule)
                     (eq (rules:rule-type rule) :form)
                     ;; Only run rule if file has extension AND it matches rule's file-types
                     ;; Files without extensions (LICENSE, README, etc.) are skipped
                     (and file-type
                          (member file-type (rules:rule-file-types rule))))
            (let* ((rule-name (rules:rule-name rule))
                   (severity (rules:rule-severity rule))
                   (rule-impl
                    (case rule-name
                      (:if-without-else (make-instance 'rules:if-without-else-rule :severity severity))
                      (:bare-progn-in-if (make-instance 'rules:bare-progn-in-if-rule :severity severity))
                      (:missing-otherwise (make-instance 'rules:missing-otherwise-rule :severity severity))
                      (:wrong-otherwise (make-instance 'rules:wrong-otherwise-rule :severity severity))
                      (:unused-variables (make-instance 'rules:unused-variables-rule :severity severity))
                      (:unused-loop-variables (make-instance 'rules:unused-loop-variables-rule :severity severity))
                      (:unused-local-nicknames (make-instance 'rules:unused-local-nicknames-rule :severity severity))
                      (:unused-imported-symbols (make-instance 'rules:unused-imported-symbols-rule :severity severity))
                      (:special-variable-naming (make-instance 'rules:special-variable-naming-rule :severity severity))
                      (:constant-naming (make-instance 'rules:constant-naming-rule :severity severity))
                      (:mixed-optional-and-key (make-instance 'rules:mixed-optional-and-key-rule :severity severity))
                      (:asdf-component-strings (make-instance 'rules:asdf-component-strings-rule :severity severity))
                      (t nil))))
              (when rule-impl
                (setf violations
                      (nconc violations
                             (rules:check-form rule-impl form file)))))))))

    violations))

(defun lint-files (files &key registry config)
  "Lint multiple FILES using REGISTRY rules and CONFIG.
Returns an alist mapping file paths to violation lists."
  (check-type files list)

  ;; Setup registry and config if not provided
  (when (and (not registry) (not config))
    (setf config (config:get-built-in-config :default)))
  (when (and config (not registry))
    (setf registry (make-registry-from-config config)))
  (when (and registry (not config))
    (setf config (config:get-built-in-config :default)))

  (loop for file in files
        for violations = (lint-file file :registry registry :config config)
        collect (cons file violations)))
