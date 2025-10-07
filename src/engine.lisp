(defpackage #:malvolio/engine
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:parser #:malvolio/parser)
   (#:rules #:malvolio/rules)
   (#:config #:malvolio/config)
   (#:violation #:malvolio/violation))
  (:export #:lint-file
           #:lint-files
           #:make-registry-from-config
           #:make-default-registry))
(in-package #:malvolio/engine)

(defun make-default-registry ()
  "Create a registry with all default rules enabled."
  (make-registry-from-config (config:get-built-in-config :recommended)))

(defun make-registry-from-config (cfg)
  "Create a registry based on CONFIG."
  (check-type cfg config:config)

  (let ((registry (rules:make-registry)))
    ;; Define all available rules with their metadata
    (let ((rule-definitions
           '((:line-length
              :description "Lines should not exceed maximum length"
              :default-severity :warning
              :type :text)
             (:trailing-whitespace
              :description "Lines should not have trailing whitespace"
              :default-severity :warning
              :type :text)
             (:no-tabs
              :description "Use spaces instead of tab characters"
              :default-severity :warning
              :type :text)
             (:final-newline
              :description "Files must end with a newline"
              :default-severity :warning
              :type :text)
             (:consecutive-blank-lines
              :description "Limit consecutive blank lines"
              :default-severity :warning
              :type :text)
             (:comment-level
              :description "Comments should use appropriate semicolon count"
              :default-severity :warning
              :type :token)
             (:if-without-else
              :description "Use 'when' or 'unless' instead of 'if' without else"
              :default-severity :warning
              :type :form)
             (:bare-progn-in-if
              :description "Use 'cond' instead of 'if' with bare 'progn'"
              :default-severity :warning
              :type :form)
             (:missing-otherwise
              :description "case/typecase should have 'otherwise' clause"
              :default-severity :warning
              :type :form)
             (:wrong-otherwise
              :description "ecase/etypecase should not have 'otherwise' or 't'"
              :default-severity :error
              :type :form)
             (:unused-variables
              :description "Variables should be used or explicitly ignored"
              :default-severity :warning
              :type :form))))

      ;; Register each rule with config-based settings
      (dolist (rule-def rule-definitions)
        (let* ((rule-name (first rule-def))
               (description (getf (rest rule-def) :description))
               (default-severity (getf (rest rule-def) :default-severity))
               (type (getf (rest rule-def) :type))
               (enabled (config:rule-enabled-p cfg rule-name))
               (severity (or (config:get-rule-option cfg rule-name :severity)
                             default-severity)))
          (rules:register-rule registry rule-name
                              :description description
                              :severity severity
                              :type type
                              :enabled enabled))))

    registry))

(defun lint-file (file &key registry config)
  "Lint a single FILE using REGISTRY rules and CONFIG.
Returns a list of VIOLATION objects."
  (check-type file pathname)

  ;; If neither registry nor config provided, use default
  (when (and (not registry) (not config))
    (setf config (config:get-built-in-config :recommended)))

  ;; Create registry from config if not provided
  (when (and config (not registry))
    (setf registry (make-registry-from-config config)))

  ;; If no config but have registry, create default config for options
  (when (and registry (not config))
    (setf config (config:get-built-in-config :recommended)))

  (check-type registry rules:registry)
  (check-type config config:config)

  (unless (probe-file file)
    (error "File not found: ~A" file))

  (let ((text (uiop:read-file-string file))
        (violations '()))

    ;; Run text-level rules
    (dolist (rule (rules:list-rules registry))
      (when (and (rules:rule-enabled-p rule)
                 (eq (rules:rule-type rule) :text))
        (let* ((rule-name (rules:rule-name rule))
               (rule-impl (case rule-name
                            (:line-length
                             (let ((max-length (or (config:get-rule-option config :line-length :max-length)
                                                   80)))
                               (make-instance 'rules:line-length-rule :max-length max-length)))
                            (:trailing-whitespace
                             (make-instance 'rules:trailing-whitespace-rule))
                            (:no-tabs
                             (make-instance 'rules:no-tabs-rule))
                            (:final-newline
                             (make-instance 'rules:final-newline-rule))
                            (:consecutive-blank-lines
                             (let ((max-consecutive (or (config:get-rule-option config :consecutive-blank-lines :max-consecutive)
                                                        2)))
                               (make-instance 'rules:consecutive-blank-lines-rule :max-consecutive max-consecutive)))
                            (t nil))))
          (when rule-impl
            (setf violations
                  (nconc violations (rules:check-text rule-impl text file)))))))

    ;; Run token-level rules
    (let ((tokens (parser:tokenize text file)))
      (dolist (rule (rules:list-rules registry))
        (when (and (rules:rule-enabled-p rule)
                   (eq (rules:rule-type rule) :token))
          (let ((rule-impl (make-instance 'rules:comment-level-rule)))
            (setf violations
                  (nconc violations (rules:check-tokens rule-impl tokens file)))))))

    ;; Run form-level rules
    (let ((forms (parser:parse-forms text file)))
      (dolist (form forms)
        (dolist (rule (rules:list-rules registry))
          (when (and (rules:rule-enabled-p rule)
                     (eq (rules:rule-type rule) :form))
            (let ((rule-impl
                   (case (rules:rule-name rule)
                     (:if-without-else (make-instance 'rules:if-without-else-rule))
                     (:bare-progn-in-if (make-instance 'rules:bare-progn-in-if-rule))
                     (:missing-otherwise (make-instance 'rules:missing-otherwise-rule))
                     (:wrong-otherwise (make-instance 'rules:wrong-otherwise-rule))
                     (:unused-variables (make-instance 'rules:unused-variables-rule))
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
    (setf config (config:get-built-in-config :recommended)))
  (when (and config (not registry))
    (setf registry (make-registry-from-config config)))
  (when (and registry (not config))
    (setf config (config:get-built-in-config :recommended)))

  (loop for file in files
        for violations = (lint-file file :registry registry :config config)
        collect (cons file violations)))
