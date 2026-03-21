(defpackage #:mallet/config
  (:use #:cl)
  (:local-nicknames
   (#:glob #:trivial-glob)
   (#:rules #:mallet/rules)
   (#:utils #:mallet/utils)
   (#:errors #:mallet/errors))
  (:export #:config
           #:make-config
           #:parse-config
           #:load-config
           #:get-rules-for-file
           #:get-built-in-config
           #:find-config-file
           #:config-rules
           #:config-path-rules
           #:config-disabled-rules
           #:config-ignore
           #:config-root-dir
           #:file-ignored-p
           #:*default-preset*
           ;; CLI overrides
           #:apply-cli-overrides
           #:config-set-severity-overrides
           ;; Path override structure and accessors
           #:path-override
           #:path-override-patterns
           #:path-override-rules
           #:path-override-disabled-rules
           ;; Multi-form config reader
           #:read-mallet-forms
           ;; Preset registry
           #:preset-definition
           #:preset-definition-name
           #:preset-definition-extends
           #:preset-definition-enable-specs
           #:preset-definition-disable-specs
           #:parse-preset-definition
           #:build-preset-registry
           #:resolve-preset
           #:*built-in-preset-names*))
(in-package #:mallet/config)

(defvar *default-preset* :default)

;;; Path-specific rule structure

(defstruct path-override
  "Structure for path-specific rule overrides.
Stores patterns that match files and the rules/disabled-rules that apply."
  (patterns '() :type list)          ; List of glob patterns
  (rules '() :type list)             ; List of rule instances
  (disabled-rules '() :type list))   ; List of disabled rule names (keywords)

;;; Preset definition structure

(defstruct preset-definition
  "Structure representing a user-defined preset parsed from a :mallet-preset form."
  (name nil :type keyword)
  (extends nil)
  (enable-specs '() :type list)
  (disable-specs '() :type list))

;;; Built-in preset names — used to detect shadowing
(defvar *built-in-preset-names* '(:default :all :none))

;;; Config data structure

(defclass config ()
  ((rules
    :initarg :rules
    :initform '()
    :accessor config-rules
    :documentation "List of rule instances")
   (disabled-rules
    :initarg :disabled-rules
    :initform '()
    :accessor config-disabled-rules
    :documentation "List of rule names (keywords) that are disabled")
   (path-rules
    :initarg :path-rules
    :initform '()
    :accessor config-path-rules
    :documentation "List of path-override structures for path-specific rules")
   (ignore
    :initarg :ignore
    :initform '()
    :accessor config-ignore
    :documentation "List of glob patterns for files to ignore")
   (root-dir
    :initarg :root-dir
    :initform nil
    :accessor config-root-dir
    :documentation "Root directory where config file is located (nil for built-in configs)")
   (set-severity-overrides
    :initarg :set-severity-overrides
    :initform '()
    :accessor config-set-severity-overrides
    :documentation "Alist of (category . severity) from :set-severity directives in the config file"))
  (:documentation "Configuration for Mallet linter containing rule instances."))

(defun make-config (&key rules disabled-rules path-rules ignore root-dir set-severity-overrides)
  "Create a new config with RULES (list of rule instances), DISABLED-RULES (list of keywords),
PATH-RULES, IGNORE patterns, and SET-SEVERITY-OVERRIDES (alist of (category . severity))."
  (make-instance 'config
                 :rules (or rules '())
                 :disabled-rules (or disabled-rules '())
                 :path-rules (or path-rules '())
                 :ignore (or ignore '())
                 :root-dir root-dir
                 :set-severity-overrides (or set-severity-overrides '())))

;;; Preset parsing and registry

(defun parse-preset-definition (sexp)
  "Parse a :mallet-preset s-expression into a preset-definition struct.
Format: (:mallet-preset :name (:extends :parent) (:enable ...) (:disable ...))"
  (check-type sexp list)
  (unless (eq (first sexp) :mallet-preset)
    (error "Preset form must start with :mallet-preset, got: ~S" (first sexp)))
  (let ((name (second sexp)))
    (unless (keywordp name)
      (error "Preset name must be a keyword, got: ~S" name))
    (let ((extends nil)
          (enable-specs '())
          (disable-specs '()))
      (dolist (form (cddr sexp))
        (when (consp form)
          (case (first form)
            (:extends
             (setf extends (second form)))
            (:enable
             (let ((rule-name (utils:resolve-rule-alias (second form)))
                   (options (cddr form)))
               (push (cons rule-name options) enable-specs)))
            (:disable
             (let ((rule-name (utils:resolve-rule-alias (second form))))
               (push rule-name disable-specs))))))
      (make-preset-definition
       :name name
       :extends extends
       :enable-specs (nreverse enable-specs)
       :disable-specs (nreverse disable-specs)))))

(defun build-preset-registry (preset-definitions)
  "Build a hash table from a list of preset-definition structs, keyed by name."
  (let ((registry (make-hash-table :test 'eq)))
    (dolist (defn preset-definitions)
      (setf (gethash (preset-definition-name defn) registry) defn))
    registry))

(defun resolve-preset (name registry &optional resolving-stack)
  "Resolve preset NAME to a config object using REGISTRY and built-in presets.
RESOLVING-STACK tracks the chain being followed to detect circular references.
Signals circular-preset-reference on cycles, unknown-preset when not found."
  (check-type name keyword)
  (check-type registry hash-table)
  ;; Circular reference detection: name appearing in the stack means a cycle
  (when (member name resolving-stack)
    (error 'errors:circular-preset-reference
           :chain (append resolving-stack (list name))))
  (let ((defn (gethash name registry)))
    (cond
      ;; Found in registry — user-defined preset
      (defn
       ;; Emit shadowing note when overriding a built-in name
       (when (member name *built-in-preset-names*)
         (format *error-output*
                 "; Using project-defined ~A preset (shadowing built-in)~%"
                 (string-downcase (symbol-name name))))
       (let* ((new-stack (append resolving-stack (list name)))
              ;; Resolve parent: follow :extends if present, else use :none as base
              (parent-config
                (if (preset-definition-extends defn)
                    (resolve-preset (preset-definition-extends defn)
                                    registry
                                    new-stack)
                    (make-none-config)))
              (parent-rules (config-rules parent-config))
              (parent-disabled (config-disabled-rules parent-config))
              ;; Build override forms for parse-override-rules
              (override-forms
                (append
                 (mapcar (lambda (spec)
                           ;; parse-override-rules expects (:enable rule-name . options)
                           (cons :enable (cons (car spec) (cdr spec))))
                         (preset-definition-enable-specs defn))
                 (mapcar (lambda (rule-name)
                           (list :disable rule-name))
                         (preset-definition-disable-specs defn)))))
         (multiple-value-bind (result-rules result-disabled)
             (parse-override-rules override-forms parent-rules parent-disabled)
           (make-config :rules result-rules
                        :disabled-rules result-disabled))))
      ;; Not in registry — try built-in presets
      (t
       (handler-case
           (get-built-in-config name)
         (error ()
           ;; Not a built-in either — collect available names and signal
           (let ((user-names (loop for k being the hash-keys of registry
                                   collect k)))
             (error 'errors:unknown-preset
                    :name name
                    :available-names (append user-names
                                             *built-in-preset-names*)))))))))

;;; Config parsing

(defun create-rule-from-spec (rule-spec)
  "Create a rule instance from a rule specification.
RULE-SPEC is a cons of (rule-name . options-plist)."
  (let ((rule-name (car rule-spec))
        (options (cdr rule-spec)))
    (apply #'rules:make-rule rule-name options)))

(defun parse-override-rules (override-forms base-rules base-disabled)
  "Parse override rules from (:enable ...) and (:disable ...) forms.
Returns (values rules disabled-rules explicit-severity-names) where:
- rules is a list of rule instances
- disabled-rules is a list of rule names that are disabled
- explicit-severity-names is a list of rule names that had an explicit :severity in their spec"
  ;; Create a hash table of base rules for easy lookup
  (let ((base-rules-map (make-hash-table :test 'eq)))
    (dolist (rule base-rules)
      (setf (gethash (rules:rule-name rule) base-rules-map) rule))

    ;; Process overrides
    (let ((override-specs '())
          (disabled '()))
      (dolist (form override-forms)
        (when (consp form)
          (let ((key (first form)))
            (case key
              (:enable
               (let ((rule-name (utils:resolve-rule-alias (second form)))
                     (options (cddr form)))
                 (push (cons rule-name options) override-specs)))
              (:disable
               (let ((rule-name (utils:resolve-rule-alias (second form))))
                 (push rule-name disabled)))))))

      ;; Build final rule list
      (let ((result-rules '())
            (result-disabled (copy-list base-disabled))
            (overridden-names (mapcar #'car override-specs))
            (explicit-severity-names '()))

        ;; Add all base rules that aren't being overridden
        (dolist (rule base-rules)
          (let ((rule-name (rules:rule-name rule)))
            (unless (member rule-name overridden-names)
              (push rule result-rules))))

        ;; Add override rules; track those with explicit :severity
        (dolist (spec (nreverse override-specs))
          (let ((rule (create-rule-from-spec spec)))
            (push rule result-rules)
            (when (getf (cdr spec) :severity)
              (push (car spec) explicit-severity-names))
            ;; Remove from disabled list if it was there
            (setf result-disabled (remove (car spec) result-disabled))))

        ;; Add newly disabled rules to disabled list
        (dolist (rule-name disabled)
          (pushnew rule-name result-disabled))

        (values (nreverse result-rules) result-disabled explicit-severity-names)))))

(defun parse-config (sexp &key preset-override preset-registry) ; mallet:suppress cyclomatic-complexity
  "Parse S-expression SEXP into a config object.
Uses new syntax: (:enable :rule-name ...), (:disable :rule-name), (:ignore ...), and (:for-paths ...).
If PRESET-OVERRIDE is provided, it overrides the :extends clause in the config file.
If PRESET-REGISTRY is provided, user-defined presets are resolved from it before
falling back to built-in presets."
  (check-type sexp list)

  (unless (eq (first sexp) :mallet-config)
    (error "Config must start with :mallet-config"))

  (flet ((resolve-extends (name)
           (if preset-registry
               (resolve-preset name preset-registry)
               (get-built-in-config name))))

  ;; Pre-pass: collect all :set-severity overrides before the main loop so that
  ;; :for-paths blocks are always processed with the complete override set, regardless
  ;; of ordering within the config file.
  ;;
  ;; When the same category appears more than once, the last directive wins.
  ;; We achieve this by reversing the collected list before building the alist:
  ;; assoc returns the first matching key, so putting the last-seen entry first
  ;; makes it authoritative.
  (let ((set-severity-overrides
          (let ((raw (loop for item in (rest sexp)
                           when (and (consp item) (eq (first item) :set-severity))
                             collect (let ((category (second item))
                                          (severity (third item)))
                                       (unless (member severity '(:error :warning :info))
                                         (error ":set-severity expects :error, :warning, or :info, but got: ~S" severity))
                                       (unless (member category '(:correctness :suspicious :cleanliness
                                                                  :style :practice :format :metrics))
                                         (error ":set-severity expects a valid category (:correctness :suspicious :cleanliness :style :practice :format :metrics), but got: ~S" category))
                                       (cons category severity)))))
            (nreverse raw))))

  (let ((rule-specs '())
        (disabled-rules '())
        (ignore-patterns '())
        (path-rules '())
        (extends nil)
        (explicit-severity-rules '()))

    ;; Parse top-level options
    (loop for item in (rest sexp)
          when (consp item)
            do (let ((key (first item)))
                 (case key
                   (:extends
                    ;; CLI preset-override takes precedence over config file :extends
                    (let ((extends-value (or preset-override (second item))))
                      (setf extends (resolve-extends extends-value))))
                   (:enable
                    ;; New syntax: (:enable :rule-name :option value ...)
                    (let ((rule-name (utils:resolve-rule-alias (second item)))
                          (options (cddr item)))
                      (push (cons rule-name options) rule-specs)
                      ;; Track rules with explicit :severity — they take precedence
                      ;; over :set-severity category overrides.
                      (when (getf options :severity)
                        (push rule-name explicit-severity-rules))))
                   (:disable
                    ;; New syntax: (:disable :rule-name)
                    (let ((rule-name (utils:resolve-rule-alias (second item))))
                      (push rule-name disabled-rules)))
                   (:set-severity
                    ;; Already collected in pre-pass above; skip here.
                    nil)
                   (:ignore
                    ;; Ignore patterns: (:ignore "pattern1" "pattern2" ...)
                    (setf ignore-patterns (rest item)))
                   (:for-paths
                    ;; Path-specific overrides: (:for-paths (pattern...) (:enable ...) (:disable ...))
                    (let ((patterns (second item))
                          (override-forms (cddr item)))
                      ;; Build base for :for-paths by merging extends + project-wide settings
                      ;; This ensures :for-paths inherits project-wide :enable/:disable
                      (let* ((extends-rules (if extends (config-rules extends) '()))
                             (extends-disabled (if extends (config-disabled-rules extends) '()))
                             ;; Merge project-wide rules with extends (same logic as main config merge)
                             (project-wide-rule-names (mapcar #'car rule-specs))
                             ;; Start with extends rules not overridden by project-wide
                             (merged-base-rules
                               (remove-if (lambda (rule)
                                            (member (rules:rule-name rule) project-wide-rule-names))
                                          extends-rules))
                             ;; Add project-wide rule specs
                             (merged-base-rules
                               (append (mapcar #'create-rule-from-spec (reverse rule-specs))
                                       merged-base-rules))
                             ;; Merge disabled lists
                             (merged-base-disabled
                               (union disabled-rules
                                      (set-difference extends-disabled project-wide-rule-names))))
                        (multiple-value-bind (override-rules override-disabled override-explicit-sev)
                            (parse-override-rules override-forms merged-base-rules merged-base-disabled)
                          ;; Apply :set-severity overrides to path-specific rules,
                          ;; but skip rules that have an explicit per-rule :severity —
                          ;; per-rule :severity takes precedence over :set-severity.
                          (when set-severity-overrides
                            (dolist (rule override-rules)
                              (let* ((cat (rules:rule-category rule))
                                     (sev-override (assoc cat set-severity-overrides)))
                                (when (and sev-override
                                           (not (member (rules:rule-name rule) override-explicit-sev)))
                                  (setf (rules:rule-severity rule) (cdr sev-override))))))
                          (push (make-path-override
                                 :patterns patterns
                                 :rules override-rules
                                 :disabled-rules override-disabled)
                                path-rules))))))))

    ;; If preset-override is provided but no :extends clause was found, use preset-override
    (when (and preset-override (not extends))
      (setf extends (resolve-extends preset-override)))

    ;; Create rule instances
    (let ((rules (mapcar #'create-rule-from-spec (nreverse rule-specs))))
      ;; If extends is specified, merge with base rules
      (when extends
        (let ((base-rules (config-rules extends))
              (rule-names (mapcar #'rules:rule-name rules)))
          ;; Add base rules that aren't overridden
          (dolist (base-rule base-rules)
            (unless (member (rules:rule-name base-rule) rule-names)
              (push base-rule rules)))
          ;; Merge disabled rules, but exclude rules that were explicitly enabled
          (setf disabled-rules
                (union disabled-rules
                       ;; Remove enabled rules from base disabled list
                       (set-difference (config-disabled-rules extends) rule-names)))))

      ;; Apply :set-severity overrides to rules in matching categories.
      ;; Per-rule :severity (from (:enable :rule :severity :x)) takes precedence
      ;; over :set-severity.  Rules without an explicit :severity get the category
      ;; override applied.
      (let ((final-rules (nreverse rules)))
        (when set-severity-overrides
          (dolist (rule final-rules)
            (let* ((cat (rules:rule-category rule))
                   (sev-override (assoc cat set-severity-overrides)))
              (when (and sev-override
                         (not (member (rules:rule-name rule) explicit-severity-rules)))
                (setf (rules:rule-severity rule) (cdr sev-override))))))
        (make-config :rules final-rules
                     :disabled-rules disabled-rules
                     :path-rules (nreverse path-rules)
                     :ignore ignore-patterns
                     :set-severity-overrides set-severity-overrides)))))))

;;; Multi-form config reader

(defun read-mallet-forms (path)
  "Read all top-level s-expressions from PATH.
Returns (values preset-forms config-form) where preset-forms is a list of
:mallet-preset s-expressions and config-form is the :mallet-config s-expression
or nil if none is present.

Binds *read-eval* to nil for safety. Signals:
- errors:unknown-config-form if an unrecognized top-level form is found
- errors:multiple-config-forms if more than one :mallet-config form appears
- errors:duplicate-preset-name if two :mallet-preset forms share a name"
  (let ((pathname (etypecase path
                    (string (uiop:parse-native-namestring path))
                    (pathname path))))
    (with-open-file (in pathname :direction :input)
      (let ((*read-eval* nil)
            (preset-forms '())
            (config-form nil))
        (loop
          (let ((form (read in nil :eof)))
            (when (eq form :eof)
              (return))
            (unless (consp form)
              (error 'errors:unknown-config-form :form form))
            (case (first form)
              (:mallet-config
               (when config-form
                 (error 'errors:multiple-config-forms))
               (setf config-form form))
              (:mallet-preset
               (let ((name (second form)))
                 (when (find name preset-forms :key #'second)
                   (error 'errors:duplicate-preset-name :name name))
                 (push form preset-forms)))
              (otherwise
               (error 'errors:unknown-config-form :form form)))))
        (values (nreverse preset-forms) config-form)))))

;;; Config file loading

(defun load-config (path &key preset-override)
  "Load configuration from file at PATH.
Sets root-dir to the directory containing the config file.
If PRESET-OVERRIDE is provided, it overrides the :extends clause in the config file.

Supports multi-sexp .mallet.lisp files with :mallet-preset and :mallet-config forms.
When no :mallet-config form is present but a :default preset is defined, the :default
preset is resolved and returned."
  (let* ((pathname (etypecase path
                     (string (uiop:parse-native-namestring path))
                     (pathname path)))
         (root-dir (uiop:pathname-directory-pathname pathname)))
    (unless (probe-file pathname)
      (error 'errors:config-not-found :path pathname))

    (multiple-value-bind (preset-forms config-sexp)
        (read-mallet-forms pathname)
      (let* ((preset-defs (mapcar #'parse-preset-definition preset-forms))
             (registry (build-preset-registry preset-defs))
             (config
               (cond
                 (config-sexp
                  (parse-config config-sexp
                                :preset-override preset-override
                                :preset-registry registry))
                 ((and (not preset-override)
                       (gethash :default registry))
                  (resolve-preset :default registry))
                 (preset-override
                  (resolve-preset preset-override registry))
                 (t
                  (get-built-in-config :default)))))
        (setf (config-root-dir config) root-dir)
        config))))

;;; Rule selection

(defun get-rules-for-file (config file-path)
  "Get the list of rule instances that apply to FILE-PATH, filtering out disabled rules."
  (check-type config config)
  (check-type file-path pathname)

  (let* ((root-dir (or (config-root-dir config) #P"/"))
         (file-namestring (namestring file-path))
         ;; Convert file path to project-relative format (like in file-ignored-p)
         (match-path (let ((root-namestring (namestring root-dir)))
                       (cond
                         ((equal root-namestring "/")
                          file-namestring)
                         ((and (>= (length file-namestring) (length root-namestring))
                               (string= file-namestring root-namestring
                                        :end1 (length root-namestring)))
                          ;; Remove root-dir prefix to get relative path with leading /
                          (concatenate 'string
                                       "/"
                                       (subseq file-namestring (length root-namestring))))
                         (t
                          ;; File not under root-dir, use full path
                          file-namestring)))))
    ;; Check path-specific rules first
    (loop for path-override in (config-path-rules config)
          when (some (lambda (pattern)
                       (glob:glob-path-match pattern match-path))
                     (path-override-patterns path-override))
            ;; Found matching path-specific rules - filter out disabled ones
            return (remove-if (lambda (rule)
                                (member (rules:rule-name rule)
                                        (path-override-disabled-rules path-override)))
                              (path-override-rules path-override))
          ;; No match? Use base rules, filtering out disabled ones
          finally (return (remove-if (lambda (rule)
                                       (member (rules:rule-name rule)
                                               (config-disabled-rules config)))
                                     (config-rules config))))))

;;; Built-in configs

(defun get-built-in-config (&optional (name *default-preset*))
  "Get a built-in configuration by NAME (:default, :all, or :none)."
  (check-type name keyword)

  (case name
    (:default
     (make-default-config))
    (:all
     (make-all-config))
    (:none
     (make-none-config))
    (otherwise
     (error "Unknown built-in config: ~A. Available: :default, :all, :none" name))))

(defun make-default-config ()
  "Create the default configuration - only universally-accepted rules.
Enables rules that catch bugs or follow strong community conventions.
Style preferences are disabled to keep output clean."
  (let ((enabled-rules
          '(;; Universally accepted - keep enabled
            :trailing-whitespace
            :no-tabs
            :missing-final-newline
            :closing-paren-on-own-line
            :wrong-otherwise
            :unused-variables
            :unused-local-functions
            :unused-local-nicknames
            :unused-imported-symbols
            :asdf-component-strings
            :asdf-operate-in-perform
            :asdf-secondary-system-name
            :asdf-if-feature-keyword
            :mixed-optional-and-key
            :missing-else
            :no-eval
            :no-ignore-errors
            :no-package-use
            :needless-let*
            :double-colon-access
            :stale-suppression
            :redundant-progn))
        (disabled-rules
          '(;; Style preferences - disabled (too noisy, no consensus)
            :line-length
            :consecutive-blank-lines
            :progn-in-conditional
            :defpackage-interned-symbol
            :missing-otherwise
            :constant-naming
            :special-variable-naming
            :missing-docstring
            :missing-package-docstring
            :missing-variable-docstring
            :missing-struct-docstring
            ;; Practice - disabled by default
            :coalton-missing-declare
            :asdf-redundant-package-prefix
            :asdf-reader-conditional
            :bare-float-literal
            ;; Cleanliness - disabled
            :unused-loop-variables
            ;; Style - disabled by default
            :one-package-per-file)))
    (make-config
     :rules (mapcar #'rules:make-rule enabled-rules)
     :disabled-rules disabled-rules)))

(defun make-all-config ()
  "Create configuration with all rules enabled.
Useful for exploration and discovering what rules exist."
  (let ((all-rules
          '(;; Correctness
            :wrong-otherwise
            :mixed-optional-and-key
            :asdf-if-feature-keyword
            :asdf-secondary-system-name
            :coalton-missing-to-boolean
            ;; Suspicious
            :no-eval
            :runtime-intern
            :runtime-unintern
            :asdf-operate-in-perform
            ;; Practice
            :coalton-missing-declare
            :no-allow-other-keys
            :no-ignore-errors
            :no-package-use
            :double-colon-access
            :error-with-string-only
            :asdf-component-strings
            :asdf-reader-conditional
            :bare-float-literal
            ;; Cleanliness
            :unused-variables
            :unused-local-functions
            :unused-local-nicknames
            :unused-imported-symbols
            :unused-loop-variables
            ;; Style
            :one-package-per-file
            :missing-else
            :progn-in-conditional
            :redundant-progn
            :missing-otherwise
            :defpackage-interned-symbol
            :special-variable-naming
            :constant-naming
            :asdf-redundant-package-prefix
            :needless-let*
            :missing-docstring
            :missing-package-docstring
            :missing-variable-docstring
            :missing-struct-docstring
            :stale-suppression
            ;; Format
            :no-tabs
            :trailing-whitespace
            :missing-final-newline
            :closing-paren-on-own-line
            :line-length
            :consecutive-blank-lines
            ;; Metrics
            :function-length
            :cyclomatic-complexity
            :comment-ratio)))
    (make-config
     :rules (mapcar (lambda (spec)
                      (if (consp spec)
                          (apply #'rules:make-rule spec)
                          (rules:make-rule spec)))
                    all-rules)
     :disabled-rules '())))  ; All rules enabled

(defun make-none-config ()
  "Create configuration with no rules enabled.
Useful for explicitly enabling only specific rules from scratch."
  (make-config
   :rules '()
   :disabled-rules '()))

;;; Ignore patterns

(defun file-ignored-p (config file-path)
  "Check if FILE-PATH should be ignored according to CONFIG's ignore patterns.
Returns T if the file matches any ignore pattern, NIL otherwise."
  (check-type config config)
  (check-type file-path pathname)

  (let* ((root-dir (or (config-root-dir config) #P"/"))
         (ignore-patterns (config-ignore config))
         ;; If we have a root-dir, make file-path relative to it for matching
         ;; This allows patterns like **/file.lisp to match file.lisp at any level
         (match-path (let ((file-namestring (namestring file-path))
                           (root-namestring (namestring root-dir)))
                       ;; If file is under root-dir, make it relative
                       (cond
                         ((equal root-namestring "/")
                          file-namestring)
                         ((and (>= (length file-namestring) (length root-namestring))
                               (string= file-namestring root-namestring
                                        :end1 (length root-namestring)))
                          ;; Remove root-dir prefix to get relative path
                          (concatenate 'string
                                       "/"
                                       (subseq file-namestring (length root-namestring))))
                         (t
                          ;; File not under root-dir, use full path
                          file-namestring)))))
    ;; Check if any ignore pattern matches the file
    (some (lambda (pattern)
            (glob:glob-path-match pattern match-path))
          ignore-patterns)))

;;; CLI override merging

(defun apply-cli-overrides (config cli-rules)
  "Apply CLI rule overrides to CONFIG. Returns new config object.
CLI overrides have highest precedence.

CLI-RULES is a plist with:
  :enable-rules - List of (rule-name . options-plist)
  :disable-rules - List of rule-name keywords"
  (check-type config config)
  (check-type cli-rules list)

  (let* ((enable-rules (getf cli-rules :enable-rules))
         (disable-rules (getf cli-rules :disable-rules))
         (base-rules (config-rules config))
         (base-disabled (config-disabled-rules config))
         (result-rules '())
         (result-disabled (copy-list base-disabled)))

    ;; Step 1: Process each base rule against CLI overrides
    (dolist (rule base-rules)
      (let* ((rule-name (rules:rule-name rule))
             (explicitly-enabled (assoc rule-name enable-rules))
             (explicitly-disabled (member rule-name disable-rules)))
        (cond
          ;; Explicitly enabled: recreate with new options (highest priority).
          ;; Preserve config-applied severity (e.g. from :set-severity) unless the
          ;; CLI explicitly provides a :severity override.
          (explicitly-enabled
           (let* ((options (cdr explicitly-enabled))
                  (new-rule (apply #'rules:make-rule rule-name options)))
             (unless (getf options :severity)
               (setf (rules:rule-severity new-rule) (rules:rule-severity rule)))
             (push new-rule result-rules)))

          ;; Explicitly disabled: add to disabled list
          (explicitly-disabled
           (pushnew rule-name result-disabled))

          ;; No CLI override: keep base rule state
          (t
           (unless (member rule-name base-disabled)
             (push rule result-rules))))))

    ;; Step 2: Add rules that were only in enable-rules (not in base config).
    ;; Apply :set-severity overrides for rules without an explicit CLI :severity,
    ;; consistent with the per-rule :severity > :set-severity precedence used in
    ;; parse-config.
    (let ((overrides (config-set-severity-overrides config)))
      (dolist (enable-spec enable-rules)
        (let ((rule-name (car enable-spec)))
          (unless (find rule-name base-rules :key #'rules:rule-name)
            (let* ((options (cdr enable-spec))
                   (new-rule (apply #'rules:make-rule rule-name options)))
              ;; Apply :set-severity when no explicit :severity was given on the CLI.
              (when (and overrides (not (getf options :severity)))
                (let* ((cat (rules:rule-category new-rule))
                       (sev-override (assoc cat overrides)))
                  (when sev-override
                    (setf (rules:rule-severity new-rule) (cdr sev-override)))))
              (push new-rule result-rules)
              ;; Remove from disabled if it was there
              (setf result-disabled (remove rule-name result-disabled)))))))

    ;; Step 3: Create new config with overridden rules
    (make-config
     :rules (nreverse result-rules)
     :disabled-rules result-disabled
     :path-rules (config-path-rules config)
     :ignore (config-ignore config)
     :root-dir (config-root-dir config)
     :set-severity-overrides (config-set-severity-overrides config))))

;;; Config file discovery

(defun find-project-root (&optional (pathname *default-pathname-defaults*))
  (check-type pathname pathname)
  (let ((pathname
          (if (uiop:directory-pathname-p pathname)
              (probe-file pathname)
              (uiop:pathname-directory-pathname pathname))))
    (when pathname
      (cond
        ((or (find-if (lambda (dir)
                        (uiop:directory-exists-p (merge-pathnames dir pathname)))
                      '(".bzr" ".git" ".hg" ".qlot"))
             (find-if (lambda (file)
                        (uiop:file-exists-p (merge-pathnames file pathname)))
                      '("qlfile" ".mallet.lisp")))
         pathname)
        ((equal (user-homedir-pathname) pathname)
         nil)
        (t
         (let ((parent (uiop:pathname-parent-directory-pathname pathname)))
           (if (equal parent pathname)
               nil
               (find-project-root parent))))))))

(defun find-config-file (start-directory)
  "Find .mallet.lisp config file starting from START-DIRECTORY.
Walks up parent directories until found or reaches root."
  (let ((dir (etypecase start-directory
               (pathname
                (uiop:ensure-directory-pathname start-directory))
               (string
                (uiop:ensure-directory-pathname
                 (uiop:parse-native-namestring start-directory))))))
    (when dir
      (let ((project-root (find-project-root dir)))
        (when project-root
          (uiop:file-exists-p
           (merge-pathnames ".mallet.lisp" project-root)))))))
