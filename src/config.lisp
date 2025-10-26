(defpackage #:mallet/config
  (:use #:cl)
  (:local-nicknames
   (#:glob #:trivial-glob)
   (#:rules #:mallet/rules))
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
           ;; Path override structure and accessors
           #:path-override
           #:path-override-patterns
           #:path-override-rules
           #:path-override-disabled-rules))
(in-package #:mallet/config)

(defvar *default-preset* :default)

;;; Path-specific rule structure

(defstruct path-override
  "Structure for path-specific rule overrides.
Stores patterns that match files and the rules/disabled-rules that apply."
  (patterns '() :type list)          ; List of glob patterns
  (rules '() :type list)             ; List of rule instances
  (disabled-rules '() :type list))   ; List of disabled rule names (keywords)

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
    :documentation "Root directory where config file is located (nil for built-in configs)"))
  (:documentation "Configuration for Mallet linter containing rule instances."))

(defun make-config (&key rules disabled-rules path-rules ignore root-dir)
  "Create a new config with RULES (list of rule instances), DISABLED-RULES (list of keywords),
PATH-RULES, and IGNORE patterns."
  (make-instance 'config
                 :rules (or rules '())
                 :disabled-rules (or disabled-rules '())
                 :path-rules (or path-rules '())
                 :ignore (or ignore '())
                 :root-dir root-dir))

;;; Config parsing

(defun create-rule-from-spec (rule-spec)
  "Create a rule instance from a rule specification.
RULE-SPEC is a cons of (rule-name . options-plist)."
  (let* ((rule-name (car rule-spec))
         (options (cdr rule-spec)))
    (apply #'rules:make-rule rule-name options)))

(defun parse-override-rules (override-forms base-rules base-disabled)
  "Parse override rules from (:enable ...) and (:disable ...) forms.
Returns (values rules disabled-rules) where:
- rules is a list of rule instances
- disabled-rules is a list of rule names that are disabled"
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
               (let* ((rule-name (second form))
                      (options (cddr form)))
                 (push (cons rule-name options) override-specs)))
              (:disable
               (let ((rule-name (second form)))
                 (push rule-name disabled)))))))

      ;; Build final rule list
      (let ((result-rules '())
            (result-disabled (copy-list base-disabled))
            (overridden-names (mapcar #'car override-specs)))

        ;; Add all base rules that aren't being overridden
        (dolist (rule base-rules)
          (let ((rule-name (rules:rule-name rule)))
            (unless (member rule-name overridden-names)
              (push rule result-rules))))

        ;; Add override rules
        (dolist (spec (nreverse override-specs))
          (let ((rule (create-rule-from-spec spec)))
            (push rule result-rules)
            ;; Remove from disabled list if it was there
            (setf result-disabled (remove (car spec) result-disabled))))

        ;; Add newly disabled rules to disabled list
        (dolist (rule-name disabled)
          (pushnew rule-name result-disabled))

        (values (nreverse result-rules) result-disabled)))))

(defun parse-config (sexp &key preset-override)
  "Parse S-expression SEXP into a config object.
Uses new syntax: (:enable :rule-name ...), (:disable :rule-name), (:ignore ...), and (:for-paths ...).
If PRESET-OVERRIDE is provided, it overrides the :extends clause in the config file."
  (check-type sexp list)

  (unless (eq (first sexp) :mallet-config)
    (error "Config must start with :mallet-config"))

  (let ((rule-specs '())
        (disabled-rules '())
        (ignore-patterns '())
        (path-rules '())
        (extends nil))

    ;; Parse top-level options
    (loop for item in (rest sexp)
          when (consp item)
            do (let ((key (first item)))
                 (case key
                   (:extends
                    ;; Process extends to get base rules
                    ;; CLI preset-override takes precedence over config file :extends
                    (let ((extends-value (or preset-override (second item))))
                      (setf extends (if (keywordp extends-value)
                                        (get-built-in-config extends-value)
                                        (load-config extends-value)))))
                   (:enable
                    ;; New syntax: (:enable :rule-name :option value ...)
                    (let* ((rule-name (second item))
                           (options (cddr item)))
                      (push (cons rule-name options) rule-specs)))
                   (:disable
                    ;; New syntax: (:disable :rule-name)
                    (let ((rule-name (second item)))
                      (push rule-name disabled-rules)))
                   (:ignore
                    ;; Ignore patterns: (:ignore "pattern1" "pattern2" ...)
                    (setf ignore-patterns (rest item)))
                   (:for-paths
                    ;; Path-specific overrides: (:for-paths (pattern...) (:enable ...) (:disable ...))
                    (let* ((patterns (second item))
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
                        (multiple-value-bind (override-rules override-disabled)
                            (parse-override-rules override-forms merged-base-rules merged-base-disabled)
                          (push (make-path-override
                                 :patterns patterns
                                 :rules override-rules
                                 :disabled-rules override-disabled)
                                path-rules))))))))

    ;; If preset-override is provided but no :extends clause was found, use preset-override
    (when (and preset-override (not extends))
      (setf extends (if (keywordp preset-override)
                        (get-built-in-config preset-override)
                        (load-config preset-override))))

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

      (make-config :rules (nreverse rules)
                   :disabled-rules disabled-rules
                   :path-rules (nreverse path-rules)
                   :ignore ignore-patterns))))

;;; Config file loading

(defun load-config (path &key preset-override)
  "Load configuration from file at PATH.
Sets root-dir to the directory containing the config file.
If PRESET-OVERRIDE is provided, it overrides the :extends clause in the config file."
  (let ((pathname (etypecase path
                    (string (uiop:parse-native-namestring path))
                    (pathname path))))
    (unless (probe-file pathname)
      (error "Config file not found: ~A" pathname))

    (with-open-file (in pathname :direction :input)
      (let* ((sexp (read in))
             (config (parse-config sexp :preset-override preset-override)))
        ;; Set root-dir to the directory containing the config file
        (setf (config-root-dir config) (uiop:pathname-directory-pathname pathname))
        config))))

;;; Rule selection

(defun get-rules-for-file (config file-path)
  "Get the list of rule instances that apply to FILE-PATH, filtering out disabled rules."
  (check-type config config)
  (check-type file-path pathname)

  (let* ((root-dir (or (config-root-dir config) #P"/"))
         (file-namestring (namestring file-path))
         ;; Convert file path to project-relative format (like in file-ignored-p)
         (match-path (let* ((root-namestring (namestring root-dir)))
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
  "Get a built-in configuration by NAME (:default or :all)."
  (check-type name keyword)

  (case name
    (:default
     (make-default-config))
    (:all
     (make-all-config))
    (otherwise
     (error "Unknown built-in config: ~A. Available: :default, :all" name))))

(defun make-default-config ()
  "Create the default configuration - only universally-accepted rules.
Enables rules that catch bugs or follow strong community conventions.
Style preferences are disabled to keep output clean."
  (let ((enabled-rules
          '(;; Universally accepted - keep enabled
            :trailing-whitespace
            :no-tabs
            :final-newline
            :wrong-otherwise
            :unused-variables
            :unused-local-functions
            :unused-local-nicknames
            :unused-imported-symbols
            :asdf-component-strings
            :mixed-optional-and-key
            :if-without-else))
        (disabled-rules
          '(;; Style preferences - disabled (too noisy, no consensus)
            :line-length
            :consecutive-blank-lines
            :bare-progn-in-if
            :missing-otherwise
            :constant-naming
            :special-variable-naming
            ;; LOOP variables - disabled
            :unused-loop-variables)))
    (make-config
     :rules (mapcar #'rules:make-rule enabled-rules)
     :disabled-rules disabled-rules)))

(defun make-all-config ()
  "Create configuration with all rules enabled.
Useful for exploration and discovering what rules exist."
  (let ((all-rules
          '(;; ERROR: Objectively wrong code
            :wrong-otherwise
            ;; WARNING: Likely bugs or dangerous patterns
            :unused-variables
            :unused-local-functions
            :missing-otherwise
            :mixed-optional-and-key
            ;; INFO: Code quality suggestions
            :unused-local-nicknames
            :unused-imported-symbols
            :constant-naming
            :unused-loop-variables
            ;; CONVENTION: Style/idiom suggestions
            :if-without-else
            :bare-progn-in-if
            :special-variable-naming
            :asdf-component-strings
            ;; FORMAT: Consensus formatting (Emacs/SLIME standards)
            :no-tabs
            :trailing-whitespace
            :final-newline
            ;; INFO: Subjective preferences
            :line-length
            :consecutive-blank-lines
            :function-length
            :cyclomatic-complexity)))
    (make-config
     :rules (mapcar (lambda (spec)
                      (if (consp spec)
                          (apply #'rules:make-rule spec)
                          (rules:make-rule spec)))
                    all-rules)
     :disabled-rules '())))  ; All rules enabled

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
         (match-path (let* ((file-namestring (namestring file-path))
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

(defun get-all-rules-by-severity (severity)
  "Get all rule instances with the given SEVERITY from the :all preset."
  (let ((all-config (make-all-config)))
    (remove-if-not (lambda (rule)
                     (eq (rules:rule-severity rule) severity))
                   (config-rules all-config))))

(defun apply-cli-overrides (config cli-rules)
  "Apply CLI rule overrides to CONFIG. Returns new config object.
CLI overrides have highest precedence.

CLI-RULES is a plist with:
  :enable-rules - List of (rule-name . options-plist)
  :disable-rules - List of rule-name keywords
  :enable-groups - List of severity keywords
  :disable-groups - List of severity keywords"
  (check-type config config)
  (check-type cli-rules list)

  (let* ((enable-rules (getf cli-rules :enable-rules))
         (disable-rules (getf cli-rules :disable-rules))
         (enable-groups (getf cli-rules :enable-groups))
         (disable-groups (getf cli-rules :disable-groups))
         (base-rules (config-rules config))
         (base-disabled (config-disabled-rules config))
         (result-rules '())
         (result-disabled (copy-list base-disabled)))

    ;; Step 1: Process each base rule against CLI overrides
    (dolist (rule base-rules)
      (let* ((severity (rules:rule-severity rule))
             (rule-name (rules:rule-name rule))
             (explicitly-enabled (assoc rule-name enable-rules))
             (explicitly-disabled (member rule-name disable-rules))
             (group-enabled (member severity enable-groups))
             (group-disabled (member severity disable-groups)))
        (cond
          ;; Explicitly enabled: recreate with new options (highest priority)
          (explicitly-enabled
           (let ((options (cdr explicitly-enabled)))
             (push (apply #'rules:make-rule rule-name options) result-rules)))

          ;; Explicitly disabled: add to disabled list
          (explicitly-disabled
           (pushnew rule-name result-disabled))

          ;; Group enabled: include rule, remove from disabled
          (group-enabled
           (push rule result-rules)
           (setf result-disabled (remove rule-name result-disabled)))

          ;; Group disabled: add to disabled list
          (group-disabled
           (pushnew rule-name result-disabled))

          ;; No CLI override: keep base rule state
          (t
           (unless (member rule-name base-disabled)
             (push rule result-rules))))))

    ;; Step 2: Add rules that were only in enable-rules (not in base config)
    (dolist (enable-spec enable-rules)
      (let ((rule-name (car enable-spec)))
        (unless (find rule-name base-rules :key #'rules:rule-name)
          (let ((options (cdr enable-spec)))
            (push (apply #'rules:make-rule rule-name options) result-rules)
            ;; Remove from disabled if it was there
            (setf result-disabled (remove rule-name result-disabled))))))

    ;; Step 3: Add rules from enabled groups that aren't in base config
    (dolist (severity enable-groups)
      (let ((group-rules (get-all-rules-by-severity severity)))
        (dolist (rule group-rules)
          (let ((rule-name (rules:rule-name rule)))
            (unless (or (find rule-name result-rules :key #'rules:rule-name)
                        (find rule-name base-rules :key #'rules:rule-name)
                        (assoc rule-name enable-rules))  ; Skip if explicitly enabled
              (push rule result-rules)
              (setf result-disabled (remove rule-name result-disabled)))))))

    ;; Step 4: Create new config with overridden rules
    (make-config
     :rules (nreverse result-rules)
     :disabled-rules result-disabled
     :path-rules (config-path-rules config)
     :ignore (config-ignore config)
     :root-dir (config-root-dir config))))

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
