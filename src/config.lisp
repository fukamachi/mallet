(defpackage #:malo/config
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:glob #:trivial-glob))
  (:export #:config
           #:make-config
           #:parse-config
           #:load-config
           #:merge-configs
           #:rule-enabled-p
           #:get-rule-option
           #:get-built-in-config
           #:find-config-file
           #:config-overrides
           #:apply-overrides-for-file
           #:*default-preset*))
(in-package #:malo/config)

(defvar *default-preset* :default)

;;; Config data structure

(defclass config ()
  ((rules
    :initarg :rules
    :initform (make-hash-table :test 'eq)
    :accessor config-rules
    :documentation "Hash table mapping rule names to rule options")
   (extends
    :initarg :extends
    :initform nil
    :accessor config-extends
    :documentation "Base config to extend from")
   (overrides
    :initarg :overrides
    :initform '()
    :accessor config-overrides
    :documentation "List of (patterns . config) for path-specific overrides"))
  (:documentation "Configuration for Malo linter."))

(defun make-config (&key rules extends overrides)
  "Create a new config with RULES, optional EXTENDS, and path OVERRIDES."
  (let ((cfg (make-instance 'config :extends extends :overrides (or overrides '()))))
    (when rules
      (dolist (rule-spec rules)
        (let ((rule-name (first rule-spec))
              (options (rest rule-spec)))
          (setf (gethash rule-name (config-rules cfg))
                (a:plist-hash-table options :test 'eq)))))
    cfg))

(defun expand-path-pattern (pattern)
  "Expand a path PATTERN, converting directory names to glob patterns.
If pattern contains wildcards (* ? [ ]), return as-is.
Otherwise, treat as directory and expand to 'dir/**/*.{lisp,asd}'."
  (check-type pattern string)
  (if (or (find #\* pattern)
          (find #\? pattern)
          (find #\[ pattern)
          (find #\] pattern))
      ;; Already a glob pattern
      pattern
      ;; Directory name - expand to match all Lisp files
      (format nil "~A/**/*.{lisp,asd}" pattern)))

(defun rule-enabled-p (config rule-name)
  "Check if RULE-NAME is enabled in CONFIG."
  (check-type config config)
  (check-type rule-name keyword)

  (let ((rule-options (gethash rule-name (config-rules config))))
    (if rule-options
        (gethash :enabled rule-options t)  ; Default to enabled if not specified
        ;; If rule not in config, check extends
        (if (config-extends config)
            (rule-enabled-p (config-extends config) rule-name)
            nil))))  ; Default to disabled if not in any config

(defun get-rule-option (config rule-name option-name)
  "Get OPTION-NAME for RULE-NAME from CONFIG."
  (check-type config config)
  (check-type rule-name keyword)
  (check-type option-name keyword)

  (let ((rule-options (gethash rule-name (config-rules config))))
    (if rule-options
        (gethash option-name rule-options)
        ;; If rule not in config, check extends
        (when (config-extends config)
          (get-rule-option (config-extends config) rule-name option-name)))))

;;; Config parsing

(defun parse-config (sexp)
  "Parse S-expression SEXP into a config object.
Uses new syntax: (:enable :rule-name ...), (:disable :rule-name), and (:for-paths ...)."
  (check-type sexp list)

  (unless (eq (first sexp) :malo-config)
    (error "Config must start with :malo-config"))

  (let ((extends nil)
        (rules '())
        (overrides '()))

    ;; Parse top-level options
    (loop for item in (rest sexp)
          when (consp item)
          do (let ((key (first item)))
               (case key
                 (:extends
                  (let ((extends-value (second item)))
                    (setf extends (if (keywordp extends-value)
                                      (get-built-in-config extends-value)
                                      (load-config extends-value)))))
                 (:enable
                  ;; New syntax: (:enable :rule-name :option value ...)
                  (let* ((rule-name (second item))
                         (options (cddr item)))
                    (push (cons rule-name (list* :enabled t options)) rules)))
                 (:disable
                  ;; New syntax: (:disable :rule-name)
                  (let ((rule-name (second item)))
                    (push (cons rule-name '(:enabled nil)) rules)))
                 (:for-paths
                  ;; Path-specific overrides: (:for-paths (pattern...) (:enable ...) (:disable ...))
                  (let* ((patterns (second item))
                         (override-rules (parse-override-rules (cddr item)))
                         (expanded-patterns (mapcar #'expand-path-pattern patterns)))
                    (push (cons expanded-patterns
                                (make-config :rules override-rules))
                          overrides))))))

    (make-config :rules (nreverse rules)
                 :extends extends
                 :overrides (nreverse overrides))))

(defun parse-override-rules (override-forms)
  "Parse override rules from (:enable ...) and (:disable ...) forms."
  (let ((rules '()))
    (dolist (form override-forms)
      (when (consp form)
        (let ((key (first form)))
          (case key
            (:enable
             (let* ((rule-name (second form))
                    (options (cddr form)))
               (push (cons rule-name (list* :enabled t options)) rules)))
            (:disable
             (let ((rule-name (second form)))
               (push (cons rule-name '(:enabled nil)) rules)))))))
    (nreverse rules)))

;;; Config file loading

(defun load-config (path)
  "Load configuration from file at PATH."
  (let ((pathname (etypecase path
                    (string (uiop:parse-native-namestring path))
                    (pathname path))))
    (unless (probe-file pathname)
      (error "Config file not found: ~A" pathname))

    (with-open-file (in pathname :direction :input)
      (let ((sexp (read in)))
        (parse-config sexp)))))

;;; Config merging

(defun merge-configs (base override)
  "Merge OVERRIDE config into BASE config, with OVERRIDE taking precedence."
  (check-type base config)
  (check-type override config)

  (let ((merged (make-instance 'config)))

    ;; Copy all rules from base
    (a:maphash-keys
     (lambda (rule-name)
       (let ((base-options (gethash rule-name (config-rules base))))
         (setf (gethash rule-name (config-rules merged))
               (a:copy-hash-table base-options))))
     (config-rules base))

    ;; Merge/override with rules from override config
    (a:maphash-keys
     (lambda (rule-name)
       (let ((override-options (gethash rule-name (config-rules override)))
             (existing-options (gethash rule-name (config-rules merged))))
         (if existing-options
             ;; Merge options
             (a:maphash-keys
              (lambda (option-name)
                (setf (gethash option-name existing-options)
                      (gethash option-name override-options)))
              override-options)
             ;; Add new rule
             (setf (gethash rule-name (config-rules merged))
                   (a:copy-hash-table override-options)))))
     (config-rules override))

    ;; Keep base's extends if override doesn't have one
    (unless (config-extends override)
      (setf (config-extends merged) (config-extends base)))

    merged))

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
  (make-config
   :rules '(;; Universally accepted - keep enabled
            (:trailing-whitespace :enabled t)
            (:no-tabs :enabled t)
            (:final-newline :enabled t)
            (:wrong-otherwise :enabled t)
            (:unused-variables :enabled t)
            (:unused-local-nicknames :enabled t)
            (:unused-imported-symbols :enabled t)
            (:asdf-component-strings :enabled t)
            (:mixed-optional-and-key :enabled t)
            (:if-without-else :enabled t)
            (:special-variable-naming :enabled t)
            ;; Style preferences - disable (too noisy, no consensus)
            (:line-length :enabled nil)
            (:consecutive-blank-lines :enabled nil)
            (:bare-progn-in-if :enabled nil)
            (:missing-otherwise :enabled nil)
            (:constant-naming :enabled nil)
            ;; LOOP variables - disable (idiomatic in CL, opt-in for strict checking)
            (:unused-loop-variables :enabled nil))))

(defun make-all-config ()
  "Create configuration with all rules enabled.
Useful for exploration and discovering what rules exist."
  (make-config
   :rules '(;; ERROR: Objectively wrong code
            (:wrong-otherwise :enabled t)
            ;; WARNING: Likely bugs or dangerous patterns
            (:unused-variables :enabled t)
            (:missing-otherwise :enabled t)
            (:mixed-optional-and-key :enabled t)
            ;; INFO: Code quality suggestions
            (:unused-local-nicknames :enabled t)
            (:unused-imported-symbols :enabled t)
            (:constant-naming :enabled t)
            (:unused-loop-variables :enabled t)
            ;; CONVENTION: Style/idiom suggestions
            (:if-without-else :enabled t)
            (:bare-progn-in-if :enabled t)
            (:special-variable-naming :enabled t)
            (:asdf-component-strings :enabled t)
            ;; FORMAT: Consensus formatting (Emacs/SLIME standards)
            (:no-tabs :enabled t)
            (:trailing-whitespace :enabled t)
            (:final-newline :enabled t)
            ;; INFO: Subjective preferences
            (:line-length :enabled t :max-length 100)
            (:consecutive-blank-lines :enabled t :max-consecutive 2))))

;;; Path-based overrides

(defun apply-overrides-for-file (config file-path &key root-dir)
  "Apply path-specific overrides from CONFIG for FILE-PATH.
Returns a new config with matching overrides merged in."
  (check-type config config)
  (check-type file-path pathname)
  (check-type root-dir (or null pathname))

  (let ((merged-config config)
        (file-namestring (namestring file-path)))
    ;; Process each override entry
    (dolist (override-entry (config-overrides config))
      (let ((patterns (car override-entry))
            (override-config (cdr override-entry)))
        ;; Check if any pattern matches the file
        (when (some (lambda (pattern)
                      (let ((pattern (namestring (merge-pathnames pattern root-dir))))
                        (glob:glob-match pattern file-namestring)))
                    patterns)
          ;; Merge this override into the accumulated config
          (setf merged-config (merge-configs merged-config override-config)))))
    merged-config))

;;; Config file discovery

(defun find-project-root (&optional (pathname *default-pathname-defaults*))
  (check-type pathname pathname)
  (let ((pathname
          (if (uiop:directory-pathname-p pathname)
              (probe-file pathname)
              (uiop:pathname-directory-pathname pathname))))
    (cond
      ((or (find-if (lambda (dir)
                      (uiop:directory-exists-p (merge-pathnames dir pathname)))
                    '(".bzr" ".git" ".hg" ".qlot"))
           (find-if (lambda (file)
                      (uiop:file-exists-p (merge-pathnames file pathname)))
                    '("qlfile" ".malo.lisp")))
       pathname)
      ((equal (user-homedir-pathname) pathname)
       nil)
      (t
       (let ((parent (uiop:pathname-parent-directory-pathname pathname)))
         (if (equal parent pathname)
             nil
             (find-project-root parent)))))))

(defun find-config-file (start-directory)
  "Find .malo.lisp config file starting from START-DIRECTORY.
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
           (merge-pathnames ".malo.lisp" project-root)))))))
