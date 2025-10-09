(defpackage #:malvolio/config
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria))
  (:export #:config
           #:make-config
           #:parse-config
           #:load-config
           #:merge-configs
           #:rule-enabled-p
           #:get-rule-option
           #:get-built-in-config
           #:find-config-file))
(in-package #:malvolio/config)

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
    :documentation "Base config to extend from"))
  (:documentation "Configuration for Malvolio linter."))

(defun make-config (&key rules extends)
  "Create a new config with RULES and optional EXTENDS."
  (let ((cfg (make-instance 'config :extends extends)))
    (when rules
      (dolist (rule-spec rules)
        (let ((rule-name (first rule-spec))
              (options (rest rule-spec)))
          (setf (gethash rule-name (config-rules cfg))
                (a:plist-hash-table options :test 'eq)))))
    cfg))

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
            t))))  ; Default to enabled if not in any config

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
  "Parse S-expression SEXP into a config object."
  (check-type sexp list)

  (unless (eq (first sexp) :malvolio-config)
    (error "Config must start with :malvolio-config"))

  (let ((extends nil)
        (rules '()))

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
                 (:rules
                  (setf rules (parse-rules-spec (rest item)))))))

    (make-config :rules rules :extends extends)))

(defun parse-rules-spec (rules-sexp)
  "Parse rules specification from S-expression."
  (let ((rules '()))
    ;; rules-sexp is a list of (rule-name key value key value ...)
    (loop for rule-spec in rules-sexp
          when (consp rule-spec)
          do (let ((rule-name (first rule-spec))
                   (options (rest rule-spec)))
               (push (cons rule-name options) rules)))
    (nreverse rules)))

;;; Config file loading

(defun load-config (path)
  "Load configuration from file at PATH."
  (check-type path (or string pathname))

  (let ((pathname (uiop:parse-native-namestring path)))
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

(defun get-built-in-config (name)
  "Get a built-in configuration by NAME (:default, :all, :google)."
  (check-type name keyword)

  (case name
    (:default
     (make-default-config))
    (:all
     (make-all-config))
    (:google
     (make-google-config))
    (otherwise
     (error "Unknown built-in config: ~A. Available: :default, :all, :google" name))))

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
            ;; Style preferences - disable (too noisy, no consensus)
            (:line-length :enabled nil)
            (:consecutive-blank-lines :enabled nil)
            (:if-without-else :enabled nil)
            (:bare-progn-in-if :enabled nil)
            (:missing-otherwise :enabled nil))))

(defun make-all-config ()
  "Create configuration with all rules enabled.
Useful for exploration and discovering what rules exist."
  (make-config
   :rules '((:line-length :enabled t :max-length 100)
            (:trailing-whitespace :enabled t)
            (:no-tabs :enabled t)
            (:final-newline :enabled t)
            (:consecutive-blank-lines :enabled t :max-consecutive 2)
            (:if-without-else :enabled t)
            (:bare-progn-in-if :enabled t)
            (:missing-otherwise :enabled t)
            (:wrong-otherwise :enabled t)
            (:unused-variables :enabled t))))

(defun make-google-config ()
  "Create configuration based on Google Common Lisp Style Guide.
Uses 100 character line length per Google's guidelines."
  (make-config
   :rules '((:line-length :enabled t :max-length 100)
            (:trailing-whitespace :enabled t)
            (:no-tabs :enabled t)
            (:final-newline :enabled t)
            (:consecutive-blank-lines :enabled t :max-consecutive 2)
            (:if-without-else :enabled t)
            (:bare-progn-in-if :enabled t)
            (:missing-otherwise :enabled t)
            (:wrong-otherwise :enabled t)
            (:unused-variables :enabled t))))

;;; Config file discovery

(defun find-config-file (start-directory)
  "Find .malvolio.lisp config file starting from START-DIRECTORY.
Walks up parent directories until found or reaches root."
  (let ((dir (if (pathnamep start-directory)
                 (uiop:ensure-directory-pathname start-directory)
                 (uiop:ensure-directory-pathname
                  (uiop:parse-native-namestring start-directory)))))
    (labels ((search-dir (current-dir)
               (let ((config-path (merge-pathnames ".malvolio.lisp" current-dir)))
                 (cond
                   ((probe-file config-path)
                    config-path)
                   ;; Check if we've reached root
                   ((equal current-dir (uiop:pathname-parent-directory-pathname current-dir))
                    nil)
                   (t
                    ;; Try parent directory
                    (search-dir (uiop:pathname-parent-directory-pathname current-dir)))))))
      (search-dir dir))))
