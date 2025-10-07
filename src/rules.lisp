(defpackage #:malvolio/rules
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:violation #:malvolio/violation)
   (#:parser #:malvolio/parser))
  (:export #:rule
           #:rule-name
           #:rule-description
           #:rule-severity
           #:rule-type
           #:rule-enabled-p
           #:enable-rule
           #:disable-rule
           #:registry
           #:make-registry
           #:register-rule
           #:find-rule
           #:list-rules
           ;; Rule implementations
           #:line-length-rule
           #:comment-level-rule
           ;; Check functions
           #:check-text
           #:check-tokens))
(in-package #:malvolio/rules)

;;; Rule class

(defclass rule ()
  ((name
    :initarg :name
    :reader rule-name
    :type keyword
    :documentation "Rule identifier (keyword)")
   (description
    :initarg :description
    :reader rule-description
    :type string
    :documentation "Human-readable description")
   (severity
    :initarg :severity
    :reader rule-severity
    :type (member :error :warning :info)
    :documentation "Default severity level")
   (type
    :initarg :type
    :initform :form
    :reader rule-type
    :type (member :text :token :form :pattern)
    :documentation "Rule type determines when it runs")
   (enabled
    :initarg :enabled
    :initform t
    :accessor rule-enabled-p
    :type boolean
    :documentation "Whether the rule is enabled"))
  (:documentation "Base class for linting rules."))

(defun enable-rule (rule)
  "Enable RULE."
  (setf (rule-enabled-p rule) t))

(defun disable-rule (rule)
  "Disable RULE."
  (setf (rule-enabled-p rule) nil))

;;; Registry

(defclass registry ()
  ((rules
    :initform (make-hash-table :test 'eq)
    :accessor registry-rules
    :documentation "Hash table mapping rule names to rule objects"))
  (:documentation "Registry for managing linting rules."))

(defun make-registry ()
  "Create a new rule registry."
  (make-instance 'registry))

(defun register-rule (registry name &key description severity (type :form)
                                      (enabled t))
  "Register a new rule in REGISTRY with NAME and properties."
  (check-type registry registry)
  (check-type name keyword)
  (check-type description string)
  (check-type severity (member :error :warning :info))
  (check-type type (member :text :token :form :pattern))
  (check-type enabled boolean)

  (let ((rule (make-instance 'rule
                             :name name
                             :description description
                             :severity severity
                             :type type
                             :enabled enabled)))
    (setf (gethash name (registry-rules registry)) rule)
    rule))

(defun find-rule (registry name)
  "Find a rule by NAME in REGISTRY, returning NIL if not found."
  (check-type registry registry)
  (check-type name keyword)
  (gethash name (registry-rules registry)))

(defun list-rules (registry)
  "List all rules in REGISTRY."
  (check-type registry registry)
  (a:hash-table-values (registry-rules registry)))

;;; Generic functions for rule checking

(defgeneric check-text (rule text file)
  (:documentation "Check TEXT from FILE using RULE.
Returns a list of VIOLATION objects."))

(defgeneric check-tokens (rule tokens file)
  (:documentation "Check TOKENS from FILE using RULE.
Returns a list of VIOLATION objects."))

;;; Line length rule

(defclass line-length-rule (rule)
  ((max-length
    :initarg :max-length
    :initform 80
    :reader line-length-rule-max-length
    :type (integer 1)
    :documentation "Maximum allowed line length"))
  (:default-initargs
   :name :line-length
   :description "Lines should not exceed the maximum length"
   :severity :warning
   :type :text)
  (:documentation "Rule to check line length limits."))

(defmethod check-text ((rule line-length-rule) text file)
  "Check that lines in TEXT do not exceed MAX-LENGTH."
  (check-type text string)
  (check-type file pathname)

  (let ((violations '())
        (max-length (line-length-rule-max-length rule))
        (line-number 0))

    (with-input-from-string (stream text)
      (loop for line = (read-line stream nil nil)
            while line do
              (incf line-number)
              (let ((line-len (length line)))
                (when (> line-len max-length)
                  (push (make-instance 'violation:violation
                                       :rule :line-length
                                       :file file
                                       :line line-number
                                       :column 0
                                       :severity (rule-severity rule)
                                       :message (format nil "Line exceeds maximum length of ~A (~A characters)"
                                                       max-length line-len)
                                       :fix nil)
                        violations)))))

    (nreverse violations)))

;;; Comment level rule

(defclass comment-level-rule (rule)
  ()
  (:default-initargs
   :name :comment-level
   :description "Comments should use the appropriate number of semicolons"
   :severity :warning
   :type :token)
  (:documentation "Rule to check comment level conventions."))

(defmethod check-tokens ((rule comment-level-rule) tokens file)
  "Check that comments use the appropriate number of semicolons."
  (check-type tokens list)
  (check-type file pathname)

  (let ((violations '())
        (paren-depth 0)
        (prev-token nil))

    (dolist (token tokens)
      (let ((token-type (parser:token-type token)))
        (cond
          ;; Track parenthesis depth
          ((eq token-type :open-paren)
           (incf paren-depth))
          ((eq token-type :close-paren)
           (decf paren-depth))

          ;; Check comment tokens
          ((member token-type '(:comment-inline :comment-line
                                :comment-section :comment-file))
           (let* ((line (parser:token-line token))
                  (column (parser:token-column token))
                  (has-code-before (and prev-token
                                       (not (member (parser:token-type prev-token)
                                                   '(:comment-inline :comment-line
                                                     :comment-section :comment-file)))
                                       (= (parser:token-line prev-token) line)))
                  (expected-type
                   (cond
                     ;; Inline comment - code before it on same line
                     (has-code-before :comment-inline)
                     ;; Line comment - inside parens
                     ((> paren-depth 0) :comment-line)
                     ;; Top-level comment - outside parens (3 or 4 semicolons both OK)
                     (t :comment-section-or-file))))

             ;; Check if comment type matches expected context
             (unless (case expected-type
                       (:comment-inline (eq token-type :comment-inline))
                       (:comment-line (eq token-type :comment-line))
                       (:comment-section-or-file (member token-type
                                                        '(:comment-section :comment-file))))
               (let ((expected-semicolons
                      (case expected-type
                        (:comment-inline 1)
                        (:comment-line 2)
                        (:comment-section-or-file "3 or 4")))
                     (actual-semicolons
                      (case token-type
                        (:comment-inline 1)
                        (:comment-line 2)
                        (:comment-section 3)
                        (:comment-file 4))))
                 (push (make-instance 'violation:violation
                                      :rule :comment-level
                                      :file file
                                      :line line
                                      :column column
                                      :severity (rule-severity rule)
                                      :message (format nil "Comment should use ~A semicolon~:P (found ~A)"
                                                      expected-semicolons actual-semicolons)
                                      :fix nil)
                       violations))))))

        (setf prev-token token)))

    (nreverse violations)))
