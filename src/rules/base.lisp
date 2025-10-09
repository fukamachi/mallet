(defpackage #:malo/rules/base
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:violation #:malo/violation))
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
           #:symbol-name-from-string
           #:symbol-matches-p
           #:coalton-form-p
           #:check-text
           #:check-tokens
           #:check-form))
(in-package #:malo/rules/base)

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
    :type (member :error :warning :convention :format :info)
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
  (check-type severity (member :error :warning :convention :format :info))
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

;;; Helper functions for string-based symbol handling

(defun symbol-name-from-string (str)
  "Extract the symbol name from a string representation.
Handles qualified symbols like \"PACKAGE:NAME\" → \"NAME\"
and unqualified symbols like \"NAME\" → \"NAME\"."
  (if (stringp str)
      (let ((colon-pos (position #\: str :from-end t)))
        (if colon-pos
            (subseq str (1+ colon-pos))
            str))
      str))

(defun symbol-matches-p (str name)
  "Check if string-based symbol STR matches NAME (case-insensitive).
Works with both qualified (\"PACKAGE:IF\") and unqualified (\"IF\") symbols."
  (and (stringp str)
       (string-equal (symbol-name-from-string str) name)))

(defun coalton-form-p (form)
  "Check if FORM is a Coalton toplevel form.
Coalton code is written in (coalton-toplevel ...) or (coalton:coalton-toplevel ...).
Form-level linting rules should skip Coalton forms as they have different semantics."
  (when form
    ;; Extract expression from form object (try form-expr accessor, fall back to form itself)
    (let ((expr (handler-case
                    (malo/parser:form-expr form)
                  (error () form))))
      (when (consp expr)
        (let* ((head (first expr))
               (symbol-name (cond
                              ((stringp head) (symbol-name-from-string head))
                              ((symbolp head) (symbol-name head))
                              (t nil))))
          (and symbol-name
               (string-equal symbol-name "coalton-toplevel")))))))

;;; Generic functions for rule checking

(defgeneric check-text (rule text file)
  (:documentation "Check TEXT from FILE using RULE.
Returns a list of VIOLATION objects."))

(defgeneric check-tokens (rule tokens file)
  (:documentation "Check TOKENS from FILE using RULE.
Returns a list of VIOLATION objects."))

(defgeneric check-form (rule form file)
  (:documentation "Check FORM from FILE using RULE.
Returns a list of VIOLATION objects."))

;; Skip Coalton forms for all form-level rules
;; Coalton has different semantics (variable scoping, control flow, etc.)
;; so Common Lisp linting rules don't apply
(defmethod check-form :around ((rule rule) form file)
  "Skip Coalton toplevel forms - they have different semantics from Common Lisp."
  (if (coalton-form-p form)
      nil  ; Return empty violations list for Coalton forms
      (call-next-method)))  ; Call the actual rule implementation
