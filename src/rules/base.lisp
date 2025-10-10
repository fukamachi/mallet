(defpackage #:malo/rules/base
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:utils #:malo/utils)
   (#:violation #:malo/violation))
  (:import-from #:malo/utils
                #:symbol-name-from-string)
  (:export #:rule
           #:rule-name
           #:rule-description
           #:rule-severity
           #:rule-type
           #:rule-enabled-p
           #:rule-file-types
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
           #:check-form
           #:traverse-expr
           #:with-safe-cons-expr))
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
    :documentation "Whether the rule is enabled")
   (file-types
    :initarg :file-types
    :initform '(:lisp)
    :reader rule-file-types
    :type list
    :documentation "List of file extensions this rule applies to (e.g., :lisp, :asd, :coal)"))
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
                                      (enabled t) (file-types '(:lisp)))
  "Register a new rule in REGISTRY with NAME and properties."
  (check-type registry registry)
  (check-type name keyword)
  (check-type description string)
  (check-type severity (member :error :warning :convention :format :info))
  (check-type type (member :text :token :form :pattern))
  (check-type enabled boolean)
  (check-type file-types list)

  (let ((rule (make-instance 'rule
                             :name name
                             :description description
                             :severity severity
                             :type type
                             :enabled enabled
                             :file-types file-types)))
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

;;; Generic traversal with cycle detection

(defun traverse-expr (expr function &key (traverse-lists t))
  "Traverse EXPR, calling FUNCTION on each sub-expression with cycle detection.

FUNCTION is called with each expression encountered during traversal.
The traversal automatically handles circular structures by tracking visited cons cells.

If TRAVERSE-LISTS is T (default), recursively traverses both CAR and CDR of cons cells.
If TRAVERSE-LISTS is NIL, only traverses CAR of proper lists (useful for walking code).

Example usage:
  ;; Collect all strings in an expression
  (let ((strings '()))
    (traverse-expr expr
      (lambda (e) (when (stringp e) (push e strings))))
    strings)

  ;; Check if any subexpression matches a pattern
  (let ((found nil))
    (traverse-expr expr
      (lambda (e) (when (my-pattern-p e) (setf found t))))
    found)"
  (let ((visited (make-hash-table :test 'eq)))
    (labels ((traverse (e)
               (cond
                 ;; Atoms - call function on them
                 ((not (consp e))
                  (funcall function e))
                 ;; Cons cells - check for cycles and recurse
                 (t
                  (unless (gethash e visited)
                    (setf (gethash e visited) t)
                    ;; Call function on the cons cell itself
                    (funcall function e)
                    ;; Traverse car
                    (traverse (car e))
                    ;; Traverse cdr based on mode
                    (if traverse-lists
                        ;; Full tree traversal mode - traverse any cdr
                        (traverse (cdr e))
                        ;; List traversal mode - only traverse cdr if it's a cons (proper list)
                        (when (consp (cdr e))
                          (traverse (cdr e)))))))))
      (traverse expr)
      nil)))

;;; Safe expression checking with cycle detection

(defmacro with-safe-cons-expr ((expr visited) &body body)
  "Execute BODY only if EXPR is safe to check (cons, proper list, not visited).
Automatically marks EXPR as visited before executing BODY.

This macro handles three common safety checks for recursive expression analysis:
1. EXPR must be a cons cell (not an atom)
2. EXPR must be a proper list (not a malformed dotted pair from parse errors)
3. EXPR must not have been visited yet (prevents infinite loops on circular structures)

Example usage:
  (let ((visited (make-hash-table :test 'eq)))
    (labels ((check-expr (expr)
               (with-safe-cons-expr (expr visited)
                 ;; Your rule-specific logic here
                 (when (some-pattern-p expr)
                   (report-violation))
                 ;; Recurse on subexpressions
                 (dolist (subexpr (rest expr))
                   (check-expr subexpr)))))
      (check-expr my-form)))"
  (let ((expr-var (gensym "EXPR")))
    `(let ((,expr-var ,expr))
       (when (and (consp ,expr-var)
                  (a:proper-list-p ,expr-var)
                  (not (gethash ,expr-var ,visited)))
         (setf (gethash ,expr-var ,visited) t)
         ,@body))))
