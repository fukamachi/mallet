(defpackage #:mallet/rules/base
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:utils #:mallet/utils)
   (#:violation #:mallet/violation)
   (#:suppression #:mallet/suppression))
  (:import-from #:mallet/utils
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
           #:symbol-name-from-string
           #:symbol-matches-p
           #:coalton-form-p
           #:check-text
           #:check-tokens
           #:check-form
           #:check-form-recursive
           #:traverse-expr
           #:with-safe-cons-expr
           #:should-create-violation-p))
(in-package #:mallet/rules/base)

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
                    (mallet/parser:form-expr form)
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

;;; Recursive form checking with suppression support

(defgeneric check-form-recursive (rule expr file line column &optional function-name position-map)
  (:documentation "Recursively check an expression and its nested forms.
This generic function is provided for rules to use when they need to recursively check nested forms.
It automatically handles declare-based suppressions through its :around method.

RULE - The rule doing the checking
EXPR - The expression to check (not a form object, but the actual list expression)
FILE - The file being checked
LINE - Line number of the expression
COLUMN - Column number of the expression
FUNCTION-NAME - Optional function name context for function-specific suppressions
POSITION-MAP - Optional position map for looking up exact positions of subexpressions

Rules should call this method when recursing into nested forms to ensure suppressions are handled."))

(defmethod check-form-recursive :around ((rule rule) expr file line column &optional function-name position-map)
  "Handle declare-based suppressions automatically for recursive checking.
This :around method checks for (declare (mallet:suppress ...)) forms and manages the suppression scope."
  (declare (ignore position-map))
  ;; Use dynamic lookup to avoid circular dependency with engine package
  ;; The *suppression-state* special variable is bound by the engine during linting
  (let ((state-symbol (find-symbol "*SUPPRESSION-STATE*" "MALLET/ENGINE")))
    (if (and state-symbol (boundp state-symbol) (symbol-value state-symbol))
        (let ((state (symbol-value state-symbol))
              (declare-suppressions (extract-declare-suppressions expr)))
          ;; First, push any declare suppressions from this form onto the scope
          (when declare-suppressions
            (suppression:push-scope-suppression state declare-suppressions))

          ;; Now check if rule is suppressed at current scope (includes newly pushed suppressions)
          (unwind-protect
              (if (suppression:rule-suppressed-p state (rule-name rule)
                                                  :form-type (if function-name :function-body :lexical-scope)
                                                  :function-name function-name)
                  nil  ; Return empty violations list if suppressed
                  (call-next-method))  ; Not suppressed, proceed with checking
            ;; Always pop the suppression scope if we pushed one
            (when declare-suppressions
              (suppression:pop-scope-suppression state))))
        ;; No suppression state bound (e.g., in unit tests), just call method
        (call-next-method))))

(defun extract-declare-suppressions (expr)
  "Extract mallet:suppress declarations from the body of EXPR.
Returns a list of rule keywords to suppress, or NIL if no suppressions found.

Handles forms like:
  (let (...)
    (declare (mallet:suppress rule1 rule2))
    ...)
  (locally
    (declare (mallet:suppress rule1))
    ...)"
  (when (and (consp expr) (consp (cdr expr)))
    (let ((form-head (first expr))
          (form-body (find-form-body expr)))
      ;; Check if this form can have declarations
      (when (and form-body
                 (or (and (stringp form-head)
                          (member (symbol-name-from-string form-head)
                                  '("LET" "LET*" "FLET" "LABELS" "LOCALLY"
                                    "LAMBDA" "DEFUN" "DEFMETHOD" "DEFMACRO"
                                    "WITH-OPEN-FILE" "WITH-OUTPUT-TO-STRING"
                                    "WITH-INPUT-FROM-STRING" "HANDLER-BIND"
                                    "HANDLER-CASE" "RESTART-BIND" "RESTART-CASE")
                                  :test #'string-equal))
                     (and (symbolp form-head)
                          (member (symbol-name form-head)
                                  '("LET" "LET*" "FLET" "LABELS" "LOCALLY"
                                    "LAMBDA" "DEFUN" "DEFMETHOD" "DEFMACRO"
                                    "WITH-OPEN-FILE" "WITH-OUTPUT-TO-STRING"
                                    "WITH-INPUT-FROM-STRING" "HANDLER-BIND"
                                    "HANDLER-CASE" "RESTART-BIND" "RESTART-CASE")
                                  :test #'string-equal))))
        ;; Look for declare forms at the beginning of the body
        (loop for form in form-body
              while (and (consp form)
                         (or (eq (first form) 'declare)
                             (and (stringp (first form))
                                  (string-equal (symbol-name-from-string (first form))
                                                "DECLARE"))))
              append (loop for declaration in (rest form)
                           when (and (consp declaration)
                                     ;; Check for mallet:suppress
                                     (let ((decl-head (first declaration)))
                                       (or (and (symbolp decl-head)
                                                (eq (symbol-package decl-head)
                                                    (find-package "MALLET"))
                                                (string-equal (symbol-name decl-head)
                                                              "SUPPRESS"))
                                           (and (stringp decl-head)
                                                (search "MALLET:suppress" decl-head
                                                        :test #'char-equal)))))
                           append (mapcar (lambda (rule)
                                            (cond
                                              ((keywordp rule) rule)
                                              ((symbolp rule)
                                               (intern (symbol-name rule) :keyword))
                                              ((stringp rule)
                                               ;; Strip package prefix first, then intern as keyword
                                               (let ((name (string-upcase (symbol-name-from-string rule))))
                                                 (intern (if (and (> (length name) 0)
                                                                  (char= (char name 0) #\:))
                                                             (subseq name 1)
                                                             name)
                                                         :keyword)))
                                              (t rule)))
                                          (rest declaration))))))))

(defun find-form-body (expr)
  "Find the body of a form that may contain declarations.
Returns the body portion where declarations and code would be, or NIL if not applicable."
  (when (consp expr)
    (let ((form-head (first expr)))
      (cond
        ;; LET, LET* - skip bindings, return rest
        ((or (symbol-matches-p form-head "LET")
             (symbol-matches-p form-head "LET*"))
         (when (consp (cdr expr))
           (cddr expr)))  ; Skip bindings

        ;; FLET, LABELS - skip function definitions, return rest
        ((or (symbol-matches-p form-head "FLET")
             (symbol-matches-p form-head "LABELS"))
         (when (consp (cdr expr))
           (cddr expr)))  ; Skip function definitions

        ;; LOCALLY - body starts immediately
        ((symbol-matches-p form-head "LOCALLY")
         (cdr expr))

        ;; LAMBDA - skip lambda list, return rest
        ((symbol-matches-p form-head "LAMBDA")
         (when (consp (cdr expr))
           (cddr expr)))  ; Skip lambda list

        ;; DEFUN, DEFMETHOD, DEFMACRO - skip name and lambda list
        ((or (symbol-matches-p form-head "DEFUN")
             (symbol-matches-p form-head "DEFMACRO"))
         (when (and (consp (cdr expr)) (consp (cddr expr)))
           (cdddr expr)))  ; Skip name and lambda list

        ((symbol-matches-p form-head "DEFMETHOD")
         ;; DEFMETHOD is more complex - may have qualifiers
         (let ((rest (cdr expr)))
           (when rest
             ;; Skip method name
             (setf rest (cdr rest))
             ;; Skip qualifiers (symbols before lambda list)
             (loop while (and rest (not (listp (car rest))))
                   do (setf rest (cdr rest)))
             ;; Skip lambda list
             (when (consp rest)
               (cdr rest)))))

        ;; Various WITH- forms
        ((or (symbol-matches-p form-head "WITH-OPEN-FILE")
             (symbol-matches-p form-head "WITH-OUTPUT-TO-STRING")
             (symbol-matches-p form-head "WITH-INPUT-FROM-STRING"))
         (when (consp (cdr expr))
           (cddr expr)))  ; Skip binding form

        ;; HANDLER-BIND, HANDLER-CASE, RESTART-BIND, RESTART-CASE
        ((or (symbol-matches-p form-head "HANDLER-BIND")
             (symbol-matches-p form-head "RESTART-BIND"))
         (when (consp (cdr expr))
           (cddr expr)))  ; Skip handler/restart bindings

        ((or (symbol-matches-p form-head "HANDLER-CASE")
             (symbol-matches-p form-head "RESTART-CASE"))
         (when (consp (cdr expr))
           ;; For these, the protected form is second, clauses follow
           ;; We only check the protected form for declarations
           (list (cadr expr))))

        ;; Default - no known body structure
        (t nil)))))

;;; Suppression checking helper

(defun should-create-violation-p (rule)
  "Check if a violation should be created for RULE based on current suppression state.
Returns T if violation should be created, NIL if rule is currently suppressed.

This function checks the dynamic *SUPPRESSION-STATE* variable and determines
if the rule is suppressed at the current scope."
  ;; Use dynamic lookup to avoid circular dependency
  (let ((state-symbol (find-symbol "*SUPPRESSION-STATE*" "MALLET/ENGINE")))
    (if (and state-symbol (boundp state-symbol) (symbol-value state-symbol))
        ;; Check if rule is suppressed
        (not (funcall (find-symbol "RULE-SUPPRESSED-P" "MALLET/SUPPRESSION")
                      (symbol-value state-symbol)
                      (rule-name rule)
                      :form-type :lexical-scope))
        ;; No suppression state (e.g., in unit tests), allow violation
        t)))

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
