(defpackage #:mallet/rules/forms/coalton-to-boolean
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation))
  (:export #:coalton-missing-to-boolean-rule))
(in-package #:mallet/rules/forms/coalton-to-boolean)

;;; coalton-missing-to-boolean rule

(defclass coalton-missing-to-boolean-rule (base:coalton-rule)
  ()
  (:default-initargs
   :name :coalton-missing-to-boolean
   :description "Use to-boolean when (lisp Boolean ...) returns a Lisp boolean value"
   :severity :warning
   :category :correctness
   :type :form)
  (:documentation "Warn when a (lisp Boolean (vars) body) form does not use to-boolean.
In Coalton, (lisp Boolean ...) is an FFI call that must return a Coalton Boolean.
Returning a raw Lisp T/NIL without wrapping with to-boolean causes a type mismatch."))

;;; Helpers

(defun coalton-symbol-name-matches-p (sym name)
  "Return T if SYM (a string or symbol) has symbol-name NAME (case-insensitive).
Handles both string-based symbols (from the parser) and actual Lisp symbols.
Returns NIL for any other type (e.g., cons cells, numbers)."
  (let ((sym-name (typecase sym
                    (string (base:symbol-name-from-string sym))
                    (symbol (symbol-name sym))
                    (otherwise nil))))
    (and sym-name (string-equal sym-name name))))

(defun coalton-boolean-literal-p (expr)
  "Return T if EXPR is the Coalton Boolean constructor True or False."
  (or (coalton-symbol-name-matches-p expr "TRUE")
      (coalton-symbol-name-matches-p expr "FALSE")))

(defun body-is-coalton-boolean-literal-p (body-exprs)
  "Return T if BODY-EXPRS is exactly one Coalton Boolean literal (True or False).
These values are already Coalton Booleans and don't need to-boolean."
  (and (consp body-exprs)
       (null (cdr body-exprs))
       (coalton-boolean-literal-p (first body-exprs))))

(defun contains-to-boolean-p (expr)
  "Return T if EXPR contains any reference to to-boolean (any package qualification)."
  (let ((found nil))
    (base:traverse-expr expr
      (lambda (e)
        (when (and (not found)
                   (coalton-symbol-name-matches-p e "TO-BOOLEAN"))
          (setf found t))))
    found))

(defun body-uses-to-boolean-p (body-exprs)
  "Return T if any expression in BODY-EXPRS contains to-boolean anywhere."
  (some #'contains-to-boolean-p body-exprs))

;;; Rule implementation

(defmethod base:check-form ((rule coalton-missing-to-boolean-rule) form file)
  "Check the coalton-toplevel form for (lisp Boolean ...) without to-boolean."
  (check-type form parser:form)
  (check-type file pathname)
  (base:check-form-recursive rule
                             (parser:form-expr form)
                             file
                             (parser:form-line form)
                             (parser:form-column form)
                             nil
                             (parser:form-position-map form)))

(defmethod base:check-form-recursive ((rule coalton-missing-to-boolean-rule) expr file line column
                                     &optional function-name position-map)
  "Recursively search for (lisp Boolean (vars) body) without to-boolean in EXPR."
  (declare (ignore function-name))
  (let ((violations '())
        (visited (make-hash-table :test 'eq)))

    (labels ((check-expr (current-expr fallback-line fallback-column)
               (base:with-safe-code-expr (current-expr visited)
                 (multiple-value-bind (actual-line actual-column)
                     (base:find-actual-position current-expr position-map fallback-line fallback-column)
                   (let ((head (first current-expr))
                         (rest-args (rest current-expr)))
                     ;; Detect (lisp Boolean (vars...) body...) pattern
                     (when (coalton-symbol-name-matches-p head "LISP")
                       (let ((type-arg (first rest-args))
                             (body-exprs (cddr rest-args))) ; skip type + vars
                         ;; Only flag when:
                         ;; - type is a plain symbol (not a composite like (Optional Boolean))
                         ;; - type name is exactly "BOOLEAN"
                         ;; - body is non-empty
                         ;; - body is not a Coalton Boolean literal (True/False)
                         ;; - body does not contain to-boolean
                         (when (and (not (consp type-arg))
                                    (coalton-symbol-name-matches-p type-arg "BOOLEAN")
                                    body-exprs
                                    (not (body-is-coalton-boolean-literal-p body-exprs))
                                    (not (body-uses-to-boolean-p body-exprs))
                                    (base:should-create-violation-p rule))
                           (push (make-instance 'violation:violation
                                                :rule :coalton-missing-to-boolean
                                                :file file
                                                :line actual-line
                                                :column actual-column
                                                :severity (base:rule-severity rule)
                                                :message
                                                "Use to-boolean to convert a Lisp boolean to a Coalton Boolean")
                                 violations))))

                     ;; Recurse into all subexpressions to find nested lisp forms.
                     ;; Must recurse into both head and rest-args since lisp forms can
                     ;; appear in any position (e.g., inside let binding lists).
                     (a:nconcf violations
                               (base:collect-violations-from-subexprs rule head file
                                                                      actual-line actual-column
                                                                      position-map))
                     (a:nconcf violations
                               (base:collect-violations-from-subexprs rule rest-args file
                                                                      actual-line actual-column
                                                                      position-map)))))))
      (check-expr expr line column))
    violations))
