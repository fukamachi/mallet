(defpackage #:mallet/rules/forms/error-usage
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation))
  (:export #:error-with-string-only-rule))
(in-package #:mallet/rules/forms/error-usage)

;;; Error-With-String-Only Rule
;;;
;;; Detects calls to `error' where the first argument is a string literal,
;;; e.g. (error "something went wrong").
;;;
;;; Prefer signaling a named condition type:
;;;   (define-condition my-error (error) ())
;;;   (error 'my-error)
;;;
;;; Using a named condition allows callers to handle the error specifically
;;; and provides structured error information.

(defclass error-with-string-only-rule (base:rule)
  ()
  (:default-initargs
   :name :error-with-string-only
   :description "Prefer signaling a condition type over (error \"string\")"
   :severity :warning
   :category :practice
   :type :form)
  (:documentation "Rule to detect (error \"format-string\" ...) calls.
Using a bare string as the first argument to error misses the opportunity
to define a structured condition type that callers can handle specifically.
Prefer (error 'my-error-type ...) or define a condition class."))

(defun error-call-head-p (head)
  "Return T if HEAD (parser string symbol or CL symbol) is a call to CL:ERROR."
  (typecase head
    (string
     ;; Parser represents symbols as strings with package prefix.
     ;; error appears as \"CURRENT:error\" (unqualified) or \"cl:error\" (qualified)
     (base:symbol-matches-p head "ERROR"))
    (symbol
     ;; Directly interned CL symbol (rare but possible from reader macros)
     (and (string-equal (symbol-name head) "ERROR")
          (let ((pkg (symbol-package head)))
            (and pkg (member (package-name pkg) '("COMMON-LISP" "CL")
                             :test #'string-equal)))))
    (otherwise nil)))

(defmethod base:check-form ((rule error-with-string-only-rule) form file)
  "Check FORM from FILE for (error \"string\") calls."
  (check-type form parser:form)
  (check-type file pathname)
  (base:check-form-recursive rule
                             (parser:form-expr form)
                             file
                             (parser:form-line form)
                             (parser:form-column form)
                             nil
                             (parser:form-position-map form)))

(defmethod base:check-form-recursive
    ((rule error-with-string-only-rule) expr file line column
     &optional function-name position-map)
  "Recursively check EXPR for (error \"string\") patterns."
  (declare (ignore function-name))
  (let ((violations '())
        (visited (make-hash-table :test 'eq)))

    (labels ((check-expr (current-expr fallback-line fallback-column)
               (base:with-safe-code-expr (current-expr visited)
                 (multiple-value-bind (actual-line actual-column)
                     (base:find-actual-position
                      current-expr position-map fallback-line fallback-column)
                   (let ((head (first current-expr))
                         (rest-args (rest current-expr)))

                     ;; Check for (error "string" ...) pattern:
                     ;; head must be CL:ERROR and first arg must be a string literal.
                     ;;
                     ;; In the parsed form, symbols are represented as strings with
                     ;; a colon separator (e.g. "CURRENT:foo", ":keyword", "pkg:sym").
                     ;; True string literals never contain a colon from the parser
                     ;; perspective.  We exclude anything with a colon to avoid
                     ;; false-positives on symbol arguments.
                     (when (and (error-call-head-p head)
                                (consp rest-args)
                                (let ((arg (first rest-args)))
                                  (and (stringp arg)
                                       (not (find #\: arg))))
                                (base:should-create-violation-p rule))
                       (push (make-instance 'violation:violation
                                            :rule :error-with-string-only
                                            :file file
                                            :line actual-line
                                            :column actual-column
                                            :severity (base:rule-severity rule)
                                            :message "Prefer signaling a condition type: (error 'my-error) instead of (error \"string\")"
                                            :fix nil)
                             violations))

                     ;; Recurse into subexpressions
                     (a:nconcf violations
                               (base:collect-violations-from-subexprs
                                rule head file actual-line actual-column position-map))
                     (a:nconcf violations
                               (base:collect-violations-from-subexprs
                                rule rest-args file actual-line actual-column
                                position-map)))))))

      (check-expr expr line column))

    violations))
