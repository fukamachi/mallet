(defpackage #:mallet/rules/forms/error-usage
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation))
  (:export #:error-without-custom-condition-rule))
(in-package #:mallet/rules/forms/error-usage)

;;; Error-Without-Custom-Condition Rule
;;;
;;; Detects calls to `error' where the first argument is either:
;;;   1. A string literal, e.g. (error "something went wrong")
;;;   2. A quoted CL built-in condition, e.g. (error 'cl:simple-error ...)
;;;      or (error 'simple-error ...)
;;;
;;; The fix is to define a custom condition and signal it:
;;;   (define-condition my-error (error)
;;;     ((message :initarg :message :reader my-error-message)))
;;;   (error 'my-error :message "something went wrong")
;;;
;;; This allows callers to handle the error specifically via handler-case.
;;;
;;; NOTE: Using (error 'cl:simple-error ...) is NOT a valid fix —
;;; it is semantically equivalent to the string form and does not give callers
;;; a specific condition type to catch with handler-case.

(defparameter *cl-condition-names*
  '(;; Root condition hierarchy
    "CONDITION" "SERIOUS-CONDITION" "ERROR" "WARNING" "SIMPLE-CONDITION"
    ;; Simple variants
    "SIMPLE-ERROR" "SIMPLE-WARNING" "SIMPLE-TYPE-ERROR"
    ;; Program errors
    "PROGRAM-ERROR" "CONTROL-ERROR" "PACKAGE-ERROR"
    ;; Type errors
    "TYPE-ERROR"
    ;; Cell errors (unbound variable / undefined function)
    "CELL-ERROR" "UNBOUND-VARIABLE" "UNDEFINED-FUNCTION"
    ;; Arithmetic errors
    "ARITHMETIC-ERROR" "DIVISION-BY-ZERO"
    "FLOATING-POINT-OVERFLOW" "FLOATING-POINT-UNDERFLOW"
    "FLOATING-POINT-INEXACT" "FLOATING-POINT-INVALID-OPERATION"
    ;; Stream / IO errors
    "STREAM-ERROR" "END-OF-FILE" "FILE-ERROR" "READER-ERROR"
    ;; Other standard conditions
    "PARSE-ERROR" "PRINT-NOT-READABLE" "STORAGE-CONDITION" "STYLE-WARNING")
  "Known CL built-in condition names (uppercase). Used to flag unqualified references.")

(defclass error-without-custom-condition-rule (base:rule)
  ()
  (:default-initargs
   :name :error-without-custom-condition
   :description "Prefer signaling a custom condition over built-in CL conditions or strings"
   :severity :warning
   :category :practice
   :type :form)
  (:documentation "Rule to detect (error ...) calls that use a string or CL built-in condition.
Using a string literal or a built-in like cl:simple-error misses the opportunity
to define a structured condition type that callers can handle specifically
via handler-case. The correct fix is to define a custom condition with
define-condition and signal it with (error 'my-condition ...).
CL built-in condition types like simple-error, type-error, etc. are not
a valid fix because callers cannot distinguish them from any other error."))

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

(defun quoted-cl-condition-p (arg)
  "Return T if ARG is a parsed (QUOTE SYM) form where SYM is a CL built-in condition.

SYM is a CL condition if:
  - It has a cl: or common-lisp: package prefix, OR
  - It is unqualified (CURRENT: prefix) and its symbol name matches *cl-condition-names*

Note: The QUOTE head may be either a string (\"PACKAGE:quote\" from interpret-symbol)
or the actual CL:QUOTE symbol (introduced by the quote reader macro, not from source)."
  (when (and (consp arg)
             (let ((head (first arg)))
               (or (and (stringp head) (base:symbol-matches-p head "QUOTE"))
                   (and (symbolp head) (string-equal (symbol-name head) "QUOTE"))))
             (stringp (second arg)))
    (let* ((sym (second arg))
           (colon-pos (position #\: sym :from-end t)))
      ;; The parser (interpret-symbol) always adds a package prefix, producing
      ;; strings like "PKG:name" or "CURRENT:name".  A nil colon-pos therefore
      ;; means SYM is not a parser-produced symbol string, so returning nil
      ;; (no violation) is intentional.
      (when colon-pos
        (let ((pkg (subseq sym 0 colon-pos))
              (name (string-upcase (subseq sym (1+ colon-pos)))))
          (or
           ;; Package-qualified CL condition: cl:simple-error or common-lisp:simple-error
           (member pkg '("cl" "common-lisp") :test #'string-equal)
           ;; Unqualified (CURRENT: prefix) matching a known CL condition name
           (and (string-equal pkg "CURRENT")
                (member name *cl-condition-names* :test #'string=))))))))

(defmethod base:check-form ((rule error-without-custom-condition-rule) form file)
  "Check FORM from FILE for (error ...) calls with non-custom conditions."
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
    ((rule error-without-custom-condition-rule) expr file line column
     &optional function-name position-map)
  "Recursively check EXPR for (error ...) patterns using non-custom conditions."
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

                     ;; Check for (error "string" ...) or (error 'cl:condition ...)
                     ;; patterns: head must be CL:ERROR and first arg must be either
                     ;; a string literal (no colon) or a quoted CL built-in condition.
                     (when (and (error-call-head-p head)
                                (consp rest-args)
                                (let ((arg (first rest-args)))
                                  (or
                                   ;; String literal: parser strings have no colon
                                   (and (stringp arg)
                                        (not (find #\: arg)))
                                   ;; Quoted CL built-in condition
                                   (quoted-cl-condition-p arg)))
                                (base:should-create-violation-p rule))
                       (push (make-instance 'violation:violation
                                            :rule :error-without-custom-condition
                                            :file file
                                            :line actual-line
                                            :column actual-column
                                            :severity (base:rule-severity rule)
                                            :message "Define a custom condition type instead of using (error \"string\") or a built-in CL condition"
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
