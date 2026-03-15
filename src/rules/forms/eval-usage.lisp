;; Defines the :no-eval rule (file and package retain the old :eval-usage name for historical continuity).
(defpackage #:mallet/rules/forms/eval-usage
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation))
  (:export #:eval-usage-rule))
(in-package #:mallet/rules/forms/eval-usage)

;;; Eval-usage rule

(defclass eval-usage-rule (base:rule)
  ()
  (:default-initargs
   :name :no-eval
   :description "Avoid using cl:eval at runtime for safety"
   :severity :warning
   :category :suspicious
   :type :form)
  (:documentation "Rule to detect runtime use of cl:eval, including via funcall and apply."))

(defun form-head-name-p (head name)
  "Check if HEAD (a string symbol or interned CL symbol) matches NAME (case-insensitive).

The `string' branch handles symbols produced by the custom Eclector parse-result client
(mallet/parser), which represents all user-written symbols as strings.  The `symbol'
branch handles the handful of symbols that Eclector interns directly because they arise
from reader macros: FUNCTION (from #') and QUOTE (from ')."
  (typecase head
    (string (base:symbol-matches-p head name))
    (symbol (string-equal (symbol-name head) name))
    (otherwise nil)))

(defun eval-symbol-p (expr)
  "Return true if EXPR refers to the EVAL function (symbol or function object).
Handles:
  - bare symbol: (funcall eval ...) where eval is a string symbol from the parser
    client -- caught by the first `or' clause via `base:symbol-matches-p'.
  - #'eval -> (FUNCTION string-eval)  where FUNCTION is interned CL symbol
  - 'eval  -> (QUOTE string-eval)     where QUOTE is interned CL symbol"
  (or (base:symbol-matches-p expr "EVAL")
      ;; #'eval -> (FUNCTION "CURRENT:eval") where FUNCTION is a real CL symbol
      (and (consp expr)
           (form-head-name-p (first expr) "FUNCTION")
           (base:symbol-matches-p (second expr) "EVAL"))
      ;; 'eval -> (QUOTE "CURRENT:eval") where QUOTE is a real CL symbol
      (and (consp expr)
           (form-head-name-p (first expr) "QUOTE")
           (base:symbol-matches-p (second expr) "EVAL"))))

(defmethod base:check-form ((rule eval-usage-rule) form file)
  "Check for runtime uses of cl:eval."
  (check-type form parser:form)
  (check-type file pathname)

  (base:check-form-recursive rule
                             (parser:form-expr form)
                             file
                             (parser:form-line form)
                             (parser:form-column form)
                             nil
                             (parser:form-position-map form)))

(defmethod base:check-form-recursive ((rule eval-usage-rule) expr file line column &optional function-name position-map)
  "Recursively check for runtime eval usage.
Suppressions are handled automatically by the :around method."
  (declare (ignore function-name))

  (let ((violations '())
        (visited (make-hash-table :test 'eq)))

    (labels ((make-eval-violation (actual-line actual-column pattern-desc)
               (make-instance 'violation:violation
                              :rule :no-eval
                              :file file
                              :line actual-line
                              :column actual-column
                              :severity (base:rule-severity rule)
                              :message (format nil "Avoid using ~A at runtime for safety" pattern-desc)
                              :fix nil))

             (check-expr (current-expr fallback-line fallback-column)
               "Recursively check expression for eval usage."
               (base:with-safe-code-expr (current-expr visited)
                 (multiple-value-bind (actual-line actual-column)
                     (base:find-actual-position current-expr position-map fallback-line fallback-column)
                   (let ((head (first current-expr))
                         (rest-args (rest current-expr)))
                     ;; Direct (eval ...) call
                     (when (and (base:symbol-matches-p head "EVAL")
                                (base:should-create-violation-p rule))
                       (push (make-eval-violation actual-line actual-column "cl:eval")
                             violations))

                     ;; (funcall #'eval ...) or (funcall 'eval ...)
                     (when (and (base:symbol-matches-p head "FUNCALL")
                                (consp rest-args)
                                (eval-symbol-p (first rest-args))
                                (base:should-create-violation-p rule))
                       (push (make-eval-violation actual-line actual-column "cl:eval via funcall")
                             violations))

                     ;; (apply #'eval ...) or (apply 'eval ...)
                     (when (and (base:symbol-matches-p head "APPLY")
                                (consp rest-args)
                                (eval-symbol-p (first rest-args))
                                (base:should-create-violation-p rule))
                       (push (make-eval-violation actual-line actual-column "cl:eval via apply")
                             violations))

                     ;; Recurse into subexpressions
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
