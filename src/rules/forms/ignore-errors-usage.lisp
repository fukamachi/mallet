;; Defines the :no-ignore-errors rule (file and package retain the old :ignore-errors-usage name for historical continuity).
(defpackage #:mallet/rules/forms/ignore-errors-usage
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation))
  (:export #:ignore-errors-usage-rule))
(in-package #:mallet/rules/forms/ignore-errors-usage)

;;; ignore-errors-usage rule

(defclass ignore-errors-usage-rule (base:rule)
  ()
  (:default-initargs
   :name :no-ignore-errors
   :description "Avoid using ignore-errors; use handler-case to handle specific conditions"
   :severity :warning
   :category :practice
   :type :form)
  (:documentation "Rule to detect use of ignore-errors, which silently swallows all errors.
Use handler-case with specific condition types instead."))

(defun form-head-name-p (head name)
  "Check if HEAD (a string symbol or interned CL symbol) matches NAME (case-insensitive)."
  (typecase head
    (string (base:symbol-matches-p head name))
    (symbol (string-equal (symbol-name head) name))
    (otherwise nil)))

(defmethod base:check-form ((rule ignore-errors-usage-rule) form file)
  "Check for runtime uses of ignore-errors."
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
    ((rule ignore-errors-usage-rule) expr file line column
     &optional function-name position-map)
  "Recursively check for ignore-errors usage."
  (declare (ignore function-name))

  (let ((violations '())
        (visited (make-hash-table :test 'eq)))

    (labels ((make-violation (actual-line actual-column)
               (make-instance 'violation:violation
                              :rule :no-ignore-errors
                              :file file
                              :line actual-line
                              :column actual-column
                              :severity (base:rule-severity rule)
                              :message "Avoid using ignore-errors; use handler-case to handle specific conditions"
                              :fix nil))

             (check-expr (current-expr fallback-line fallback-column)
               (base:with-safe-code-expr (current-expr visited)
                 (multiple-value-bind (actual-line actual-column)
                     (base:find-actual-position
                      current-expr position-map fallback-line fallback-column)
                   (let ((head (first current-expr))
                         (rest-args (rest current-expr)))
                     (cond
                       ;; DEFMACRO: skip entirely (macro expansion code, not runtime)
                       ((form-head-name-p head "DEFMACRO")
                        nil)

                       (t
                        ;; Direct (ignore-errors ...) call
                        (when (and (form-head-name-p head "IGNORE-ERRORS")
                                   (base:should-create-violation-p rule))
                          (push (make-violation actual-line actual-column)
                                violations))

                        ;; Recurse into subexpressions
                        (a:nconcf violations
                                  (base:collect-violations-from-subexprs
                                   rule head file actual-line actual-column
                                   position-map))
                        (a:nconcf violations
                                  (base:collect-violations-from-subexprs
                                   rule rest-args file actual-line actual-column
                                   position-map)))))))))

      (check-expr expr line column))

    violations))
