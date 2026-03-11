(defpackage #:mallet/rules/forms/runtime-unintern
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:mallet/rules/base)
   (#:utils #:mallet/utils)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation))
  (:export #:runtime-unintern-rule))
(in-package #:mallet/rules/forms/runtime-unintern)

;;; Runtime-Unintern Rule
;;;
;;; Detects runtime calls to cl:unintern. Unlike the broader :runtime-intern rule,
;;; this rule targets only cl:unintern, which is especially dangerous because it
;;; mutates the live package structure and can break symbol identity across packages.

(defclass runtime-unintern-rule (base:rule)
  ()
  (:default-initargs
   :name :runtime-unintern
   :description "Avoid calling cl:unintern at runtime; it mutates the live package structure and can break symbol identity"
   :severity :warning
   :category :suspicious
   :type :form)
  (:documentation "Rule to detect runtime use of cl:unintern."))

(defun unintern-symbol-p (expr)
  "Return T if EXPR refers to the CL:UNINTERN function.
Handles:
 - Qualified strings: \"COMMON-LISP:unintern\", \"CL:unintern\"
 - Current-package prefix: \"CURRENT:unintern\" (parser's unqualified symbol representation)
 - Already-interned CL symbol: CL:UNINTERN
 - #'unintern -> (FUNCTION string) from reader macros
 - 'unintern  -> (QUOTE  string) from reader macros"
  (labels ((unintern-name-p (str)
             (and (stringp str)
                  (let ((name (utils:symbol-name-from-string str))
                        (pkg  (let ((pos (position #\: str)))
                                (when pos (string-upcase (subseq str 0 pos))))))
                    (and (string-equal name "UNINTERN")
                         (or (null pkg)
                             (member pkg '("CL" "COMMON-LISP" "CURRENT")
                                     :test #'string-equal)))))))
    (cond
      ;; String symbol from parser
      ((stringp expr)
       (unintern-name-p expr))
      ;; Already-interned CL symbol (e.g. CL:UNINTERN)
      ((symbolp expr)
       (and (string-equal (symbol-name expr) "UNINTERN")
            (let ((pkg (symbol-package expr)))
              (and pkg (member (package-name pkg) '("COMMON-LISP" "CL") :test #'string-equal)))))
      ;; #'unintern -> (FUNCTION "...unintern...")
      ((and (consp expr)
            (let ((h (first expr)))
              (or (eq h 'cl:function)
                  (and (stringp h) (base:symbol-matches-p h "FUNCTION"))))
            (unintern-name-p (second expr)))
       t)
      ;; 'unintern -> (QUOTE "...unintern...")
      ((and (consp expr)
            (let ((h (first expr)))
              (or (eq h 'cl:quote)
                  (and (stringp h) (base:symbol-matches-p h "QUOTE"))))
            (unintern-name-p (second expr)))
       t)
      (t nil))))

(defun form-head-name-matches-p (head name)
  "Return T if HEAD (parser string or interned CL symbol) matches NAME."
  (typecase head
    (string (base:symbol-matches-p head name))
    (symbol (string-equal (symbol-name head) name))
    (otherwise nil)))

(defmethod base:check-form ((rule runtime-unintern-rule) form file)
  "Check FORM from FILE for runtime cl:unintern usage."
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
    ((rule runtime-unintern-rule) expr file line column &optional function-name position-map)
  "Recursively check EXPR for runtime cl:unintern usage."
  (declare (ignore function-name))
  (let ((violations '())
        (visited (make-hash-table :test 'eq)))

    (labels ((make-unintern-violation (vline vcol desc)
               (make-instance 'violation:violation
                              :rule :runtime-unintern
                              :file file
                              :line vline
                              :column vcol
                              :severity (base:rule-severity rule)
                              :message (format nil "Avoid calling ~A at runtime" desc)
                              :fix nil))

             (check-expr (current-expr fallback-line fallback-column)
               (base:with-safe-code-expr (current-expr visited)
                 (multiple-value-bind (actual-line actual-column)
                     (base:find-actual-position
                      current-expr position-map fallback-line fallback-column)
                   (let ((head (first current-expr))
                         (rest-args (rest current-expr)))

                     ;; Skip DEFMACRO bodies entirely (compile-time, not runtime)
                     (when (form-head-name-matches-p head "DEFMACRO")
                       (return-from check-expr nil))

                     ;; Direct call: (unintern ...)
                     (when (and (unintern-symbol-p head)
                                (base:should-create-violation-p rule))
                       (push (make-unintern-violation actual-line actual-column "cl:unintern")
                             violations))

                     ;; (funcall #'unintern ...) or (funcall 'unintern ...)
                     (when (and (form-head-name-matches-p head "FUNCALL")
                                (consp rest-args)
                                (unintern-symbol-p (first rest-args))
                                (base:should-create-violation-p rule))
                       (push (make-unintern-violation actual-line actual-column
                                                      "cl:unintern via funcall")
                             violations))

                     ;; (apply #'unintern ...) or (apply 'unintern ...)
                     (when (and (form-head-name-matches-p head "APPLY")
                                (consp rest-args)
                                (unintern-symbol-p (first rest-args))
                                (base:should-create-violation-p rule))
                       (push (make-unintern-violation actual-line actual-column
                                                      "cl:unintern via apply")
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
