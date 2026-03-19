(defpackage #:mallet/rules/forms/control-flow
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation))
  (:export #:if-without-else-rule
           #:progn-in-conditional-rule
           #:redundant-progn-rule
           #:missing-otherwise-rule
           #:wrong-otherwise-rule))
(in-package #:mallet/rules/forms/control-flow)

;;; If-without-else rule

(defclass if-without-else-rule (base:rule)
  ()
  (:default-initargs
   :name :missing-else
   :description "Use 'when' or 'unless' instead of 'if' without else clause"
   :severity :warning
   :category :style
   :type :form)
  (:documentation "Rule to detect 'if' forms without an else clause."))

(defmethod base:check-form ((rule if-without-else-rule) form file)
  "Check that IF forms have an else clause, otherwise suggest when/unless."
  (check-type form parser:form)
  (check-type file pathname)

  ;; Start checking using the generic method for suppression support
  ;; Pass the position-map so nested forms can look up their actual positions
  (base:check-form-recursive rule
                             (parser:form-expr form)
                             file
                             (parser:form-line form)
                             (parser:form-column form)
                             nil  ; function-name
                             (parser:form-position-map form)))

(defmethod base:check-form-recursive ((rule if-without-else-rule) expr file line column &optional function-name position-map)
  "Recursively check for IF forms without else clauses.
Suppressions are handled automatically by the :around method."
  (declare (ignore function-name))

  (let ((violations '())
        (visited (make-hash-table :test 'eq)))

    (labels ((check-expr (current-expr fallback-line fallback-column)
               "Recursively check expression for if-without-else violations."
               (base:with-safe-code-expr (current-expr visited)
                 ;; Look up the actual position of this expression if position-map is available
                 (multiple-value-bind (actual-line actual-column)
                     (base:find-actual-position current-expr position-map fallback-line fallback-column)
                   (let ((head (first current-expr))
                         (rest-args (rest current-expr)))
                     ;; Check if this is an IF form
                     (when (base:symbol-matches-p head "IF")
                       ;; IF should have 3 args (condition then else)
                       ;; If it has only 2 args, it's missing else clause
                       (when (and (= (length rest-args) 2)
                                  (base:should-create-violation-p rule))
                         (push (make-instance 'violation:violation
                                              :rule :missing-else
                                              :file file
                                              :line actual-line
                                              :column actual-column
                                              :severity (base:rule-severity rule)
                                              :message
                                              "Use 'when' or 'unless' instead of 'if' without else clause"
                                              :fix nil)
                               violations)))

                     ;; Recursively check nested forms
                     (a:nconcf violations
                               (base:collect-violations-from-subexprs rule head file
                                                                      actual-line actual-column
                                                                      position-map))
                     (a:nconcf violations
                               (base:collect-violations-from-subexprs rule rest-args file
                                                                      actual-line actual-column
                                                                      position-map)))))))

      ;; Start checking from the provided expression
      (check-expr expr line column))

    violations))

;;; Bare-progn rule

(defclass progn-in-conditional-rule (base:rule)
  ()
  (:default-initargs
   :name :progn-in-conditional
   :description "Bare 'progn' in 'if', 'and', or 'or' can be simplified"
   :severity :info
   :category :style
   :type :form)
  (:documentation "Rule to detect bare 'progn' in 'if', 'and', or 'or' clauses."))

(defmethod base:coalton-aware-p ((rule progn-in-conditional-rule)) t)

(defmethod base:check-form ((rule progn-in-conditional-rule) form file)
  "Check that IF/AND/OR forms don't have bare PROGN in key positions."
  (check-type form parser:form)
  (check-type file pathname)

  ;; Start checking using the generic method for suppression support
  ;; Pass the position-map so nested forms can look up their actual positions
  (base:check-form-recursive rule
                             (parser:form-expr form)
                             file
                             (parser:form-line form)
                             (parser:form-column form)
                             nil  ; function-name
                             (parser:form-position-map form)))

(defmethod base:check-form-recursive ((rule progn-in-conditional-rule) expr file line column &optional function-name position-map)
  "Recursively check for bare PROGN in IF/AND/OR clauses.
Suppressions are handled automatically by the :around method."
  (declare (ignore function-name))

  (let ((violations '())
        (visited (make-hash-table :test 'eq)))

    (labels ((is-progn-p (form-expr)
               "Check if expression is a bare progn form."
               (and (consp form-expr)
                    (base:symbol-matches-p (first form-expr) "PROGN")))

             (check-expr (current-expr fallback-line fallback-column)
               "Recursively check expression for bare progn in if/and/or."
               (base:with-safe-code-expr (current-expr visited)
                 ;; Look up the actual position of this expression if position-map is available
                 (multiple-value-bind (actual-line actual-column)
                     (base:find-actual-position current-expr position-map fallback-line fallback-column)
                   (let ((head (first current-expr))
                         (rest-args (rest current-expr)))
                     ;; Check if this is an IF form
                     (when (base:symbol-matches-p head "IF")
                       ;; IF should have 2 or 3 args (condition then [else])
                       (when (>= (length rest-args) 2)
                         (let ((then-clause (second rest-args))
                               (else-clause (when (>= (length rest-args) 3)
                                              (third rest-args))))
                           ;; Check if either then or else clause is a bare progn
                           ;; Report only one violation per IF form
                           (when (and (or (is-progn-p then-clause)
                                          (is-progn-p else-clause))
                                      (base:should-create-violation-p rule))
                             (push (make-instance 'violation:violation
                                                  :rule :progn-in-conditional
                                                  :file file
                                                  :line actual-line
                                                  :column actual-column
                                                  :severity (base:rule-severity rule)
                                                  :message
                                                  "Use 'cond' instead of 'if' with bare 'progn'"
                                                  :fix nil)
                                   violations)))))

                     ;; Check if this is an AND form with progn as last argument
                     (when (base:symbol-matches-p head "AND")
                       (when (and (>= (length rest-args) 2)
                                  (is-progn-p (car (last rest-args)))
                                  (base:should-create-violation-p rule))
                         (push (make-instance 'violation:violation
                                              :rule :progn-in-conditional
                                              :file file
                                              :line actual-line
                                              :column actual-column
                                              :severity (base:rule-severity rule)
                                              :message
                                              "Use 'when' instead of 'and' with bare 'progn'"
                                              :fix nil)
                               violations)))

                     ;; Check if this is an OR form with progn as last argument
                     (when (base:symbol-matches-p head "OR")
                       (when (and (>= (length rest-args) 2)
                                  (is-progn-p (car (last rest-args)))
                                  (base:should-create-violation-p rule))
                         (push (make-instance 'violation:violation
                                              :rule :progn-in-conditional
                                              :file file
                                              :line actual-line
                                              :column actual-column
                                              :severity (base:rule-severity rule)
                                              :message
                                              "Use 'unless' instead of 'or' with bare 'progn'"
                                              :fix nil)
                               violations)))

                     ;; Recursively check nested forms
                     (a:nconcf violations
                               (base:collect-violations-from-subexprs rule head file
                                                                      actual-line actual-column
                                                                      position-map))
                     (a:nconcf violations
                               (base:collect-violations-from-subexprs rule rest-args file
                                                                      actual-line actual-column
                                                                      position-map)))))))

      ;; Start checking from the provided expression
      (check-expr expr line column))

    violations))

;;; Redundant-progn rule

(defclass redundant-progn-rule (base:rule)
  ()
  (:default-initargs
   :name :redundant-progn
   :description "PROGN with a single body form is redundant"
   :severity :info
   :category :style
   :type :form)
  (:documentation "Rule to detect PROGN forms with exactly one body form."))

(defmethod base:coalton-aware-p ((rule redundant-progn-rule)) t)

(defmethod base:check-form ((rule redundant-progn-rule) form file)
  "Check that PROGN forms have more than one body form."
  (check-type form parser:form)
  (check-type file pathname)

  (base:check-form-recursive rule
                             (parser:form-expr form)
                             file
                             (parser:form-line form)
                             (parser:form-column form)
                             nil  ; function-name
                             (parser:form-position-map form)))

(defmethod base:check-form-recursive ((rule redundant-progn-rule) expr file line column &optional function-name position-map)
  "Recursively check for PROGN forms with exactly one body form.
Suppressions are handled automatically by the :around method."
  (declare (ignore function-name))

  (let ((violations '())
        (visited (make-hash-table :test 'eq)))

    (labels ((check-expr (current-expr fallback-line fallback-column)
               "Recursively check expression for redundant progn."
               (base:with-safe-code-expr (current-expr visited)
                 (multiple-value-bind (actual-line actual-column)
                     (base:find-actual-position current-expr position-map fallback-line fallback-column)
                   (let ((head (first current-expr))
                         (rest-args (rest current-expr)))
                     ;; Check if this is a PROGN form with exactly one body form
                     ;; Skip when the single element is ,@splice (UNQUOTE-SPLICING) —
                     ;; at expansion time it may produce multiple forms.
                     (when (base:symbol-matches-p head "PROGN")
                       (when (and (= (length rest-args) 1)
                                  (let ((single (first rest-args)))
                                    (not (and (consp single)
                                              (symbolp (first single))
                                              (string= (symbol-name (first single))
                                                       "UNQUOTE-SPLICING"))))
                                  (base:should-create-violation-p rule))
                         (push (make-instance 'violation:violation
                                              :rule :redundant-progn
                                              :file file
                                              :line actual-line
                                              :column actual-column
                                              :severity (base:rule-severity rule)
                                              :message
                                              "PROGN with a single body form is redundant"
                                              :fix nil)
                               violations)))

                     ;; Recursively check nested forms
                     (a:nconcf violations
                               (base:collect-violations-from-subexprs rule head file
                                                                      actual-line actual-column
                                                                      position-map))
                     (a:nconcf violations
                               (base:collect-violations-from-subexprs rule rest-args file
                                                                      actual-line actual-column
                                                                      position-map)))))))

      ;; Start checking from the provided expression
      (check-expr expr line column))

    violations))

;;; Missing-otherwise rule

(defclass missing-otherwise-rule (base:rule)
  ()
  (:default-initargs
   :name :missing-otherwise
   :description "case/typecase should have 'otherwise' clause"
   :severity :warning
   :category :style
   :type :form)
  (:documentation "Rule to detect case/typecase without otherwise clause."))

(defmethod base:check-form ((rule missing-otherwise-rule) form file)
  "Check that CASE and TYPECASE forms have an 'otherwise' clause."
  (check-type form parser:form)
  (check-type file pathname)

  ;; Start checking using the generic method for suppression support
  ;; Pass the position-map so nested forms can look up their actual positions
  (base:check-form-recursive rule
                             (parser:form-expr form)
                             file
                             (parser:form-line form)
                             (parser:form-column form)
                             nil  ; function-name
                             (parser:form-position-map form)))

(defmethod base:check-form-recursive ((rule missing-otherwise-rule) expr file line column &optional function-name position-map)
  "Recursively check for CASE/TYPECASE without otherwise clauses.
Suppressions are handled automatically by the :around method."
  (declare (ignore function-name))

  (let ((violations '())
        (visited (make-hash-table :test 'eq)))

    (labels ((has-otherwise-clause-p (clauses)
               "Check if clauses list has an otherwise clause."
               (some (lambda (clause)
                       (when (consp clause)
                         (let ((key (first clause)))
                           (base:symbol-matches-p key "OTHERWISE"))))
                     clauses))

             (find-t-clause (clauses)
               "Find the T clause and return it, or nil if none."
               (find-if (lambda (clause)
                          (when (consp clause)
                            (let ((key (first clause)))
                              (base:symbol-matches-p key "T"))))
                        clauses))

             (check-expr (current-expr fallback-line fallback-column)
               "Recursively check expression for missing otherwise."
               (base:with-safe-code-expr (current-expr visited)
                 ;; Look up the actual position of this expression if position-map is available
                 (multiple-value-bind (actual-line actual-column)
                     (base:find-actual-position current-expr position-map fallback-line fallback-column)
                   (let ((head (first current-expr))
                         (rest-args (rest current-expr)))
                     ;; Check if this is a CASE or TYPECASE form
                     (when (and (stringp head)
                                (member (base:symbol-name-from-string head)
                                        '("CASE" "TYPECASE")
                                        :test #'string-equal))
                       (let* ((form-name (base:symbol-name-from-string head))
                              (keyform (first rest-args))
                              (clauses (rest rest-args))
                              (t-clause (find-t-clause clauses)))
                         (declare (ignore keyform))

                         ;; Check if it has T instead of OTHERWISE
                         (when (and t-clause
                                    (base:should-create-violation-p rule))
                           (push (make-instance 'violation:violation
                                                :rule :missing-otherwise
                                                :file file
                                                :line actual-line
                                                :column actual-column
                                                :severity (base:rule-severity rule)
                                                :message
                                                (format nil
                                                        "Use 'otherwise' instead of 't' in '~A'"
                                                        (string-downcase form-name))
                                                :fix nil)
                                 violations))

                         ;; Check if it has OTHERWISE clause
                         (unless (or (has-otherwise-clause-p clauses)
                                     t-clause
                                     (not (base:should-create-violation-p rule)))
                           (push (make-instance 'violation:violation
                                                :rule :missing-otherwise
                                                :file file
                                                :line actual-line
                                                :column actual-column
                                                :severity (base:rule-severity rule)
                                                :message
                                                (format nil "'~A' should have 'otherwise' clause"
                                                        (string-downcase form-name))
                                                :fix nil)
                                 violations))))

                     ;; Recursively check nested forms
                     (a:nconcf violations
                               (base:collect-violations-from-subexprs rule head file
                                                                      actual-line actual-column
                                                                      position-map))
                     (a:nconcf violations
                               (base:collect-violations-from-subexprs rule rest-args file
                                                                      actual-line actual-column
                                                                      position-map)))))))

      ;; Start checking from the provided expression
      (check-expr expr line column))

    violations))

;;; Wrong-otherwise rule

(defclass wrong-otherwise-rule (base:rule)
  ()
  (:default-initargs
   :name :wrong-otherwise
   :description "ecase/etypecase should not have 'otherwise' or 't' clause"
   :severity :error
   :category :correctness
   :type :form)
  (:documentation "Rule to detect otherwise/t in ecase/etypecase."))

(defmethod base:check-form ((rule wrong-otherwise-rule) form file)
  "Check that ECASE and ETYPECASE forms don't have otherwise or t clause."
  (check-type form parser:form)
  (check-type file pathname)

  ;; Start checking from the form's expression using the generic method
  ;; This will handle suppressions automatically through the :around method
  ;; Pass the position-map so nested forms can look up their actual positions
  (base:check-form-recursive rule
                             (parser:form-expr form)
                             file
                             (parser:form-line form)
                             (parser:form-column form)
                             nil  ; function-name
                             (parser:form-position-map form)))

(defmethod base:check-form-recursive ((rule wrong-otherwise-rule) expr file line column &optional function-name position-map)
  "Recursively check for ECASE/ETYPECASE with otherwise/t clauses.
Suppressions are handled automatically by the :around method."
  (declare (ignore function-name))  ; Not tracking function context for this rule

  (let ((violations '())
        (visited (make-hash-table :test 'eq)))  ; Track visited cons cells

    (labels ((find-catch-all-clause (clauses)
               "Find the otherwise or t clause and return it, or nil if none."
               (find-if (lambda (clause)
                          (when (consp clause)
                            (let ((key (first clause)))
                              (or (base:symbol-matches-p key "OTHERWISE")
                                  (base:symbol-matches-p key "T")))))
                        clauses))

             (check-expr (current-expr fallback-line fallback-column)
               "Recursively check expression for wrong otherwise."
               (base:with-safe-code-expr (current-expr visited)
                 ;; Look up the actual position of this expression if position-map is available
                 (multiple-value-bind (actual-line actual-column)
                     (base:find-actual-position current-expr position-map fallback-line fallback-column)
                   (let ((head (first current-expr))
                         (rest-args (rest current-expr)))
                     ;; Check if this is an ECASE or ETYPECASE form
                     (when (and (stringp head)
                                (member (base:symbol-name-from-string head)
                                        '("ECASE" "ETYPECASE")
                                        :test #'string-equal))
                       (let* ((form-name (base:symbol-name-from-string head))
                              (keyform (first rest-args))
                              (clauses (rest rest-args))
                              (catch-all-clause (find-catch-all-clause clauses)))
                         (declare (ignore keyform))

                         ;; Check if it has OTHERWISE or T clause
                         (when (and catch-all-clause
                                    (base:should-create-violation-p rule))
                           (push (make-instance 'violation:violation
                                                :rule :wrong-otherwise
                                                :file file
                                                :line actual-line
                                                :column actual-column
                                                :severity (base:rule-severity rule)
                                                :message
                                                (format nil
                                                        "'~A' should not have 'otherwise' or 't' clause"
                                                        (string-downcase form-name))
                                                :fix nil)
                                 violations))))

                     ;; Recursively check nested forms
                     (a:nconcf violations
                               (base:collect-violations-from-subexprs rule head file
                                                                      actual-line actual-column
                                                                      position-map))
                     (a:nconcf violations
                               (base:collect-violations-from-subexprs rule rest-args file
                                                                      actual-line actual-column
                                                                      position-map)))))))

      ;; Start checking from the provided expression
      (check-expr expr line column))

    violations))
