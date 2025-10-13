(defpackage #:mallet/rules/forms/control-flow
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation))
  (:export #:if-without-else-rule
           #:bare-progn-in-if-rule
           #:missing-otherwise-rule
           #:wrong-otherwise-rule))
(in-package #:mallet/rules/forms/control-flow)

;;; If-without-else rule

(defclass if-without-else-rule (base:rule)
  ()
  (:default-initargs
   :name :if-without-else
   :description "Use 'when' or 'unless' instead of 'if' without else clause"
   :severity :warning
   :type :form)
  (:documentation "Rule to detect 'if' forms without an else clause."))

(defmethod base:check-form ((rule if-without-else-rule) form file)
  "Check that IF forms have an else clause, otherwise suggest when/unless."
  (check-type form parser:form)
  (check-type file pathname)

  (let ((violations '())
        (position-map (parser:form-position-map form))
        (visited (make-hash-table :test 'eq)))  ; Track visited cons cells
    (labels ((check-expr (expr fallback-line fallback-column)
               "Recursively check expression for if-without-else violations."
               (base:with-safe-cons-expr (expr visited)
                 ;; Get actual position of this expression
                 (multiple-value-bind (line column)
                     (parser:find-position expr position-map fallback-line fallback-column)
                   (let ((head (first expr))
                         (rest-args (rest expr)))
                     ;; Check if this is an IF form
                     (when (base:symbol-matches-p head "IF")
                       ;; IF should have 3 args (condition then else)
                       ;; If it has only 2 args, it's missing else clause
                       (when (= (length rest-args) 2)
                         (push (make-instance 'violation:violation
                                              :rule :if-without-else
                                              :file file
                                              :line line
                                              :column column
                                              :severity (base:rule-severity rule)
                                              :message
                                              "Use 'when' or 'unless' instead of 'if' without else clause"
                                              :fix nil)
                               violations)))

                     ;; Recursively check head if it's a cons
                     (when (consp head)
                       (check-expr head line column))

                     ;; Recursively check rest-args
                     ;; Only iterate if it's a proper list (not a dotted pair like (a . b))
                     (when (a:proper-list-p rest-args)
                       (dolist (subexpr rest-args)
                         (check-expr subexpr line column))))))))

      ;; Start checking from the form's expression
      (check-expr (parser:form-expr form)
                  (parser:form-line form)
                  (parser:form-column form)))

    (nreverse violations)))

;;; Bare-progn-in-if rule

(defclass bare-progn-in-if-rule (base:rule)
  ()
  (:default-initargs
   :name :bare-progn-in-if
   :description "Use 'cond' instead of 'if' with bare 'progn'"
   :severity :warning
   :type :form)
  (:documentation "Rule to detect bare 'progn' in 'if' clauses."))

(defmethod base:check-form ((rule bare-progn-in-if-rule) form file)
  "Check that IF forms don't have bare PROGN in then/else clauses."
  (check-type form parser:form)
  (check-type file pathname)

  (let ((violations '())
        (position-map (parser:form-position-map form))
        (visited (make-hash-table :test 'eq)))  ; Track visited cons cells
    (labels ((is-progn-p (expr)
               "Check if expression is a bare progn form."
               (and (consp expr)
                    (base:symbol-matches-p (first expr) "PROGN")))

             (check-expr (expr fallback-line fallback-column)
               "Recursively check expression for bare progn in if."
               (base:with-safe-cons-expr (expr visited)
                 ;; Get actual position of this expression
                 (multiple-value-bind (line column)
                     (parser:find-position expr position-map fallback-line fallback-column)
                   (let ((head (first expr))
                         (rest-args (rest expr)))
                     ;; Check if this is an IF form
                     (when (base:symbol-matches-p head "IF")
                       ;; IF should have 2 or 3 args (condition then [else])
                       (when (>= (length rest-args) 2)
                         (let ((then-clause (second rest-args))
                               (else-clause (when (>= (length rest-args) 3)
                                              (third rest-args))))
                           ;; Check if then clause is a bare progn
                           (when (is-progn-p then-clause)
                             (push (make-instance 'violation:violation
                                                  :rule :bare-progn-in-if
                                                  :file file
                                                  :line line
                                                  :column column
                                                  :severity (base:rule-severity rule)
                                                  :message
                                                  "Use 'cond' instead of 'if' with bare 'progn'"
                                                  :fix nil)
                                   violations))

                           ;; Check if else clause is a bare progn
                           (when (is-progn-p else-clause)
                             (push (make-instance 'violation:violation
                                                  :rule :bare-progn-in-if
                                                  :file file
                                                  :line line
                                                  :column column
                                                  :severity (base:rule-severity rule)
                                                  :message
                                                  "Use 'cond' instead of 'if' with bare 'progn'"
                                                  :fix nil)
                                   violations)))))

                     ;; Recursively check head if it's a cons
                     (when (consp head)
                       (check-expr head line column))

                     ;; Recursively check rest-args
                     ;; Only iterate if it's a proper list (not a dotted pair like (a . b))
                     (when (a:proper-list-p rest-args)
                       (dolist (subexpr rest-args)
                         (check-expr subexpr line column))))))))

      ;; Start checking from the form's expression
      (check-expr (parser:form-expr form)
                  (parser:form-line form)
                  (parser:form-column form)))

    (nreverse violations)))

;;; Missing-otherwise rule

(defclass missing-otherwise-rule (base:rule)
  ()
  (:default-initargs
   :name :missing-otherwise
   :description "case/typecase should have 'otherwise' clause"
   :severity :warning
   :type :form)
  (:documentation "Rule to detect case/typecase without otherwise clause."))

(defmethod base:check-form ((rule missing-otherwise-rule) form file)
  "Check that CASE and TYPECASE forms have an 'otherwise' clause."
  (check-type form parser:form)
  (check-type file pathname)

  (let ((violations '())
        (position-map (parser:form-position-map form))
        (visited (make-hash-table :test 'eq)))  ; Track visited cons cells
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

             (check-expr (expr fallback-line fallback-column)
               "Recursively check expression for missing otherwise."
               (base:with-safe-cons-expr (expr visited)
                 ;; Get actual position of this expression
                 (multiple-value-bind (line column)
                     (parser:find-position expr position-map fallback-line fallback-column)
                   (let ((head (first expr))
                         (rest-args (rest expr)))
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
                         (when t-clause
                           ;; Find the position of the t clause
                           (multiple-value-bind (clause-line clause-column)
                               (parser:find-position t-clause position-map line column)
                             (push (make-instance 'violation:violation
                                                  :rule :missing-otherwise
                                                  :file file
                                                  :line clause-line
                                                  :column clause-column
                                                  :severity (base:rule-severity rule)
                                                  :message
                                                  (format nil
                                                          "Use 'otherwise' instead of 't' in '~A'"
                                                          (string-downcase form-name))
                                                  :fix nil)
                                   violations)))

                         ;; Check if it has OTHERWISE clause
                         (unless (or (has-otherwise-clause-p clauses) t-clause)
                           (push (make-instance 'violation:violation
                                                :rule :missing-otherwise
                                                :file file
                                                :line line
                                                :column column
                                                :severity (base:rule-severity rule)
                                                :message
                                                (format nil "'~A' should have 'otherwise' clause"
                                                        (string-downcase form-name))
                                                :fix nil)
                                 violations))))

                     ;; Recursively check head if it's a cons
                     (when (consp head)
                       (check-expr head line column))

                     ;; Recursively check rest-args
                     ;; Only iterate if it's a proper list (not a dotted pair like (a . b))
                     (when (a:proper-list-p rest-args)
                       (dolist (subexpr rest-args)
                         (check-expr subexpr line column))))))))

      ;; Start checking from the form's expression
      (check-expr (parser:form-expr form)
                  (parser:form-line form)
                  (parser:form-column form)))

    (nreverse violations)))

;;; Wrong-otherwise rule

(defclass wrong-otherwise-rule (base:rule)
  ()
  (:default-initargs
   :name :wrong-otherwise
   :description "ecase/etypecase should not have 'otherwise' or 't' clause"
   :severity :error
   :type :form)
  (:documentation "Rule to detect otherwise/t in ecase/etypecase."))

(defmethod base:check-form ((rule wrong-otherwise-rule) form file)
  "Check that ECASE and ETYPECASE forms don't have otherwise or t clause."
  (check-type form parser:form)
  (check-type file pathname)

  (let ((violations '())
        (position-map (parser:form-position-map form))
        (visited (make-hash-table :test 'eq)))  ; Track visited cons cells
    (labels ((find-catch-all-clause (clauses)
               "Find the otherwise or t clause and return it, or nil if none."
               (find-if (lambda (clause)
                          (when (consp clause)
                            (let ((key (first clause)))
                              (or (base:symbol-matches-p key "OTHERWISE")
                                  (base:symbol-matches-p key "T")))))
                        clauses))

             (check-expr (expr fallback-line fallback-column)
               "Recursively check expression for wrong otherwise."
               (base:with-safe-cons-expr (expr visited)
                 ;; Get actual position of this expression
                 (multiple-value-bind (line column)
                     (parser:find-position expr position-map fallback-line fallback-column)
                   (let ((head (first expr))
                         (rest-args (rest expr)))
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
                         (when catch-all-clause
                           ;; Find the position of the catch-all clause
                           (multiple-value-bind (clause-line clause-column)
                               (parser:find-position catch-all-clause position-map line column)
                             (push (make-instance 'violation:violation
                                                  :rule :wrong-otherwise
                                                  :file file
                                                  :line clause-line
                                                  :column clause-column
                                                  :severity (base:rule-severity rule)
                                                  :message
                                                  (format nil
                                                          "'~A' should not have 'otherwise' or 't' clause"
                                                          (string-downcase form-name))
                                                  :fix nil)
                                   violations)))))

                     ;; Recursively check head if it's a cons (e.g., in let bindings)
                     (when (consp head)
                       (check-expr head line column))

                     ;; Recursively check rest-args
                     ;; Only iterate if it's a proper list (not a dotted pair like (a . b))
                     (when (a:proper-list-p rest-args)
                       (dolist (subexpr rest-args)
                         (check-expr subexpr line column))))))))

      ;; Start checking from the form's expression
      (check-expr (parser:form-expr form)
                  (parser:form-line form)
                  (parser:form-column form)))

    (nreverse violations)))
