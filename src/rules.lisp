(defpackage #:malvolio/rules
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:violation #:malvolio/violation)
   (#:parser #:malvolio/parser))
  (:export #:rule
           #:rule-name
           #:rule-description
           #:rule-severity
           #:rule-type
           #:rule-enabled-p
           #:enable-rule
           #:disable-rule
           #:registry
           #:make-registry
           #:register-rule
           #:find-rule
           #:list-rules
           ;; Rule implementations
           #:line-length-rule
           #:comment-level-rule
           #:if-without-else-rule
           #:bare-progn-in-if-rule
           #:missing-otherwise-rule
           #:wrong-otherwise-rule
           ;; Check functions
           #:check-text
           #:check-tokens
           #:check-form))
(in-package #:malvolio/rules)

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
    :type (member :error :warning :info)
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
    :documentation "Whether the rule is enabled"))
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
                                      (enabled t))
  "Register a new rule in REGISTRY with NAME and properties."
  (check-type registry registry)
  (check-type name keyword)
  (check-type description string)
  (check-type severity (member :error :warning :info))
  (check-type type (member :text :token :form :pattern))
  (check-type enabled boolean)

  (let ((rule (make-instance 'rule
                             :name name
                             :description description
                             :severity severity
                             :type type
                             :enabled enabled)))
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

;;; Line length rule

(defclass line-length-rule (rule)
  ((max-length
    :initarg :max-length
    :initform 80
    :reader line-length-rule-max-length
    :type (integer 1)
    :documentation "Maximum allowed line length"))
  (:default-initargs
   :name :line-length
   :description "Lines should not exceed the maximum length"
   :severity :warning
   :type :text)
  (:documentation "Rule to check line length limits."))

(defmethod check-text ((rule line-length-rule) text file)
  "Check that lines in TEXT do not exceed MAX-LENGTH."
  (check-type text string)
  (check-type file pathname)

  (let ((violations '())
        (max-length (line-length-rule-max-length rule))
        (line-number 0))

    (with-input-from-string (stream text)
      (loop for line = (read-line stream nil nil)
            while line do
              (incf line-number)
              (let ((line-len (length line)))
                (when (> line-len max-length)
                  (push (make-instance 'violation:violation
                                       :rule :line-length
                                       :file file
                                       :line line-number
                                       :column 0
                                       :severity (rule-severity rule)
                                       :message (format nil "Line exceeds maximum length of ~A (~A characters)"
                                                       max-length line-len)
                                       :fix nil)
                        violations)))))

    (nreverse violations)))

;;; Comment level rule

(defclass comment-level-rule (rule)
  ()
  (:default-initargs
   :name :comment-level
   :description "Comments should use the appropriate number of semicolons"
   :severity :warning
   :type :token)
  (:documentation "Rule to check comment level conventions."))

(defmethod check-tokens ((rule comment-level-rule) tokens file)
  "Check that comments use the appropriate number of semicolons."
  (check-type tokens list)
  (check-type file pathname)

  (let ((violations '())
        (paren-depth 0)
        (prev-token nil))

    (dolist (token tokens)
      (let ((token-type (parser:token-type token)))
        (cond
          ;; Track parenthesis depth
          ((eq token-type :open-paren)
           (incf paren-depth))
          ((eq token-type :close-paren)
           (decf paren-depth))

          ;; Check comment tokens
          ((member token-type '(:comment-inline :comment-line
                                :comment-section :comment-file))
           (let* ((line (parser:token-line token))
                  (column (parser:token-column token))
                  (has-code-before (and prev-token
                                       (not (member (parser:token-type prev-token)
                                                   '(:comment-inline :comment-line
                                                     :comment-section :comment-file)))
                                       (= (parser:token-line prev-token) line)))
                  (expected-type
                   (cond
                     ;; Inline comment - code before it on same line
                     (has-code-before :comment-inline)
                     ;; Line comment - inside parens
                     ((> paren-depth 0) :comment-line)
                     ;; Top-level comment - outside parens (3 or 4 semicolons both OK)
                     (t :comment-section-or-file))))

             ;; Check if comment type matches expected context
             (unless (case expected-type
                       (:comment-inline (eq token-type :comment-inline))
                       (:comment-line (eq token-type :comment-line))
                       (:comment-section-or-file (member token-type
                                                        '(:comment-section :comment-file))))
               (let ((expected-semicolons
                      (case expected-type
                        (:comment-inline 1)
                        (:comment-line 2)
                        (:comment-section-or-file "3 or 4")))
                     (actual-semicolons
                      (case token-type
                        (:comment-inline 1)
                        (:comment-line 2)
                        (:comment-section 3)
                        (:comment-file 4))))
                 (push (make-instance 'violation:violation
                                      :rule :comment-level
                                      :file file
                                      :line line
                                      :column column
                                      :severity (rule-severity rule)
                                      :message (format nil "Comment should use ~A semicolon~:P (found ~A)"
                                                      expected-semicolons actual-semicolons)
                                      :fix nil)
                       violations))))))

        (setf prev-token token)))

    (nreverse violations)))

;;; If-without-else rule

(defclass if-without-else-rule (rule)
  ()
  (:default-initargs
   :name :if-without-else
   :description "Use 'when' or 'unless' instead of 'if' without else clause"
   :severity :warning
   :type :form)
  (:documentation "Rule to detect 'if' forms without an else clause."))

(defmethod check-form ((rule if-without-else-rule) form file)
  "Check that IF forms have an else clause, otherwise suggest when/unless."
  (check-type form parser:form)
  (check-type file pathname)

  (let ((violations '()))
    (labels ((check-expr (expr line column)
               "Recursively check expression for if-without-else violations."
               (when (consp expr)
                 (let ((head (first expr))
                       (rest-args (rest expr)))
                   ;; Check if this is an IF form
                   (when (and (symbolp head)
                              (string= (symbol-name head) "IF"))
                     ;; IF should have 3 args (condition then else)
                     ;; If it has only 2 args, it's missing else clause
                     (when (= (length rest-args) 2)
                       (push (make-instance 'violation:violation
                                            :rule :if-without-else
                                            :file file
                                            :line line
                                            :column column
                                            :severity (rule-severity rule)
                                            :message
                                            "Use 'when' or 'unless' instead of 'if' without else clause"
                                            :fix nil)
                             violations)))

                   ;; Recursively check nested forms
                   (dolist (subexpr rest-args)
                     (check-expr subexpr line column))))))

      ;; Start checking from the form's expression
      (check-expr (parser:form-expr form)
                  (parser:form-line form)
                  (parser:form-column form)))

    (nreverse violations)))

;;; Wrong-otherwise rule

(defclass wrong-otherwise-rule (rule)
  ()
  (:default-initargs
   :name :wrong-otherwise
   :description "ecase/etypecase should not have 'otherwise' or 't' clause"
   :severity :error
   :type :form)
  (:documentation "Rule to detect otherwise/t in ecase/etypecase."))

(defmethod check-form ((rule wrong-otherwise-rule) form file)
  "Check that ECASE and ETYPECASE forms don't have otherwise or t clause."
  (check-type form parser:form)
  (check-type file pathname)

  (let ((violations '()))
    (labels ((has-catch-all-clause-p (clauses)
               "Check if clauses list has an otherwise or t clause."
               (some (lambda (clause)
                       (when (consp clause)
                         (let ((key (first clause)))
                           (and (symbolp key)
                                (or (string= (symbol-name key) "OTHERWISE")
                                    (eq key t))))))
                     clauses))

             (check-expr (expr line column)
               "Recursively check expression for wrong otherwise."
               (when (consp expr)
                 (let ((head (first expr))
                       (rest-args (rest expr)))
                   ;; Check if this is an ECASE or ETYPECASE form
                   (when (and (symbolp head)
                              (member (symbol-name head)
                                      '("ECASE" "ETYPECASE")
                                      :test #'string=))
                     (let* ((form-name (symbol-name head))
                            (keyform (first rest-args))
                            (clauses (rest rest-args)))
                       (declare (ignore keyform))

                       ;; Check if it has OTHERWISE or T clause
                       (when (has-catch-all-clause-p clauses)
                         (push (make-instance 'violation:violation
                                              :rule :wrong-otherwise
                                              :file file
                                              :line line
                                              :column column
                                              :severity (rule-severity rule)
                                              :message
                                              (format nil
                                                      "'~A' should not have 'otherwise' or 't' clause"
                                                      (string-downcase form-name))
                                              :fix nil)
                               violations))))

                   ;; Recursively check nested forms
                   (dolist (subexpr rest-args)
                     (check-expr subexpr line column))))))

      ;; Start checking from the form's expression
      (check-expr (parser:form-expr form)
                  (parser:form-line form)
                  (parser:form-column form)))

    (nreverse violations)))

;;; Bare-progn-in-if rule

(defclass bare-progn-in-if-rule (rule)
  ()
  (:default-initargs
   :name :bare-progn-in-if
   :description "Use 'cond' instead of 'if' with bare 'progn'"
   :severity :warning
   :type :form)
  (:documentation "Rule to detect bare 'progn' in 'if' clauses."))

(defmethod check-form ((rule bare-progn-in-if-rule) form file)
  "Check that IF forms don't have bare PROGN in then/else clauses."
  (check-type form parser:form)
  (check-type file pathname)

  (let ((violations '()))
    (labels ((is-progn-p (expr)
               "Check if expression is a bare progn form."
               (and (consp expr)
                    (symbolp (first expr))
                    (string= (symbol-name (first expr)) "PROGN")))

             (check-expr (expr line column)
               "Recursively check expression for bare progn in if."
               (when (consp expr)
                 (let ((head (first expr))
                       (rest-args (rest expr)))
                   ;; Check if this is an IF form
                   (when (and (symbolp head)
                              (string= (symbol-name head) "IF"))
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
                                                :severity (rule-severity rule)
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
                                                :severity (rule-severity rule)
                                                :message
                                                "Use 'cond' instead of 'if' with bare 'progn'"
                                                :fix nil)
                                 violations)))))

                   ;; Recursively check nested forms
                   (dolist (subexpr rest-args)
                     (check-expr subexpr line column))))))

      ;; Start checking from the form's expression
      (check-expr (parser:form-expr form)
                  (parser:form-line form)
                  (parser:form-column form)))

    (nreverse violations)))

;;; Wrong-otherwise rule

(defclass wrong-otherwise-rule (rule)
  ()
  (:default-initargs
   :name :wrong-otherwise
   :description "ecase/etypecase should not have 'otherwise' or 't' clause"
   :severity :error
   :type :form)
  (:documentation "Rule to detect otherwise/t in ecase/etypecase."))

(defmethod check-form ((rule wrong-otherwise-rule) form file)
  "Check that ECASE and ETYPECASE forms don't have otherwise or t clause."
  (check-type form parser:form)
  (check-type file pathname)

  (let ((violations '()))
    (labels ((has-catch-all-clause-p (clauses)
               "Check if clauses list has an otherwise or t clause."
               (some (lambda (clause)
                       (when (consp clause)
                         (let ((key (first clause)))
                           (and (symbolp key)
                                (or (string= (symbol-name key) "OTHERWISE")
                                    (eq key t))))))
                     clauses))

             (check-expr (expr line column)
               "Recursively check expression for wrong otherwise."
               (when (consp expr)
                 (let ((head (first expr))
                       (rest-args (rest expr)))
                   ;; Check if this is an ECASE or ETYPECASE form
                   (when (and (symbolp head)
                              (member (symbol-name head)
                                      '("ECASE" "ETYPECASE")
                                      :test #'string=))
                     (let* ((form-name (symbol-name head))
                            (keyform (first rest-args))
                            (clauses (rest rest-args)))
                       (declare (ignore keyform))

                       ;; Check if it has OTHERWISE or T clause
                       (when (has-catch-all-clause-p clauses)
                         (push (make-instance 'violation:violation
                                              :rule :wrong-otherwise
                                              :file file
                                              :line line
                                              :column column
                                              :severity (rule-severity rule)
                                              :message
                                              (format nil
                                                      "'~A' should not have 'otherwise' or 't' clause"
                                                      (string-downcase form-name))
                                              :fix nil)
                               violations))))

                   ;; Recursively check nested forms
                   (dolist (subexpr rest-args)
                     (check-expr subexpr line column))))))

      ;; Start checking from the form's expression
      (check-expr (parser:form-expr form)
                  (parser:form-line form)
                  (parser:form-column form)))

    (nreverse violations)))

;;; Missing-otherwise rule

(defclass missing-otherwise-rule (rule)
  ()
  (:default-initargs
   :name :missing-otherwise
   :description "case/typecase should have 'otherwise' clause"
   :severity :warning
   :type :form)
  (:documentation "Rule to detect case/typecase without otherwise clause."))

(defmethod check-form ((rule missing-otherwise-rule) form file)
  "Check that CASE and TYPECASE forms have an 'otherwise' clause."
  (check-type form parser:form)
  (check-type file pathname)

  (let ((violations '()))
    (labels ((has-otherwise-clause-p (clauses)
               "Check if clauses list has an otherwise clause."
               (some (lambda (clause)
                       (when (consp clause)
                         (let ((key (first clause)))
                           (and (symbolp key)
                                (string= (symbol-name key) "OTHERWISE")))))
                     clauses))

             (has-t-clause-p (clauses)
               "Check if clauses list has a T clause (should use otherwise)."
               (some (lambda (clause)
                       (when (consp clause)
                         (let ((key (first clause)))
                           (and (symbolp key)
                                (eq key t)))))
                     clauses))

             (check-expr (expr line column)
               "Recursively check expression for missing otherwise."
               (when (consp expr)
                 (let ((head (first expr))
                       (rest-args (rest expr)))
                   ;; Check if this is a CASE or TYPECASE form
                   (when (and (symbolp head)
                              (member (symbol-name head)
                                      '("CASE" "TYPECASE")
                                      :test #'string=))
                     (let* ((form-name (symbol-name head))
                            (keyform (first rest-args))
                            (clauses (rest rest-args)))
                       (declare (ignore keyform))

                       ;; Check if it has T instead of OTHERWISE
                       (when (has-t-clause-p clauses)
                         (push (make-instance 'violation:violation
                                              :rule :missing-otherwise
                                              :file file
                                              :line line
                                              :column column
                                              :severity (rule-severity rule)
                                              :message
                                              (format nil
                                                      "Use 'otherwise' instead of 't' in '~A'"
                                                      (string-downcase form-name))
                                              :fix nil)
                               violations))

                       ;; Check if it has OTHERWISE clause
                       (unless (or (has-otherwise-clause-p clauses)
                                   (has-t-clause-p clauses))
                         (push (make-instance 'violation:violation
                                              :rule :missing-otherwise
                                              :file file
                                              :line line
                                              :column column
                                              :severity (rule-severity rule)
                                              :message
                                              (format nil "'~A' should have 'otherwise' clause"
                                                      (string-downcase form-name))
                                              :fix nil)
                               violations))))

                   ;; Recursively check nested forms
                   (dolist (subexpr rest-args)
                     (check-expr subexpr line column))))))

      ;; Start checking from the form's expression
      (check-expr (parser:form-expr form)
                  (parser:form-line form)
                  (parser:form-column form)))

    (nreverse violations)))

;;; Wrong-otherwise rule

(defclass wrong-otherwise-rule (rule)
  ()
  (:default-initargs
   :name :wrong-otherwise
   :description "ecase/etypecase should not have 'otherwise' or 't' clause"
   :severity :error
   :type :form)
  (:documentation "Rule to detect otherwise/t in ecase/etypecase."))

(defmethod check-form ((rule wrong-otherwise-rule) form file)
  "Check that ECASE and ETYPECASE forms don't have otherwise or t clause."
  (check-type form parser:form)
  (check-type file pathname)

  (let ((violations '()))
    (labels ((has-catch-all-clause-p (clauses)
               "Check if clauses list has an otherwise or t clause."
               (some (lambda (clause)
                       (when (consp clause)
                         (let ((key (first clause)))
                           (and (symbolp key)
                                (or (string= (symbol-name key) "OTHERWISE")
                                    (eq key t))))))
                     clauses))

             (check-expr (expr line column)
               "Recursively check expression for wrong otherwise."
               (when (consp expr)
                 (let ((head (first expr))
                       (rest-args (rest expr)))
                   ;; Check if this is an ECASE or ETYPECASE form
                   (when (and (symbolp head)
                              (member (symbol-name head)
                                      '("ECASE" "ETYPECASE")
                                      :test #'string=))
                     (let* ((form-name (symbol-name head))
                            (keyform (first rest-args))
                            (clauses (rest rest-args)))
                       (declare (ignore keyform))

                       ;; Check if it has OTHERWISE or T clause
                       (when (has-catch-all-clause-p clauses)
                         (push (make-instance 'violation:violation
                                              :rule :wrong-otherwise
                                              :file file
                                              :line line
                                              :column column
                                              :severity (rule-severity rule)
                                              :message
                                              (format nil
                                                      "'~A' should not have 'otherwise' or 't' clause"
                                                      (string-downcase form-name))
                                              :fix nil)
                               violations))))

                   ;; Recursively check nested forms
                   (dolist (subexpr rest-args)
                     (check-expr subexpr line column))))))

      ;; Start checking from the form's expression
      (check-expr (parser:form-expr form)
                  (parser:form-line form)
                  (parser:form-column form)))

    (nreverse violations)))
