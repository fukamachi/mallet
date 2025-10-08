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
           #:trailing-whitespace-rule
           #:no-tabs-rule
           #:final-newline-rule
           #:consecutive-blank-lines-rule
           #:comment-level-rule
           #:if-without-else-rule
           #:bare-progn-in-if-rule
           #:missing-otherwise-rule
           #:wrong-otherwise-rule
           #:unused-variables-rule
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

;;; Helper functions for string-based symbol handling

(defun symbol-name-from-string (str)
  "Extract the symbol name from a string representation.
Handles qualified symbols like \"PACKAGE:NAME\" → \"NAME\"
and unqualified symbols like \"NAME\" → \"NAME\"."
  (if (stringp str)
      (let ((colon-pos (position #\: str :from-end t)))
        (if colon-pos
            (subseq str (1+ colon-pos))
            str))
      str))

(defun symbol-matches-p (str name)
  "Check if string-based symbol STR matches NAME (case-insensitive).
Works with both qualified (\"PACKAGE:IF\") and unqualified (\"IF\") symbols."
  (and (stringp str)
       (string-equal (symbol-name-from-string str) name)))

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
        (max-length (line-length-rule-max-length rule)))

    (with-input-from-string (stream text)
      (loop for line-number from 0
            for line = (read-line stream nil nil)
            while line
            do (let ((line-len (length line)))
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

;;; Trailing whitespace rule

(defclass trailing-whitespace-rule (rule)
  ()
  (:default-initargs
   :name :trailing-whitespace
   :description "Lines should not have trailing whitespace"
   :severity :warning
   :type :text)
  (:documentation "Rule to check for trailing whitespace on lines."))

(defmethod check-text ((rule trailing-whitespace-rule) text file)
  "Check that lines in TEXT do not have trailing whitespace."
  (check-type text string)
  (check-type file pathname)

  (let ((violations '()))

    (with-input-from-string (stream text)
      (loop for line-number from 0
            for line = (read-line stream nil nil)
            while line do
              ;; Check if line ends with whitespace (space or tab)
              (when (and (plusp (length line))
                         (member (char line (1- (length line))) '(#\Space #\Tab)))
                (push (make-instance 'violation:violation
                                     :rule :trailing-whitespace
                                     :file file
                                     :line line-number
                                     :column 0
                                     :severity (rule-severity rule)
                                     :message "Line has trailing whitespace"
                                     :fix nil)
                      violations))))

    (nreverse violations)))

;;; No tabs rule

(defclass no-tabs-rule (rule)
  ()
  (:default-initargs
   :name :no-tabs
   :description "Use spaces instead of tab characters"
   :severity :warning
   :type :text)
  (:documentation "Rule to check for tab characters."))

(defmethod check-text ((rule no-tabs-rule) text file)
  "Check that TEXT does not contain tab characters."
  (check-type text string)
  (check-type file pathname)

  (let ((violations '()))

    (with-input-from-string (stream text)
      (loop for line-number from 0
            for line = (read-line stream nil nil)
            while line
            do ;; Check if line contains tab character
              (when (find #\Tab line)
                (push (make-instance 'violation:violation
                                     :rule :no-tabs
                                     :file file
                                     :line line-number
                                     :column 0
                                     :severity (rule-severity rule)
                                     :message "Tab character found (use spaces instead)"
                                     :fix nil)
                      violations))))

    (nreverse violations)))

;;; Final newline rule

(defclass final-newline-rule (rule)
  ()
  (:default-initargs
   :name :final-newline
   :description "Files must end with a newline"
   :severity :warning
   :type :text)
  (:documentation "Rule to check that files end with a newline."))

(defmethod check-text ((rule final-newline-rule) text file)
  "Check that TEXT ends with a newline character."
  (check-type text string)
  (check-type file pathname)

  (let ((violations '()))
    ;; Empty files are OK
    (when (and (plusp (length text))
               (not (char= (char text (1- (length text))) #\Newline)))
      (push (make-instance 'violation:violation
                           :rule :final-newline
                           :file file
                           :line (count #\Newline text)
                           :column 0
                           :severity (rule-severity rule)
                           :message "File must end with a newline"
                           :fix nil)
            violations))

    violations))

;;; Consecutive blank lines rule

(defclass consecutive-blank-lines-rule (rule)
  ((max-consecutive
    :initarg :max-consecutive
    :initform 2
    :reader consecutive-blank-lines-rule-max
    :type (integer 1)
    :documentation "Maximum allowed consecutive blank lines"))
  (:default-initargs
   :name :consecutive-blank-lines
   :description "Limit consecutive blank lines"
   :severity :warning
   :type :text)
  (:documentation "Rule to check for too many consecutive blank lines."))

(defmethod check-text ((rule consecutive-blank-lines-rule) text file)
  "Check that TEXT does not have too many consecutive blank lines."
  (check-type text string)
  (check-type file pathname)

  (let ((violations '())
        (blank-count 0)
        (max-consecutive (consecutive-blank-lines-rule-max rule))
        (violation-line nil))

    (with-input-from-string (stream text)
      (loop for line-number from 0
            for line = (read-line stream nil nil)
            while line
            do ;; A line is blank if it's empty or contains only whitespace
              (if (or (zerop (length line))
                      (every (lambda (ch) (member ch '(#\Space #\Tab))) line))
                  (progn
                    (incf blank-count)
                    (when (and (null violation-line)
                               (> blank-count max-consecutive))
                      ;; Mark where the violation starts (first excessive blank line)
                      (setf violation-line line-number)))
                  (progn
                    ;; Non-blank line, check if we had a violation
                    (when violation-line
                      (push (make-instance 'violation:violation
                                           :rule :consecutive-blank-lines
                                           :file file
                                           :line violation-line
                                           :column 0
                                           :severity (rule-severity rule)
                                           :message (format nil "More than ~A consecutive blank lines" max-consecutive)
                                           :fix nil)
                            violations)
                      (setf violation-line nil))
                    ;; Reset counter
                    (setf blank-count 0)))))

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
                   (when (symbol-matches-p head "IF")
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
                   (when (listp rest-args)
                     (dolist (subexpr rest-args)
                       (check-expr subexpr line column)))))))

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
                    (symbol-matches-p (first expr) "PROGN")))

             (check-expr (expr line column)
               "Recursively check expression for bare progn in if."
               (when (consp expr)
                 (let ((head (first expr))
                       (rest-args (rest expr)))
                   ;; Check if this is an IF form
                   (when (symbol-matches-p head "IF")
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
                   (when (listp rest-args)
                     (dolist (subexpr rest-args)
                       (check-expr subexpr line column)))))))

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
                           (symbol-matches-p key "OTHERWISE"))))
                     clauses))

             (has-t-clause-p (clauses)
               "Check if clauses list has a T clause (should use otherwise)."
               (some (lambda (clause)
                       (when (consp clause)
                         (let ((key (first clause)))
                           (symbol-matches-p key "T"))))
                     clauses))

             (check-expr (expr line column)
               "Recursively check expression for missing otherwise."
               (when (consp expr)
                 (let ((head (first expr))
                       (rest-args (rest expr)))
                   ;; Check if this is a CASE or TYPECASE form
                   (when (and (stringp head)
                              (member (symbol-name-from-string head)
                                      '("CASE" "TYPECASE")
                                      :test #'string-equal))
                     (let* ((form-name (symbol-name-from-string head))
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
                   (when (listp rest-args)
                     (dolist (subexpr rest-args)
                       (check-expr subexpr line column)))))))

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
                           (or (symbol-matches-p key "OTHERWISE")
                               (symbol-matches-p key "T")))))
                     clauses))

             (check-expr (expr line column)
               "Recursively check expression for wrong otherwise."
               (when (consp expr)
                 (let ((head (first expr))
                       (rest-args (rest expr)))
                   ;; Check if this is an ECASE or ETYPECASE form
                   (when (and (stringp head)
                              (member (symbol-name-from-string head)
                                      '("ECASE" "ETYPECASE")
                                      :test #'string-equal))
                     (let* ((form-name (symbol-name-from-string head))
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
                   (when (listp rest-args)
                     (dolist (subexpr rest-args)
                       (check-expr subexpr line column)))))))

      ;; Start checking from the form's expression
      (check-expr (parser:form-expr form)
                  (parser:form-line form)
                  (parser:form-column form)))

    (nreverse violations)))

;;; Unused-variables rule

(defclass unused-variables-rule (rule)
  ()
  (:default-initargs
   :name :unused-variables
   :description "Variables should be used or explicitly ignored"
   :severity :warning
   :type :form)
  (:documentation "Rule to detect unused variables in bindings."))

(defmethod check-form ((rule unused-variables-rule) form file)
  "Check that all bound variables are either used or explicitly ignored."
  (check-type form parser:form)
  (check-type file pathname)

  (let ((violations '()))
    (labels ((ignored-var-p (var-name ignored-vars)
               "Check if variable should be ignored (in ignore list or starts with _)."
               (let ((name (symbol-name-from-string var-name)))
                 (or (member name ignored-vars :test #'string=)
                     (and (> (length name) 0)
                          (char= (char name 0) #\_)))))

             (extract-bindings (binding-form)
               "Extract variable names from binding form (supports simple and nested destructuring)."
               (labels ((extract-from-pattern (pattern)
                          "Recursively extract all variable names from a destructuring pattern."
                          (cond
                            ;; Simple variable
                            ((stringp pattern)
                             (if (and (> (length pattern) 0)
                                      (char= (char pattern 0) #\&))
                                 nil  ; Skip lambda-list keywords
                                 (list pattern)))
                            ;; Dotted pair - extract both parts
                            ((and (consp pattern) (stringp (cdr pattern)))
                             (append (extract-from-pattern (car pattern))
                                     (extract-from-pattern (cdr pattern))))
                            ;; Proper list - recurse on each element
                            ((consp pattern)
                             (append (extract-from-pattern (car pattern))
                                     (when (cdr pattern)
                                       (extract-from-pattern (cdr pattern)))))
                            ;; Anything else (NIL, numbers, etc.)
                            (t nil))))
                 (cond
                   ;; Simple string (variable name)
                   ((stringp binding-form)
                    (list binding-form))
                   ;; (var value) form - extract from var part only
                   ((and (consp binding-form)
                         (not (null binding-form))
                         (not (null (cdr binding-form)))
                         (not (cddr binding-form)))  ; exactly 2 elements
                    (extract-from-pattern (first binding-form)))
                   ;; Other cases - try to extract from the whole pattern
                   ((consp binding-form)
                    (extract-from-pattern binding-form))
                   (t nil))))

             (parse-loop-clauses (clauses)
               "Parse LOOP clauses and extract variable bindings and body.
Returns (values bindings body-clauses)."
               (let ((bindings '())
                     (body-start nil)
                     (i 0))
                 ;; First pass: collect bindings and find where body starts
                 (loop while (< i (length clauses))
                       for clause = (nth i clauses)
                       do (cond
                            ;; FOR/AS/WITH - binding clauses
                            ((and (stringp clause)
                                  (member (symbol-name-from-string clause)
                                          '("FOR" "AS" "WITH")
                                          :test #'string-equal))
                             ;; Next element is the variable or pattern
                             (when (< (1+ i) (length clauses))
                               (let ((var-or-pattern (nth (1+ i) clauses)))
                                 (when var-or-pattern
                                   (push (list var-or-pattern) bindings)
                                   (incf i))))  ; Skip the variable
                             ;; Skip the rest of the binding clause (from, to, in, =, etc.)
                             ;; Keep advancing until we hit another keyword or body keyword
                             (loop while (and (< (1+ i) (length clauses))
                                              (let ((next (nth (1+ i) clauses)))
                                                (not (and (stringp next)
                                                          (member (symbol-name-from-string next)
                                                                  '("FOR" "AS" "WITH" "DO" "COLLECT" "APPEND"
                                                                    "NCONC" "COUNT" "SUM" "MAXIMIZE" "MINIMIZE"
                                                                    "WHEN" "UNLESS" "IF" "WHILE" "UNTIL" "REPEAT"
                                                                    "ALWAYS" "NEVER" "THEREIS" "RETURN")
                                                                  :test #'string-equal)))))
                                   do (incf i)))
                            ;; Body keywords - mark body start if not already marked
                            ((and (stringp clause)
                                  (member (symbol-name-from-string clause)
                                          '("DO" "COLLECT" "APPEND" "NCONC" "COUNT" "SUM"
                                            "MAXIMIZE" "MINIMIZE" "WHEN" "UNLESS" "IF"
                                            "WHILE" "UNTIL" "REPEAT" "ALWAYS" "NEVER" "THEREIS" "RETURN")
                                          :test #'string-equal))
                             (unless body-start
                               (setf body-start i))))
                          (incf i))
                 (values (nreverse bindings)
                         (if body-start
                             (nthcdr body-start clauses)
                             '()))))

             (find-references (var-name body)
               "Find if VAR-NAME is referenced in BODY, respecting variable shadowing."
               (let ((target-name (symbol-name-from-string var-name)))
                 (labels ((binds-same-name-p (binding-form)
                            "Check if BINDING-FORM introduces a variable with target-name."
                            (let ((vars (extract-bindings binding-form)))
                              (some (lambda (v)
                                      (string-equal (symbol-name-from-string v) target-name))
                                    vars)))

                          (search-expr (expr)
                            (cond
                              ((null expr) nil)

                              ;; String matching our variable is a reference
                              ((stringp expr)
                               (string-equal (symbol-name-from-string expr) target-name))

                              ;; Check if it's a binding form that might shadow
                              ((consp expr)
                               (let ((head (first expr))
                                     (rest-args (rest expr)))
                                 (cond
                                   ;; LET and LET* - check bindings for shadowing
                                   ((and (stringp head)
                                         (or (string-equal (symbol-name-from-string head) "LET")
                                             (string-equal (symbol-name-from-string head) "LET*")))
                                    (if (>= (length rest-args) 2)
                                        (let ((bindings (first rest-args))
                                              (body (rest rest-args)))
                                          (if (and (listp bindings)
                                                   (some #'binds-same-name-p bindings))
                                              ;; Variable is shadowed - search init forms but not body
                                              (some (lambda (binding)
                                                      (when (consp binding)
                                                        (search-expr (second binding))))  ; init form
                                                    bindings)
                                              ;; Not shadowed, search everything
                                              (or (search-expr rest-args))))
                                        (search-expr rest-args)))

                                   ;; DEFUN, LAMBDA, DEFMACRO - check parameters
                                   ((and (stringp head)
                                         (or (string-equal (symbol-name-from-string head) "DEFUN")
                                             (string-equal (symbol-name-from-string head) "LAMBDA")
                                             (string-equal (symbol-name-from-string head) "DEFMACRO")))
                                    (let* ((lambda-list-pos (if (string-equal (symbol-name-from-string head) "LAMBDA") 0 1))
                                           (lambda-list (when (> (length rest-args) lambda-list-pos)
                                                          (nth lambda-list-pos rest-args))))
                                      (if (and (listp lambda-list)
                                               (some (lambda (param)
                                                       (and (stringp param)
                                                            (not (char= (char (symbol-name-from-string param) 0) #\&))
                                                            (string-equal (symbol-name-from-string param) target-name)))
                                                     lambda-list))
                                          nil  ; Shadowed by parameter
                                          (search-expr rest-args))))

                                   ;; DESTRUCTURING-BIND, MULTIPLE-VALUE-BIND
                                   ((and (stringp head)
                                         (or (string-equal (symbol-name-from-string head) "DESTRUCTURING-BIND")
                                             (string-equal (symbol-name-from-string head) "MULTIPLE-VALUE-BIND")))
                                    (if (>= (length rest-args) 2)
                                        (let ((vars (first rest-args)))
                                          (if (and (listp vars)
                                                   (some (lambda (v)
                                                           (and (stringp v)
                                                                (string-equal (symbol-name-from-string v) target-name)))
                                                         vars))
                                              nil  ; Shadowed
                                              (search-expr rest-args)))
                                        (search-expr rest-args)))

                                   ;; DOLIST, DOTIMES
                                   ((and (stringp head)
                                         (or (string-equal (symbol-name-from-string head) "DOLIST")
                                             (string-equal (symbol-name-from-string head) "DOTIMES")))
                                    (if (>= (length rest-args) 1)
                                        (let* ((spec (first rest-args))
                                               (var (when (listp spec) (first spec))))
                                          (if (and (stringp var)
                                                   (string-equal (symbol-name-from-string var) target-name))
                                              nil  ; Shadowed
                                              (search-expr rest-args)))
                                        (search-expr rest-args)))

                                   ;; DO
                                   ((and (stringp head)
                                         (string-equal (symbol-name-from-string head) "DO"))
                                    (if (>= (length rest-args) 1)
                                        (let ((var-clauses (first rest-args)))
                                          (if (and (listp var-clauses)
                                                   (some #'binds-same-name-p var-clauses))
                                              nil  ; Shadowed
                                              (search-expr rest-args)))
                                        (search-expr rest-args)))

                                   ;; LOOP - check for FOR/AS/WITH variables
                                   ((and (stringp head)
                                         (string-equal (symbol-name-from-string head) "LOOP"))
                                    ;; Use parse-loop-clauses to extract all bindings (supports destructuring)
                                    (multiple-value-bind (bindings body)
                                        (parse-loop-clauses rest-args)
                                      (declare (ignore body))
                                      (let ((shadowed (some #'binds-same-name-p bindings)))
                                        (if shadowed
                                            nil
                                            (search-expr rest-args)))))

                                   ;; QUASIQUOTE - only search unquoted expressions
                                   ((or (eq head 'eclector.reader:quasiquote)
                                        (and (symbolp head)
                                             (string-equal (symbol-name head) "QUASIQUOTE")
                                             (string-equal (package-name (symbol-package head)) "ECLECTOR.READER")))
                                    ;; Search only in UNQUOTE and UNQUOTE-SPLICING
                                    (labels ((search-quasi (expr)
                                               (cond
                                                 ((null expr) nil)
                                                 ((stringp expr) nil)  ; Quoted symbols don't count
                                                 ((consp expr)
                                                  (let ((h (first expr)))
                                                    (cond
                                                      ;; UNQUOTE - search the unquoted expression
                                                      ((or (eq h 'eclector.reader:unquote)
                                                           (and (symbolp h)
                                                                (string-equal (symbol-name h) "UNQUOTE")
                                                                (string-equal (package-name (symbol-package h)) "ECLECTOR.READER")))
                                                       (when (rest expr)
                                                         (search-expr (second expr))))
                                                      ;; UNQUOTE-SPLICING - search the unquoted expression
                                                      ((or (eq h 'eclector.reader:unquote-splicing)
                                                           (and (symbolp h)
                                                                (string-equal (symbol-name h) "UNQUOTE-SPLICING")
                                                                (string-equal (package-name (symbol-package h)) "ECLECTOR.READER")))
                                                       (when (rest expr)
                                                         (search-expr (second expr))))
                                                      ;; Other forms - recursively search for unquotes
                                                      (t
                                                       (or (search-quasi (car expr))
                                                           (search-quasi (cdr expr)))))))
                                                 (t nil))))
                                      (some #'search-quasi rest-args)))

                                   ;; Default: search recursively
                                   (t
                                    (or (search-expr (car expr))
                                        (search-expr (cdr expr)))))))

                              (t nil))))
                   (some #'search-expr body))))

             (extract-ignored-vars (body)
               "Extract variable names from (declare (ignore ...)) forms."
               (let ((ignored '()))
                 (dolist (form body)
                   (when (and (consp form)
                              (symbol-matches-p (first form) "DECLARE"))
                     (when (listp (rest form))
                       (dolist (decl-spec (rest form))
                         (when (and (consp decl-spec)
                                    (symbol-matches-p (first decl-spec) "IGNORE"))
                           (when (listp (rest decl-spec))
                             (dolist (var (rest decl-spec))
                               (when (stringp var)
                                 (push (symbol-name-from-string var) ignored)))))))))
                 ignored))

             (parse-lambda-list (lambda-list)
               "Parse lambda list into regular params and &aux params.
Returns (values regular-params aux-params)."
               (let ((aux-pos (position-if (lambda (x)
                                             (and (stringp x)
                                                  (string-equal (symbol-name-from-string x) "&AUX")))
                                           lambda-list)))
                 (if aux-pos
                     (values (subseq lambda-list 0 aux-pos)
                             (subseq lambda-list (1+ aux-pos)))
                     (values lambda-list nil))))

             (calculate-scope (form-type binding remaining-bindings body aux-context)
               "Calculate the scope where a variable binding is available.
Returns a list of forms where the variable should be checked for usage."
               (ecase form-type
                 (:let
                  ;; Parallel bindings - scope is just the body
                  body)
                 (:let*
                  ;; Sequential bindings - scope is subsequent bindings' init forms + body
                  (append (mapcar (lambda (b)
                                    (if (consp b)
                                        (second b)
                                        nil))
                                  remaining-bindings)
                          body))
                 (:defun-regular
                  ;; Regular parameters - scope is body (used for non-&aux params)
                  body)
                 (:defun-aux
                  ;; &aux parameters - evaluated sequentially like LET*
                  ;; Scope is subsequent &aux bindings' init forms + body
                  (append (mapcar (lambda (b)
                                    (if (consp b)
                                        (second b)
                                        nil))
                                  remaining-bindings)
                          aux-context))  ; aux-context contains the body
                 (:do
                  ;; DO variables - scope is other variables' step forms + test + result + body
                  ;; aux-context is (var-clauses test-form result-form . body-forms)
                  ;; First element is the original var-clauses for extracting step forms
                  (let ((var-clauses (first aux-context))
                        (rest-context (rest aux-context)))
                    (append
                     ;; All step forms (including this variable's own - we'll filter later)
                     (when (listp var-clauses)
                       (mapcar (lambda (clause)
                                 (when (and (consp clause) (>= (length clause) 3))
                                   (third clause)))  ; step form
                               var-clauses))
                     ;; Test, result, and body
                     rest-context)))))

             (check-binding-form (form-type bindings body fallback-line fallback-column position-map &optional aux-context message-prefix)
               "Check bindings for unused variables using scope-based approach.
FORM-TYPE determines how scope is calculated (:let, :let*, :defun-regular, :defun-aux, :do).
MESSAGE-PREFIX is the prefix for violation messages (default 'Variable')."
               (let ((ignored-vars (extract-ignored-vars body))
                     (body-without-declares (remove-if
                                             (lambda (form)
                                               (and (consp form)
                                                    (symbol-matches-p (first form) "DECLARE")))
                                             body)))
                 (loop for remaining-bindings on bindings
                       for binding = (first remaining-bindings)
                       for subsequent-bindings = (rest remaining-bindings)
                       do (let ((var-names (extract-bindings binding)))
                            (dolist (var-name var-names)
                              (let ((scope (if (eq form-type :do)
                                               ;; Special handling for DO: exclude own step form
                                               ;; aux-context is (var-clauses test result . body)
                                               (let* ((var-clauses (first aux-context))
                                                      (rest-context (rest aux-context))
                                                      (var-index (position binding bindings))
                                                      (own-clause (when (and var-index var-clauses)
                                                                    (nth var-index var-clauses)))
                                                      (own-step-form (when (and (consp own-clause) (>= (length own-clause) 3))
                                                                       (third own-clause)))
                                                      (all-step-forms (when (listp var-clauses)
                                                                        (mapcar (lambda (clause)
                                                                                  (when (and (consp clause) (>= (length clause) 3))
                                                                                    (third clause)))
                                                                                var-clauses)))
                                                      (other-step-forms (remove own-step-form all-step-forms)))
                                                 (append other-step-forms rest-context))
                                               ;; Normal scope calculation for other forms
                                               (calculate-scope form-type binding subsequent-bindings body-without-declares aux-context))))
                                (unless (or (ignored-var-p var-name ignored-vars)
                                            (find-references var-name scope))
                                  ;; Find position for this binding or variable name
                                  (multiple-value-bind (var-line var-column)
                                      (if position-map
                                          (parser:find-position binding position-map fallback-line fallback-column)
                                          (values fallback-line fallback-column))
                                    (push (make-instance 'violation:violation
                                                         :rule :unused-variables
                                                         :file file
                                                         :line var-line
                                                         :column var-column
                                                         :severity (rule-severity rule)
                                                         :message
                                                         (format nil "~A '~A' is unused"
                                                                 (or message-prefix "Variable")
                                                                 (symbol-name-from-string var-name))
                                                         :fix nil)
                                          violations)))))))))

             (check-expr (expr line column position-map)
               "Recursively check expression for unused variables."
               (when (consp expr)
                 (let ((head (first expr))
                       (rest-args (rest expr)))
                   ;; Check DEFUN
                   (when (symbol-matches-p head "DEFUN")
                     (when (>= (length rest-args) 3)
                       (let* ((lambda-list (second rest-args))
                              (body (cddr rest-args)))
                         (multiple-value-bind (regular-params aux-params)
                             (parse-lambda-list lambda-list)
                           ;; Check regular parameters (excluding &optional, &key, &rest markers)
                           (let ((param-bindings (loop for param in regular-params
                                                       when (and (stringp param)
                                                                 (not (char= (char (symbol-name-from-string param) 0) #\&)))
                                                       collect (list param))))
                             (when param-bindings
                               (check-binding-form :defun-regular param-bindings body line column position-map)))
                           ;; Check &aux parameters (sequential like LET*)
                           (when aux-params
                             (check-binding-form :defun-aux aux-params body line column position-map body))))))

                   ;; Check LAMBDA
                   (when (symbol-matches-p head "LAMBDA")
                     (when (>= (length rest-args) 2)
                       (let* ((lambda-list (first rest-args))
                              (body (rest rest-args)))
                         (multiple-value-bind (regular-params aux-params)
                             (parse-lambda-list lambda-list)
                           ;; Check regular parameters (excluding &optional, &key, &rest markers)
                           (let ((param-bindings (loop for param in regular-params
                                                       when (and (stringp param)
                                                                 (not (char= (char (symbol-name-from-string param) 0) #\&)))
                                                       collect (list param))))
                             (when param-bindings
                               (check-binding-form :defun-regular param-bindings body line column position-map)))
                           ;; Check &aux parameters (sequential like LET*)
                           (when aux-params
                             (check-binding-form :defun-aux aux-params body line column position-map body))))))

                   ;; Check LET
                   (when (and (stringp head)
                              (string-equal (symbol-name-from-string head) "LET"))
                     (when (>= (length rest-args) 2)
                       (let ((bindings (first rest-args))
                             (body (rest rest-args)))
                         (check-binding-form :let bindings body line column position-map))))

                   ;; Check LET* (sequential bindings)
                   (when (and (stringp head)
                              (string-equal (symbol-name-from-string head) "LET*"))
                     (when (>= (length rest-args) 2)
                       (let ((bindings (first rest-args))
                             (body (rest rest-args)))
                         (check-binding-form :let* bindings body line column position-map))))

                   ;; Check DESTRUCTURING-BIND (parallel bindings like LET)
                   (when (symbol-matches-p head "DESTRUCTURING-BIND")
                     (when (>= (length rest-args) 3)
                       (let* ((lambda-list (first rest-args))
                              ;; Second arg is the value form, skip it
                              (body (cddr rest-args))
                              ;; Convert lambda-list to binding format for check-binding-form
                              (bindings (if (listp lambda-list)
                                            (mapcar #'list lambda-list)
                                            (list (list lambda-list)))))
                         (check-binding-form :let bindings body line column position-map))))

                   ;; Check MULTIPLE-VALUE-BIND (parallel bindings like LET)
                   (when (symbol-matches-p head "MULTIPLE-VALUE-BIND")
                     (when (>= (length rest-args) 3)
                       (let* ((vars (first rest-args))
                              ;; Second arg is the values form, skip it
                              (body (cddr rest-args))
                              ;; Convert var list to binding format
                              (bindings (if (listp vars)
                                            (mapcar #'list vars)
                                            (list (list vars)))))
                         (check-binding-form :let bindings body line column position-map))))

                   ;; Check DEFMACRO (like DEFUN with &aux support)
                   (when (symbol-matches-p head "DEFMACRO")
                     (when (>= (length rest-args) 3)
                       (let* ((lambda-list (second rest-args))
                              (body (cddr rest-args)))
                         (multiple-value-bind (regular-params aux-params)
                             (parse-lambda-list lambda-list)
                           ;; Check regular parameters (excluding &optional, &key, &rest, &body markers)
                           (let ((param-bindings (loop for param in regular-params
                                                       when (and (stringp param)
                                                                 (not (char= (char (symbol-name-from-string param) 0) #\&)))
                                                       collect (list param))))
                             (when param-bindings
                               (check-binding-form :defun-regular param-bindings body line column position-map)))
                           ;; Check &aux parameters (sequential like LET*)
                           (when aux-params
                             (check-binding-form :defun-aux aux-params body line column position-map body))))))

                   ;; Check DOLIST
                   (when (symbol-matches-p head "DOLIST")
                     (when (>= (length rest-args) 2)
                       (let* ((spec (first rest-args))
                              (body (rest rest-args)))
                         (when (and (listp spec) (>= (length spec) 2))
                           (let* ((var (first spec))
                                  (bindings (list (list var))))
                             (check-binding-form :let bindings body line column position-map))))))

                   ;; Check DOTIMES
                   (when (symbol-matches-p head "DOTIMES")
                     (when (>= (length rest-args) 2)
                       (let* ((spec (first rest-args))
                              (body (rest rest-args)))
                         (when (and (listp spec) (>= (length spec) 2))
                           (let* ((var (first spec))
                                  (bindings (list (list var))))
                             (check-binding-form :let bindings body line column position-map))))))

                   ;; Check DO
                   (when (symbol-matches-p head "DO")
                     (when (>= (length rest-args) 2)
                       (let* ((var-clauses (first rest-args))
                              (end-test-clause (second rest-args))
                              (body (cddr rest-args))
                              ;; end-test-clause is (test-form [result-forms...])
                              (test-form (when (listp end-test-clause) (first end-test-clause)))
                              (result-forms (when (listp end-test-clause) (rest end-test-clause)))
                              ;; aux-context for DO contains: var-clauses, test, result, and body
                              ;; First element is the original var-clauses for scope calculation
                              (do-context (cons var-clauses (append (list test-form) result-forms body)))
                              ;; DO variable clauses are (var init [step])
                              ;; Extract only the variable names for check-binding-form
                              (bindings (when (listp var-clauses)
                                          (mapcar (lambda (clause)
                                                    (if (consp clause)
                                                        (list (first clause))  ; Just the var name
                                                        (list clause)))  ; Simple var
                                                  var-clauses))))
                         (when bindings
                           (check-binding-form :do bindings body line column position-map do-context)))))

                   ;; Check LOOP
                   (when (symbol-matches-p head "LOOP")
                     (multiple-value-bind (bindings body)
                         (parse-loop-clauses rest-args)
                       (when bindings
                         ;; All LOOP variables are in scope for body only (not binding clauses)
                         ;; Treat as parallel bindings like LET
                         (check-binding-form :let bindings body line column position-map))))

                   ;; Check QUASIQUOTE (backquote) - only check unquoted parts
                   (when (or (eq head 'eclector.reader:quasiquote)
                             (and (symbolp head)
                                  (string-equal (symbol-name head) "QUASIQUOTE")
                                  (string-equal (package-name (symbol-package head)) "ECLECTOR.READER")))
                     ;; For backquoted forms, only check UNQUOTE and UNQUOTE-SPLICING subforms
                     (labels ((check-quasi (expr)
                                (when (consp expr)
                                  (let ((h (first expr)))
                                    (cond
                                      ;; UNQUOTE - check the unquoted expression
                                      ((or (eq h 'eclector.reader:unquote)
                                           (and (symbolp h)
                                                (string-equal (symbol-name h) "UNQUOTE")
                                                (string-equal (package-name (symbol-package h)) "ECLECTOR.READER")))
                                       (when (rest expr)
                                         (check-expr (second expr) line column position-map)))
                                      ;; UNQUOTE-SPLICING - check the unquoted expression
                                      ((or (eq h 'eclector.reader:unquote-splicing)
                                           (and (symbolp h)
                                                (string-equal (symbol-name h) "UNQUOTE-SPLICING")
                                                (string-equal (package-name (symbol-package h)) "ECLECTOR.READER")))
                                       (when (rest expr)
                                         (check-expr (second expr) line column position-map)))
                                      ;; Other forms - recursively look for unquotes
                                      (t
                                       (dolist (subexpr expr)
                                         (check-quasi subexpr))))))))
                       (dolist (arg rest-args)
                         (check-quasi arg))))

                   ;; Recursively check nested forms (skip if we already handled it above)
                   (unless (or (eq head 'eclector.reader:quasiquote)
                               (and (symbolp head)
                                    (string-equal (symbol-name head) "QUASIQUOTE")
                                    (string-equal (package-name (symbol-package head)) "ECLECTOR.READER")))
                     (when (listp rest-args)
                       (dolist (subexpr rest-args)
                         (when (consp subexpr)
                           (check-expr subexpr line column position-map)))))))))

      ;; Start checking from the form's expression with position-map
      (let ((position-map (parser:form-position-map form)))
        (check-expr (parser:form-expr form)
                    (parser:form-line form)
                    (parser:form-column form)
                    position-map)))

    (nreverse violations)))
