(defpackage #:mallet/rules/forms/metrics
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation))
  (:export #:function-length-rule
           #:cyclomatic-complexity-rule))
(in-package #:mallet/rules/forms/metrics)

;;; Function-length rule

(defclass function-length-rule (base:rule)
  ((max-lines
    :initarg :max-lines
    :initform 50
    :accessor max-lines))
  (:default-initargs
   :name :function-length
   :description "Function exceeds maximum length"
   :severity :info
   :type :form))

(defmethod base:check-form ((rule function-length-rule) form file)
  "Check function length."
  (check-type form parser:form)
  (check-type file pathname)

  ;; Call recursive checker
  (base:check-form-recursive rule
                             (parser:form-expr form)
                             file
                             (parser:form-line form)
                             (parser:form-column form)
                             nil
                             (parser:form-position-map form)))

(defmethod base:check-form-recursive ((rule function-length-rule) expr file line column
                                      &optional function-name position-map)
  (declare (ignore function-name))

  (let ((violations '())
        (visited (make-hash-table :test 'eq)))

    (labels ((check-expr (current-expr fallback-line fallback-column)
               (base:with-safe-cons-expr (current-expr visited)
                 (multiple-value-bind (actual-line actual-column)
                     (base:find-actual-position current-expr position-map
                                                fallback-line fallback-column)
                   (let ((head (first current-expr)))

                     ;; Check if this is a function definition
                     (when (is-function-definition-p head)
                       (let ((length (calculate-function-length
                                      current-expr position-map
                                      actual-line actual-column)))
                         (when (and (> length (max-lines rule))
                                    (base:should-create-violation-p rule))
                           (push (make-function-length-violation
                                  rule file actual-line actual-column
                                  length (get-function-name current-expr))
                                 violations))))

                     ;; Recurse into nested forms
                     ;; Special handling for flet/labels to check inner functions
                     (cond
                       ;; flet/labels: check both inner functions and body
                       ((or (base:symbol-matches-p head "FLET")
                            (base:symbol-matches-p head "LABELS"))
                        (when (and (consp (rest current-expr))
                                   (a:proper-list-p (second current-expr)))
                          ;; Check each inner function definition
                          ;; Inner functions have structure: (name lambda-list . body)
                          (dolist (func-def (second current-expr))
                            (when (consp func-def)
                              (multiple-value-bind (func-line func-column)
                                  (base:find-actual-position func-def position-map
                                                             actual-line actual-column)
                                (let ((length (calculate-function-length
                                               func-def position-map
                                               func-line func-column))
                                      (func-name (get-inner-function-name func-def)))
                                  (when (and (> length (max-lines rule))
                                             (base:should-create-violation-p rule))
                                    (push (make-function-length-violation
                                           rule file func-line func-column
                                           length func-name)
                                          violations))))))
                          ;; Check body forms
                          (dolist (body-form (cddr current-expr))
                            (when (consp body-form)
                              (check-expr body-form actual-line actual-column)))))

                       ;; Other forms: recurse normally
                       (t
                        (dolist (subexpr (rest current-expr))
                          (when (consp subexpr)
                            (check-expr subexpr actual-line actual-column))))))))))

      (check-expr expr line column))

    violations))

(defun is-function-definition-p (head)
  "Check if HEAD is a function-defining form."
  (and (stringp head)
       (member (base:symbol-name-from-string head)
               '("DEFUN" "DEFMETHOD" "DEFMACRO" "DEFGENERIC")
               :test #'string-equal)))

(defun calculate-function-length (expr position-map start-line start-column)
  "Calculate the length of a function definition in lines.
   V1: Simple line counting (end-line - start-line + 1)."
  (declare (ignore start-column))

  ;; Find end position from position-map
  ;; The position-map stores (line . column) for the START of each expression
  ;; We need to find the end by looking at the entire form's extent

  ;; For now, use a simple heuristic: find the maximum line number
  ;; of any sub-expression in this function
  (let ((max-line start-line))
    (maphash (lambda (k v)
               (when (and (eq-subexpr-p k expr)
                          (> (car v) max-line))
                 (setf max-line (car v))))
             position-map)
    ;; Length = end-line - start-line + 1
    (- max-line start-line -1)))

(defun eq-subexpr-p (needle haystack)
  "Check if NEEDLE is EQ to HAYSTACK or any sub-expression in HAYSTACK."
  (cond
    ((eq needle haystack) t)
    ((not (consp haystack)) nil)
    (t (or (eq-subexpr-p needle (car haystack))
           (eq-subexpr-p needle (cdr haystack))))))

(defun get-function-name (expr)
  "Extract function name from definition form."
  (when (consp expr)
    (let ((name (second expr)))
      (cond
        ((symbolp name) (symbol-name name))
        ((stringp name) (base:symbol-name-from-string name))
        (t "CL:LAMBDA")))))

(defun get-inner-function-name (func-def)
  "Extract function name from flet/labels inner function definition.
   Inner functions have structure: (name lambda-list . body)"
  (when (consp func-def)
    (let ((name (first func-def)))
      (cond
        ((symbolp name) (symbol-name name))
        ((stringp name) (base:symbol-name-from-string name))
        (t "CL:LAMBDA")))))

(defun make-function-length-violation (rule file line column length func-name)
  "Create a violation for function length."
  (make-instance 'violation:violation
                 :rule :function-length
                 :file file
                 :line line
                 :column column
                 :severity (base:rule-severity rule)
                 :message (format nil "Function '~A' is ~D lines (max: ~D)"
                                  func-name length (max-lines rule))
                 :fix nil))

;;; Cyclomatic-complexity rule

(defclass cyclomatic-complexity-rule (base:rule)
  ((max-complexity
    :initarg :max-complexity
    :initform 10
    :accessor max-complexity))
  (:default-initargs
   :name :cyclomatic-complexity
   :description "Function has high cyclomatic complexity"
   :severity :info
   :type :form))

(defmethod base:check-form ((rule cyclomatic-complexity-rule) form file)
  "Check cyclomatic complexity."
  (check-type form parser:form)
  (check-type file pathname)

  (base:check-form-recursive rule
                             (parser:form-expr form)
                             file
                             (parser:form-line form)
                             (parser:form-column form)
                             nil
                             (parser:form-position-map form)))

(defmethod base:check-form-recursive ((rule cyclomatic-complexity-rule) expr file line column
                                      &optional function-name position-map)
  (declare (ignore function-name))

  (let ((violations '())
        (visited (make-hash-table :test 'eq)))

    (labels ((check-expr (current-expr fallback-line fallback-column)
               (base:with-safe-cons-expr (current-expr visited)
                 (multiple-value-bind (actual-line actual-column)
                     (base:find-actual-position current-expr position-map
                                                fallback-line fallback-column)
                   (let ((head (first current-expr)))

                     ;; Check if this is a function definition
                     (when (is-function-definition-p head)
                       (let ((complexity (calculate-complexity
                                          current-expr visited)))
                         (when (and (> complexity (max-complexity rule))
                                    (base:should-create-violation-p rule))
                           (push (make-complexity-violation
                                  rule file actual-line actual-column
                                  complexity (get-function-name current-expr))
                                 violations))))

                     ;; Recurse into nested forms
                     ;; Special handling for flet/labels to check inner functions
                     (cond
                       ;; flet/labels: check inner functions separately
                       ((or (base:symbol-matches-p head "FLET")
                            (base:symbol-matches-p head "LABELS"))
                        (when (and (consp (rest current-expr))
                                   (a:proper-list-p (second current-expr)))
                          ;; Check each inner function definition
                          (dolist (func-def (second current-expr))
                            (when (consp func-def)
                              (multiple-value-bind (func-line func-column)
                                  (base:find-actual-position func-def position-map
                                                             actual-line actual-column)
                                (let ((complexity (calculate-complexity
                                                   func-def visited))
                                      (func-name (get-inner-function-name func-def)))
                                  (when (and (> complexity (max-complexity rule))
                                             (base:should-create-violation-p rule))
                                    (push (make-complexity-violation
                                           rule file func-line func-column
                                           complexity func-name)
                                          violations))))))
                          ;; Check body forms
                          (dolist (body-form (cddr current-expr))
                            (when (consp body-form)
                              (check-expr body-form actual-line actual-column)))))

                       ;; defun/defmethod/etc: don't recurse (already handled)
                       ((is-function-definition-p head)
                        nil)

                       ;; Other forms: recurse normally
                       (t
                        (dolist (subexpr (rest current-expr))
                          (when (consp subexpr)
                            (check-expr subexpr actual-line actual-column))))))))))

      (check-expr expr line column))

    violations))

(defun calculate-complexity (function-expr visited)
  "Calculate cyclomatic complexity of FUNCTION-EXPR.
   Uses modified variant (case counts as 1, not per-clause)."
  ;; Base complexity
  (let ((complexity 1))

    ;; Walk the function body and count decision points
    (labels ((walk (expr)
               (when (and (consp expr)
                          (not (gethash expr visited)))
                 (setf (gethash expr visited) t)

                 (let ((head (first expr)))
                   ;; Add complexity for this form
                   (incf complexity (form-complexity head expr))

                   ;; Recurse (but don't enter nested function definitions)
                   (unless (is-nested-function-p head)
                     (dolist (subexpr (rest expr))
                       (walk subexpr)))))))

      ;; Walk the function body (skip defun/defmethod/etc and name)
      (let ((body (get-function-body function-expr)))
        (dolist (form body)
          (walk form))))

    complexity))

(defun is-nested-function-p (head)
  "Check if HEAD is a nested function-defining form (flet, labels)."
  (and (stringp head)
       (member (base:symbol-name-from-string head)
               '("FLET" "LABELS")
               :test #'string-equal)))

(defun get-function-body (expr)
  "Extract function body from a function definition form.
   Returns a list of body forms."
  (when (consp expr)
    (let ((head (first expr)))
      (cond
        ;; defun/defmacro: (defun name lambda-list . body)
        ((or (base:symbol-matches-p head "DEFUN")
             (base:symbol-matches-p head "DEFMACRO"))
         (cdddr expr))

        ;; defmethod: (defmethod name [qualifiers] lambda-list . body)
        ((base:symbol-matches-p head "DEFMETHOD")
         ;; Skip past name and qualifiers to find lambda-list
         (let ((rest (cddr expr)))
           ;; Skip qualifiers (keywords or other non-list elements)
           (loop while (and rest (not (consp (first rest))))
                 do (setf rest (cdr rest)))
           ;; Now rest starts with lambda-list, body follows
           (cdr rest)))

        ;; defgeneric: usually no body (just lambda-list and options)
        ((base:symbol-matches-p head "DEFGENERIC")
         nil)

        ;; flet/labels inner function: (name lambda-list . body)
        (t
         (cddr expr))))))

(defun form-complexity (head expr)
  "Return complexity added by this form.
   HEAD is the car of the form, EXPR is the full form."
  (cond
    ;; Conditionals: +1 each
    ((conditional-p head) 1)

    ;; COND: +1 per clause
    ((cond-p head) (count-cond-clauses expr))

    ;; CASE/TYPECASE: +1 total (modified variant)
    ((case-p head) 1)

    ;; Simple loops: +1 each
    ((simple-loop-p head) 1)

    ;; LOOP: +1 for loop + count internal conditionals
    ((loop-p head) (calculate-loop-complexity expr))

    ;; AND/OR: +(N-1) for N arguments
    ((logical-operator-p head) (max 0 (1- (length (rest expr)))))

    ;; Exception handling: +1 per handler
    ((ignore-errors-p head) 1)
    ((handler-case-p head) (count-handler-clauses expr))
    ((handler-bind-p head) (count-handler-bind-clauses expr))
    ((restart-case-p head) (count-restart-clauses expr))
    ((restart-bind-p head) (count-restart-bind-clauses expr))

    ;; Default: no complexity
    (t 0)))

(defun conditional-p (head)
  "Check if HEAD is a simple conditional (if, when, unless)."
  (and (stringp head)
       (member (base:symbol-name-from-string head)
               '("IF" "WHEN" "UNLESS")
               :test #'string-equal)))

(defun cond-p (head)
  "Check if HEAD is COND."
  (and (stringp head)
       (string-equal (base:symbol-name-from-string head) "COND")))

(defun count-cond-clauses (expr)
  "Count clauses in a COND form."
  (let ((clauses (rest expr)))
    (count-if #'consp clauses)))

(defun case-p (head)
  "Check if HEAD is a case-like form (case, typecase, ecase, etypecase)."
  (and (stringp head)
       (member (base:symbol-name-from-string head)
               '("CASE" "TYPECASE" "ECASE" "ETYPECASE")
               :test #'string-equal)))

(defun simple-loop-p (head)
  "Check if HEAD is a simple loop (dotimes, dolist, do, do*)."
  (and (stringp head)
       (member (base:symbol-name-from-string head)
               '("DOTIMES" "DOLIST" "DO" "DO*")
               :test #'string-equal)))

(defun loop-p (head)
  "Check if HEAD is LOOP."
  (and (stringp head)
       (string-equal (base:symbol-name-from-string head) "LOOP")))

(defun calculate-loop-complexity (expr)
  "Calculate complexity of a LOOP form.
   Base +1 for the loop, +1 for each conditional keyword."
  (let ((complexity 1)  ; Base for the loop itself
        (keywords (rest expr)))
    ;; Count conditional keywords in the loop
    (dolist (kw keywords)
      (when (and (stringp kw)
                 (member (base:symbol-name-from-string kw)
                         '("WHEN" "UNLESS" "IF" "WHILE" "UNTIL")
                         :test #'string-equal))
        (incf complexity)))
    complexity))

(defun logical-operator-p (head)
  "Check if HEAD is a logical operator (and, or)."
  (and (stringp head)
       (member (base:symbol-name-from-string head)
               '("AND" "OR")
               :test #'string-equal)))

(defun ignore-errors-p (head)
  "Check if HEAD is IGNORE-ERRORS."
  (and (stringp head)
       (string-equal (base:symbol-name-from-string head) "IGNORE-ERRORS")))

(defun handler-case-p (head)
  "Check if HEAD is HANDLER-CASE."
  (and (stringp head)
       (string-equal (base:symbol-name-from-string head) "HANDLER-CASE")))

(defun handler-bind-p (head)
  "Check if HEAD is HANDLER-BIND."
  (and (stringp head)
       (string-equal (base:symbol-name-from-string head) "HANDLER-BIND")))

(defun restart-case-p (head)
  "Check if HEAD is RESTART-CASE."
  (and (stringp head)
       (string-equal (base:symbol-name-from-string head) "RESTART-CASE")))

(defun restart-bind-p (head)
  "Check if HEAD is RESTART-BIND."
  (and (stringp head)
       (string-equal (base:symbol-name-from-string head) "RESTART-BIND")))

(defun count-handler-clauses (expr)
  "Count handler clauses in HANDLER-CASE.
   (handler-case form (error ...) (warning ...)) -> 2"
  (let ((clauses (cddr expr)))  ; Skip handler-case and form
    (count-if #'consp clauses)))

(defun count-handler-bind-clauses (expr)
  "Count handler bindings in HANDLER-BIND.
   (handler-bind ((error ...) (warning ...)) ...) -> 2"
  (let ((bindings (second expr)))
    (if (consp bindings)
        (count-if #'consp bindings)
        0)))

(defun count-restart-clauses (expr)
  "Count restart clauses in RESTART-CASE.
   (restart-case form (retry ...) (abort ...)) -> 2"
  (let ((clauses (cddr expr)))  ; Skip restart-case and form
    (count-if #'consp clauses)))

(defun count-restart-bind-clauses (expr)
  "Count restart bindings in RESTART-BIND.
   (restart-bind ((retry ...) (abort ...)) ...) -> 2"
  (let ((bindings (second expr)))
    (if (consp bindings)
        (count-if #'consp bindings)
        0)))

(defun make-complexity-violation (rule file line column complexity func-name)
  "Create a violation for cyclomatic complexity."
  (make-instance 'violation:violation
                 :rule :cyclomatic-complexity
                 :file file
                 :line line
                 :column column
                 :severity (base:rule-severity rule)
                 :message (format nil "Function '~A' has cyclomatic complexity of ~D (max: ~D)"
                                  func-name complexity (max-complexity rule))
                 :fix nil))
