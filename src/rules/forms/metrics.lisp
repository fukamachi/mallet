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
  ((max
    :initarg :max
    :initform 20
    :accessor max-complexity)
   (variant
    :initarg :variant
    :initform :standard
    :accessor complexity-variant
    :documentation "Variant: :standard (count per clause) or :modified (case as +1 total)"))
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
                                          current-expr visited (complexity-variant rule))))
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
                                                   func-def visited (complexity-variant rule)))
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

(defun calculate-complexity (function-expr visited &optional (variant :standard))
  "Calculate cyclomatic complexity of FUNCTION-EXPR.
   VARIANT can be :standard (count per clause) or :modified (case as +1 total)."
  ;; Base complexity
  (let ((complexity 1))

    ;; Walk the function body and count decision points
    (labels ((walk (expr)
               (when (and (consp expr)
                          (not (gethash expr visited)))
                 (setf (gethash expr visited) t)

                 (let ((head (first expr)))
                   ;; Add complexity for this form
                   (incf complexity (form-complexity head expr variant))

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

(defun form-complexity (head expr &optional (variant :standard))
  "Return complexity added by this form.
   HEAD is the car of the form, EXPR is the full form.
   VARIANT is :standard (count per clause) or :modified (case as +1 total)."
  (cond
    ;; Conditionals: +1 each
    ((conditional-p head) 1)

    ;; COND: +1 per clause
    ((cond-p head) (count-cond-clauses expr))

    ;; CASE/TYPECASE: depends on variant
    ((case-p head)
     (if (eq variant :modified)
         1  ; Modified: entire case statement = +1
         (count-case-clauses expr)))  ; Standard: count per clause

    ;; Simple iteration: +0 (dotimes, dolist)
    ((simple-iteration-p head) 0)

    ;; DO/DO*: +1 (has end-test condition)
    ((do-loop-p head) 1)

    ;; LOOP: +1 for loop + count internal conditionals
    ((loop-p head) (calculate-loop-complexity expr))

    ;; AND/OR: +1 for each operator (regardless of argument count)
    ((logical-operator-p head) 1)

    ;; Exception handling: +1 per handler
    ((ignore-errors-p head) 1)
    ((handler-case-p head) (count-handler-clauses expr))
    ((handler-bind-p head) (count-handler-bind-clauses expr))
    ((restart-case-p head) (count-restart-clauses expr))
    ((restart-bind-p head) (count-restart-bind-clauses expr))

    ;; Third-party macros: Alexandria
    ((alexandria-conditional-p head) 1)  ; if-let, when-let, when-let*
    ((alexandria-xor-p head) 1)  ; xor (like or)
    ((alexandria-destructuring-case-p head)
     (if (eq variant :modified)
         1  ; Modified: entire case = +1
         (count-destructuring-case-clauses expr)))  ; Standard: count per clause

    ;; Third-party macros: Trivia
    ((trivia-match-p head)
     (if (eq variant :modified)
         1  ; Modified: entire match = +1
         (count-trivia-match-clauses expr)))  ; Standard: count per clause
    ((trivia-conditional-p head) 1)  ; if-match, when-match, unless-match

    ;; Third-party macros: string-case
    ((string-case-p head)
     (if (eq variant :modified)
         1  ; Modified: entire case = +1
         (count-string-case-clauses expr)))  ; Standard: count per clause

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
  "Count clauses in a COND form, excluding final t/otherwise clause.
   Like if-elseif-else: only if and elseif count, not the final else."
  (let ((clauses (rest expr)))
    (if (null clauses)
        0
        (let ((clause-count (count-if #'consp clauses)))
          ;; Check if the last clause is t or otherwise (the 'else' clause)
          (let ((last-clause (car (last clauses))))
            (if (and (consp last-clause)
                     (let ((test (first last-clause)))
                       (or (eq test t)
                           (and (stringp test)
                                (let ((name (base:symbol-name-from-string test)))
                                  (or (string-equal name "T")
                                      (string-equal name "OTHERWISE")))))))
                ;; Exclude the final else clause
                (max 0 (1- clause-count))
                ;; All clauses are conditionals
                clause-count))))))

(defun case-p (head)
  "Check if HEAD is a case-like form (case, typecase, ecase, etypecase, ccase, ctypecase)."
  (and (stringp head)
       (member (base:symbol-name-from-string head)
               '("CASE" "TYPECASE" "ECASE" "ETYPECASE" "CCASE" "CTYPECASE")
               :test #'string-equal)))

(defun count-case-clauses (expr)
  "Count clauses in a CASE-like form, excluding otherwise/t clause.
   Like switch statements: each case counts, but not the default case."
  (let* ((head (first expr))
         (is-ecase (and (stringp head)
                        (member (base:symbol-name-from-string head)
                                '("ECASE" "ETYPECASE" "CCASE" "CTYPECASE")
                                :test #'string-equal)))
         ;; case/typecase: (case expr clause1 clause2 ...)
         ;; Skip first two elements (operator and expression)
         (clauses (cddr expr)))
    (if (null clauses)
        0
        (let ((clause-count (count-if #'consp clauses)))
          (if is-ecase
              ;; ecase/etypecase/ccase/ctypecase don't have otherwise
              clause-count
              ;; Check if the last clause is otherwise/t (the 'default' clause)
              (let ((last-clause (car (last clauses))))
                (if (and (consp last-clause)
                         (let ((key (first last-clause)))
                           (or (eq key t)
                               (and (stringp key)
                                    (let ((name (base:symbol-name-from-string key)))
                                      (or (string-equal name "T")
                                          (string-equal name "OTHERWISE")))))))
                    ;; Exclude the default clause
                    (max 0 (1- clause-count))
                    ;; All clauses are cases
                    clause-count)))))))

(defun simple-iteration-p (head)
  "Check if HEAD is simple iteration without conditionals (dotimes, dolist)."
  (and (stringp head)
       (member (base:symbol-name-from-string head)
               '("DOTIMES" "DOLIST")
               :test #'string-equal)))

(defun do-loop-p (head)
  "Check if HEAD is DO or DO* (has end-test condition)."
  (and (stringp head)
       (member (base:symbol-name-from-string head)
               '("DO" "DO*")
               :test #'string-equal)))

(defun loop-p (head)
  "Check if HEAD is LOOP."
  (and (stringp head)
       (string-equal (base:symbol-name-from-string head) "LOOP")))

(defun calculate-loop-complexity (expr)
  "Calculate complexity of a LOOP form.
   Only count conditional keywords (when, unless, if, while, until).
   Simple loops without conditionals add no complexity."
  (let ((complexity 0)  ; No base cost for the loop itself
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

;;; Third-party macro recognizers: Alexandria

(defun alexandria-conditional-p (head)
  "Check if HEAD is an Alexandria conditional macro (if-let, when-let, when-let*)."
  (and (stringp head)
       (member (base:symbol-name-from-string head)
               '("IF-LET" "WHEN-LET" "WHEN-LET*")
               :test #'string-equal)))

(defun alexandria-xor-p (head)
  "Check if HEAD is Alexandria XOR (like OR)."
  (and (stringp head)
       (string-equal (base:symbol-name-from-string head) "XOR")))

(defun alexandria-destructuring-case-p (head)
  "Check if HEAD is Alexandria destructuring-case or destructuring-ecase."
  (and (stringp head)
       (member (base:symbol-name-from-string head)
               '("DESTRUCTURING-CASE" "DESTRUCTURING-ECASE")
               :test #'string-equal)))

(defun count-destructuring-case-clauses (expr)
  "Count clauses in destructuring-case/destructuring-ecase.
   Like CASE: count per clause, excluding otherwise/t for non-e variants."
  (let* ((head (first expr))
         (is-ecase (and (stringp head)
                        (string-equal (base:symbol-name-from-string head) "DESTRUCTURING-ECASE")))
         ;; (destructuring-case expr clause1 clause2 ...)
         (clauses (cddr expr)))
    (if (null clauses)
        0
        (let ((clause-count (count-if #'consp clauses)))
          (if is-ecase
              clause-count
              ;; Check for otherwise/t in last clause
              (let ((last-clause (car (last clauses))))
                (if (and (consp last-clause)
                         (let ((key (first last-clause)))
                           (or (eq key t)
                               (and (stringp key)
                                    (let ((name (base:symbol-name-from-string key)))
                                      (or (string-equal name "T")
                                          (string-equal name "OTHERWISE")))))))
                    (max 0 (1- clause-count))
                    clause-count)))))))

;;; Third-party macro recognizers: Trivia

(defun trivia-match-p (head)
  "Check if HEAD is a Trivia match macro (match, ematch, match*, multiple-value-match, multiple-value-ematch)."
  (and (stringp head)
       (member (base:symbol-name-from-string head)
               '("MATCH" "EMATCH" "MATCH*" "MULTIPLE-VALUE-MATCH" "MULTIPLE-VALUE-EMATCH")
               :test #'string-equal)))

(defun trivia-conditional-p (head)
  "Check if HEAD is a Trivia conditional macro (if-match, when-match, unless-match)."
  (and (stringp head)
       (member (base:symbol-name-from-string head)
               '("IF-MATCH" "WHEN-MATCH" "UNLESS-MATCH")
               :test #'string-equal)))

(defun count-trivia-match-clauses (expr)
  "Count clauses in Trivia match forms.
   Like CASE: count per clause, excluding otherwise/_ for non-e variants."
  (let* ((head (first expr))
         (is-ematch (and (stringp head)
                         (member (base:symbol-name-from-string head)
                                 '("EMATCH" "MULTIPLE-VALUE-EMATCH")
                                 :test #'string-equal)))
         ;; match/ematch: (match expr clause1 clause2 ...)
         ;; match*/multiple-value-match: (match* (expr1 expr2) clause1 clause2 ...)
         (clauses (cddr expr)))
    (if (null clauses)
        0
        (let ((clause-count (count-if #'consp clauses)))
          (if is-ematch
              clause-count
              ;; Check for otherwise/_ pattern in last clause
              (let ((last-clause (car (last clauses))))
                (if (and (consp last-clause)
                         (let ((pattern (first last-clause)))
                           (or (eq pattern '_)
                               (and (stringp pattern)
                                    (let ((name (base:symbol-name-from-string pattern)))
                                      (or (string-equal name "_")
                                          (string-equal name "OTHERWISE")))))))
                    (max 0 (1- clause-count))
                    clause-count)))))))

;;; Third-party macro recognizers: string-case

(defun string-case-p (head)
  "Check if HEAD is string-case:string-case."
  (and (stringp head)
       (string-equal (base:symbol-name-from-string head) "STRING-CASE")))

(defun count-string-case-clauses (expr)
  "Count clauses in string-case.
   Like CASE: count per clause, excluding otherwise/t."
  (let ((clauses (cddr expr)))  ; (string-case expr clause1 clause2 ...)
    (if (null clauses)
        0
        (let ((clause-count (count-if #'consp clauses)))
          ;; Check for otherwise/t in last clause
          (let ((last-clause (car (last clauses))))
            (if (and (consp last-clause)
                     (let ((key (first last-clause)))
                       (or (eq key t)
                           (and (stringp key)
                                (let ((name (base:symbol-name-from-string key)))
                                  (or (string-equal name "T")
                                      (string-equal name "OTHERWISE")))))))
                (max 0 (1- clause-count))
                clause-count))))))

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
