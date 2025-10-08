(defpackage #:malvolio/rules/forms/variables
  (:use #:cl)
  (:local-nicknames
   (#:base #:malvolio/rules/base)
   (#:parser #:malvolio/parser)
   (#:violation #:malvolio/violation))
  (:export #:unused-variables-rule))
(in-package #:malvolio/rules/forms/variables)

;;; Unused-variables rule

(defclass unused-variables-rule (base:rule)
  ()
  (:default-initargs
   :name :unused-variables
   :description "Variables should be used or explicitly ignored"
   :severity :warning
   :type :form)
  (:documentation "Rule to detect unused variables in bindings."))

(defmethod base:check-form ((rule unused-variables-rule) form file)
  "Check that all bound variables are either used or explicitly ignored."
  (check-type form parser:form)
  (check-type file pathname)

  (let ((violations '()))
    (labels ((ignored-var-p (var-name ignored-vars)
               "Check if variable should be ignored (in ignore list or starts with _)."
               (let ((name (base:symbol-name-from-string var-name)))
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
                                  (member (base:symbol-name-from-string clause)
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
                                                          (member (base:symbol-name-from-string next)
                                                                  '("FOR" "AS" "WITH" "DO" "COLLECT" "APPEND"
                                                                    "NCONC" "COUNT" "SUM" "MAXIMIZE" "MINIMIZE"
                                                                    "WHEN" "UNLESS" "IF" "WHILE" "UNTIL" "REPEAT"
                                                                    "ALWAYS" "NEVER" "THEREIS" "RETURN")
                                                                  :test #'string-equal)))))
                                   do (incf i)))
                            ;; Body keywords - mark body start if not already marked
                            ((and (stringp clause)
                                  (member (base:symbol-name-from-string clause)
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
               (let ((target-name (base:symbol-name-from-string var-name)))
                 (labels ((binds-same-name-p (binding-form)
                            "Check if BINDING-FORM introduces a variable with target-name."
                            (let ((vars (extract-bindings binding-form)))
                              (some (lambda (v)
                                      (string-equal (base:symbol-name-from-string v) target-name))
                                    vars)))

                          (search-expr (expr)
                            (cond
                              ((null expr) nil)

                              ;; String matching our variable is a reference
                              ((stringp expr)
                               (string-equal (base:symbol-name-from-string expr) target-name))

                              ;; Check if it's a binding form that might shadow
                              ((consp expr)
                               (let ((head (first expr))
                                     (rest-args (rest expr)))
                                 (cond
                                   ;; LET and LET* - check bindings for shadowing
                                   ((and (stringp head)
                                         (or (string-equal (base:symbol-name-from-string head) "LET")
                                             (string-equal (base:symbol-name-from-string head) "LET*")))
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
                                         (or (string-equal (base:symbol-name-from-string head) "DEFUN")
                                             (string-equal (base:symbol-name-from-string head) "LAMBDA")
                                             (string-equal (base:symbol-name-from-string head) "DEFMACRO")))
                                    (let* ((lambda-list-pos (if (string-equal (base:symbol-name-from-string head) "LAMBDA") 0 1))
                                           (lambda-list (when (> (length rest-args) lambda-list-pos)
                                                          (nth lambda-list-pos rest-args))))
                                      (if (and (listp lambda-list)
                                               (some (lambda (param)
                                                       (and (stringp param)
                                                            (not (char= (char (base:symbol-name-from-string param) 0) #\&))
                                                            (string-equal (base:symbol-name-from-string param) target-name)))
                                                     lambda-list))
                                          nil  ; Shadowed by parameter
                                          (search-expr rest-args))))

                                   ;; DESTRUCTURING-BIND, MULTIPLE-VALUE-BIND
                                   ((and (stringp head)
                                         (or (string-equal (base:symbol-name-from-string head) "DESTRUCTURING-BIND")
                                             (string-equal (base:symbol-name-from-string head) "MULTIPLE-VALUE-BIND")))
                                    (if (>= (length rest-args) 2)
                                        (let ((vars (first rest-args)))
                                          (if (and (listp vars)
                                                   (some (lambda (v)
                                                           (and (stringp v)
                                                                (string-equal (base:symbol-name-from-string v) target-name)))
                                                         vars))
                                              nil  ; Shadowed
                                              (search-expr rest-args)))
                                        (search-expr rest-args)))

                                   ;; DOLIST, DOTIMES
                                   ((and (stringp head)
                                         (or (string-equal (base:symbol-name-from-string head) "DOLIST")
                                             (string-equal (base:symbol-name-from-string head) "DOTIMES")))
                                    (if (>= (length rest-args) 1)
                                        (let* ((spec (first rest-args))
                                               (var (when (listp spec) (first spec))))
                                          (if (and (stringp var)
                                                   (string-equal (base:symbol-name-from-string var) target-name))
                                              nil  ; Shadowed
                                              (search-expr rest-args)))
                                        (search-expr rest-args)))

                                   ;; DO
                                   ((and (stringp head)
                                         (string-equal (base:symbol-name-from-string head) "DO"))
                                    (if (>= (length rest-args) 1)
                                        (let ((var-clauses (first rest-args)))
                                          (if (and (listp var-clauses)
                                                   (some #'binds-same-name-p var-clauses))
                                              nil  ; Shadowed
                                              (search-expr rest-args)))
                                        (search-expr rest-args)))

                                   ;; LOOP - check for FOR/AS/WITH variables
                                   ((and (stringp head)
                                         (string-equal (base:symbol-name-from-string head) "LOOP"))
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
                              (base:symbol-matches-p (first form) "DECLARE"))
                     (when (listp (rest form))
                       (dolist (decl-spec (rest form))
                         (when (and (consp decl-spec)
                                    (base:symbol-matches-p (first decl-spec) "IGNORE"))
                           (when (listp (rest decl-spec))
                             (dolist (var (rest decl-spec))
                               (when (stringp var)
                                 (push (base:symbol-name-from-string var) ignored)))))))))
                 ignored))

             (parse-lambda-list (lambda-list)
               "Parse lambda list into regular params and &aux params.
Returns (values regular-params aux-params)."
               (let ((aux-pos (position-if (lambda (x)
                                             (and (stringp x)
                                                  (string-equal (base:symbol-name-from-string x) "&AUX")))
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
                                                    (base:symbol-matches-p (first form) "DECLARE")))
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
                                  ;; Try to find position of the variable name first, then binding, then fallback
                                  (multiple-value-bind (var-line var-column)
                                      (if position-map
                                          (multiple-value-bind (name-line name-column)
                                              (parser:find-position var-name position-map nil nil)
                                            (if name-line
                                                (values name-line name-column)
                                                (parser:find-position binding position-map fallback-line fallback-column)))
                                          (values fallback-line fallback-column))
                                    (push (make-instance 'violation:violation
                                                         :rule :unused-variables
                                                         :file file
                                                         :line var-line
                                                         :column var-column
                                                         :severity (base:rule-severity rule)
                                                         :message
                                                         (format nil "~A '~A' is unused"
                                                                 (or message-prefix "Variable")
                                                                 (base:symbol-name-from-string var-name))
                                                         :fix nil)
                                          violations)))))))))

             (check-expr (expr line column position-map)
               "Recursively check expression for unused variables."
               (when (consp expr)
                 (let ((head (first expr))
                       (rest-args (rest expr)))
                   ;; Check DEFUN
                   (when (base:symbol-matches-p head "DEFUN")
                     (when (>= (length rest-args) 3)
                       (let* ((lambda-list (second rest-args))
                              (body (cddr rest-args)))
                         (multiple-value-bind (regular-params aux-params)
                             (parse-lambda-list lambda-list)
                           ;; Check regular parameters (excluding &optional, &key, &rest markers)
                           (let ((param-bindings (loop for param in regular-params
                                                       when (and (stringp param)
                                                                 (not (char= (char (base:symbol-name-from-string param) 0) #\&)))
                                                       collect (list param))))
                             (when param-bindings
                               (check-binding-form :defun-regular param-bindings body line column position-map)))
                           ;; Check &aux parameters (sequential like LET*)
                           (when aux-params
                             (check-binding-form :defun-aux aux-params body line column position-map body))))))

                   ;; Check LAMBDA
                   (when (base:symbol-matches-p head "LAMBDA")
                     (when (>= (length rest-args) 2)
                       (let* ((lambda-list (first rest-args))
                              (body (rest rest-args)))
                         (multiple-value-bind (regular-params aux-params)
                             (parse-lambda-list lambda-list)
                           ;; Check regular parameters (excluding &optional, &key, &rest markers)
                           (let ((param-bindings (loop for param in regular-params
                                                       when (and (stringp param)
                                                                 (not (char= (char (base:symbol-name-from-string param) 0) #\&)))
                                                       collect (list param))))
                             (when param-bindings
                               (check-binding-form :defun-regular param-bindings body line column position-map)))
                           ;; Check &aux parameters (sequential like LET*)
                           (when aux-params
                             (check-binding-form :defun-aux aux-params body line column position-map body))))))

                   ;; Check LET
                   (when (and (stringp head)
                              (string-equal (base:symbol-name-from-string head) "LET"))
                     (when (>= (length rest-args) 2)
                       (let ((bindings (first rest-args))
                             (body (rest rest-args)))
                         (check-binding-form :let bindings body line column position-map))))

                   ;; Check LET* (sequential bindings)
                   (when (and (stringp head)
                              (string-equal (base:symbol-name-from-string head) "LET*"))
                     (when (>= (length rest-args) 2)
                       (let ((bindings (first rest-args))
                             (body (rest rest-args)))
                         (check-binding-form :let* bindings body line column position-map))))

                   ;; Check DESTRUCTURING-BIND (parallel bindings like LET)
                   (when (base:symbol-matches-p head "DESTRUCTURING-BIND")
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
                   (when (base:symbol-matches-p head "MULTIPLE-VALUE-BIND")
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
                   (when (base:symbol-matches-p head "DEFMACRO")
                     (when (>= (length rest-args) 3)
                       (let* ((lambda-list (second rest-args))
                              (body (cddr rest-args)))
                         (multiple-value-bind (regular-params aux-params)
                             (parse-lambda-list lambda-list)
                           ;; Check regular parameters (excluding &optional, &key, &rest, &body markers)
                           (let ((param-bindings (loop for param in regular-params
                                                       when (and (stringp param)
                                                                 (not (char= (char (base:symbol-name-from-string param) 0) #\&)))
                                                       collect (list param))))
                             (when param-bindings
                               (check-binding-form :defun-regular param-bindings body line column position-map)))
                           ;; Check &aux parameters (sequential like LET*)
                           (when aux-params
                             (check-binding-form :defun-aux aux-params body line column position-map body))))))

                   ;; Check DOLIST
                   (when (base:symbol-matches-p head "DOLIST")
                     (when (>= (length rest-args) 2)
                       (let* ((spec (first rest-args))
                              (body (rest rest-args)))
                         (when (and (listp spec) (>= (length spec) 2))
                           (let* ((var (first spec))
                                  (bindings (list (list var))))
                             (check-binding-form :let bindings body line column position-map))))))

                   ;; Check DOTIMES
                   (when (base:symbol-matches-p head "DOTIMES")
                     (when (>= (length rest-args) 2)
                       (let* ((spec (first rest-args))
                              (body (rest rest-args)))
                         (when (and (listp spec) (>= (length spec) 2))
                           (let* ((var (first spec))
                                  (bindings (list (list var))))
                             (check-binding-form :let bindings body line column position-map))))))

                   ;; Check DO
                   (when (base:symbol-matches-p head "DO")
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
                   (when (base:symbol-matches-p head "LOOP")
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
