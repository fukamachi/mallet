(defpackage #:malo/rules/forms/variables
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:malo/rules/base)
   (#:parser #:malo/parser)
   (#:violation #:malo/violation))
  (:export #:unused-variables-rule))
(in-package #:malo/rules/forms/variables)

;;; Utilities

(defun debug-mode-p ()
  "Check if debug mode is enabled."
  (and (find-symbol "*DEBUG-MODE*" "MALO")
       (symbol-value (find-symbol "*DEBUG-MODE*" "MALO"))))

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

  (handler-case
      (handler-bind ((type-error
                       (lambda (e)
                         (when (debug-mode-p)
                           (let ((form-head (when (consp (parser:form-expr form))
                                              (let ((head (first (parser:form-expr form))))
                                                (when (stringp head)
                                                  (base:symbol-name-from-string head))))))
                             (format *error-output* "~%Warning: Skipping unused-variables check for ~A form at ~A:~A~%  Error: ~A~%"
                                     (or form-head "unknown") file (parser:form-line form) e)
                             (uiop:print-condition-backtrace e))))))
        (let ((violations '()))
          (labels ((special-variable-p (name)
                     "Check if NAME follows special variable convention (*name*)."
                     (and (> (length name) 2)
                          (char= (char name 0) #\*)
                          (char= (char name (1- (length name))) #\*)))

                   (ignored-var-p (var-name ignored-vars)
                     "Check if variable should be ignored (in ignore list, starts with _, is NIL, or is a special variable)."
                     (let ((name (base:symbol-name-from-string var-name)))
                       (or (member name ignored-vars :test #'string=)
                           (string-equal name "NIL")
                           (special-variable-p name)
                           (and (> (length name) 0)
                                (char= (char name 0) #\_)))))

                   (extract-bindings (binding-form)
                     "Extract variable names from binding form (supports simple and nested destructuring)."
                     (labels ((extract-from-pattern (pattern)
                                "Recursively extract all variable names from a destructuring pattern."
                                (cond
                                  ;; Simple variable
                                  ((stringp pattern)
                                   (let ((symbol-name (base:symbol-name-from-string pattern)))
                                     (if (and (> (length symbol-name) 0)
                                              (or (char= (char symbol-name 0) #\&)   ; Lambda-list keywords
                                                  (char= (char symbol-name 0) #\:))) ; Keyword arguments
                                         nil  ; Skip lambda-list keywords and keyword arguments
                                         (list pattern))))
                                  ;; Dotted pair - extract both parts
                                  ((and (consp pattern) (stringp (cdr pattern)))
                                   (append (extract-from-pattern (car pattern))
                                           (extract-from-pattern (cdr pattern))))
                                  ;; Proper list - iterate through elements, filtering out non-variables
                                  ;; This handles destructuring patterns like (a b c), (a &key b c), etc.
                                  ;; Non-string elements (numbers, other forms) are automatically skipped
                                  ((and (consp pattern) (a:proper-list-p pattern))
                                   (mapcan #'extract-from-pattern pattern))
                                  ;; Anything else (NIL, numbers, keywords, etc.)
                                  (t nil))))
                       (cond
                         ;; Simple string (variable name)
                         ((stringp binding-form)
                          (list binding-form))
                         ;; Binding with init form: (var init-form) or (var init-form supplied-p)
                         ;; Check if this is a binding (has non-string elements = values, not just variables)
                         ;; This handles LET bindings and lambda parameters with defaults
                         ((and (consp binding-form)
                               (a:proper-list-p binding-form)
                               (>= (length binding-form) 2)
                               (<= (length binding-form) 3)
                               ;; At least one element after the first is not a string (value/init form)
                               (some (lambda (elem) (not (stringp elem))) (rest binding-form)))
                          ;; Extract only variable(s) from first element, skip init forms
                          (extract-from-pattern (first binding-form)))
                         ;; Pure destructuring pattern: all elements are strings (variables)
                         ;; or contains lambda-list keywords, or nested patterns
                         ;; Examples: (a b), (a &key b c), ((a b) c), (a . b)
                         ((consp binding-form)
                          (extract-from-pattern binding-form))
                         ;; Anything else is not a valid binding form
                         (t nil))))

                   (parse-loop-clauses (clauses)
                     "Parse LOOP clauses and extract variable bindings and body.
Returns (values bindings body-clauses)."
                     ;; Guard: if clauses is not a proper list, return empty bindings
                     (unless (a:proper-list-p clauses)
                       (return-from parse-loop-clauses (values '() '())))
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
                                          (if (and (a:proper-list-p rest-args) (>= (length rest-args) 2))
                                              (let ((bindings (first rest-args))
                                                    (body (rest rest-args)))
                                                (if (and (a:proper-list-p bindings)
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
                                                 (lambda-list (when (and (a:proper-list-p rest-args) (> (length rest-args) lambda-list-pos))
                                                                (nth lambda-list-pos rest-args))))
                                            (if (and (a:proper-list-p lambda-list)
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
                                          (if (and (a:proper-list-p rest-args) (>= (length rest-args) 2))
                                              (let ((vars (first rest-args)))
                                                ;; Extract all vars from pattern (handles dotted pairs, nested destructuring)
                                                (let ((all-vars (extract-bindings vars)))
                                                  (if (some (lambda (v)
                                                              (string-equal (base:symbol-name-from-string v) target-name))
                                                            all-vars)
                                                      nil  ; Shadowed
                                                      (search-expr rest-args))))
                                              (search-expr rest-args)))

                                         ;; DOLIST, DOTIMES
                                         ((and (stringp head)
                                               (or (string-equal (base:symbol-name-from-string head) "DOLIST")
                                                   (string-equal (base:symbol-name-from-string head) "DOTIMES")))
                                          (if (and (a:proper-list-p rest-args) (>= (length rest-args) 1))
                                              (let* ((spec (first rest-args))
                                                     (var (when (a:proper-list-p spec) (first spec))))
                                                (if (and (stringp var)
                                                         (string-equal (base:symbol-name-from-string var) target-name))
                                                    nil  ; Shadowed
                                                    (search-expr rest-args)))
                                              (search-expr rest-args)))

                                         ;; DO
                                         ((and (stringp head)
                                               (string-equal (base:symbol-name-from-string head) "DO"))
                                          (if (and (a:proper-list-p rest-args) (>= (length rest-args) 1))
                                              (let ((var-clauses (first rest-args)))
                                                (if (and (a:proper-list-p var-clauses)
                                                         (some #'binds-same-name-p var-clauses))
                                                    nil  ; Shadowed
                                                    (search-expr rest-args)))
                                              (search-expr rest-args)))

                                         ;; LOOP - check for FOR/AS/WITH variables
                                         ((and (stringp head)
                                               (string-equal (base:symbol-name-from-string head) "LOOP"))
                                          ;; Use parse-loop-clauses to extract all bindings (supports destructuring)
                                          (if (a:proper-list-p rest-args)
                                              (multiple-value-bind (bindings body)
                                                  (parse-loop-clauses rest-args)
                                                (declare (ignore body))
                                                (let ((shadowed (some #'binds-same-name-p bindings)))
                                                  (if shadowed
                                                      nil
                                                      (search-expr rest-args))))
                                              ;; If rest-args is not a proper list, skip LOOP handling
                                              (search-expr rest-args)))

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
                                                                 (when (consp (cdr expr))
                                                                   (search-quasi (cdr expr))))))))
                                                       (t nil))))
                                            (some #'search-quasi rest-args)))

                                         ;; Default: search recursively
                                         (t
                                          (or (search-expr (car expr))
                                              (when (consp (cdr expr))
                                                (search-expr (cdr expr))))))))

                                    (t nil))))
                         (some #'search-expr body))))

                   (extract-ignored-vars (body)
                     "Extract variable names from (declare (ignore ...)) and (declare (ignorable ...)) forms."
                     (let ((ignored '()))
                       (dolist (form body)
                         (when (and (consp form)
                                    (base:symbol-matches-p (first form) "DECLARE"))
                           ;; Only iterate if it's a proper list (not a dotted pair)
                           (when (and (listp (rest form)) (a:proper-list-p (rest form)))
                             (dolist (decl-spec (rest form))
                               (when (and (consp decl-spec)
                                          (or (base:symbol-matches-p (first decl-spec) "IGNORE")
                                              (base:symbol-matches-p (first decl-spec) "IGNORABLE")))
                                 ;; Only iterate if it's a proper list (not a dotted pair)
                                 (when (and (listp (rest decl-spec)) (a:proper-list-p (rest decl-spec)))
                                   (dolist (var (rest decl-spec))
                                     (when (stringp var)
                                       (push (base:symbol-name-from-string var) ignored)))))))))
                       ignored))

                   (extract-lambda-list-vars (lambda-list allow-destructuring)
                     "Extract all variable names from a lambda list.
ALLOW-DESTRUCTURING controls whether required parameters can be destructuring patterns.
Returns list of variable names."
                     (let ((vars '())
                           (state :required)  ; :required, :optional, :rest, :key, :aux
                           (i 0))
                       (loop while (< i (length lambda-list))
                             for elem = (nth i lambda-list)
                             do (cond
                                  ;; Lambda-list keywords
                                  ((and (stringp elem)
                                        (let ((name (base:symbol-name-from-string elem)))
                                          (and (> (length name) 0)
                                               (char= (char name 0) #\&))))
                                   (let ((kw (base:symbol-name-from-string elem)))
                                     (cond
                                       ((or (string-equal kw "&OPTIONAL"))
                                        (setf state :optional))
                                       ((or (string-equal kw "&REST") (string-equal kw "&BODY"))
                                        (setf state :rest))
                                       ((string-equal kw "&KEY")
                                        (setf state :key))
                                       ((string-equal kw "&AUX")
                                        (setf state :aux))
                                       ;; Other keywords like &allow-other-keys, &environment, &whole - skip
                                       )))
                                  ;; Parameter
                                  (t
                                   (ecase state
                                     (:required
                                      ;; Required params: simple var or destructuring pattern (if allowed)
                                      (if allow-destructuring
                                          ;; Use extract-from-pattern for full destructuring support
                                          (let ((extracted (extract-bindings (list elem))))
                                            (setf vars (append vars extracted)))
                                          ;; Simple variable only
                                          (when (stringp elem)
                                            (push elem vars))))
                                     (:optional
                                      ;; &optional param: var, (var), (var default), (var default supplied-p)
                                      (cond
                                        ((stringp elem)
                                         (push elem vars))
                                        ((and (consp elem) (stringp (first elem)))
                                         ;; (var ...) - extract var
                                         (push (first elem) vars)
                                         ;; Extract supplied-p if present: (var default supplied-p)
                                         (when (and (>= (length elem) 3) (stringp (third elem)))
                                           (push (third elem) vars)))))
                                     (:rest
                                      ;; &rest/&body param: just a variable name
                                      (when (stringp elem)
                                        (push elem vars))
                                      ;; After &rest, next keyword or aux
                                      (setf state :required))
                                     (:key
                                      ;; &key param: var, (var), (var default), (var default supplied-p),
                                      ;; ((:keyword var)), ((:keyword var) default), ((:keyword var) default supplied-p)
                                      (cond
                                        ((stringp elem)
                                         (push elem vars))
                                        ((and (consp elem)
                                              (consp (first elem))
                                              (= (length (first elem)) 2)
                                              (stringp (second (first elem))))
                                         ;; ((:keyword var) ...) - extract var
                                         (push (second (first elem)) vars)
                                         ;; Extract supplied-p if present
                                         (when (and (>= (length elem) 3) (stringp (third elem)))
                                           (push (third elem) vars)))
                                        ((and (consp elem) (stringp (first elem)))
                                         ;; (var ...) - extract var
                                         (push (first elem) vars)
                                         ;; Extract supplied-p if present
                                         (when (and (>= (length elem) 3) (stringp (third elem)))
                                           (push (third elem) vars)))))
                                     (:aux
                                      ;; &aux: handled separately like LET* bindings
                                      nil))))
                                (incf i))
                       (nreverse vars)))

                   (parse-lambda-list-for-aux (lambda-list)
                     "Split lambda list into parts before and after &aux.
Returns (values non-aux-part aux-params)."
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
                           (when (a:proper-list-p var-clauses)
                             (mapcar (lambda (clause)
                                       (when (and (a:proper-list-p clause) (>= (length clause) 3))
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
                                                            (own-step-form (when (and (a:proper-list-p own-clause) (>= (length own-clause) 3))
                                                                             (third own-clause)))
                                                            (all-step-forms (when (a:proper-list-p var-clauses)
                                                                              (mapcar (lambda (clause)
                                                                                        (when (and (a:proper-list-p clause) (>= (length clause) 3))
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
                           (when (and (a:proper-list-p rest-args)
                                      (>= (length rest-args) 3)
                                      ;; Validate lambda-list is actually a list
                                      (listp (second rest-args)))
                             (let* ((lambda-list (second rest-args))
                                    (body (cddr rest-args)))
                               (multiple-value-bind (non-aux-part aux-params)
                                   (parse-lambda-list-for-aux lambda-list)
                                 ;; Extract variables from lambda list (no destructuring for defun)
                                 (let* ((var-names (extract-lambda-list-vars non-aux-part nil))
                                        (param-bindings (mapcar #'list var-names)))
                                   (when param-bindings
                                     (check-binding-form :defun-regular param-bindings body line column position-map)))
                                 ;; Check &aux parameters (sequential like LET*)
                                 (when aux-params
                                   (check-binding-form :defun-aux aux-params body line column position-map body))))))

                         ;; Check LAMBDA
                         (when (base:symbol-matches-p head "LAMBDA")
                           (when (and (a:proper-list-p rest-args)
                                      (>= (length rest-args) 2)
                                      ;; Validate lambda-list is actually a list (not a string or other type)
                                      (listp (first rest-args)))
                             (let* ((lambda-list (first rest-args))
                                    (body (rest rest-args)))
                               (multiple-value-bind (non-aux-part aux-params)
                                   (parse-lambda-list-for-aux lambda-list)
                                 ;; Extract variables from lambda list (no destructuring for lambda)
                                 (let* ((var-names (extract-lambda-list-vars non-aux-part nil))
                                        (param-bindings (mapcar #'list var-names)))
                                   (when param-bindings
                                     (check-binding-form :defun-regular param-bindings body line column position-map)))
                                 ;; Check &aux parameters (sequential like LET*)
                                 (when aux-params
                                   (check-binding-form :defun-aux aux-params body line column position-map body))))))

                         ;; Check LET
                         (when (and (stringp head)
                                    (string-equal (base:symbol-name-from-string head) "LET"))
                           (when (and (a:proper-list-p rest-args) (>= (length rest-args) 2))
                             (handler-case
                                 (let ((bindings (first rest-args))
                                       (body (rest rest-args)))
                                   (check-binding-form :let bindings body line column position-map))
                               (type-error (e)
                                 (format *error-output* "~%Warning: Skipping LET form at ~A:~A due to unexpected structure: ~A~%"
                                         file line e)
                                 nil))))

                         ;; Check LET* (sequential bindings)
                         (when (and (stringp head)
                                    (string-equal (base:symbol-name-from-string head) "LET*"))
                           (when (and (a:proper-list-p rest-args) (>= (length rest-args) 2))
                             (let ((bindings (first rest-args))
                                   (body (rest rest-args)))
                               (check-binding-form :let* bindings body line column position-map))))

                         ;; Check DESTRUCTURING-BIND (parallel bindings like LET)
                         (when (base:symbol-matches-p head "DESTRUCTURING-BIND")
                           (when (and (a:proper-list-p rest-args) (>= (length rest-args) 3))
                             (let* ((lambda-list (first rest-args))
                                    ;; Second arg is the value form, skip it
                                    (body (cddr rest-args))
                                    ;; Extract variables from destructuring lambda list (allow destructuring)
                                    (var-names (extract-lambda-list-vars lambda-list t))
                                    (bindings (mapcar #'list var-names)))
                               (when bindings
                                 (check-binding-form :let bindings body line column position-map)))))

                         ;; Check MULTIPLE-VALUE-BIND (parallel bindings like LET)
                         (when (base:symbol-matches-p head "MULTIPLE-VALUE-BIND")
                           (when (and (a:proper-list-p rest-args) (>= (length rest-args) 3))
                             (let* ((vars (first rest-args))
                                    ;; Second arg is the values form, skip it
                                    (body (cddr rest-args))
                                    ;; Pass vars as a single binding - extract-bindings handles patterns
                                    (bindings (list vars)))
                               (check-binding-form :let bindings body line column position-map))))

                         ;; Check DEFMACRO (like DEFUN with &aux support and destructuring)
                         (when (base:symbol-matches-p head "DEFMACRO")
                           (when (and (a:proper-list-p rest-args)
                                      (>= (length rest-args) 3)
                                      ;; Validate lambda-list is actually a list
                                      (listp (second rest-args)))
                             (let* ((lambda-list (second rest-args))
                                    (body (cddr rest-args)))
                               (multiple-value-bind (non-aux-part aux-params)
                                   (parse-lambda-list-for-aux lambda-list)
                                 ;; Extract variables from macro lambda list (allow destructuring)
                                 (let* ((var-names (extract-lambda-list-vars non-aux-part t))
                                        (param-bindings (mapcar #'list var-names)))
                                   (when param-bindings
                                     (check-binding-form :defun-regular param-bindings body line column position-map)))
                                 ;; Check &aux parameters (sequential like LET*)
                                 (when aux-params
                                   (check-binding-form :defun-aux aux-params body line column position-map body))))))

                         ;; Check DOLIST
                         (when (base:symbol-matches-p head "DOLIST")
                           (when (and (a:proper-list-p rest-args) (>= (length rest-args) 2))
                             (let* ((spec (first rest-args))
                                    (body (rest rest-args)))
                               (when (and (a:proper-list-p spec) (>= (length spec) 2))
                                 (let* ((var (first spec))
                                        (bindings (list (list var))))
                                   (check-binding-form :let bindings body line column position-map))))))

                         ;; Check DOTIMES
                         ;; Note: DOTIMES variables are conventionally ignorable since they're often
                         ;; just used for counting. We skip checking them to avoid false positives.
                         ;; Example: (dotimes (i 10) (print "hello")) - 'i' not used is normal

                         ;; Check DO
                         (when (base:symbol-matches-p head "DO")
                           (when (and (a:proper-list-p rest-args) (>= (length rest-args) 2))
                             (let* ((var-clauses (first rest-args))
                                    (end-test-clause (second rest-args))
                                    (body (cddr rest-args))
                                    ;; end-test-clause is (test-form [result-forms...])
                                    (test-form (when (a:proper-list-p end-test-clause) (first end-test-clause)))
                                    (result-forms (when (a:proper-list-p end-test-clause) (rest end-test-clause)))
                                    ;; aux-context for DO contains: var-clauses, test, result, and body
                                    ;; First element is the original var-clauses for scope calculation
                                    (do-context (cons var-clauses (append (list test-form) result-forms body)))
                                    ;; DO variable clauses are (var init [step])
                                    ;; Extract only the variable names for check-binding-form
                                    (bindings (when (a:proper-list-p var-clauses)
                                                (mapcar (lambda (clause)
                                                          (if (consp clause)
                                                              (list (first clause))  ; Just the var name
                                                              (list clause)))  ; Simple var
                                                        var-clauses))))
                               (when bindings
                                 (check-binding-form :do bindings body line column position-map do-context)))))

                         ;; Check LOOP
                         (when (base:symbol-matches-p head "LOOP")
                           (when (a:proper-list-p rest-args)
                             (multiple-value-bind (bindings body)
                                 (parse-loop-clauses rest-args)
                               (when bindings
                                 ;; All LOOP variables are in scope for body only (not binding clauses)
                                 ;; Treat as parallel bindings like LET
                                 (check-binding-form :let bindings body line column position-map)))))

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
                                             ;; Only iterate if it's a proper list (not a dotted pair)
                                             (when (and (listp expr) (a:proper-list-p expr))
                                               (dolist (subexpr expr)
                                                 (check-quasi subexpr)))))))))
                             ;; Only iterate if it's a proper list (not a dotted pair)
                             (when (and (a:proper-list-p rest-args) (a:proper-list-p rest-args))
                               (dolist (arg rest-args)
                                 (check-quasi arg)))))

                         ;; Recursively check nested forms (skip if we already handled it above)
                         (unless (or (eq head 'eclector.reader:quasiquote)
                                     (and (symbolp head)
                                          (string-equal (symbol-name head) "QUASIQUOTE")
                                          (string-equal (package-name (symbol-package head)) "ECLECTOR.READER")))
                           ;; Only iterate if it's a proper list (not a dotted pair)
                           (when (and (a:proper-list-p rest-args) (a:proper-list-p rest-args))
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
    (type-error (e)
      ;; Print warning message only in debug mode
      nil)))
