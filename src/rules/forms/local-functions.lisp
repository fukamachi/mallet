(defpackage #:mallet/rules/forms/local-functions
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)
   (#:utils #:mallet/utils)
   (#:violation #:mallet/violation))
  (:export #:unused-local-functions-rule))
(in-package #:mallet/rules/forms/local-functions)

;;; Utilities

(defvar *violations* nil
  "Dynamic variable holding the list of violations found during checking.
Bound in base:check-form and accessible to all helper functions.")

(defvar *file* nil
  "Dynamic variable holding the current file being checked.
Bound in base:check-form and accessible to all helper functions.")

(defun special-variable-p (name)
  "Check if NAME follows special variable convention (*name*)."
  (and (> (length name) 2)
       (char= (char name 0) #\*)
       (char= (char name (1- (length name))) #\*)))

(defun ignored-var-p (var-name ignored-vars)
  "Check if variable should be ignored (in ignore list, starts with _, is NIL, or is a special variable)."
  (let ((name (base:symbol-name-from-string var-name)))
    (or (member name ignored-vars :test #'string=)
        (string-equal name "NIL")
        (special-variable-p name)
        (and (> (length name) 0)
             (char= (char name 0) #\_)))))

(defun extract-ignored-vars (body)
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

;;; Shadow finding (simplified for function references)

(defstruct shadow-info
  "Information about a shadowing point for a function.
FORM: The shadowing form itself (for debugging)
TYPE: Shadow type - :complete for complete shadowing
SEARCHABLE: For partial shadows, sub-expressions that CAN be searched"
  form
  type
  searchable)

(defvar *shadows* nil
  "Dynamic variable holding the list of shadow-info structs during shadow finding.
Bound in find-shadows and accessible to all helper functions.")

(defun binds-function-p (func-def target-name)
  "Check if FUNC-DEF introduces a function with target-name.
FUNC-DEF should be a (name lambda-list . body) form."
  (and (consp func-def)
       (stringp (first func-def))
       (string-equal (base:symbol-name-from-string (first func-def)) target-name)))

(defun find-shadows-in-expr (expr target-name)
  "Recursively find all shadowing points in expr.
Modifies *SHADOWS* special variable by pushing new shadow-info structs."
  (when (consp expr)
    (let ((head (first expr))
          (rest-args (rest expr)))
      (cond
        ;; QUOTE - skip entirely
        ((or (eq head 'cl:quote)
             (eq head 'quote)
             (base:symbol-matches-p head "QUOTE"))
         nil)

        ;; FLET - check for shadowing in function definitions
        ((base:symbol-matches-p head "FLET")
         (when (and (consp rest-args)
                    (a:proper-list-p (first rest-args)))
           (let ((func-defs (first rest-args)))
             (when (some (lambda (fd) (binds-function-p fd target-name)) func-defs)
               ;; Complete shadow - cannot search this form
               (push (make-shadow-info
                      :form expr
                      :type :complete
                      :searchable nil)
                     *shadows*))
             ;; Continue searching if not shadowed
             (unless (some (lambda (fd) (binds-function-p fd target-name)) func-defs)
               (dolist (arg rest-args)
                 (find-shadows-in-expr arg target-name))))))

        ;; LABELS - check for shadowing in function definitions
        ((base:symbol-matches-p head "LABELS")
         (when (and (consp rest-args)
                    (a:proper-list-p (first rest-args)))
           (let ((func-defs (first rest-args)))
             (when (some (lambda (fd) (binds-function-p fd target-name)) func-defs)
               ;; Complete shadow - cannot search this form
               (push (make-shadow-info
                      :form expr
                      :type :complete
                      :searchable nil)
                     *shadows*))
             ;; Continue searching if not shadowed
             (unless (some (lambda (fd) (binds-function-p fd target-name)) func-defs)
               (dolist (arg rest-args)
                 (find-shadows-in-expr arg target-name))))))

        ;; Default: recursively search
        (t
         (when (consp (car expr))
           (find-shadows-in-expr (car expr) target-name))
         (when (consp (cdr expr))
           (find-shadows-in-expr (cdr expr) target-name)))))))

(defun find-shadows (func-name body)
  "Find all points where FUNC-NAME is shadowed in BODY.
Returns list of SHADOW-INFO structs."
  (let ((target-name (base:symbol-name-from-string func-name))
        (*shadows* '()))
    ;; Find all shadows in the body
    (dolist (form body)
      (find-shadows-in-expr form target-name))
    (nreverse *shadows*)))

;;; Function reference finding

(defun find-function-references (func-name body)
  "Find if FUNC-NAME is referenced in function position in BODY.
This is used for checking unused local functions (flet/labels).
Unlike variable references, this ONLY matches when the name appears in function position (CAR of a list)."
  (let ((target-name (base:symbol-name-from-string func-name))
        (shadows (find-shadows func-name body)))
    (labels ((matches-shadow-p (expr)
               "Check if EXPR matches any shadow in the list."
               (find expr shadows :key #'shadow-info-form :test #'eq))

             (search-expr (expr in-function-position)
               "Search for function references. Only matches in function position."
               (cond
                 ((null expr) nil)

                 ;; String matching target name - only count if in function position
                 ((stringp expr)
                  (and in-function-position
                       (string-equal (base:symbol-name-from-string expr) target-name)))

                 ;; Check for shadows first
                 ((consp expr)
                  (let ((shadow (matches-shadow-p expr)))
                    (if shadow
                        ;; Shadowed - cannot search
                        nil
                        ;; Not shadowed - search recursively
                        (cond
                          ;; QUOTE - if in function position, check if quoted symbol matches
                          ((or (eq (first expr) 'cl:quote)
                               (eq (first expr) 'quote)
                               (base:symbol-matches-p (first expr) "QUOTE"))
                           ;; In function position (e.g., (funcall 'foo ...)), check quoted value
                           (when (and in-function-position
                                      (rest expr)
                                      (stringp (second expr)))
                             (string-equal (base:symbol-name-from-string (second expr)) target-name)))
                          ;; FUNCTION (#') - the argument is in function namespace, so it's a reference
                          ((or (eq (first expr) 'cl:function)
                               (eq (first expr) 'function)
                               (base:symbol-matches-p (first expr) "FUNCTION"))
                           (when (rest expr)
                             (let ((func-arg (second expr)))
                               (or
                                ;; Check if direct argument is a matching symbol
                                (and (stringp func-arg)
                                     (string-equal (base:symbol-name-from-string func-arg) target-name))
                                ;; If it's a lambda, recursively search its body
                                (when (and (consp func-arg)
                                           (or (base:symbol-matches-p (first func-arg) "LAMBDA")
                                               (eq (first func-arg) 'cl:lambda)
                                               (eq (first func-arg) 'lambda)))
                                  ;; Lambda format: (lambda (args...) body...)
                                  ;; Skip lambda keyword and lambda-list, search body
                                  (when (cddr func-arg)
                                    (some (lambda (body-form) (search-expr body-form nil))
                                          (cddr func-arg))))))))
                          ;; FUNCALL/APPLY - first argument after operator is in function namespace
                          ((or (base:symbol-matches-p (first expr) "FUNCALL")
                               (base:symbol-matches-p (first expr) "APPLY"))
                           (when (rest expr)
                             (or (search-expr (second expr) t)  ; First arg can be function
                                 (some (lambda (arg) (search-expr arg nil))
                                       (cddr expr)))))  ; Rest are value position
                          ;; Default: CAR is function position, CDR is value position
                          (t
                           (or (search-expr (car expr) t)
                               (some (lambda (arg) (search-expr arg nil))
                                     (cdr expr))))))))

                 (t nil))))

      ;; Search all forms in body, none start in function position
      (some (lambda (form) (search-expr form nil)) body))))

;;; FLET/LABELS checking

(defun check-flet-bindings (expr line column position-map rule)
  "Check FLET for unused local function bindings."
  (let ((rest-args (rest expr)))
    (when (and (consp rest-args)
               (a:proper-list-p (first rest-args)))
      (let ((func-defs (first rest-args))
            (body (rest rest-args)))
        (when (a:proper-list-p func-defs)
          (dolist (func-def func-defs)
            (when (and (consp func-def)
                       (stringp (first func-def)))
              (let* ((func-name (first func-def))
                     (ignored-vars (extract-ignored-vars body))
                     (is-ignored (ignored-var-p func-name ignored-vars))
                     (is-referenced (find-function-references func-name body)))
                (unless (or is-ignored is-referenced)
                  ;; Check if rule is suppressed
                  (let ((state-symbol (find-symbol "*SUPPRESSION-STATE*" "MALLET/ENGINE")))
                    (unless (and state-symbol
                                 (boundp state-symbol)
                                 (symbol-value state-symbol)
                                 (funcall (find-symbol "RULE-SUPPRESSED-P" "MALLET/SUPPRESSION")
                                          (symbol-value state-symbol)
                                          (base:rule-name rule)))
                      ;; Find position of function name
                      (multiple-value-bind (func-line func-column)
                          (parser:find-position func-name position-map line column)
                        (when (and func-line func-column)
                          (push (make-instance 'violation:violation
                                               :rule (base:rule-name rule)
                                               :file *file*
                                               :line func-line
                                               :column func-column
                                               :severity (base:rule-severity rule)
                                               :message
                                               (format nil "Local function '~A' is unused"
                                                       (base:symbol-name-from-string func-name))
                                               :fix nil)
                                *violations*))))))))))))))

(defun check-labels-bindings (expr line column position-map rule)
  "Check LABELS for unused local function bindings."
  (let ((rest-args (rest expr)))
    (when (and (consp rest-args)
               (a:proper-list-p (first rest-args)))
      (let ((func-defs (first rest-args))
            (body (rest rest-args)))
        (when (a:proper-list-p func-defs)
          (dolist (func-def func-defs)
            (when (and (consp func-def)
                       (stringp (first func-def)))
              (let* ((func-name (first func-def))
                     (ignored-vars (extract-ignored-vars body))
                     (is-ignored (ignored-var-p func-name ignored-vars))
                     ;; For LABELS, functions can reference each other (unlike FLET)
                     ;; We search in OTHER function bodies AND the main body
                     ;; (but NOT the function's own body - self-recursion doesn't count as "used")
                     ;; Extract bodies from OTHER func-defs: each is (name lambda-list . body)
                     (other-func-bodies (mapcan (lambda (fd)
                                                  (when (and (consp fd)
                                                             (consp (rest fd))
                                                             ;; Skip the current function
                                                             (not (eq fd func-def)))
                                                    ;; Skip name and lambda-list, get body
                                                    (copy-list (cddr fd))))
                                                func-defs))
                     (all-forms (append other-func-bodies body))
                     (is-referenced (find-function-references func-name all-forms)))
                (unless (or is-ignored is-referenced)
                  ;; Check if rule is suppressed
                  (let ((state-symbol (find-symbol "*SUPPRESSION-STATE*" "MALLET/ENGINE")))
                    (unless (and state-symbol
                                 (boundp state-symbol)
                                 (symbol-value state-symbol)
                                 (funcall (find-symbol "RULE-SUPPRESSED-P" "MALLET/SUPPRESSION")
                                          (symbol-value state-symbol)
                                          (base:rule-name rule)))
                      ;; Find position of function name
                      (multiple-value-bind (func-line func-column)
                          (parser:find-position func-name position-map line column)
                        (when (and func-line func-column)
                          (push (make-instance 'violation:violation
                                               :rule (base:rule-name rule)
                                               :file *file*
                                               :line func-line
                                               :column func-column
                                               :severity (base:rule-severity rule)
                                               :message
                                               (format nil "Local function '~A' is unused"
                                                       (base:symbol-name-from-string func-name))
                                               :fix nil)
                                *violations*))))))))))))))

(defun check-expr (expr line column position-map rule)
  "Recursively check expression for unused local functions."
  (when (consp expr)
    (let ((head (first expr))
          (rest-args (rest expr)))
      ;; 1. Dispatch to specific checkers for FLET/LABELS
      (cond
        ((base:symbol-matches-p head "FLET")
         (check-flet-bindings expr line column position-map rule))
        ((base:symbol-matches-p head "LABELS")
         (check-labels-bindings expr line column position-map rule)))

      ;; 2. Handle special forms for recursion
      (cond
        ;; QUOTE - skip entirely (pure data)
        ((or (eq head 'cl:quote)
             (eq head 'quote)
             (base:symbol-matches-p head "QUOTE"))
         nil)

        ;; DEFSTRUCT - skip entirely
        ((base:symbol-matches-p head "DEFSTRUCT")
         nil)

        ;; DEFCLASS - skip entirely
        ((base:symbol-matches-p head "DEFCLASS")
         nil)

        ;; DEFPACKAGE - skip entirely
        ((base:symbol-matches-p head "DEFPACKAGE")
         nil)

        ;; DEFTYPE - skip entirely
        ((base:symbol-matches-p head "DEFTYPE")
         nil)

        ;; DEFSETF - skip entirely
        ((base:symbol-matches-p head "DEFSETF")
         nil)

        ;; DEFINE-MODIFY-MACRO - skip entirely
        ((base:symbol-matches-p head "DEFINE-MODIFY-MACRO")
         nil)

        ;; DEFINE-SETF-EXPANDER - skip entirely
        ((base:symbol-matches-p head "DEFINE-SETF-EXPANDER")
         nil)

        ;; QUASIQUOTE - only check unquoted parts
        ((or (eq head 'eclector.reader:quasiquote)
             (and (symbolp head)
                  (string-equal (symbol-name head) "QUASIQUOTE")
                  (string-equal (package-name (symbol-package head)) "ECLECTOR.READER")))
         (labels ((check-quasi (expr)
                    "Recursively check quasiquoted expression, only descending into unquotes."
                    (when (consp expr)
                      (let ((h (first expr)))
                        (cond
                          ;; UNQUOTE - check the unquoted expression
                          ((or (eq h 'eclector.reader:unquote)
                               (and (symbolp h)
                                    (string-equal (symbol-name h) "UNQUOTE")
                                    (string-equal (package-name (symbol-package h)) "ECLECTOR.READER")))
                           (when (rest expr)
                             (check-expr (second expr) line column position-map rule)))
                          ;; UNQUOTE-SPLICING - check the unquoted expression
                          ((or (eq h 'eclector.reader:unquote-splicing)
                               (and (symbolp h)
                                    (string-equal (symbol-name h) "UNQUOTE-SPLICING")
                                    (string-equal (package-name (symbol-package h)) "ECLECTOR.READER")))
                           (when (rest expr)
                             (check-expr (second expr) line column position-map rule)))
                          ;; Other forms - recursively look for unquotes
                          (t
                           (when (and (listp expr) (a:proper-list-p expr))
                             (dolist (subexpr expr)
                               (check-quasi subexpr)))))))))
           (when (and (a:proper-list-p rest-args))
             (dolist (arg rest-args)
               (check-quasi arg)))))

        ;; 3. Default: recursively check all nested forms
        (t
         (when (and (a:proper-list-p rest-args))
           (dolist (subexpr rest-args)
             (when (consp subexpr)
               ;; Look up the position of this subexpression before recursing
               (multiple-value-bind (subexpr-line subexpr-column)
                   (parser:find-position subexpr position-map line column)
                 ;; Recurse using check-form-recursive to get suppression handling
                 (let ((nested-violations
                        (base:check-form-recursive rule subexpr *file*
                                                  (or subexpr-line line)
                                                  (or subexpr-column column)
                                                  nil
                                                  position-map)))
                   ;; Accumulate violations from nested call
                   (setf *violations* (append *violations* nested-violations))))))))))))

;;; Rule class

(defclass unused-local-functions-rule (base:rule)
  ()
  (:default-initargs
   :name :unused-local-functions
   :description "Local functions (flet/labels) should be used or explicitly ignored"
   :severity :warning
   :type :form)
  (:documentation "Rule to detect unused local functions in flet and labels forms."))

(defmethod base:check-form ((rule unused-local-functions-rule) form file)
  "Check that all local functions are either used or explicitly ignored."
  (check-type form parser:form)
  (check-type file pathname)

  ;; Delegate to check-form-recursive for automatic suppression handling
  (base:check-form-recursive rule
                             (parser:form-expr form)
                             file
                             (parser:form-line form)
                             (parser:form-column form)
                             nil ; no function-name context
                             (parser:form-position-map form)))

(defmethod base:check-form-recursive ((rule unused-local-functions-rule) expr file line column
                                       &optional function-name position-map)
  "Recursively check for unused local functions with automatic suppression handling."
  (declare (ignore function-name))

  (handler-case
      ;; Bind special variables and call check-expr
      (let ((*violations* '())
            (*file* file))
        (check-expr expr line column position-map rule)
        (nreverse *violations*))
    (error (e)
      ;; Only show warning in debug mode
      (when (utils:debug-mode-p)
        (format *error-output* "~%Warning: Error checking unused-local-functions at ~A:~A~%  ~A~%"
                file line e))
      ;; Return empty list on error
      '())))
