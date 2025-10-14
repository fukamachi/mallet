(defpackage #:mallet/rules/forms/variables
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)
   (#:loop-parser #:mallet/parser/loop)
   (#:violation #:mallet/violation)
   (#:scope #:mallet/utils/scope)
   (#:utils #:mallet/utils))
  (:export #:unused-variables-rule
           #:unused-loop-variables-rule))
(in-package #:mallet/rules/forms/variables)

;;; Utilities

(defvar *violations* nil
  "Dynamic variable holding the list of violations found during checking.
Bound in base:check-form and accessible to all helper functions.")

(defvar *file* nil
  "Dynamic variable holding the current file being checked.
Bound in base:check-form and accessible to all helper functions.")

(defvar *shadows* nil
  "Dynamic variable holding the list of shadow-info structs during shadow finding.
Bound in find-shadows and accessible to all helper functions.")

(defun debug-mode-p ()
  "Check if debug mode is enabled."
  (and (find-symbol "*DEBUG-MODE*" "MALLET")
       (symbol-value (find-symbol "*DEBUG-MODE*" "MALLET"))))

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

;;; Binding extraction

(defun extract-from-pattern (pattern)
  "Recursively extract all variable names from a destructuring pattern."
  (cond
    ;; Simple variable
    ((stringp pattern)
     (if (or (utils:lambda-list-keyword-p pattern)   ; Lambda-list keywords
             (utils:keyword-string-p pattern))       ; Keyword arguments
         nil  ; Skip lambda-list keywords and keyword arguments
         (list pattern)))
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
    (t nil)))

(defun extract-bindings (binding-form &optional (context :binding))
  "Extract variable names from binding form (supports simple and nested destructuring).
CONTEXT determines interpretation of ambiguous 2-element lists:
  :binding - (var init-form) extracts only var (default, for LET/LET*/DO)
  :destructuring - (a b) extracts both a and b (for LOOP, DESTRUCTURING-BIND)"
  (cond
    ;; Simple string (variable name)
    ((stringp binding-form)
     (list binding-form))
    ;; Standard LET/LET* binding: (var init-form)
    ;; These are 2-element lists where only the first is a variable
    ;; The second element is code (the init form), not a variable binding
    ;; BUT: In :destructuring context, all strings are variables
    ((and (consp binding-form)
          (utils:proper-list-of-exact-length-p binding-form 2)
          (stringp (first binding-form))
          (eq context :binding))  ; Only treat as LET binding in :binding context
     ;; Extract only the variable (first element), not the init form
     (list (first binding-form)))
    ;; Lambda list binding with default/supplied-p: (var default) or (var default supplied-p)
    ;; Check if this has non-string elements indicating it's not pure destructuring
    ((and (consp binding-form)
          (utils:proper-list-of-length-range-p binding-form 2 3)
          ;; At least one element after the first is not a string (value/init form)
          (some (lambda (elem) (not (stringp elem))) (rest binding-form)))
     ;; Extract only variable(s) from first element, skip init forms
     (extract-from-pattern (first binding-form)))
    ;; Pure destructuring pattern: all elements are strings (variables)
    ;; or contains lambda-list keywords, or nested patterns
    ;; Examples: (a b c), (a &key b c), ((a b) c), (a . b)
    ((consp binding-form)
     (extract-from-pattern binding-form))
    ;; Anything else is not a valid binding form
    (t nil)))

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

;;; Lambda list processing

(defun contains-lambda-list-keyword-p (pattern)
  "Check if PATTERN (a list) contains any lambda-list keywords.
This indicates it's a nested lambda list rather than a simple destructuring pattern."
  (when (and (consp pattern) (a:proper-list-p pattern))
    (some #'utils:lambda-list-keyword-p pattern)))

(defun extract-lambda-list-vars (lambda-list allow-destructuring)
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
               ((utils:lambda-list-keyword-p elem)
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
                   ;; Required params: simple var, destructuring pattern, or nested lambda list (if allowed)
                   (cond
                     ((not allow-destructuring)
                      ;; Simple variable only
                      (when (stringp elem)
                        (push elem vars)))
                     ((stringp elem)
                      ;; Simple string variable
                      (push elem vars))
                     ((contains-lambda-list-keyword-p elem)
                      ;; Nested lambda list - recursively extract variables
                      (let ((extracted (extract-lambda-list-vars elem t)))
                        (setf vars (append vars extracted))))
                     (t
                      ;; Simple destructuring pattern - extract all strings
                      (let ((extracted (extract-bindings (list elem) :destructuring)))
                        (setf vars (append vars extracted))))))
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
                           (utils:proper-list-of-exact-length-p (first elem) 2)
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

(defun parse-lambda-list-for-aux (lambda-list)
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

(defun extract-optional-key-defaults (lambda-list)
  "Extract default forms from &optional and &key parameters in LAMBDA-LIST.
Returns a list of default forms that can reference earlier parameters."
  (let ((defaults '())
        (state :required)
        (i 0))
    (loop while (< i (length lambda-list))
          for elem = (nth i lambda-list)
          do (cond
               ;; Lambda-list keywords
               ((utils:lambda-list-keyword-p elem)
                (let ((kw (base:symbol-name-from-string elem)))
                  (cond
                    ((string-equal kw "&OPTIONAL")
                     (setf state :optional))
                    ((or (string-equal kw "&REST") (string-equal kw "&BODY"))
                     (setf state :rest))
                    ((string-equal kw "&KEY")
                     (setf state :key))
                    ((string-equal kw "&AUX")
                     ;; Stop processing - &aux is handled separately
                     (return)))))
               ;; Parameter
               (t
                (ecase state
                  (:required
                   ;; Required parameters have no defaults
                   nil)
                  (:optional
                   ;; &optional: (var default) or (var default supplied-p)
                   (when (and (consp elem) (>= (length elem) 2))
                     ;; Second element is the default form
                     (push (second elem) defaults)))
                  (:rest
                   ;; &rest has no default
                   (setf state :required))
                  (:key
                   ;; &key: (var default), (var default supplied-p),
                   ;; ((:keyword var) default), ((:keyword var) default supplied-p)
                   (cond
                     ;; ((:keyword var) default ...) form
                     ((and (consp elem)
                           (consp (first elem))
                           (>= (length elem) 2))
                      ;; Second element is the default form
                      (push (second elem) defaults))
                     ;; (var default ...) form
                     ((and (consp elem)
                           (stringp (first elem))
                           (>= (length elem) 2))
                      ;; Second element is the default form
                      (push (second elem) defaults)))))))
             (incf i))
    (nreverse defaults)))

(defun calculate-scope (form-type remaining-bindings body aux-context &optional optional-key-defaults)
  "Calculate the scope where a variable binding is available.
Returns a list of forms where the variable should be checked for usage.
OPTIONAL-KEY-DEFAULTS is a list of default forms from &optional and &key parameters (for :defun-regular only)."
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
     ;; Regular parameters - scope is optional/key defaults + body
     ;; This allows required parameters to be used in keyword parameter defaults
     (append optional-key-defaults body))
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
                    (when (utils:proper-list-of-min-length-p clause 3)
                      (third clause)))  ; step form
                  var-clauses))
        ;; Test, result, and body
        rest-context)))
    (:do*
     ;; DO* variables - sequential binding like LET*, then mutual recursion like DO
     ;; aux-context is (var-clauses test-form result-form . body-forms)
     (let ((var-clauses (first aux-context))
           (rest-context (rest aux-context)))
       (append
        ;; Subsequent bindings' init forms (sequential like LET*)
        (mapcar (lambda (clause)
                  (when (utils:proper-list-of-min-length-p clause 2)
                    (second clause)))  ; init form
                remaining-bindings)
        ;; All step forms (including this variable's own - we'll filter later)
        (when (a:proper-list-p var-clauses)
          (mapcar (lambda (clause)
                    (when (utils:proper-list-of-min-length-p clause 3)
                      (third clause)))  ; step form
                  var-clauses))
        ;; Test, result, and body
        rest-context)))))

;;; Two-Phase Architecture for Shadow-Aware Reference Finding

(defstruct shadow-info
  "Information about a shadowing point for a variable.
FORM: The shadowing form itself (for debugging)
TYPE: Shadow type - determines how to handle partial shadowing:
      :complete - Fully shadowed, cannot search this form
      :partial-let - LET shadowing: can search ALL init forms (parallel)
      :partial-let* - LET* shadowing: can search up to and including shadow (sequential)
      :partial-loop-sequential - LOOP sequential FOR: like LET*
      :partial-loop-and - LOOP AND: like LET (parallel)
SEARCHABLE: For partial shadows, sub-expressions that CAN be searched"
  form
  type
  searchable)

;;; Shadow finding

(defun binds-same-name-p (binding-form target-name)
  "Check if BINDING-FORM introduces a variable with target-name."
  (let ((vars (extract-bindings binding-form)))
    (some (lambda (v)
            (string-equal (base:symbol-name-from-string v) target-name))
          vars)))

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

        ;; LET - check for shadowing in bindings
        ((utils:form-head-matches-p head "LET")
         (when (utils:proper-list-of-min-length-p rest-args 2)
           (let ((bindings (first rest-args)))
             (when (and (a:proper-list-p bindings)
                        (some (lambda (b) (binds-same-name-p b target-name)) bindings))
               ;; Found shadowing - record partial-let shadow
               ;; Searchable: ALL init forms (parallel bindings)
               (let ((init-forms (mapcar (lambda (b)
                                          (when (consp b) (second b)))
                                        bindings)))
                 (push (make-shadow-info
                        :form expr
                        :type :partial-let
                        :searchable (remove nil init-forms))
                       *shadows*)))
             ;; Continue searching in non-shadowed parts or inside the shadow
             (when (and (a:proper-list-p bindings)
                        (not (some (lambda (b) (binds-same-name-p b target-name)) bindings)))
               ;; No shadow here, continue recursion
               (dolist (binding bindings)
                 (when (consp binding)
                   (find-shadows-in-expr (second binding) target-name)))
               (dolist (form (rest rest-args))
                 (find-shadows-in-expr form target-name))))))

        ;; LET* - check for shadowing in bindings
        ((utils:form-head-matches-p head "LET*")
         (when (utils:proper-list-of-min-length-p rest-args 2)
           (let ((bindings (first rest-args)))
             (when (and (a:proper-list-p bindings)
                        (some (lambda (b) (binds-same-name-p b target-name)) bindings))
               ;; Found shadowing - record partial-let* shadow
               ;; Searchable: init forms UP TO AND INCLUDING the shadowing binding
               (let ((searchable-inits '()))
                 (loop for binding in bindings
                       do (when (consp binding)
                            (push (second binding) searchable-inits))
                       until (binds-same-name-p binding target-name))
                 (push (make-shadow-info
                        :form expr
                        :type :partial-let*
                        :searchable (nreverse searchable-inits))
                       *shadows*)))
             ;; Continue searching in non-shadowed parts
             (when (and (a:proper-list-p bindings)
                        (not (some (lambda (b) (binds-same-name-p b target-name)) bindings)))
               (dolist (binding bindings)
                 (when (consp binding)
                   (find-shadows-in-expr (second binding) target-name)))
               (dolist (form (rest rest-args))
                 (find-shadows-in-expr form target-name))))))

        ;; DEFUN/LAMBDA/DEFMACRO - check parameters for shadowing
        ((or (utils:form-head-matches-p head "DEFUN")
             (utils:form-head-matches-p head "LAMBDA")
             (utils:form-head-matches-p head "DEFMACRO"))
         (let* ((lambda-list-pos (if (string-equal (base:symbol-name-from-string head) "LAMBDA") 0 1))
                (lambda-list (when (utils:proper-list-of-min-length-p rest-args (1+ lambda-list-pos))
                               (nth lambda-list-pos rest-args))))
           (when (and (a:proper-list-p lambda-list)
                      (some (lambda (param)
                              (and (stringp param)
                                   (not (utils:lambda-list-keyword-p param))
                                   (string-equal (base:symbol-name-from-string param) target-name)))
                            lambda-list))
             ;; Complete shadow - parameter shadows the variable
             (push (make-shadow-info
                    :form expr
                    :type :complete
                    :searchable nil)
                   *shadows*))
           ;; Continue searching in non-shadowed parts
           (unless (and (a:proper-list-p lambda-list)
                        (some (lambda (param)
                                (and (stringp param)
                                     (not (utils:lambda-list-keyword-p param))
                                     (string-equal (base:symbol-name-from-string param) target-name)))
                              lambda-list))
             (dolist (arg rest-args)
               (find-shadows-in-expr arg target-name)))))

        ;; DESTRUCTURING-BIND, MULTIPLE-VALUE-BIND
        ((or (utils:form-head-matches-p head "DESTRUCTURING-BIND")
             (utils:form-head-matches-p head "MULTIPLE-VALUE-BIND"))
         (when (utils:proper-list-of-min-length-p rest-args 2)
           (let ((vars (first rest-args))
                 (init-form (second rest-args)))  ; The init-form is evaluated in outer scope
             (let ((all-vars (extract-bindings vars :destructuring)))
               (when (some (lambda (v)
                             (string-equal (base:symbol-name-from-string v) target-name))
                           all-vars)
                 ;; Partial shadow - init-form can be searched (evaluated in outer scope)
                 ;; Body is shadowed (new bindings active)
                 (push (make-shadow-info
                        :form expr
                        :type :partial-let  ; Reuse partial-let type (similar behavior)
                        :searchable (list init-form))  ; Only init-form is searchable
                       *shadows*)))
             ;; Continue searching if not shadowed
             (unless (some (lambda (v)
                             (string-equal (base:symbol-name-from-string v) target-name))
                           (extract-bindings vars :destructuring))
               (dolist (arg rest-args)
                 (find-shadows-in-expr arg target-name))))))

        ;; DOLIST, DOTIMES
        ;; (dolist (var list-form [result-form]) body...)
        ;; (dotimes (var count-form [result-form]) body...)
        ;; The list-form/count-form is evaluated in outer scope (searchable)
        ;; The result-form and body are evaluated with variable bound (not searchable)
        ((or (utils:form-head-matches-p head "DOLIST")
             (utils:form-head-matches-p head "DOTIMES"))
         (when (utils:proper-list-of-min-length-p rest-args 1)
           (let* ((spec (first rest-args))
                  (var (when (a:proper-list-p spec) (first spec)))
                  (source-form (when (utils:proper-list-of-min-length-p spec 2)
                                (second spec))))
             (when (and (stringp var)
                        (string-equal (base:symbol-name-from-string var) target-name))
               ;; Partial shadow - source-form (list/count) evaluated in outer scope
               (push (make-shadow-info
                      :form expr
                      :type :partial-let
                      :searchable (if source-form (list source-form) nil))
                     *shadows*))
             ;; Continue searching if not shadowed
             (unless (and (stringp var)
                          (string-equal (base:symbol-name-from-string var) target-name))
               (dolist (arg rest-args)
                 (find-shadows-in-expr arg target-name))))))

        ;; DO
        ((utils:form-head-matches-p head "DO")
         (when (utils:proper-list-of-min-length-p rest-args 2)
           (let ((var-clauses (first rest-args)))
             (when (and (a:proper-list-p var-clauses)
                        (some (lambda (b) (binds-same-name-p b target-name)) var-clauses))
               ;; Complete shadow
               (push (make-shadow-info
                      :form expr
                      :type :complete
                      :searchable nil)
                     *shadows*))
             ;; Continue searching if not shadowed
             (unless (and (a:proper-list-p var-clauses)
                          (some (lambda (b) (binds-same-name-p b target-name)) var-clauses))
               (dolist (arg rest-args)
                 (find-shadows-in-expr arg target-name))))))

        ;; DO*
        ((utils:form-head-matches-p head "DO*")
         (when (utils:proper-list-of-min-length-p rest-args 2)
           (let ((var-clauses (first rest-args)))
             (when (and (a:proper-list-p var-clauses)
                        (some (lambda (b) (binds-same-name-p b target-name)) var-clauses))
               ;; Complete shadow
               (push (make-shadow-info
                      :form expr
                      :type :complete
                      :searchable nil)
                     *shadows*))
             ;; Continue searching if not shadowed
             (unless (and (a:proper-list-p var-clauses)
                          (some (lambda (b) (binds-same-name-p b target-name)) var-clauses))
               (dolist (arg rest-args)
                 (find-shadows-in-expr arg target-name))))))

        ;; LOOP - check for FOR/AS/WITH variables with AND detection
        ((utils:form-head-matches-p head "LOOP")
         (when (a:proper-list-p rest-args)
           (multiple-value-bind (loop-bindings body)
               (loop-parser:parse-loop-clauses rest-args)
             (declare (ignore body))
             ;; Check if any binding shadows our variable
             (let ((shadowing-binding
                    (find-if (lambda (lb)
                              (let ((vars (extract-bindings (loop-parser:loop-binding-pattern lb) :destructuring)))
                                (some (lambda (v)
                                        (string-equal (base:symbol-name-from-string v) target-name))
                                      vars)))
                            loop-bindings)))
               (when shadowing-binding
                 ;; Found shadowing - determine type based on parallelism
                 (if (loop-parser:loop-binding-is-parallel shadowing-binding)
                     ;; Parallel binding (connected by AND) - like LET
                     ;; Searchable: current binding's init-form AND all previous parallel bindings in the group
                     (let* ((shadow-pos (position shadowing-binding loop-bindings))
                            ;; Find start of parallel group (last non-parallel binding before this one)
                            (group-start (or (loop for i from (1- shadow-pos) downto 0
                                                  when (not (loop-parser:loop-binding-is-parallel (nth i loop-bindings)))
                                                  return (1+ i))
                                            0))
                            ;; All bindings from group start up to and including shadow
                            (parallel-group (subseq loop-bindings group-start (1+ shadow-pos)))
                            ;; All init-forms in the parallel group
                            (searchable-forms (mapcan (lambda (lb) (copy-list (loop-parser:loop-binding-init-form lb)))
                                                     parallel-group)))
                       (push (make-shadow-info
                              :form expr
                              :type :partial-loop-and
                              :searchable searchable-forms)
                             *shadows*))
                     ;; Sequential binding - like LET*
                     ;; Searchable: all init-forms UP TO AND INCLUDING the shadowing binding
                     (let* ((shadow-pos (position shadowing-binding loop-bindings))
                            (searchable-bindings (subseq loop-bindings 0 (1+ shadow-pos)))
                            (searchable-forms (mapcan (lambda (lb) (copy-list (loop-parser:loop-binding-init-form lb)))
                                                     searchable-bindings)))
                       (push (make-shadow-info
                              :form expr
                              :type :partial-loop-sequential
                              :searchable searchable-forms)
                             *shadows*))))
               ;; Continue searching if not shadowed
               (unless shadowing-binding
                 (dolist (clause rest-args)
                   (find-shadows-in-expr clause target-name)))))))

        ;; QUASIQUOTE - only search unquoted expressions
        ((or (eq head 'eclector.reader:quasiquote)
             (and (symbolp head)
                  (string-equal (symbol-name head) "QUASIQUOTE")
                  (string-equal (package-name (symbol-package head)) "ECLECTOR.READER")))
         ;; Search for shadows in unquoted parts
         (labels ((search-quasi (expr)
                    (when (consp expr)
                      (let ((h (first expr)))
                        (cond
                          ;; UNQUOTE - search the expression
                          ((or (eq h 'eclector.reader:unquote)
                               (and (symbolp h)
                                    (string-equal (symbol-name h) "UNQUOTE")
                                    (string-equal (package-name (symbol-package h)) "ECLECTOR.READER")))
                           (when (rest expr)
                             (find-shadows-in-expr (second expr) target-name)))
                          ;; UNQUOTE-SPLICING
                          ((or (eq h 'eclector.reader:unquote-splicing)
                               (and (symbolp h)
                                    (string-equal (symbol-name h) "UNQUOTE-SPLICING")
                                    (string-equal (package-name (symbol-package h)) "ECLECTOR.READER")))
                           (when (rest expr)
                             (find-shadows-in-expr (second expr) target-name)))
                          ;; Recurse
                          (t
                           (when (consp (car expr))
                             (search-quasi (car expr)))
                           (when (consp (cdr expr))
                             (search-quasi (cdr expr)))))))))
           (dolist (arg rest-args)
             (search-quasi arg))))

        ;; Default: recursively search
        (t
         (when (consp (car expr))
           (find-shadows-in-expr (car expr) target-name))
         (when (consp (cdr expr))
           (find-shadows-in-expr (cdr expr) target-name)))))))

(defun find-shadows (var-name body)
  "Find all points where VAR-NAME is shadowed in BODY.
Returns list of SHADOW-INFO structs.

This is Phase 1 of the two-phase reference finding approach. It pre-computes
all shadowing points and their types, which allows Phase 2 to search for
references without worrying about shadow detection during traversal."
  (let ((target-name (base:symbol-name-from-string var-name))
        (*shadows* '()))
    ;; Find all shadows in the body
    (dolist (form body)
      (find-shadows-in-expr form target-name))
    (nreverse *shadows*)))

(defun find-references-with-shadows (var-name body shadows)
  "Find if VAR-NAME is referenced in BODY, respecting pre-computed SHADOWS.
This is Phase 2 of the two-phase reference finding approach. It searches for
references while respecting the shadow boundaries computed by find-shadows."
  (let ((target-name (base:symbol-name-from-string var-name)))
    (labels ((matches-shadow-p (expr)
               "Check if EXPR matches any shadow in the list, return shadow-info if found."
               (find expr shadows :key #'shadow-info-form :test #'eq))

             (search-expr (expr)
               "Recursively search for references to var-name in expr."
               (cond
                 ((null expr) nil)

                 ;; String matching our variable is a reference
                 ((stringp expr)
                  (string-equal (base:symbol-name-from-string expr) target-name))

                 ;; Check if this form is a shadow FIRST (before checking strings)
                 ;; This prevents finding variable names in binding positions
                 ((consp expr)
                  (let ((shadow (matches-shadow-p expr)))
                    (if shadow
                        ;; This form shadows the variable
                        (case (shadow-info-type shadow)
                          (:complete
                           ;; Complete shadow - cannot search this form at all
                           nil)
                          ((:partial-let :partial-let* :partial-loop-sequential :partial-loop-and)
                           ;; Partial shadow - can search searchable sub-expressions
                           (some #'search-expr (shadow-info-searchable shadow))))
                        ;; Not a shadow - search recursively
                        (cond
                          ;; QUOTE - pure data, skip
                          ((or (eq (first expr) 'cl:quote)
                               (eq (first expr) 'quote)
                               (base:symbol-matches-p (first expr) "QUOTE"))
                           nil)
                          ;; QUASIQUOTE - only search unquoted parts
                          ((or (eq (first expr) 'eclector.reader:quasiquote)
                               (and (symbolp (first expr))
                                    (string-equal (symbol-name (first expr)) "QUASIQUOTE")
                                    (string-equal (package-name (symbol-package (first expr))) "ECLECTOR.READER")))
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
                             (some #'search-quasi (rest expr))))
                          ;; Default: search recursively (OR-based search)
                          (t
                           (or (search-expr (car expr))
                               (when (consp (cdr expr))
                                 (search-expr (cdr expr)))))))))

                 (t nil))))

      ;; Search all forms in body using OR-based search
      (some #'search-expr body))))

(defun find-references (var-name body)
  "Find if VAR-NAME is referenced in BODY, respecting variable shadowing.
Uses two-phase architecture: Phase 1 finds all shadows, Phase 2 searches for references."
  ;; Phase 1: Find all shadowing points
  (let ((shadows (find-shadows var-name body)))
    ;; Phase 2: Search for references respecting shadows
    (find-references-with-shadows var-name body shadows)))

;;; Binding position calculation

(defun find-variable-object-in-binding (var-name binding)
  "Find the actual string object for VAR-NAME in BINDING structure.
Returns the string object (not a copy) so it can be looked up in position-map with EQ.
VAR-NAME is a string to match against (by content).
BINDING is the binding form (e.g., \"a\", (\"a\" 10), ((\"a\" \"b\") values))."
  (let ((var-name-stripped (base:symbol-name-from-string var-name)))
    (labels ((search-binding (form)
               "Recursively search FORM for a string matching VAR-NAME."
               (cond
                 ;; Found a matching string - return it (the actual object)
                 ((and (stringp form)
                       (string-equal (base:symbol-name-from-string form) var-name-stripped))
                  form)
                 ;; Cons - search both parts
                 ((consp form)
                  (or (search-binding (car form))
                      (when (consp (cdr form))
                        (search-binding (cdr form)))))
                 ;; Not found
                 (t nil))))
      (search-binding binding))))

(defun find-variable-in-position-map (var-name binding position-map)
  "Find the position of VAR-NAME by locating the actual string object in BINDING.
Uses EQ lookup in POSITION-MAP to get the exact position from the reader.
Returns (values line column) if found, or (values nil nil) if not found."
  (when (and binding position-map)
    (let* ((var-object (find-variable-object-in-binding var-name binding))
           (pos (when var-object (gethash var-object position-map))))
      (if pos
          (values (car pos) (cdr pos))
          (values nil nil)))))

;;; Binding checking

(defun check-binding-form (form-type bindings body fallback-line fallback-column position-map rule &optional aux-context message-prefix optional-key-defaults)
  "Check bindings for unused variables using scope-based approach.
FORM-TYPE determines how scope is calculated (:let, :let*, :defun-regular, :defun-aux, :do).
MESSAGE-PREFIX is the prefix for violation messages (default 'Variable').
OPTIONAL-KEY-DEFAULTS is a list of default forms from &optional and &key parameters (for :defun-regular only).
Pushes violations to *violations* special variable."
  (let ((ignored-vars (extract-ignored-vars body))
        (body-without-declares (remove-if
                                (lambda (form)
                                  (and (consp form)
                                       (base:symbol-matches-p (first form) "DECLARE")))
                                body)))
    ;; First pass: Record ALL variable positions in scope stack
    ;; Look up the actual variable string object in the binding structure,
    ;; then use EQ to look it up in position-map. This works correctly because:
    ;; 1. Position-map uses :test 'eq (object identity)
    ;; 2. Each symbol read creates a unique string object
    ;; 3. extract-bindings returns the actual string objects from bindings
    ;; 4. Looking up by object identity finds the exact position
    (loop for binding in bindings
          do (let ((var-names (extract-bindings binding)))
               (dolist (var-name var-names)
                 ;; Find position by looking up the actual variable string object
                 (multiple-value-bind (var-line var-column)
                     (cond
                       (position-map
                        ;; Find the actual string object and look it up directly
                        (multiple-value-bind (obj-line obj-column)
                            (find-variable-in-position-map var-name binding position-map)
                          (if obj-line
                              (values obj-line obj-column)
                              (values fallback-line fallback-column))))
                       ;; Last resort: use fallback
                       (t
                        (values fallback-line fallback-column)))
                   ;; Record this variable's position in scope stack
                   (when (and var-line var-column)
                     (scope:record-variable var-name var-line var-column))))))

    ;; Second pass: Check each variable for usage and create violations
    (loop for remaining-bindings on bindings
          for binding = (first remaining-bindings)
          for subsequent-bindings = (rest remaining-bindings)
          do (let ((var-names (extract-bindings binding)))
               (dolist (var-name var-names)
                 (let ((scope (cond
                                ;; DO*: sequential init forms (like LET*) + all step forms except own
                                ((eq form-type :do*)
                                 (let* ((base-scope (calculate-scope form-type subsequent-bindings body-without-declares aux-context optional-key-defaults))
                                        (var-clauses (first aux-context))
                                        (var-index (position binding bindings))
                                        (own-clause (when (and var-index var-clauses)
                                                      (nth var-index var-clauses)))
                                        (own-step-form (when (utils:proper-list-of-min-length-p own-clause 3)
                                                         (third own-clause))))
                                   ;; Filter out own step form from base-scope
                                   (remove own-step-form base-scope :test #'eq)))
                                ;; DO: parallel init, only step forms + test/result/body, exclude own step form
                                ((eq form-type :do)
                                 (let* ((var-clauses (first aux-context))
                                        (rest-context (rest aux-context))
                                        (var-index (position binding bindings))
                                        (own-clause (when (and var-index var-clauses)
                                                      (nth var-index var-clauses)))
                                        (own-step-form (when (utils:proper-list-of-min-length-p own-clause 3)
                                                         (third own-clause)))
                                        (all-step-forms (when (a:proper-list-p var-clauses)
                                                          (mapcar (lambda (clause)
                                                                    (when (utils:proper-list-of-min-length-p clause 3)
                                                                      (third clause)))
                                                                  var-clauses)))
                                        (other-step-forms (remove own-step-form all-step-forms)))
                                   (append other-step-forms rest-context)))
                                ;; Normal scope calculation for other forms
                                (t
                                 (calculate-scope form-type subsequent-bindings body-without-declares aux-context optional-key-defaults)))))
                   (let ((is-ignored (ignored-var-p var-name ignored-vars))
                         (is-referenced (find-references var-name scope)))
                     (unless (or is-ignored is-referenced)
                       ;; Check if rule is suppressed before creating violation
                       (let ((state-symbol (find-symbol "*SUPPRESSION-STATE*" "MALLET/ENGINE")))
                         (unless (and state-symbol
                                      (boundp state-symbol)
                                      (symbol-value state-symbol)
                                      (funcall (find-symbol "RULE-SUPPRESSED-P" "MALLET/SUPPRESSION")
                                               (symbol-value state-symbol)
                                               (base:rule-name rule)))
                           ;; Look up position from scope stack for violation reporting
                           (multiple-value-bind (scope-line scope-column)
                               (scope:lookup-variable-position var-name)
                             (when (and scope-line scope-column)
                               (push (make-instance 'violation:violation
                                                    :rule (base:rule-name rule)
                                                    :file *file*
                                                    :line scope-line
                                                    :column scope-column
                                                    :severity (base:rule-severity rule)
                                                    :message
                                                    (format nil "~A '~A' is unused"
                                                            (or message-prefix "Variable")
                                                            (base:symbol-name-from-string var-name))
                                                    :fix nil)
                                     *violations*)))))))))))))

(defun check-defun-bindings (expr line column position-map rule)
  "Check DEFUN for unused parameter bindings."
  (let ((rest-args (rest expr)))
    (when (and (a:proper-list-p rest-args)
               (>= (length rest-args) 3)
               (listp (second rest-args)))
      (let* ((lambda-list (second rest-args))
             (body (cddr rest-args)))
        (multiple-value-bind (non-aux-part aux-params)
            (parse-lambda-list-for-aux lambda-list)
          ;; Extract default forms from &optional and &key parameters
          (let* ((optional-key-defaults (extract-optional-key-defaults non-aux-part))
                 (var-names (extract-lambda-list-vars non-aux-part nil))
                 (param-bindings (mapcar #'list var-names)))
            (when param-bindings
            (scope:with-new-scope
                (check-binding-form :defun-regular param-bindings body line column position-map rule nil nil optional-key-defaults))))
          (when aux-params
            (scope:with-new-scope
              (check-binding-form :defun-aux aux-params body line column position-map rule body))))))))

(defun check-lambda-bindings (expr line column position-map rule)
  "Check LAMBDA for unused parameter bindings."
  (let ((rest-args (rest expr)))
    (when (and (a:proper-list-p rest-args)
               (>= (length rest-args) 2)
               (listp (first rest-args)))
      (let* ((lambda-list (first rest-args))
             (body (rest rest-args)))
        (multiple-value-bind (non-aux-part aux-params)
            (parse-lambda-list-for-aux lambda-list)
          ;; Extract default forms from &optional and &key parameters
          (let* ((optional-key-defaults (extract-optional-key-defaults non-aux-part))
                 (var-names (extract-lambda-list-vars non-aux-part nil))
                 (param-bindings (mapcar #'list var-names)))
            (when param-bindings
            (scope:with-new-scope
                (check-binding-form :defun-regular param-bindings body line column position-map rule nil nil optional-key-defaults))))
          (when aux-params
            (scope:with-new-scope
              (check-binding-form :defun-aux aux-params body line column position-map rule body))))))))

(defun check-let-bindings (expr line column position-map rule)
  "Check LET for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (utils:proper-list-of-min-length-p rest-args 2)
      (handler-case
          (let ((bindings (first rest-args))
                (body (rest rest-args)))
            (scope:with-new-scope
              (check-binding-form :let bindings body line column position-map rule)))
        (type-error (e)
          (format *error-output* "~%Warning: Skipping LET form at ~A:~A due to unexpected structure:~%  ~A~%"
                  *file* line e)
          nil)))))

(defun check-let*-bindings (expr line column position-map rule)
  "Check LET* for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (utils:proper-list-of-min-length-p rest-args 2)
      (let ((bindings (first rest-args))
            (body (rest rest-args)))
        (scope:with-new-scope
          (check-binding-form :let* bindings body line column position-map rule))))))

(defun find-original-loop-variable (pattern clauses)
  "Find the original variable string object in CLAUSES that matches PATTERN.
PATTERN is a stripped string or cons from loop parser.
CLAUSES is the raw loop clause list from the parse tree.
Returns the original string object that should be in the position-map."
  (let ((pattern-str (if (stringp pattern)
                         (base:symbol-name-from-string pattern)
                         ;; For destructuring patterns, just search for the whole structure
                         nil)))
    (when pattern-str
      ;; Scan through clauses looking for FOR/AS/WITH keywords
      ;; The variable is right after the keyword
      (loop for i from 0 below (1- (length clauses))
            for elem = (nth i clauses)
            for next-elem = (nth (1+ i) clauses)
            when (and (stringp elem)
                      (member (base:symbol-name-from-string elem)
                              '("FOR" "AS" "WITH" "AND")
                              :test #'string-equal)
                      (stringp next-elem)
                      (string-equal (base:symbol-name-from-string next-elem) pattern-str))
            return next-elem))))

(defun check-loop-bindings (expr line column position-map rule)
  "Check LOOP for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (a:proper-list-p rest-args)
      (multiple-value-bind (loop-bindings body)
          (loop-parser:parse-loop-clauses rest-args)
        (when loop-bindings
          (multiple-value-bind (loop-line loop-column)
              (parser:find-position expr position-map line column)
            ;; Create bindings using ORIGINAL variable objects from rest-args
            ;; This is crucial for position-map lookup which uses EQ
            (let ((bindings (mapcar (lambda (lb)
                                      (let* ((pattern (loop-parser:loop-binding-pattern lb))
                                             (original-var (find-original-loop-variable pattern rest-args)))
                                        ;; Use original variable if found, otherwise fall back to stripped pattern
                                        (list (or original-var pattern))))
                                    loop-bindings)))
              (scope:with-new-scope
                (check-binding-form :let bindings body loop-line loop-column position-map rule)))))))))

(defun check-do-bindings (expr line column position-map rule)
  "Check DO for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (utils:proper-list-of-min-length-p rest-args 2)
      (let* ((var-clauses (first rest-args))
             (end-test-clause (second rest-args))
             (body (cddr rest-args))
             (test-form (when (a:proper-list-p end-test-clause) (first end-test-clause)))
             (result-forms (when (a:proper-list-p end-test-clause) (rest end-test-clause)))
             (do-context (cons var-clauses (append (list test-form) result-forms body)))
             (bindings (when (a:proper-list-p var-clauses)
                        (mapcar (lambda (clause)
                                 (if (consp clause)
                                     (list (first clause))
                                     (list clause)))
                               var-clauses))))
        (when bindings
          (scope:with-new-scope
            (check-binding-form :do bindings body line column position-map rule do-context)))))))

(defun check-do*-bindings (expr line column position-map rule)
  "Check DO* for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (utils:proper-list-of-min-length-p rest-args 2)
      (let* ((var-clauses (first rest-args))
             (end-test-clause (second rest-args))
             (body (cddr rest-args))
             (test-form (when (a:proper-list-p end-test-clause) (first end-test-clause)))
             (result-forms (when (a:proper-list-p end-test-clause) (rest end-test-clause)))
             (do-context (cons var-clauses (append (list test-form) result-forms body))))
        ;; For DO*, pass full var-clauses as bindings (not simplified)
        ;; This allows calculate-scope to extract init forms for sequential binding check
        (when var-clauses
          (scope:with-new-scope
            (check-binding-form :do* var-clauses body line column position-map rule do-context)))))))

(defun check-destructuring-bind-bindings (expr line column position-map rule)
  "Check DESTRUCTURING-BIND for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (utils:proper-list-of-min-length-p rest-args 3)
      (let* ((pattern (first rest-args))
             (body (cddr rest-args))
             ;; Use extract-bindings which handles dotted pairs correctly
             ;; e.g., (string . attributes) or (a b c) or ((a b) c)
             (var-names (extract-bindings pattern :destructuring))
             (bindings (mapcar #'list var-names)))
        (when bindings
          (scope:with-new-scope
            (check-binding-form :let bindings body line column position-map rule)))))))

(defun check-multiple-value-bind-bindings (expr line column position-map rule)
  "Check MULTIPLE-VALUE-BIND for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (utils:proper-list-of-min-length-p rest-args 3)
      (let* ((vars (first rest-args))
             (body (cddr rest-args))
             (bindings (list vars)))
        (scope:with-new-scope
          (check-binding-form :let bindings body line column position-map rule))))))

(defun check-dolist-bindings (expr line column position-map rule)
  "Check DOLIST for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (utils:proper-list-of-min-length-p rest-args 2)
      (let* ((spec (first rest-args))
             (body (rest rest-args)))
        (when (utils:proper-list-of-min-length-p spec 2)
          (let* ((var (first spec))
                 (bindings (list (list var))))
            (scope:with-new-scope
                (check-binding-form :let bindings body line column position-map rule))))))))

(defun check-do-symbols-bindings (expr line column position-map rule)
  "Check DO-SYMBOLS/DO-EXTERNAL-SYMBOLS/DO-ALL-SYMBOLS for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (utils:proper-list-of-min-length-p rest-args 2)
      (let* ((spec (first rest-args))
             (body (rest rest-args)))
        (when (utils:proper-list-of-min-length-p spec 1)
          (let* ((var (first spec))
                 (bindings (list (list var))))
            (scope:with-new-scope
                (check-binding-form :let bindings body line column position-map rule))))))))

(defun check-with-slots-bindings (expr line column position-map rule)
  "Check WITH-SLOTS for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (utils:proper-list-of-min-length-p rest-args 3)
      (let* ((slot-specs (first rest-args))
             (body (cddr rest-args)))
        (when (a:proper-list-p slot-specs)
          (let ((bindings (mapcar (lambda (spec)
                                   (list (if (consp spec)
                                            (first spec)  ; (var slot-name)
                                            spec)))       ; var
                                 slot-specs)))
            (scope:with-new-scope
                (check-binding-form :let bindings body line column position-map rule))))))))

(defun check-with-accessors-bindings (expr line column position-map rule)
  "Check WITH-ACCESSORS for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (utils:proper-list-of-min-length-p rest-args 3)
      (let* ((accessor-specs (first rest-args))
             (body (cddr rest-args)))
        (when (a:proper-list-p accessor-specs)
          (let ((bindings (mapcar (lambda (spec)
                                    (when (and (consp spec) (>= (length spec) 2))
                                      (list (first spec))))  ; (var accessor-name)
                                  accessor-specs)))
            (scope:with-new-scope
              (check-binding-form :let (remove nil bindings) body line column position-map rule))))))))

(defun check-with-input-from-string-bindings (expr line column position-map rule)
  "Check WITH-INPUT-FROM-STRING for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (utils:proper-list-of-min-length-p rest-args 2)
      (let* ((spec (first rest-args))
             (body (rest rest-args)))
        (when (utils:proper-list-of-min-length-p spec 2)
          (let* ((var (first spec))
                 (bindings (list (list var))))
            (scope:with-new-scope
                (check-binding-form :let bindings body line column position-map rule))))))))

(defun check-with-output-to-string-bindings (expr line column position-map rule)
  "Check WITH-OUTPUT-TO-STRING for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (utils:proper-list-of-min-length-p rest-args 1)
      (let* ((spec (first rest-args))
             (body (rest rest-args)))
        (when (a:proper-list-p spec)
          (let* ((var (first spec))
                 (bindings (list (list var))))
            (scope:with-new-scope
                (check-binding-form :let bindings body line column position-map rule))))))))

(defun check-with-open-file-bindings (expr line column position-map rule)
  "Check WITH-OPEN-FILE for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (utils:proper-list-of-min-length-p rest-args 2)
      (let* ((spec (first rest-args))
             (body (rest rest-args)))
        (when (utils:proper-list-of-min-length-p spec 2)
          (let* ((var (first spec))
                 (bindings (list (list var))))
            (scope:with-new-scope
                (check-binding-form :let bindings body line column position-map rule))))))))

(defun check-defmacro-bindings (expr line column position-map rule)
  "Check DEFMACRO for unused parameter bindings."
  (let ((rest-args (rest expr)))
    (when (and (a:proper-list-p rest-args)
               (>= (length rest-args) 3)
               (listp (second rest-args)))
      (let* ((lambda-list (second rest-args))
             (body (cddr rest-args)))
        (multiple-value-bind (non-aux-part aux-params)
            (parse-lambda-list-for-aux lambda-list)
          ;; Extract default forms from &optional and &key parameters
          (let* ((optional-key-defaults (extract-optional-key-defaults non-aux-part))
                 (var-names (extract-lambda-list-vars non-aux-part t))
                 (param-bindings (mapcar #'list var-names)))
            (when param-bindings
              (scope:with-new-scope
                (check-binding-form :defun-regular param-bindings body line column position-map rule nil nil optional-key-defaults))))
          (when aux-params
            (scope:with-new-scope
              (check-binding-form :defun-aux aux-params body line column position-map rule body))))))))

(defun check-expr (expr line column position-map rule)
  "Recursively check expression for unused variables."
  (when (consp expr)
    (let ((head (first expr))
          (rest-args (rest expr)))
      ;; 1. Dispatch to specific checkers for binding forms
      (cond
        ((base:symbol-matches-p head "DEFUN")
         (check-defun-bindings expr line column position-map rule))
        ((base:symbol-matches-p head "LAMBDA")
         (check-lambda-bindings expr line column position-map rule))
        ((and (stringp head) (string-equal (base:symbol-name-from-string head) "LET"))
         (check-let-bindings expr line column position-map rule))
        ((and (stringp head) (string-equal (base:symbol-name-from-string head) "LET*"))
         (check-let*-bindings expr line column position-map rule))
        ;; LOOP forms are checked by unused-loop-variables-rule, not unused-variables-rule
        ;; ((base:symbol-matches-p head "LOOP")
        ;;  (check-loop-bindings expr line column position-map rule))
        ((base:symbol-matches-p head "DO")
         (check-do-bindings expr line column position-map rule))
        ((base:symbol-matches-p head "DO*")
         (check-do*-bindings expr line column position-map rule))
        ((base:symbol-matches-p head "DESTRUCTURING-BIND")
         (check-destructuring-bind-bindings expr line column position-map rule))
        ((base:symbol-matches-p head "MULTIPLE-VALUE-BIND")
         (check-multiple-value-bind-bindings expr line column position-map rule))
        ((base:symbol-matches-p head "DOLIST")
         (check-dolist-bindings expr line column position-map rule))
        ((base:symbol-matches-p head "DEFMACRO")
         (check-defmacro-bindings expr line column position-map rule))
        ;; DO-SYMBOLS family
        ((or (base:symbol-matches-p head "DO-SYMBOLS")
             (base:symbol-matches-p head "DO-EXTERNAL-SYMBOLS")
             (base:symbol-matches-p head "DO-ALL-SYMBOLS"))
         (check-do-symbols-bindings expr line column position-map rule))
        ;; WITH-* macros
        ((base:symbol-matches-p head "WITH-SLOTS")
         (check-with-slots-bindings expr line column position-map rule))
        ((base:symbol-matches-p head "WITH-ACCESSORS")
         (check-with-accessors-bindings expr line column position-map rule))
        ((base:symbol-matches-p head "WITH-INPUT-FROM-STRING")
         (check-with-input-from-string-bindings expr line column position-map rule))
        ((base:symbol-matches-p head "WITH-OUTPUT-TO-STRING")
         (check-with-output-to-string-bindings expr line column position-map rule))
        ((base:symbol-matches-p head "WITH-OPEN-FILE")
         (check-with-open-file-bindings expr line column position-map rule)))

      ;; 2. Handle special forms for recursion
      (cond
        ;; QUOTE - skip entirely (pure data)
        ((or (eq head 'cl:quote)
             (eq head 'quote)
             (base:symbol-matches-p head "QUOTE"))
         nil)

        ;; DEFSTRUCT - skip entirely (has its own special syntax for slots)
        ((base:symbol-matches-p head "DEFSTRUCT")
         nil)

        ;; DEFCLASS - skip entirely (slot definitions with special syntax)
        ((base:symbol-matches-p head "DEFCLASS")
         nil)

        ;; DEFPACKAGE - skip entirely (special clause syntax)
        ((base:symbol-matches-p head "DEFPACKAGE")
         nil)

        ;; DEFTYPE - skip entirely (has lambda list for type parameters)
        ((base:symbol-matches-p head "DEFTYPE")
         nil)

        ;; DEFSETF - skip entirely (special lambda list forms)
        ((base:symbol-matches-p head "DEFSETF")
         nil)

        ;; DEFINE-MODIFY-MACRO - skip entirely (special lambda list forms)
        ((base:symbol-matches-p head "DEFINE-MODIFY-MACRO")
         nil)

        ;; DEFINE-SETF-EXPANDER - skip entirely (lambda list)
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
                 ;; But we need to accumulate violations from nested calls
                 (let ((nested-violations
                        (base:check-form-recursive rule subexpr *file*
                                                  (or subexpr-line line)
                                                  (or subexpr-column column)
                                                  nil
                                                  position-map)))
                   ;; Accumulate violations from nested call
                   (setf *violations* (append *violations* nested-violations))))))))))))

;;; Unused-variables rules

(defclass unused-variables-rule (base:rule)
  ()
  (:default-initargs
   :name :unused-variables
   :description "Variables should be used or explicitly ignored"
   :severity :warning
   :type :form)
  (:documentation "Rule to detect unused variables in bindings."))

(defclass unused-loop-variables-rule (base:rule)
  ()
  (:default-initargs
   :name :unused-loop-variables
   :description "LOOP variables should be used or explicitly ignored"
   :severity :info
   :type :form)
  (:documentation "Rule to detect unused LOOP variables.
Disabled by default since unused LOOP variables are common and idiomatic in CL
(e.g., counter vars, partial destructuring patterns)."))

(defmethod base:check-form ((rule unused-variables-rule) form file)
  "Check that all bound variables are either used or explicitly ignored.
Skips LOOP forms - use unused-loop-variables-rule for those."
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

(defmethod base:check-form-recursive ((rule unused-variables-rule) expr file line column
                                       &optional function-name position-map)
  "Recursively check for unused variables with automatic suppression handling."
  (declare (ignore function-name))

  (handler-case
      (handler-bind ((type-error
                       (lambda (e)
                         (when (debug-mode-p)
                           (let ((form-head (when (consp expr)
                                              (let ((head (first expr)))
                                                (when (stringp head)
                                                  (base:symbol-name-from-string head))))))
                             (format *error-output* "~%Warning: Skipping unused-variables check for ~A form at ~A:~A~%  Error: ~A~%"
                                     (or form-head "unknown") file line e)
                             (uiop:print-condition-backtrace e))))))
        ;; Bind special variables and call check-expr
        (let ((*violations* '())
              (*file* file)
              (scope:*scope-stack* nil))  ; Initialize clean scope stack for this form
          (check-expr expr line column position-map rule)
          (nreverse *violations*)))
    (type-error (e)
      (declare (ignore e))
      ;; Return empty list on error
      '())))

(defun check-loop-expr (expr line column position-map rule)
  "Recursively check expression for LOOP forms with unused variables.
Similar to check-expr but only processes LOOP forms."
  (when (consp expr)
    (let ((head (first expr))
          (rest-args (rest expr)))
      ;; 1. Check if this is a LOOP form
      (when (base:symbol-matches-p head "LOOP")
        (check-loop-bindings expr line column position-map rule))

      ;; 2. Handle special forms for recursion
      (cond
        ;; QUOTE - skip entirely (pure data)
        ((or (eq head 'cl:quote)
             (eq head 'quote)
             (base:symbol-matches-p head "QUOTE"))
         nil)

        ;; DEFSTRUCT - skip entirely (has its own special syntax for slots)
        ((base:symbol-matches-p head "DEFSTRUCT")
         nil)

        ;; DEFCLASS - skip entirely (slot definitions with special syntax)
        ((base:symbol-matches-p head "DEFCLASS")
         nil)

        ;; DEFPACKAGE - skip entirely (special clause syntax)
        ((base:symbol-matches-p head "DEFPACKAGE")
         nil)

        ;; DEFTYPE - skip entirely (has lambda list for type parameters)
        ((base:symbol-matches-p head "DEFTYPE")
         nil)

        ;; DEFSETF - skip entirely (special lambda list forms)
        ((base:symbol-matches-p head "DEFSETF")
         nil)

        ;; DEFINE-MODIFY-MACRO - skip entirely (special lambda list forms)
        ((base:symbol-matches-p head "DEFINE-MODIFY-MACRO")
         nil)

        ;; DEFINE-SETF-EXPANDER - skip entirely (lambda list)
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
                             (check-loop-expr (second expr) line column position-map rule)))
                          ;; UNQUOTE-SPLICING - check the unquoted expression
                          ((or (eq h 'eclector.reader:unquote-splicing)
                               (and (symbolp h)
                                    (string-equal (symbol-name h) "UNQUOTE-SPLICING")
                                    (string-equal (package-name (symbol-package h)) "ECLECTOR.READER")))
                           (when (rest expr)
                             (check-loop-expr (second expr) line column position-map rule)))
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
                 ;; But we need to accumulate violations from nested calls
                 (let ((nested-violations
                        (base:check-form-recursive rule subexpr *file*
                                                  (or subexpr-line line)
                                                  (or subexpr-column column)
                                                  nil
                                                  position-map)))
                   ;; Accumulate violations from nested call
                   (setf *violations* (append *violations* nested-violations))))))))))))

(defmethod base:check-form ((rule unused-loop-variables-rule) form file)
  "Check that LOOP variables are either used or explicitly ignored.
Recursively descends into all forms to find nested LOOPs."
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

(defmethod base:check-form-recursive ((rule unused-loop-variables-rule) expr file line column
                                       &optional function-name position-map)
  "Recursively check for unused LOOP variables with automatic suppression handling."
  (declare (ignore function-name))

  (handler-case
      (handler-bind ((type-error
                       (lambda (e)
                         (when (debug-mode-p)
                           (format *error-output* "~%Warning: Skipping unused-loop-variables check at ~A:~A~%  Error: ~A~%"
                                   file line e)
                           (uiop:print-condition-backtrace e)))))
        ;; Bind special variables and recursively check for LOOPs
        (let ((*violations* '())
              (*file* file)
              (scope:*scope-stack* nil))  ; Initialize clean scope stack for this form
          (check-loop-expr expr line column position-map rule)
          (nreverse *violations*)))
    (type-error (e)
      (declare (ignore e))
      ;; Return empty list on error
      '())))
