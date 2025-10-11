(defpackage #:malo/rules/forms/variables
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:malo/rules/base)
   (#:parser #:malo/parser)
   (#:loop-parser #:malo/parser/loop)
   (#:violation #:malo/violation))
  (:export #:unused-variables-rule
           #:unused-loop-variables-rule))
(in-package #:malo/rules/forms/variables)

;;; Utilities

(defvar *current-form-position* nil
  "Dynamic variable tracking the position (line . column) of the current form being checked.
Used to provide accurate positions for binding violations without relying on position-map lookups,
which can return stale data when multiple forms have identical patterns.")

(defvar *violations* nil
  "Dynamic variable holding the list of violations found during checking.
Bound in base:check-form and accessible to all helper functions.")

(defvar *file* nil
  "Dynamic variable holding the current file being checked.
Bound in base:check-form and accessible to all helper functions.")

(defun debug-mode-p ()
  "Check if debug mode is enabled."
  (and (find-symbol "*DEBUG-MODE*" "MALO")
       (symbol-value (find-symbol "*DEBUG-MODE*" "MALO"))))

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
          (a:proper-list-p binding-form)
          (= (length binding-form) 2)
          (stringp (first binding-form))
          (eq context :binding))  ; Only treat as LET binding in :binding context
     ;; Extract only the variable (first element), not the init form
     (list (first binding-form)))
    ;; Lambda list binding with default/supplied-p: (var default) or (var default supplied-p)
    ;; Check if this has non-string elements indicating it's not pure destructuring
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
                       (let ((extracted (extract-bindings (list elem) :destructuring)))
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

(defun calculate-scope (form-type binding remaining-bindings body aux-context)
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

(defun find-shadows-in-expr (expr target-name shadows)
  "Recursively find all shadowing points in expr.
Modifies SHADOWS list by pushing new shadow-info structs."
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
        ((and (stringp head)
              (string-equal (base:symbol-name-from-string head) "LET"))
         (when (and (a:proper-list-p rest-args) (>= (length rest-args) 2))
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
                       shadows)))
             ;; Continue searching in non-shadowed parts or inside the shadow
             (when (and (a:proper-list-p bindings)
                        (not (some (lambda (b) (binds-same-name-p b target-name)) bindings)))
               ;; No shadow here, continue recursion
               (dolist (binding bindings)
                 (when (consp binding)
                   (find-shadows-in-expr (second binding) target-name shadows)))
               (dolist (form (rest rest-args))
                 (find-shadows-in-expr form target-name shadows))))))

        ;; LET* - check for shadowing in bindings
        ((and (stringp head)
              (string-equal (base:symbol-name-from-string head) "LET*"))
         (when (and (a:proper-list-p rest-args) (>= (length rest-args) 2))
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
                       shadows)))
             ;; Continue searching in non-shadowed parts
             (when (and (a:proper-list-p bindings)
                        (not (some (lambda (b) (binds-same-name-p b target-name)) bindings)))
               (dolist (binding bindings)
                 (when (consp binding)
                   (find-shadows-in-expr (second binding) target-name shadows)))
               (dolist (form (rest rest-args))
                 (find-shadows-in-expr form target-name shadows))))))

        ;; DEFUN/LAMBDA/DEFMACRO - check parameters for shadowing
        ((and (stringp head)
              (or (string-equal (base:symbol-name-from-string head) "DEFUN")
                  (string-equal (base:symbol-name-from-string head) "LAMBDA")
                  (string-equal (base:symbol-name-from-string head) "DEFMACRO")))
         (let* ((lambda-list-pos (if (string-equal (base:symbol-name-from-string head) "LAMBDA") 0 1))
                (lambda-list (when (and (a:proper-list-p rest-args) (> (length rest-args) lambda-list-pos))
                               (nth lambda-list-pos rest-args))))
           (when (and (a:proper-list-p lambda-list)
                      (some (lambda (param)
                              (and (stringp param)
                                   (not (char= (char (base:symbol-name-from-string param) 0) #\&))
                                   (string-equal (base:symbol-name-from-string param) target-name)))
                            lambda-list))
             ;; Complete shadow - parameter shadows the variable
             (push (make-shadow-info
                    :form expr
                    :type :complete
                    :searchable nil)
                   shadows))
           ;; Continue searching in non-shadowed parts
           (unless (and (a:proper-list-p lambda-list)
                        (some (lambda (param)
                                (and (stringp param)
                                     (not (char= (char (base:symbol-name-from-string param) 0) #\&))
                                     (string-equal (base:symbol-name-from-string param) target-name)))
                              lambda-list))
             (dolist (arg rest-args)
               (find-shadows-in-expr arg target-name shadows)))))

        ;; DESTRUCTURING-BIND, MULTIPLE-VALUE-BIND
        ((and (stringp head)
              (or (string-equal (base:symbol-name-from-string head) "DESTRUCTURING-BIND")
                  (string-equal (base:symbol-name-from-string head) "MULTIPLE-VALUE-BIND")))
         (when (and (a:proper-list-p rest-args) (>= (length rest-args) 2))
           (let ((vars (first rest-args)))
             (let ((all-vars (extract-bindings vars :destructuring)))
               (when (some (lambda (v)
                             (string-equal (base:symbol-name-from-string v) target-name))
                           all-vars)
                 ;; Complete shadow
                 (push (make-shadow-info
                        :form expr
                        :type :complete
                        :searchable nil)
                       shadows)))
             ;; Continue searching if not shadowed
             (unless (some (lambda (v)
                             (string-equal (base:symbol-name-from-string v) target-name))
                           (extract-bindings vars :destructuring))
               (dolist (arg rest-args)
                 (find-shadows-in-expr arg target-name shadows))))))

        ;; DOLIST, DOTIMES
        ((and (stringp head)
              (or (string-equal (base:symbol-name-from-string head) "DOLIST")
                  (string-equal (base:symbol-name-from-string head) "DOTIMES")))
         (when (and (a:proper-list-p rest-args) (>= (length rest-args) 1))
           (let* ((spec (first rest-args))
                  (var (when (a:proper-list-p spec) (first spec))))
             (when (and (stringp var)
                        (string-equal (base:symbol-name-from-string var) target-name))
               ;; Complete shadow
               (push (make-shadow-info
                      :form expr
                      :type :complete
                      :searchable nil)
                     shadows))
             ;; Continue searching if not shadowed
             (unless (and (stringp var)
                          (string-equal (base:symbol-name-from-string var) target-name))
               (dolist (arg rest-args)
                 (find-shadows-in-expr arg target-name shadows))))))

        ;; DO
        ((and (stringp head)
              (string-equal (base:symbol-name-from-string head) "DO"))
         (when (and (a:proper-list-p rest-args) (>= (length rest-args) 2))
           (let ((var-clauses (first rest-args)))
             (when (and (a:proper-list-p var-clauses)
                        (some (lambda (b) (binds-same-name-p b target-name)) var-clauses))
               ;; Complete shadow
               (push (make-shadow-info
                      :form expr
                      :type :complete
                      :searchable nil)
                     shadows))
             ;; Continue searching if not shadowed
             (unless (and (a:proper-list-p var-clauses)
                          (some (lambda (b) (binds-same-name-p b target-name)) var-clauses))
               (dolist (arg rest-args)
                 (find-shadows-in-expr arg target-name shadows))))))

        ;; LOOP - check for FOR/AS/WITH variables with AND detection
        ((and (stringp head)
              (string-equal (base:symbol-name-from-string head) "LOOP"))
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
                             shadows))
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
                             shadows))))
               ;; Continue searching if not shadowed
               (unless shadowing-binding
                 (dolist (clause rest-args)
                   (find-shadows-in-expr clause target-name shadows)))))))

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
                             (find-shadows-in-expr (second expr) target-name shadows)))
                          ;; UNQUOTE-SPLICING
                          ((or (eq h 'eclector.reader:unquote-splicing)
                               (and (symbolp h)
                                    (string-equal (symbol-name h) "UNQUOTE-SPLICING")
                                    (string-equal (package-name (symbol-package h)) "ECLECTOR.READER")))
                           (when (rest expr)
                             (find-shadows-in-expr (second expr) target-name shadows)))
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
           (find-shadows-in-expr (car expr) target-name shadows))
         (when (consp (cdr expr))
           (find-shadows-in-expr (cdr expr) target-name shadows)))))))

(defun find-shadows (var-name body)
  "Find all points where VAR-NAME is shadowed in BODY.
Returns list of SHADOW-INFO structs.

This is Phase 1 of the two-phase reference finding approach. It pre-computes
all shadowing points and their types, which allows Phase 2 to search for
references without worrying about shadow detection during traversal."
  (let ((target-name (base:symbol-name-from-string var-name))
        (shadows '()))
    ;; Find all shadows in the body
    (dolist (form body)
      (find-shadows-in-expr form target-name shadows))
    (nreverse shadows)))

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

(defun calculate-variable-position (binding var-name form-type binding-line binding-column)
  "Calculate the actual position of VAR-NAME within BINDING.
Returns (values line column). Falls back to binding position if calculation fails."
  (handler-case
      (let ((var-name-normalized (base:symbol-name-from-string var-name)))
        (cond
          ;; For simple bindings like (var init-form), var is after one paren and potential whitespace
          ;; We'll estimate column as binding-column + 1 (opening paren)
          ;; This is a heuristic - actual position depends on whitespace
          ((and (consp binding)
                (stringp (first binding))
                (string-equal (base:symbol-name-from-string (first binding)) var-name-normalized))
           ;; The variable is the first element of the binding
           ;; Estimate: after opening paren + potential whitespace
           ;; Conservative estimate: binding-column + 1
           (values binding-line (+ binding-column 1)))
          ;; For other cases, use binding position as-is
          (t
           (values binding-line binding-column))))
    (error ()
      ;; On any error, fall back to binding position
      (values binding-line binding-column))))

;;; Binding checking

(defun check-binding-form (form-type bindings body fallback-line fallback-column position-map rule &optional aux-context message-prefix)
  "Check bindings for unused variables using scope-based approach.
FORM-TYPE determines how scope is calculated (:let, :let*, :defun-regular, :defun-aux, :do).
MESSAGE-PREFIX is the prefix for violation messages (default 'Variable').
Pushes violations to *violations* special variable."
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
                   (let ((is-ignored (ignored-var-p var-name ignored-vars))
                         (is-referenced (find-references var-name scope)))
                     (unless (or is-ignored is-referenced)
                       ;; Find position for this binding or variable name
                       (multiple-value-bind (var-line var-column)
                           (cond
                             ;; Prefer *current-form-position* if set (contains the enclosing form's position)
                             (*current-form-position*
                              ;; Calculate actual variable position from the current form position
                              (calculate-variable-position binding var-name form-type
                                                          (car *current-form-position*)
                                                          (cdr *current-form-position*)))
                             ;; Try position-map lookup for binding position
                             (position-map
                              (multiple-value-bind (binding-line binding-column)
                                  ;; Look up the binding form's position
                                  ;; For single-element bindings like ((pattern)), look up the pattern directly
                                  (parser:find-position (if (and (= (length binding) 1) (consp binding))
                                                            (first binding)
                                                            binding)
                                                        position-map nil nil)
                                (if binding-line
                                    ;; Calculate actual variable position from binding position
                                    (calculate-variable-position binding var-name form-type binding-line binding-column)
                                    (values fallback-line fallback-column))))
                             ;; Last resort: use fallback
                             (t
                              (values fallback-line fallback-column)))
                         (push (make-instance 'violation:violation
                                              :rule (base:rule-name rule)
                                              :file *file*
                                              :line var-line
                                              :column var-column
                                              :severity (base:rule-severity rule)
                                              :message
                                              (format nil "~A '~A' is unused"
                                                      (or message-prefix "Variable")
                                                      (base:symbol-name-from-string var-name))
                                              :fix nil)
                               *violations*))))))))))

(defun check-defun-bindings (expr line column position-map rule)
  "Check DEFUN for unused parameter bindings."
  (let ((rest-args (rest expr)))
    (when (and (a:proper-list-p rest-args)
               (>= (length rest-args) 3)
               (listp (second rest-args)))
      (let* ((lambda-list (second rest-args))
             (body (cddr rest-args)))
        ;; Bind *current-form-position* for this DEFUN
        (let ((*current-form-position* (cons line column)))
          (multiple-value-bind (non-aux-part aux-params)
              (parse-lambda-list-for-aux lambda-list)
            (let* ((var-names (extract-lambda-list-vars non-aux-part nil))
                   (param-bindings (mapcar #'list var-names)))
              (when param-bindings
                (check-binding-form :defun-regular param-bindings body line column position-map rule)))
            (when aux-params
              (check-binding-form :defun-aux aux-params body line column position-map rule body))))))))

(defun check-lambda-bindings (expr line column position-map rule)
  "Check LAMBDA for unused parameter bindings."
  (let ((rest-args (rest expr)))
    (when (and (a:proper-list-p rest-args)
               (>= (length rest-args) 2)
               (listp (first rest-args)))
      (let* ((lambda-list (first rest-args))
             (body (rest rest-args)))
        (let ((*current-form-position* (cons line column)))
          (multiple-value-bind (non-aux-part aux-params)
              (parse-lambda-list-for-aux lambda-list)
            (let* ((var-names (extract-lambda-list-vars non-aux-part nil))
                   (param-bindings (mapcar #'list var-names)))
              (when param-bindings
                (check-binding-form :defun-regular param-bindings body line column position-map rule)))
            (when aux-params
              (check-binding-form :defun-aux aux-params body line column position-map rule body))))))))

(defun check-let-bindings (expr line column position-map rule)
  "Check LET for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (and (a:proper-list-p rest-args) (>= (length rest-args) 2))
      (handler-case
          (let ((bindings (first rest-args))
                (body (rest rest-args)))
            ;; Don't set *current-form-position* for LET - let each binding be looked up individually
            (check-binding-form :let bindings body line column position-map rule))
        (type-error (e)
          (format *error-output* "~%Warning: Skipping LET form at ~A:~A due to unexpected structure: ~A~%"
                  *file* line e)
          nil)))))

(defun check-let*-bindings (expr line column position-map rule)
  "Check LET* for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (and (a:proper-list-p rest-args) (>= (length rest-args) 2))
      (let ((bindings (first rest-args))
            (body (rest rest-args)))
        ;; Don't set *current-form-position* for LET* - let each binding be looked up individually
        (check-binding-form :let* bindings body line column position-map rule)))))

(defun check-loop-bindings (expr line column position-map rule)
  "Check LOOP for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (a:proper-list-p rest-args)
      (multiple-value-bind (loop-bindings body)
          (loop-parser:parse-loop-clauses rest-args)
        (when loop-bindings
          (multiple-value-bind (loop-line loop-column)
              (parser:find-position expr position-map line column)
            (let ((*current-form-position* (cons loop-line loop-column)))
              (let ((bindings (mapcar (lambda (lb)
                                       (list (loop-parser:loop-binding-pattern lb)))
                                     loop-bindings)))
                (check-binding-form :let bindings body loop-line loop-column position-map rule)))))))))

(defun check-do-bindings (expr line column position-map rule)
  "Check DO for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (and (a:proper-list-p rest-args) (>= (length rest-args) 2))
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
          (let ((*current-form-position* (cons line column)))
            (check-binding-form :do bindings body line column position-map rule do-context)))))))

(defun check-destructuring-bind-bindings (expr line column position-map rule)
  "Check DESTRUCTURING-BIND for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (and (a:proper-list-p rest-args) (>= (length rest-args) 3))
      (let* ((lambda-list (first rest-args))
             (body (cddr rest-args))
             (var-names (extract-lambda-list-vars lambda-list t))
             (bindings (mapcar #'list var-names)))
        (when bindings
          (let ((*current-form-position* (cons line column)))
            (check-binding-form :let bindings body line column position-map rule)))))))

(defun check-multiple-value-bind-bindings (expr line column position-map rule)
  "Check MULTIPLE-VALUE-BIND for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (and (a:proper-list-p rest-args) (>= (length rest-args) 3))
      (let* ((vars (first rest-args))
             (body (cddr rest-args))
             (bindings (list vars)))
        (let ((*current-form-position* (cons line column)))
          (check-binding-form :let bindings body line column position-map rule))))))

(defun check-dolist-bindings (expr line column position-map rule)
  "Check DOLIST for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (and (a:proper-list-p rest-args) (>= (length rest-args) 2))
      (let* ((spec (first rest-args))
             (body (rest rest-args)))
        (when (and (a:proper-list-p spec) (>= (length spec) 2))
          (let* ((var (first spec))
                 (bindings (list (list var))))
            ;; Calculate variable position from DOLIST form position
            ;; For (dolist (var ...) ...), variable is at: column + 1 + len("dolist") + 1 + 1
            ;; But calculate-variable-position adds +1, so we use column + 8
            (let ((*current-form-position* (cons line (+ column 8))))
              (check-binding-form :let bindings body line column position-map rule))))))))

(defun check-do-symbols-bindings (expr line column position-map rule)
  "Check DO-SYMBOLS/DO-EXTERNAL-SYMBOLS/DO-ALL-SYMBOLS for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (and (a:proper-list-p rest-args) (>= (length rest-args) 2))
      (let* ((spec (first rest-args))
             (body (rest rest-args)))
        (when (and (a:proper-list-p spec) (>= (length spec) 1))
          (let* ((var (first spec))
                 (bindings (list (list var))))
            (let ((*current-form-position* (cons line (+ column 1))))
              (check-binding-form :let bindings body line column position-map rule))))))))

(defun check-with-slots-bindings (expr line column position-map rule)
  "Check WITH-SLOTS for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (and (a:proper-list-p rest-args) (>= (length rest-args) 3))
      (let* ((slot-specs (first rest-args))
             (body (cddr rest-args)))
        (when (a:proper-list-p slot-specs)
          (let ((bindings (mapcar (lambda (spec)
                                   (list (if (consp spec)
                                            (first spec)  ; (var slot-name)
                                            spec)))       ; var
                                 slot-specs)))
            (let ((*current-form-position* (cons line (+ column 1))))
              (check-binding-form :let bindings body line column position-map rule))))))))

(defun check-with-accessors-bindings (expr line column position-map rule)
  "Check WITH-ACCESSORS for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (and (a:proper-list-p rest-args) (>= (length rest-args) 3))
      (let* ((accessor-specs (first rest-args))
             (body (cddr rest-args)))
        (when (a:proper-list-p accessor-specs)
          (let ((bindings (mapcar (lambda (spec)
                                   (when (and (consp spec) (>= (length spec) 2))
                                     (list (first spec))))  ; (var accessor-name)
                                 accessor-specs)))
            (let ((*current-form-position* (cons line (+ column 1))))
              (check-binding-form :let (remove nil bindings) body line column position-map rule))))))))

(defun check-with-input-from-string-bindings (expr line column position-map rule)
  "Check WITH-INPUT-FROM-STRING for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (and (a:proper-list-p rest-args) (>= (length rest-args) 2))
      (let* ((spec (first rest-args))
             (body (rest rest-args)))
        (when (and (a:proper-list-p spec) (>= (length spec) 2))
          (let* ((var (first spec))
                 (bindings (list (list var))))
            (let ((*current-form-position* (cons line (+ column 1))))
              (check-binding-form :let bindings body line column position-map rule))))))))

(defun check-with-output-to-string-bindings (expr line column position-map rule)
  "Check WITH-OUTPUT-TO-STRING for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (and (a:proper-list-p rest-args) (>= (length rest-args) 1))
      (let* ((spec (first rest-args))
             (body (rest rest-args)))
        (when (a:proper-list-p spec)
          (let* ((var (first spec))
                 (bindings (list (list var))))
            (let ((*current-form-position* (cons line (+ column 1))))
              (check-binding-form :let bindings body line column position-map rule))))))))

(defun check-with-open-file-bindings (expr line column position-map rule)
  "Check WITH-OPEN-FILE for unused variable bindings."
  (let ((rest-args (rest expr)))
    (when (and (a:proper-list-p rest-args) (>= (length rest-args) 2))
      (let* ((spec (first rest-args))
             (body (rest rest-args)))
        (when (and (a:proper-list-p spec) (>= (length spec) 2))
          (let* ((var (first spec))
                 (bindings (list (list var))))
            (let ((*current-form-position* (cons line (+ column 1))))
              (check-binding-form :let bindings body line column position-map rule))))))))

(defun check-defmacro-bindings (expr line column position-map rule)
  "Check DEFMACRO for unused parameter bindings."
  (let ((rest-args (rest expr)))
    (when (and (a:proper-list-p rest-args)
               (>= (length rest-args) 3)
               (listp (second rest-args)))
      (let* ((lambda-list (second rest-args))
             (body (cddr rest-args)))
        (let ((*current-form-position* (cons line column)))
          (multiple-value-bind (non-aux-part aux-params)
              (parse-lambda-list-for-aux lambda-list)
            (let* ((var-names (extract-lambda-list-vars non-aux-part t))
                   (param-bindings (mapcar #'list var-names)))
              (when param-bindings
                (check-binding-form :defun-regular param-bindings body line column position-map rule)))
            (when aux-params
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
        ((base:symbol-matches-p head "LOOP")
         (check-loop-bindings expr line column position-map rule))
        ((base:symbol-matches-p head "DO")
         (check-do-bindings expr line column position-map rule))
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
                 (check-expr subexpr
                            (or subexpr-line line)
                            (or subexpr-column column)
                            position-map
                            rule))))))))))

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
        ;; Bind special variables and call check-expr
        (let ((*violations* '())
              (*file* file))
          (let ((position-map (parser:form-position-map form)))
            (check-expr (parser:form-expr form)
                        (parser:form-line form)
                        (parser:form-column form)
                        position-map
                        rule))
          (nreverse *violations*)))
    (type-error (e)
      ;; Print warning message only in debug mode
      nil)))

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
               (check-loop-expr subexpr line column position-map rule)))))))))

(defmethod base:check-form ((rule unused-loop-variables-rule) form file)
  "Check that LOOP variables are either used or explicitly ignored.
Recursively descends into all forms to find nested LOOPs."
  (check-type form parser:form)
  (check-type file pathname)

  (handler-case
      (handler-bind ((type-error
                       (lambda (e)
                         (when (debug-mode-p)
                           (format *error-output* "~%Warning: Skipping unused-loop-variables check at ~A:~A~%  Error: ~A~%"
                                   file (parser:form-line form) e)
                           (uiop:print-condition-backtrace e)))))
        ;; Bind special variables and recursively check for LOOPs
        (let ((*violations* '())
              (*file* file))
          (let ((position-map (parser:form-position-map form)))
            (check-loop-expr (parser:form-expr form)
                            (parser:form-line form)
                            (parser:form-column form)
                            position-map
                            rule))
          (nreverse *violations*)))
    (type-error (e)
      nil)))
