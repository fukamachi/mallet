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
  (:export #:needless-let*-rule
           #:unused-variables-rule
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

(defvar *parsing-context* nil
  "Dynamic variable tracking parsing context: :known-body or :unknown.
Used in find-references-with-shadows to determine if we should apply Lisp-2 semantics.")

(defvar *known-macro-signatures* (make-hash-table :test 'equal)
  "Hash table mapping macro names to their indentation signatures (Lem-style).
Key: macro name (uppercase string), Value: indentation spec.
Format follows Lem convention:
  - Integer N: skip N args, then &body starts
  - (&body) or (&rest &body): all args are body
  - (N &body): skip N args, then &body
  - (N &lambda &body): skip N args, lambda list, then &body")

(defvar *clause-element-types* (make-hash-table :test 'equal)
  "Registry of clause first-element types for clause-based macros.
Key: uppercase macro name string.
Value: :expression or :literal (future: :binding).")

(defun register-macro-signature (name signature)
  "Register a macro signature following Lem's convention.
Examples:
  (register-macro-signature \"with-open-file\" 1) - skip 1 arg, then body
  (register-macro-signature \"when\" 1) - skip 1 arg (test), then body
  (register-macro-signature \"progn\" '(&rest &body)) - all args are body"
  (setf (gethash (string-upcase name) *known-macro-signatures*) signature))

(defun register-clause-element-type (name type)
  "Register the clause first-element TYPE (:expression or :literal) for macro NAME."
  (setf (gethash (string-upcase (string (base:symbol-name-from-string name))) *clause-element-types*)
        type))

(defun get-clause-element-type (name)
  "Look up the clause first-element type for macro NAME."
  (gethash (string-upcase (string (base:symbol-name-from-string name))) *clause-element-types*))

(defun parse-macro-signature (signature)
  "Parse Lem-style macro signature to extract body information.
Returns:
  - Integer position where &body begins (0-indexed after macro name)
  - (:clauses position) for clause-based macros with &rest clause bodies
  - NIL if no body is present."
  (etypecase signature
    (integer signature)  ; Simple case: integer means body starts at that position
    (list
     ;; Complex signature - look for &body or &rest &body
     (let ((pos 0))
       (dolist (elem signature)
         (cond
           ;; Clause-based macro: &rest followed by &body
           ((and (eq elem '&rest)
                 (member '&body signature))
            (return-from parse-macro-signature (list :clauses pos)))
           ;; Direct &body - simple body position
           ((eq elem '&body)
            (return-from parse-macro-signature pos))
           ((eq elem '&lambda)
            ;; Lambda list - skip it and continue
            (incf pos))
           ((integerp elem)
            ;; Skip N arguments
            (incf pos elem))
           (t
            ;; Unknown element, skip
            (incf pos))))
       ;; No &body found
       nil))))

(defun known-macro-body-position (head)
  "Get the body start position for a known macro, or NIL if unknown.
HEAD is a string (potentially package-prefixed)."
  (when (stringp head)
    ;; Strip package prefix if present
    (let* ((colon-pos (or (search "::" head) (position #\: head)))
           (package-name (when colon-pos (subseq head 0 colon-pos)))
           (symbol-name (if colon-pos
                            (subseq head (+ colon-pos (if (search "::" head) 2 1)))
                            head)))
      ;; Only check CL macros (same logic as known-function-p)
      (when (or (null package-name)
                (member package-name '("CL" "COMMON-LISP" "CURRENT") :test #'string-equal))
        (let ((signature (gethash (string-upcase symbol-name) *known-macro-signatures*)))
          (when signature
            (parse-macro-signature signature)))))))

;; Common CL macros are registered via register-macros-from-lambda-lists below
;; This provides unified registration that serves both:
;; 1. Context-aware parsing (all macros get signatures registered)
;; 2. Unused variable checking (binding forms get extractors registered)

;;; Unified Binding Form Checker
;;
;; This system allows easy registration of custom binding forms by specifying:
;; 1. Lem-style signature - where body starts
;; 2. Binding extractor - how to extract variables from the binding position
;; 3. Scope type - :let or :let* for binding semantics

(defstruct binding-form-descriptor
  "Descriptor for a binding form that can be checked with generic checker.
SIGNATURE: Lem-style signature indicating where body starts (integer or list)
BINDING-POSITION: Index in rest-args where binding spec is located (0-indexed, default 0)
BINDING-EXTRACTOR: Function (binding-spec) -> list of bindings for check-binding-form
                   Returns list of binding forms suitable for check-binding-form
SCOPE-TYPE: :let or :let* indicating binding semantics
REPORT-VIOLATIONS: Whether to report violations for this form (default t)
                   Set to NIL for forms where unused variables are conventionally acceptable"
  signature
  (binding-position 0)
  binding-extractor
  scope-type
  (report-violations t))

(defvar *binding-form-registry* (make-hash-table :test 'equal)
  "Registry of binding forms for generic checking.
Key: uppercase macro name string
Value: binding-form-descriptor")

(defun register-binding-form (name signature binding-extractor scope-type &key (binding-position 0) (report-violations t))
  "Register a binding form for generic checking.
NAME: macro name (string, will be uppercased)
SIGNATURE: Lem-style signature (integer or list) indicating where body starts
BINDING-EXTRACTOR: function (binding-spec) -> list of bindings
SCOPE-TYPE: :let or :let*
BINDING-POSITION: where to find binding spec in rest-args (default 0)
REPORT-VIOLATIONS: whether to report violations for this form (default t)"
  (setf (gethash (string-upcase name) *binding-form-registry*)
        (make-binding-form-descriptor
         :signature signature
         :binding-position binding-position
         :binding-extractor binding-extractor
         :scope-type scope-type
         :report-violations report-violations)))

(defun check-binding-form-generic (expr line column position-map rule)
  "Generic checker for registered binding forms using Lem-style signatures.
Returns T if this form was handled, NIL otherwise."
  (when (consp expr)
    (let* ((head (first expr))
           (head-name (when (stringp head)
                        (string-upcase (base:symbol-name-from-string head))))
           (descriptor (when head-name
                         (gethash head-name *binding-form-registry*))))
      (when descriptor
        (let* ((signature (binding-form-descriptor-signature descriptor))
               (body-pos (parse-macro-signature signature))
               (rest-args (rest expr)))
          (when (and body-pos
                     (> (length rest-args) body-pos))
            ;; Extract binding spec and body based on signature
            (let* ((binding-pos (binding-form-descriptor-binding-position descriptor))
                   (binding-spec (when (< binding-pos (length rest-args))
                                   (nth binding-pos rest-args)))
                   (body (subseq rest-args body-pos))
                   (scope-type (binding-form-descriptor-scope-type descriptor))
                   (report-violations (binding-form-descriptor-report-violations descriptor)))
              (when (and binding-spec report-violations)
                ;; Apply the extractor function to get bindings
                ;; Only check if report-violations is true
                (let ((bindings (funcall (binding-form-descriptor-binding-extractor descriptor)
                                         binding-spec)))
                  (when bindings
                    (scope:with-new-scope
                        (check-binding-form scope-type bindings body line column position-map rule)))))
              ;; Return T to indicate we handled this form
              t)))))))

(defun param-name (param)
  "Extract parameter name as string, handling both symbols and strings.
For strings, extracts just the symbol name (strips package prefix if present)."
  (etypecase param
    (string (base:symbol-name-from-string param))
    (symbol (symbol-name param))))

(defun param-p (param)
  "Check if PARAM is a parameter (string or symbol, not a list)."
  (or (stringp param) (symbolp param)))

(defun infer-binding-info (lambda-list)
  "Infer binding information from lambda list structure.
Returns (values binding-position extraction-type) or (values nil nil) if no bindings.

Extraction types:
  :first - First element is the binding (e.g., ((var value) &body))
  :each - Each element is a binding (e.g., (vars values &body))
  nil - No bindings

Heuristics:
  - ((var ...rest) &body) → position 0, extract :first
  - (vars ... &body) where first param is symbol → position 0, extract :each
  - (value &body) where value is symbol, no list params → no bindings"
  (let ((first-param (first lambda-list)))
    (cond
      ;; First param is a list: ((var value) &body) pattern
      ((consp first-param)
       (values 0 :first))
      ;; First param is a symbol/string, check if it's likely a binding spec
      ;; Heuristic: If followed by another param before &body, it's probably a binding spec
      ;; (e.g., (slots instance &body) vs (lock &body))
      ((and (param-p first-param)
            (>= (length lambda-list) 2)
            (param-p (second lambda-list))
            (not (or (string-equal (param-name (second lambda-list)) "&BODY")
                     (string-equal (param-name (second lambda-list)) "&REST"))))
       ;; Two parameters before &body, first is likely a binding spec list
       (values 0 :each))
      ;; Single value before &body - no bindings
      (t
       (values nil nil)))))

(defun make-binding-extractor (extraction-type)
  "Create a binding extractor based on extraction type.

Extraction types:
  :first - Extract first element as the binding (e.g., (var value) -> var)
  :each - Extract each element as a binding (e.g., (var1 var2 var3) -> var1, var2, var3)
  nil - No extraction (returns nil)"
  (ecase extraction-type
    (:first
     (lambda (spec)
       (cond
         ;; Destructuring in first position: ((var1 var2) ...) - extract all vars
         ((and (utils:proper-list-of-min-length-p spec 1)
               (consp (first spec)))
          (let ((vars (extract-bindings (first spec) :destructuring)))
            (mapcar #'list vars)))
         ;; List with string as first element: (var) or (var value ...) - extract var
         ((and (utils:proper-list-of-min-length-p spec 1)
               (stringp (first spec)))
          (list (list (first spec))))
         ;; Just a variable string - wrap in list
         ((stringp spec)
          (list (list spec)))
         ;; Can't parse
         (t nil))))
    (:each
     (lambda (spec)
       (cond
         ;; List of binding specs - each element is a binding
         ((a:proper-list-p spec)
          (mapcar (lambda (elem)
                    (list (if (consp elem)
                              (first elem)  ; (var slot-name) -> var
                              elem)))       ; var -> var
                  spec))
         ;; Single spec
         ((stringp spec)
          (list (list spec)))
         (t nil))))
    ((nil)
     (lambda (spec)
       (declare (ignore spec))
       nil))))

(defun register-macro-from-lambda-list (name lambda-list &key (scope-type :let) (report-violations t) (bindings :auto))
  "Register a macro for unused variable checking from its lambda list.
Hybrid approach: auto-infers bindings from structure, with explicit override support.

Examples:
  ;; Auto-inferred (binding position and extraction type):
  (register-macro-from-lambda-list 'with-open-file '((var pathname &rest options) &body body))
  (register-macro-from-lambda-list 'with-slots '(slots instance &body body))

  ;; Explicit override - no bindings:
  (register-macro-from-lambda-list 'with-lock-held '(lock &body body) :bindings nil)

  ;; Explicit override - custom extraction:
  (register-macro-from-lambda-list 'weird-macro '(a b c &body body)
                                   :bindings (:param 1 :extract :first))

The function will:
1. Find where &body starts (for signature)
2. Infer binding position and extraction type from structure (or use explicit :bindings)
3. Create appropriate extractor based on extraction type

For complete custom control, use register-binding-form directly."
  (let ((signature (parse-macro-lambda-list-signature lambda-list))
        (name-string (etypecase name
                       (string name)
                       (symbol (symbol-name name)))))
    (when signature
      ;; Always register signature for context-aware parsing
      (register-macro-signature name-string signature)

      ;; Register binding form if it has bindings
      (multiple-value-bind (binding-pos extraction-type)
          (cond
            ;; Explicit :bindings nil means no bindings
            ((null bindings)
             (values nil nil))
            ;; Explicit :bindings with plist specification
            ((and (listp bindings) (not (eq bindings :auto)))
             (values (getf bindings :param)
                     (getf bindings :extract)))
            ;; Default :auto or any other value - auto-infer from lambda list
            (t
             (infer-binding-info lambda-list)))
        (when binding-pos
          ;; Has bindings - register with extractor
          (let ((extractor (make-binding-extractor extraction-type)))
            (register-binding-form name-string signature extractor scope-type
                                   :binding-position binding-pos
                                   :report-violations report-violations)))))))

(defun parse-macro-lambda-list-signature (lambda-list)
  "Parse macro lambda list to create Lem-style signature.
Returns signature (integer or list) indicating where &body starts.

Handles nested structures:
  (test &body body) -> 1
  (&body body) -> 0
  (&rest (clause &body body)) -> (pos &rest &body) for nested clause bodies"
  (loop for elem in lambda-list
        for pos from 0
        for next-elem = (when (< (1+ pos) (length lambda-list))
                          (nth (1+ pos) lambda-list))
        do (cond
             ;; Check for &body first
             ((and (param-p elem)
                   (string-equal (param-name elem) "&BODY"))
              (return pos))
             ;; Check for &rest followed by list containing &body
             ;; This handles nested structures like (case test &rest (clause &body body))
             ((and (param-p elem)
                   (string-equal (param-name elem) "&REST")
                   (consp next-elem)
                   ;; Check if the nested list contains &body
                   (some (lambda (nested-elem)
                           (and (param-p nested-elem)
                                (string-equal (param-name nested-elem) "&BODY")))
                         next-elem))
              ;; Return (pos &rest &body) to indicate nested clause bodies
              (return (list pos '&rest '&body))))
        finally (return nil)))

(defun register-macros-from-lambda-lists (macro-specs)
  "Batch register multiple macros from lambda list specifications.
Each spec is (name lambda-list &key options...).

Hybrid approach: Automatically infers bindings from lambda list structure,
with explicit overrides via :bindings keyword.

Examples:
  (register-macros-from-lambda-lists
    '(;; Auto-inferred - ((var value) &body) pattern:
      (with-open-file ((var pathname &rest options) &body body))
      (dolist ((var list) &body body))

      ;; Auto-inferred - (specs instance &body) pattern:
      (with-slots (slots instance &body body))

      ;; Explicit override - no bindings:
      (with-lock-held (lock &body body) :bindings nil)

      ;; Explicit override - don't report violations:
      (dotimes ((i count) &body body) :report-violations nil)

      ;; Explicit override - custom extraction:
      (weird-macro (a b c &body body) :bindings (:param 1 :extract :first))))"
  (dolist (spec macro-specs)
    (destructuring-bind (name lambda-list &rest options) spec
      (apply #'register-macro-from-lambda-list name lambda-list options))))

;; Register all Common Lisp macros with unified system
;; This handles both context-aware parsing AND unused variable checking

(register-macros-from-lambda-lists
 '(;; === Control Flow Macros ===
   ;; Simple body forms - test/forms are evaluated
   (when (test &body body) :bindings nil)
   (unless (test &body body) :bindings nil)
   (prog1 (first-form &body body) :bindings nil)
   (prog2 (first-form second-form &body body) :bindings nil)
   (multiple-value-prog1 (first-form &body body) :bindings nil)
   (return-from (name &optional value) :bindings nil)

   ;; Nested clause bodies - CASE, COND, TYPECASE families
   ;; Each clause has its own body
   (cond (&rest (test &body body)) :bindings nil)
   (case (keyform &rest (keys &body body)) :bindings nil)
   (ecase (keyform &rest (keys &body body)) :bindings nil)
   (ccase (keyform &rest (keys &body body)) :bindings nil)
   (typecase (keyform &rest (type &body body)) :bindings nil)
   (etypecase (keyform &rest (type &body body)) :bindings nil)
   (ctypecase (keyform &rest (type &body body)) :bindings nil)

   ;; Logical operators - all forms are evaluated
   (and (&body forms) :bindings nil)
   (or (&body forms) :bindings nil)

   ;; === Exception Handling ===
   (unwind-protect (protected-form &body cleanup-forms) :bindings nil)
   (catch (tag &body body) :bindings nil)
   ;; HANDLER-BIND: bindings is ((condition-type handler-fn) ...)
   (handler-bind (bindings &body body) :bindings nil)
   ;; HANDLER-CASE: clauses are (condition-type lambda-list &body body)
   (handler-case (form &rest (condition-type lambda-list &body body)) :bindings nil)
   (restart-bind (bindings &body body) :bindings nil)
   (restart-case (form &rest (restart-name lambda-list &body body)) :bindings nil)

   ;; === WITH-* Macros (with bindings) ===
   ;; First arg is a spec list that introduces bindings
   (with-open-file ((var pathname &rest options) &body body))
   (with-open-stream ((var stream) &body body))
   (with-input-from-string ((var string &rest options) &body body))
   (with-output-to-string ((var &rest options) &body body))
   ;; WITH-SLOTS/WITH-ACCESSORS: slots/accessors evaluated, then instance evaluated
   (with-slots (slots instance &body body))
   (with-accessors (accessors instance &body body))
   ;; These also introduce bindings
   (with-hash-table-iterator ((name hash-table) &body body))
   (with-package-iterator ((name package-list &rest symbol-types) &body body))

   ;; === WITH-* Macros (without bindings) ===
   ;; These evaluate arguments but don't introduce new variables
   (with-standard-io-syntax (&body body) :bindings nil)
   (with-compilation-unit ((options) &body body) :bindings nil)
   (with-condition-restarts (condition-form restarts-form &body body) :bindings nil)
   (with-simple-restart ((name format-control &rest format-args) &body body) :bindings nil)

   ;; === Iteration Macros ===
   ;; Spec is (var init-form &optional result-form)
   ;; var is a binding, init-form is evaluated in outer scope
   (dolist ((var list-form &optional result-form) &body body))
   (dotimes ((var count-form &optional result-form) &body body) :report-violations nil)
   (do-symbols ((var &optional package-form result-form) &body body))
   (do-external-symbols ((var &optional package-form result-form) &body body))
   (do-all-symbols ((var &optional result-form) &body body))
   ;; TODO: DO and DO* need special handling for their complex binding structure
   ;; (do ((var init step)*) (end-test result*) body*)
   ;; For now, register with basic structure
   (do (var-clauses end-test-form &body body) :bindings nil)
   (do* (var-clauses end-test-form &body body) :bindings nil)
   (loop (&body forms) :bindings nil)

   ;; === Local Definition Macros ===
   ;; These need special handling - :bindings nil for now
   ;; definitions is ((name lambda-list &body body) ...)
   (flet (definitions &body body) :bindings nil)
   (labels (definitions &body body) :bindings nil)
   (macrolet (definitions &body body) :bindings nil)
   (symbol-macrolet (definitions &body body) :bindings nil)

   ;; === Block and Scope Macros ===
   (block (name &body body) :bindings nil)
   (tagbody (&body statements) :bindings nil)
   ;; PROG/PROG* have bindings: (prog ((var init)*) body*)
   (prog (bindings &body body) :bindings nil)
   (prog* (bindings &body body) :bindings nil)
   (locally (&body body) :bindings nil)
   (progn (&body body) :bindings nil)

   ;; === Other Standard Macros ===
   ;; EVAL-WHEN: situations is (:compile-toplevel :load-toplevel :execute)
   (eval-when ((situations) &body body) :bindings nil)
   (multiple-value-call (function-form &body forms) :bindings nil)
   ;; MULTIPLE-VALUE-BIND has bindings - handled specially in check-multiple-value-bind-bindings
   ;; TODO: Add unified registration once we refactor MULTIPLE-VALUE-BIND
   ))

;; Clause element type registry for clause-based macros
(dolist (entry '(("COND" . :expression)
                 ("CASE" . :literal)
                 ("ECASE" . :literal)
                 ("CCASE" . :literal)
                 ("TYPECASE" . :literal)
                 ("ETYPECASE" . :literal)
                 ("CTYPECASE" . :literal)
                 ("HANDLER-CASE" . :literal)
                 ("RESTART-CASE" . :literal)))
  (register-clause-element-type (car entry) (cdr entry)))

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
    ;; Lambda list with keywords - use proper lambda list extraction
    ;; Examples: (&key x y), (&optional (x 1)), (a &key (b "default"))
    ;; This handles default values correctly (won't extract "default" as a variable)
    ((and (consp binding-form)
          (contains-lambda-list-keyword-p binding-form))
     (extract-lambda-list-vars binding-form t))
    ;; Pure destructuring pattern: all elements are variables or nested patterns
    ;; Examples: (a b c), ((a b) c), (a . b)
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

(defun extract-required-param (elem allow-destructuring vars)
  "Extract variable(s) from required parameter ELEM.
Returns updated VARS list."
  (cond
    ((not allow-destructuring)
     ;; Simple variable only
     (if (stringp elem)
         (cons elem vars)
         vars))
    ((stringp elem)
     ;; Simple string variable
     (cons elem vars))
    ((contains-lambda-list-keyword-p elem)
     ;; Nested lambda list - recursively extract variables
     (let ((extracted (extract-lambda-list-vars elem t)))
       (append vars extracted)))
    (t
     ;; Simple destructuring pattern - extract all strings
     (let ((extracted (extract-bindings (list elem) :destructuring)))
       (append vars extracted)))))

(defun extract-optional-param (elem vars)
  "Extract variable(s) from &optional parameter ELEM.
Returns updated VARS list."
  (cond
    ((stringp elem)
     (cons elem vars))
    ((and (consp elem) (stringp (first elem)))
     ;; (var ...) - extract var
     (let ((new-vars (cons (first elem) vars)))
       ;; Extract supplied-p if present: (var default supplied-p)
       (if (and (>= (length elem) 3) (stringp (third elem)))
           (cons (third elem) new-vars)
           new-vars)))
    (t vars)))

(defun extract-key-param (elem vars)
  "Extract variable(s) from &key parameter ELEM.
Returns updated VARS list."
  (cond
    ((stringp elem)
     (cons elem vars))
    ((and (consp elem)
          (consp (first elem))
          (utils:proper-list-of-exact-length-p (first elem) 2)
          (stringp (second (first elem))))
     ;; ((:keyword var) ...) - extract var
     (let ((new-vars (cons (second (first elem)) vars)))
       ;; Extract supplied-p if present
       (if (and (>= (length elem) 3) (stringp (third elem)))
           (cons (third elem) new-vars)
           new-vars)))
    ((and (consp elem) (stringp (first elem)))
     ;; (var ...) - extract var
     (let ((new-vars (cons (first elem) vars)))
       ;; Extract supplied-p if present
       (if (and (>= (length elem) 3) (stringp (third elem)))
           (cons (third elem) new-vars)
           new-vars)))
    (t vars)))


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
                   (setf vars (extract-required-param elem allow-destructuring vars)))
                  (:optional
                   (setf vars (extract-optional-param elem vars)))
                  (:rest
                   ;; &rest/&body param: just a variable name
                   (when (stringp elem)
                     (push elem vars))
                   ;; After &rest, next keyword or aux
                   (setf state :required))
                  (:key
                   (setf vars (extract-key-param elem vars)))
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

(defun handle-let-shadow (expr rest-args target-name)
  "Handle LET form shadow detection."
  (when (utils:proper-list-of-min-length-p rest-args 2)
    (let ((bindings (first rest-args)))
      (when (and (a:proper-list-p bindings)
                 (some (lambda (b) (binds-same-name-p b target-name)) bindings))
        ;; Found shadowing - record partial-let shadow
        (let ((init-forms (mapcar (lambda (b)
                                    (when (consp b) (second b)))
                                  bindings)))
          (push (make-shadow-info
                 :form expr
                 :type :partial-let
                 :searchable (remove nil init-forms))
                *shadows*)))
      ;; Continue searching if not shadowed
      (when (and (a:proper-list-p bindings)
                 (not (some (lambda (b) (binds-same-name-p b target-name)) bindings)))
        (dolist (binding bindings)
          (when (consp binding)
            (find-shadows-in-expr (second binding) target-name)))
        (dolist (form (rest rest-args))
          (find-shadows-in-expr form target-name))))))

(defun handle-let*-shadow (expr rest-args target-name)
  "Handle LET* form shadow detection."
  (when (utils:proper-list-of-min-length-p rest-args 2)
    (let ((bindings (first rest-args)))
      (when (and (a:proper-list-p bindings)
                 (some (lambda (b) (binds-same-name-p b target-name)) bindings))
        ;; Found shadowing
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
      ;; Continue searching if not shadowed
      (when (and (a:proper-list-p bindings)
                 (not (some (lambda (b) (binds-same-name-p b target-name)) bindings)))
        (dolist (binding bindings)
          (when (consp binding)
            (find-shadows-in-expr (second binding) target-name)))
        (dolist (form (rest rest-args))
          (find-shadows-in-expr form target-name))))))

(defun lambda-shadows-p (lambda-list target-name)
  "Check if lambda-list contains a parameter matching target-name."
  (and (a:proper-list-p lambda-list)
       (some (lambda (param)
               (and (stringp param)
                    (not (utils:lambda-list-keyword-p param))
                    (string-equal (base:symbol-name-from-string param) target-name)))
             lambda-list)))

(defun handle-lambda-shadow (expr head rest-args target-name)
  "Handle DEFUN/LAMBDA/DEFMACRO form shadow detection."
  (let* ((lambda-list-pos (if (string-equal (base:symbol-name-from-string head) "LAMBDA") 0 1))
         (lambda-list (when (utils:proper-list-of-min-length-p rest-args (1+ lambda-list-pos))
                        (nth lambda-list-pos rest-args))))
    (when (lambda-shadows-p lambda-list target-name)
      (push (make-shadow-info
             :form expr
             :type :complete
             :searchable nil)
            *shadows*))
    ;; Continue searching if not shadowed
    (unless (lambda-shadows-p lambda-list target-name)
      (dolist (arg rest-args)
        (find-shadows-in-expr arg target-name)))))

(defun handle-destructuring-bind-shadow (expr rest-args target-name)
  "Handle DESTRUCTURING-BIND/MULTIPLE-VALUE-BIND shadow detection."
  (when (utils:proper-list-of-min-length-p rest-args 2)
    (let* ((vars (first rest-args))
           (init-form (second rest-args))
           (all-vars (extract-bindings vars :destructuring)))
      (when (some (lambda (v)
                    (string-equal (base:symbol-name-from-string v) target-name))
                  all-vars)
        (push (make-shadow-info
               :form expr
               :type :partial-let
               :searchable (list init-form))
              *shadows*))
      ;; Continue searching if not shadowed
      (unless (some (lambda (v)
                      (string-equal (base:symbol-name-from-string v) target-name))
                    all-vars)
        (dolist (arg rest-args)
          (find-shadows-in-expr arg target-name))))))

(defun handle-dolist-shadow (expr rest-args target-name)
  "Handle DOLIST/DOTIMES shadow detection."
  (when (utils:proper-list-of-min-length-p rest-args 1)
    (let* ((spec (first rest-args))
           (var (when (a:proper-list-p spec) (first spec)))
           (source-form (when (utils:proper-list-of-min-length-p spec 2)
                          (second spec))))
      (when (and (stringp var)
                 (string-equal (base:symbol-name-from-string var) target-name))
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

(defun handle-do-shadow (expr rest-args target-name)
  "Handle DO/DO* shadow detection."
  (when (utils:proper-list-of-min-length-p rest-args 2)
    (let ((var-clauses (first rest-args)))
      (when (and (a:proper-list-p var-clauses)
                 (some (lambda (b) (binds-same-name-p b target-name)) var-clauses))
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

(defun handle-loop-shadow (expr rest-args target-name)
  "Handle LOOP shadow detection with AND parallelism support."
  (when (a:proper-list-p rest-args)
    (multiple-value-bind (loop-bindings body)
        (loop-parser:parse-loop-clauses rest-args)
      (declare (ignore body))
      (let ((shadowing-binding
              (find-if (lambda (lb)
                         (let ((vars (extract-bindings (loop-parser:loop-binding-pattern lb) :destructuring)))
                           (some (lambda (v)
                                   (string-equal (base:symbol-name-from-string v) target-name))
                                 vars)))
                       loop-bindings)))
        (when shadowing-binding
          (if (loop-parser:loop-binding-is-parallel shadowing-binding)
              ;; Parallel binding (AND)
              (let* ((shadow-pos (position shadowing-binding loop-bindings))
                     (group-start (or (loop for i from (1- shadow-pos) downto 0
                                            when (not (loop-parser:loop-binding-is-parallel (nth i loop-bindings)))
                                              return (1+ i))
                                      0))
                     (parallel-group (subseq loop-bindings group-start (1+ shadow-pos)))
                     (searchable-forms (mapcan (lambda (lb) (copy-list (loop-parser:loop-binding-init-form lb)))
                                               parallel-group)))
                (push (make-shadow-info
                       :form expr
                       :type :partial-loop-and
                       :searchable searchable-forms)
                      *shadows*))
              ;; Sequential binding
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

(defun handle-quasiquote-shadow (rest-args target-name)
  "Handle QUASIQUOTE shadow detection - search only unquoted parts."
  (labels ((search-quasi (expr)
             (when (consp expr)
               (let ((h (first expr)))
                 (cond
                   ((or (eq h 'eclector.reader:unquote)
                        (and (symbolp h)
                             (string-equal (symbol-name h) "UNQUOTE")
                             (string-equal (package-name (symbol-package h)) "ECLECTOR.READER")))
                    (when (rest expr)
                      (find-shadows-in-expr (second expr) target-name)))
                   ((or (eq h 'eclector.reader:unquote-splicing)
                        (and (symbolp h)
                             (string-equal (symbol-name h) "UNQUOTE-SPLICING")
                             (string-equal (package-name (symbol-package h)) "ECLECTOR.READER")))
                    (when (rest expr)
                      (find-shadows-in-expr (second expr) target-name)))
                   (t
                    (when (consp (car expr))
                      (search-quasi (car expr)))
                    (when (consp (cdr expr))
                      (search-quasi (cdr expr)))))))))
    (dolist (arg rest-args)
      (search-quasi arg))))


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

        ;; LET
        ((utils:form-head-matches-p head "LET")
         (handle-let-shadow expr rest-args target-name))

        ;; LET*
        ((utils:form-head-matches-p head "LET*")
         (handle-let*-shadow expr rest-args target-name))

        ;; DEFUN/LAMBDA/DEFMACRO
        ((or (utils:form-head-matches-p head "DEFUN")
             (utils:form-head-matches-p head "LAMBDA")
             (utils:form-head-matches-p head "DEFMACRO"))
         (handle-lambda-shadow expr head rest-args target-name))

        ;; DESTRUCTURING-BIND, MULTIPLE-VALUE-BIND
        ((or (utils:form-head-matches-p head "DESTRUCTURING-BIND")
             (utils:form-head-matches-p head "MULTIPLE-VALUE-BIND"))
         (handle-destructuring-bind-shadow expr rest-args target-name))

        ;; DOLIST, DOTIMES
        ((or (utils:form-head-matches-p head "DOLIST")
             (utils:form-head-matches-p head "DOTIMES"))
         (handle-dolist-shadow expr rest-args target-name))

        ;; DO
        ((utils:form-head-matches-p head "DO")
         (handle-do-shadow expr rest-args target-name))

        ;; DO*
        ((utils:form-head-matches-p head "DO*")
         (handle-do-shadow expr rest-args target-name))

        ;; LOOP
        ((utils:form-head-matches-p head "LOOP")
         (handle-loop-shadow expr rest-args target-name))

        ;; QUASIQUOTE
        ((or (eq head 'eclector.reader:quasiquote)
             (and (symbolp head)
                  (string-equal (symbol-name head) "QUASIQUOTE")
                  (string-equal (package-name (symbol-package head)) "ECLECTOR.READER")))
         (handle-quasiquote-shadow rest-args target-name))

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
  (let ((target-name (base:symbol-name-from-string var-name))
        (*parsing-context* :known-body))  ; Track parsing context: :known-body or :unknown
    (labels ((matches-shadow-p (expr)
               "Check if EXPR matches any shadow in the list, return shadow-info if found."
               (find expr shadows :key #'shadow-info-form :test #'eq))

             (known-function-p (head)
               "Check if HEAD is a known function (not a macro) in the CL package.
Returns T if it's a function we can apply Lisp-2 semantics to safely.
Assumes CURRENT package imports CL symbols (standard convention)."
               (when (stringp head)
                 ;; Check if symbol has a package prefix
                 (let* ((colon-pos (or (search "::" head) (position #\: head)))
                        (package-name (when colon-pos (subseq head 0 colon-pos)))
                        (symbol-name (if colon-pos
                                         (subseq head (+ colon-pos (if (search "::" head) 2 1)))
                                         head)))
                   ;; Only check CL functions if:
                   ;; 1. No package prefix (unqualified), OR
                   ;; 2. Package is explicitly CL or COMMON-LISP, OR
                   ;; 3. Package is CURRENT (assumes it imports CL)
                   (when (or (null package-name)
                             (member package-name '("CL" "COMMON-LISP" "CURRENT") :test #'string-equal))
                     ;; Check if symbol exists in CL package and is a function (not a macro)
                     (let ((symbol (find-symbol (string-upcase symbol-name) (find-package "COMMON-LISP"))))
                       (and symbol
                            (fboundp symbol)
                            (not (macro-function symbol))))))))

             (known-special-operator-p (head)
               "Check if HEAD is a known CL special operator or standard macro.
These forms restore :known-body context even when inside unknown macros."
               (when (stringp head)
                 ;; Check if symbol has a package prefix
                 (let* ((colon-pos (or (search "::" head) (position #\: head)))
                        (package-name (when colon-pos (subseq head 0 colon-pos)))
                        (symbol-name (if colon-pos
                                         (subseq head (+ colon-pos (if (search "::" head) 2 1)))
                                         head)))
                   ;; Only check CL forms
                   (when (or (null package-name)
                             (member package-name '("CL" "COMMON-LISP" "CURRENT") :test #'string-equal))
                     (let ((symbol (find-symbol (string-upcase symbol-name) (find-package "COMMON-LISP"))))
                       (and symbol
                            (or (special-operator-p symbol)
                                (macro-function symbol))))))))

             (search-clause (clause clause-type)
               "Search CLAUSE respecting its first element type."
               (when (consp clause)
                 (let ((first-elem (first clause))
                       (body-forms (rest clause)))
                   (or
                    (case clause-type
                      (:expression
                       ;; Expression position should check CAR even if it matches a CL function
                       (let ((*parsing-context* :unknown))
                         (search-expr first-elem nil)))
                      (:literal nil)
                      (otherwise nil))
                    (let ((*parsing-context* :known-body))
                      (some (lambda (form) (search-expr form nil)) body-forms))))))

             (search-expr (expr &optional in-function-position)
               "Recursively search for references to var-name in expr.
IN-FUNCTION-POSITION is true if we're looking at the first element of a form (function call position)."
               (cond
                 ((null expr) nil)

                 ;; String matching our variable is a reference
                 ;; In known context: Skip if in function position (Lisp-2)
                 ;; In unknown context: Check even in function position (conservative)
                 ((stringp expr)
                  (and (or (not in-function-position)
                           (eq *parsing-context* :unknown))  ; In unknown context, check CAR too
                       (string-equal (base:symbol-name-from-string expr) target-name)))

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
                          ;; Known function in known context - apply Lisp-2
                          ((and (eq *parsing-context* :known-body)
                                (known-function-p (first expr)))
                           (or (search-expr (car expr) t)  ; Skip CAR (function position)
                               (some (lambda (arg) (search-expr arg nil))
                                     (cdr expr))))

                          ;; Known special operator/macro - restore :known-body context
                          ((known-special-operator-p (first expr))
                           ;; Check if it has a specific body position signature
                           (let ((body-info (known-macro-body-position (first expr))))
                             (cond
                               ;; Clause-based macro
                               ((and (consp body-info) (eq (first body-info) :clauses))
                                (let* ((clause-pos (second body-info))
                                       (macro-name (first expr))
                                       (clause-type (or (get-clause-element-type macro-name) :expression))
                                       (all-args (rest expr))
                                       (args (subseq all-args 0 (min clause-pos (length all-args))))
                                       (clauses (when (< clause-pos (length all-args))
                                                  (nthcdr clause-pos all-args))))
                                  (or (let ((*parsing-context* :unknown))
                                        (some (lambda (arg) (search-expr arg nil)) args))
                                      (some (lambda (clause)
                                              (search-clause clause clause-type))
                                            clauses))))
                               ;; Simple body position
                               ((integerp body-info)
                                (let* ((all-args (rest expr))
                                       (args (subseq all-args 0 (min body-info (length all-args))))
                                       (body-forms (when (> (length all-args) body-info)
                                                     (subseq all-args body-info))))
                                  ;; Search macro arguments with :unknown context
                                  ;; Search body forms with :known-body context
                                  (or (let ((*parsing-context* :unknown))
                                        (some (lambda (arg) (search-expr arg nil)) args))
                                      (let ((*parsing-context* :known-body))
                                        (some (lambda (form) (search-expr form nil)) body-forms)))))
                               ;; No signature - just restore :known-body context for all args
                               (t
                                (let ((*parsing-context* :known-body))
                                  (some (lambda (arg) (search-expr arg nil)) (cdr expr)))))))

                          ;; Unknown form OR in unknown context - be conservative
                          (t
                           (let ((*parsing-context* :unknown))  ; Enter/stay in unknown
                             (or (search-expr (car expr) nil)   ; Check CAR too
                                 (some (lambda (arg) (search-expr arg nil))
                                       (cdr expr)))))))))

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
                 ;; Cons - search both parts (including dotted pairs)
                 ((consp form)
                  (or (search-binding (car form))
                      (search-binding (cdr form))))
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
          (when (utils:debug-mode-p)
            (format *error-output* "~%Warning: Skipping LET form at ~A:~A due to unexpected structure:~%  ~A~%"
                    *file* line e))
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

(defun check-handler-case-bindings (expr line column position-map rule)
  "Check HANDLER-CASE for unused variable bindings in error handler clauses.
Each clause has the structure: (condition-type lambda-list &body body)
Special case: :no-error clause also has lambda-list with potential bindings."
  (let ((rest-args (rest expr)))
    (when (utils:proper-list-of-min-length-p rest-args 1)
      ;; Skip the first form (protected form), then process each error handler clause
      (dolist (clause (rest rest-args))
        (when (and (a:proper-list-p clause)
                   (utils:proper-list-of-min-length-p clause 2))
          (let* ((lambda-list (second clause))
                 (body (cddr clause)))
            ;; Lambda-list should be a list of parameters
            (when (a:proper-list-p lambda-list)
              (let ((bindings (mapcar #'list lambda-list)))
                (scope:with-new-scope
                    (check-binding-form :let bindings body line column position-map rule))))))))))

(defun check-restart-case-bindings (expr line column position-map rule)
  "Check RESTART-CASE for unused variable bindings in restart handler clauses.
Each clause has the structure: (restart-name lambda-list &body body)"
  (let ((rest-args (rest expr)))
    (when (utils:proper-list-of-min-length-p rest-args 1)
      ;; Skip the first form (restartable form), then process each restart clause
      (dolist (clause (rest rest-args))
        (when (and (a:proper-list-p clause)
                   (utils:proper-list-of-min-length-p clause 2))
          (let* ((lambda-list (second clause))
                 (body (cddr clause)))
            ;; Lambda-list should be a list of parameters
            (when (a:proper-list-p lambda-list)
              (let ((bindings (mapcar #'list lambda-list)))
                (scope:with-new-scope
                    (check-binding-form :let bindings body line column position-map rule))))))))))

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

(defun dispatch-binding-form-check (head expr line column position-map rule)
  "Dispatch to specific checker for complex binding forms.
Returns T if a binding form was checked, NIL otherwise."
  (cond
    ((base:symbol-matches-p head "DEFUN")
     (check-defun-bindings expr line column position-map rule)
     t)
    ((base:symbol-matches-p head "LAMBDA")
     (check-lambda-bindings expr line column position-map rule)
     t)
    ((and (stringp head) (string-equal (base:symbol-name-from-string head) "LET"))
     (check-let-bindings expr line column position-map rule)
     t)
    ((and (stringp head) (string-equal (base:symbol-name-from-string head) "LET*"))
     (check-let*-bindings expr line column position-map rule)
     t)
    ((base:symbol-matches-p head "DO")
     (check-do-bindings expr line column position-map rule)
     t)
    ((base:symbol-matches-p head "DO*")
     (check-do*-bindings expr line column position-map rule)
     t)
    ((base:symbol-matches-p head "DESTRUCTURING-BIND")
     (check-destructuring-bind-bindings expr line column position-map rule)
     t)
    ((base:symbol-matches-p head "MULTIPLE-VALUE-BIND")
     (check-multiple-value-bind-bindings expr line column position-map rule)
     t)
    ((base:symbol-matches-p head "HANDLER-CASE")
     (check-handler-case-bindings expr line column position-map rule)
     t)
    ((base:symbol-matches-p head "RESTART-CASE")
     (check-restart-case-bindings expr line column position-map rule)
     t)
    ((base:symbol-matches-p head "DEFMACRO")
     (check-defmacro-bindings expr line column position-map rule)
     t)
    (t nil)))

(defun should-skip-form-p (head)
  "Check if HEAD represents a form that should be skipped entirely for variable checking."
  (or
   ;; QUOTE - skip entirely (pure data)
   (eq head 'cl:quote)
   (eq head 'quote)
   (base:symbol-matches-p head "QUOTE")
   ;; Special definition forms with their own syntax
   (base:symbol-matches-p head "DEFSTRUCT")
   (base:symbol-matches-p head "DEFCLASS")
   (base:symbol-matches-p head "DEFPACKAGE")
   (base:symbol-matches-p head "DEFTYPE")
   (base:symbol-matches-p head "DEFSETF")
   (base:symbol-matches-p head "DEFINE-MODIFY-MACRO")
   (base:symbol-matches-p head "DEFINE-SETF-EXPANDER")))

(defun quasiquote-form-p (head)
  "Check if HEAD represents a quasiquote form."
  (or (eq head 'eclector.reader:quasiquote)
      (and (symbolp head)
           (string-equal (symbol-name head) "QUASIQUOTE")
           (string-equal (package-name (symbol-package head)) "ECLECTOR.READER"))))

(defun check-quasiquote-expr (rest-args line column position-map rule)
  "Check quasiquoted expression, only descending into unquotes."
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

(defun check-expr (expr line column position-map rule)
  "Recursively check expression for unused variables."
  (when (consp expr)
    (let ((head (first expr))
          (rest-args (rest expr)))
      ;; 1. Try generic checker first for simple binding forms
      ;; The generic checker handles: WITH-*, DOLIST, DOTIMES, DO-SYMBOLS family
      ;; Returns T if it handled the form, NIL otherwise
      (unless (check-binding-form-generic expr line column position-map rule)
        ;; 2. Dispatch to specific checkers for complex binding forms
        (dispatch-binding-form-check head expr line column position-map rule))

      ;; 3. Handle special forms for recursion
      (cond
        ;; Forms to skip entirely
        ((should-skip-form-p head)
         nil)

        ;; QUASIQUOTE - only check unquoted parts
        ((quasiquote-form-p head)
         (check-quasiquote-expr rest-args line column position-map rule))

        ;; Default: recursively check all nested forms
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


;;; Needless LET* rule

(defun let*-analyzable-binding-p (binding)
  "Check if BINDING is a simple LET* binding with a string variable."
  (or (stringp binding)
      (and (consp binding)
           (stringp (first binding)))))

(defun let*-binding-init-form (binding)
  "Extract init form from LET* BINDING if present.
Returns NIL when no init form exists."
  (when (and (consp binding)
             (consp (cdr binding)))
    (second binding)))

(defun let*-binding-depends-on-bindings-p (binding previous-bindings)
  "Check if BINDING's init form references any variables in PREVIOUS-BINDINGS."
  (let ((init-form (let*-binding-init-form binding)))
    (when init-form
      (loop for previous-binding in previous-bindings
            for vars = (extract-bindings previous-binding)
            thereis (some (lambda (prev-var)
                            (find-references prev-var (list init-form)))
                          vars)))))

(defun needless-let*-bindings-p (bindings)
  "Return T when LET* BINDINGS are independent (no sequential dependencies).
Returns NIL for empty bindings, unknown binding shapes, or when dependencies exist."
  (when (and (listp bindings)
             (a:proper-list-p bindings))
    (let ((seen '())
          (count 0))
      (dolist (binding bindings)
        (incf count)
        (unless (let*-analyzable-binding-p binding)
          (return-from needless-let*-bindings-p nil))
        (when (let*-binding-depends-on-bindings-p binding seen)
          (return-from needless-let*-bindings-p nil))
        (push binding seen))
      (when (> count 0) t))))

(defclass needless-let*-rule (base:rule)
  ()
  (:default-initargs
   :name :needless-let*
   :description "Use 'let' instead of 'let*' when bindings don't depend on each other"
   :severity :info
   :type :form)
  (:documentation "Rule to detect LET* forms whose bindings are independent."))

(defmethod base:check-form ((rule needless-let*-rule) form file)
  "Check for LET* forms that can be rewritten as LET."
  (check-type form parser:form)
  (check-type file pathname)

  (base:check-form-recursive rule
                             (parser:form-expr form)
                             file
                             (parser:form-line form)
                             (parser:form-column form)
                             nil
                             (parser:form-position-map form)))

(defmethod base:check-form-recursive ((rule needless-let*-rule) expr file line column
                                      &optional function-name position-map)
  (declare (ignore function-name))

  (let ((violations '())
        (visited (make-hash-table :test 'eq)))

    (labels ((check-expr (current-expr fallback-line fallback-column)
               (base:with-safe-code-expr (current-expr visited)
                 (multiple-value-bind (actual-line actual-column)
                     (base:find-actual-position current-expr position-map fallback-line fallback-column)
                   (let ((head (first current-expr))
                         (rest-args (rest current-expr)))
                     (when (base:symbol-matches-p head "LET*")
                       (let ((bindings (first rest-args)))
                         (when (and (needless-let*-bindings-p bindings)
                                    (base:should-create-violation-p rule))
                           (push (make-instance 'violation:violation
                                                :rule :needless-let*
                                                :file file
                                                :line actual-line
                                                :column actual-column
                                                :severity (base:rule-severity rule)
                                                :message "Use 'let' instead of 'let*' when bindings don't depend on each other"
                                                :fix nil)
                                 violations))))
                     (a:nconcf violations
                               (base:collect-violations-from-subexprs rule head file
                                                                      actual-line actual-column
                                                                      position-map))
                     (a:nconcf violations
                               (base:collect-violations-from-subexprs rule rest-args file
                                                                      actual-line actual-column
                                                                      position-map)))))))

      (check-expr expr line column))

    violations))


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
                         (when (utils:debug-mode-p)
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
                         (when (utils:debug-mode-p)
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
