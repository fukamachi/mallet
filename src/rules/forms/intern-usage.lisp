(defpackage #:mallet/rules/forms/intern-usage
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:mallet/rules/base)
   (#:utils #:mallet/utils)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation))
  (:export #:*prohibited-intern-functions*
           #:prohibited-intern-function-p
           #:package-context
           #:make-package-context
           #:package-context-local-nicknames
           #:package-context-imported-symbols
           #:build-package-context
           #:resolve-intern-usage
           #:intern-usage-rule))
(in-package #:mallet/rules/forms/intern-usage)

;;; Target Function Registry
;;;
;;; The six prohibited symbol-interning functions with their owning packages.
;;; Each entry stores a :packages list because a library's canonical package name
;;; may differ from its nickname — e.g. UIOP's actual package is "UIOP/DRIVER".

(defparameter *prohibited-intern-functions*
  '((:name "INTERN"          :packages ("COMMON-LISP" "CL")               :display "cl:intern")
    (:name "UNINTERN"        :packages ("COMMON-LISP" "CL")               :display "cl:unintern")
    (:name "INTERN*"         :packages ("UIOP" "UIOP/DRIVER" "UIOP/UTILITY") :display "uiop:intern*")
    (:name "SYMBOLICATE"     :packages ("ALEXANDRIA")                     :display "alexandria:symbolicate")
    (:name "FORMAT-SYMBOL"   :packages ("ALEXANDRIA")                     :display "alexandria:format-symbol")
    (:name "MAKE-KEYWORD"    :packages ("ALEXANDRIA")                     :display "alexandria:make-keyword"))
  "List of prohibited symbol-interning functions.
Each entry: :name (uppercase symbol name), :packages (list of full package names),
:display (human-readable canonical form).")

(defun extract-package-from-symbol-string (str)
  "Extract the package prefix from a symbol string like \"PKG:NAME\" or \"PKG::NAME\".
Returns NIL if the string has no colon (unqualified symbol).
Uses the first colon position, so double-colon access \"PKG::NAME\" returns \"PKG\"."
  (let ((colon-pos (position #\: str)))
    (when colon-pos
      (subseq str 0 colon-pos))))

;;; Package Context
;;;
;;; A package context holds the local-nickname and import-from mappings extracted
;;; from the file's defpackage form.  It is used by RESOLVE-INTERN-USAGE to
;;; resolve nickname-prefixed and imported symbols to their canonical packages.

(defstruct (package-context (:conc-name package-context-))
  "Context extracted from a file's defpackage form for resolving symbol packages.

LOCAL-NICKNAMES maps NICKNAME (uppercase string) to CANONICAL-PACKAGE (uppercase).
IMPORTED-SYMBOLS maps SYMBOL-NAME (uppercase string) to SOURCE-PACKAGE (uppercase)."
  (local-nicknames  (make-hash-table :test 'equalp))
  (imported-symbols (make-hash-table :test 'equalp)))

(defun defpackage-form-p* (expr)
  "Return T if EXPR is a defpackage or define-package form."
  (and (consp expr)
       (stringp (first expr))
       (let ((name (utils:symbol-name-from-string (first expr))))
         (or (string-equal name "DEFPACKAGE")
             (string-equal name "DEFINE-PACKAGE")))))

(defun populate-context-from-defpackage (context defpackage-expr)
  "Populate CONTEXT from clauses in DEFPACKAGE-EXPR."
  (dolist (clause (cddr defpackage-expr))
    (when (and (consp clause) (stringp (first clause)))
      (let ((keyword (first clause)))
        (cond
          ;; (:local-nicknames (#:a #:alexandria) ...)
          ((string-equal keyword ":local-nicknames")
           (dolist (pair (rest clause))
             (when (and (consp pair) (>= (length pair) 2))
               (let ((nick (string-upcase (utils:symbol-name-from-string (first pair))))
                     (pkg  (string-upcase (utils:symbol-name-from-string (second pair)))))
                 (setf (gethash nick (package-context-local-nicknames context)) pkg)))))
          ;; (:import-from #:uiop #:intern* ...)
          ((string-equal keyword ":import-from")
           (when (>= (length clause) 2)
             (let ((pkg (string-upcase (utils:symbol-name-from-string (second clause)))))
               (dolist (sym-str (cddr clause))
                 (let ((sym (string-upcase (utils:symbol-name-from-string sym-str))))
                   (setf (gethash sym (package-context-imported-symbols context)) pkg)))))))))))

(defun build-package-context (forms)
  "Build a PACKAGE-CONTEXT from FORMS (a list of PARSER:FORM objects).

Scans the forms for DEFPACKAGE or DEFINE-PACKAGE forms and extracts
local-nickname and import-from mappings into the returned context."
  (let ((context (make-package-context)))
    (dolist (form forms)
      (let ((expr (parser:form-expr form)))
        (when (defpackage-form-p* expr)
          (populate-context-from-defpackage context expr))))
    context))

(defun prohibited-intern-function-p (symbol-string)
  "Return the display name if SYMBOL-STRING refers to a prohibited intern function, NIL otherwise.

SYMBOL-STRING is a string representing a symbol as produced by the mallet parser:
- Qualified:   \"PACKAGE:name\" (package name is the full CL package name)
- Unqualified: \"name\"         (no colon prefix)

Matching rules:
- Qualified with a known package: matches by package AND name (avoids false positives).
- Unqualified (no colon):         matches by name only (conservative — flags likely usage).
- Qualified with an unknown package: no match."
  (unless (stringp symbol-string)
    (return-from prohibited-intern-function-p nil))
  (let ((name (utils:symbol-name-from-string symbol-string))
        (package (extract-package-from-symbol-string symbol-string)))
    (dolist (entry *prohibited-intern-functions*)
      (when (string-equal name (getf entry :name))
        (when (or (null package)
                  (member package (getf entry :packages) :test #'string-equal))
          (return-from prohibited-intern-function-p (getf entry :display)))))
    nil))

(defun resolve-intern-usage (symbol-string context)
  "Return the display name if SYMBOL-STRING refers to a prohibited intern function
given package CONTEXT, NIL otherwise.

SYMBOL-STRING is a parser-produced symbol string (qualified or unqualified).
CONTEXT is a PACKAGE-CONTEXT built by BUILD-PACKAGE-CONTEXT.

Resolution order:
1. If SYMBOL-STRING has a package prefix that is a local nickname in CONTEXT,
   resolve to the canonical package and check against the registry.
2. If SYMBOL-STRING has the CURRENT: prefix (parser's current-package marker),
   check import-from mappings; flag only if explicitly imported from a prohibited package.
   No match if the name is not in imports (avoids false positives for user-defined fns).
3. If SYMBOL-STRING has any other package prefix, delegate to PROHIBITED-INTERN-FUNCTION-P
   (handles known canonical packages like COMMON-LISP:, UIOP:, ALEXANDRIA:).
4. If SYMBOL-STRING has no package prefix, check import-from mappings only; NIL if not imported."
  (unless (stringp symbol-string)
    (return-from resolve-intern-usage nil))
  (unless context
    (return-from resolve-intern-usage nil))
  (let* ((name    (string-upcase (utils:symbol-name-from-string symbol-string)))
         (prefix  (extract-package-from-symbol-string symbol-string)))
    (if prefix
        ;; Has a package prefix
        (let* ((prefix-upper (string-upcase prefix))
               (canonical-pkg (gethash prefix-upper
                                       (package-context-local-nicknames context))))
          (cond
            (canonical-pkg
             ;; Prefix is a local nickname → resolve to canonical package
             (prohibited-intern-function-p
              (concatenate 'string canonical-pkg ":" name)))
            ;; "CURRENT:" prefix means the current package — only flag if explicitly imported
            ((string= prefix-upper "CURRENT")
             (let ((imported-pkg (gethash name (package-context-imported-symbols context))))
               (when imported-pkg
                 (prohibited-intern-function-p
                  (concatenate 'string imported-pkg ":" name)))))
            (t
             ;; Not a nickname → try direct check (handles COMMON-LISP:, UIOP:, etc.)
             (prohibited-intern-function-p symbol-string))))
        ;; No package prefix — only flag if explicitly imported
        (let ((imported-pkg (gethash name
                                     (package-context-imported-symbols context))))
          (when imported-pkg
            (prohibited-intern-function-p
             (concatenate 'string imported-pkg ":" name)))))))

;;; Intern-Usage Rule Class

(defvar *intern-check-context* nil
  "Dynamic variable: the current file's PACKAGE-CONTEXT during intern-usage checking.")

(defclass intern-usage-rule (base:rule)
  ((context-cache
    :initform nil
    :accessor intern-usage-rule-context-cache
    :documentation "Cons (file . context) to avoid re-reading the file per form."))
  (:default-initargs
   :name :intern-usage
   :description "Avoid runtime use of symbol-interning functions"
   :severity :warning
   :type :form)
  (:documentation "Rule to detect runtime use of cl:intern, cl:unintern, uiop:intern*,
alexandria:symbolicate, alexandria:format-symbol, and alexandria:make-keyword."))

(defun form-head-name-matches-p (head name)
  "Return T if HEAD (parser string or reader-macro interned symbol) matches NAME."
  (typecase head
    (string (string-equal (utils:symbol-name-from-string head) name))
    (symbol (string-equal (symbol-name head) name))
    (otherwise nil)))

(defun intern-callee-p (expr context)
  "Return the display name if EXPR is a prohibited intern function reference, NIL otherwise.
Handles: bare symbol string, already-interned CL symbol, #'func (FUNCTION form),
'func (QUOTE form)."
  (cond
    ;; Bare symbol: "COMMON-LISP:intern"
    ((stringp expr)
     (resolve-intern-usage expr context))
    ;; Already-interned Lisp symbol head (e.g. CL:INTERN as a symbol object).
    ;; Build a "PKG:name" string from the symbol's package and name, then check.
    ((symbolp expr)
     (let* ((pkg (symbol-package expr))
            (pkg-name (if pkg (package-name pkg) nil))
            (sym-name (symbol-name expr))
            (sym-string (if pkg-name
                            (concatenate 'string pkg-name ":" sym-name)
                            sym-name)))
       (resolve-intern-usage sym-string context)))
    ;; #'func → (FUNCTION "PKG:func") where FUNCTION may be interned CL symbol
    ((and (consp expr)
          (form-head-name-matches-p (first expr) "FUNCTION")
          (stringp (second expr)))
     (resolve-intern-usage (second expr) context))
    ;; 'func → (QUOTE "PKG:func") where QUOTE may be interned CL symbol
    ((and (consp expr)
          (form-head-name-matches-p (first expr) "QUOTE")
          (stringp (second expr)))
     (resolve-intern-usage (second expr) context))
    (t nil)))

(defun eval-when-has-execute-p (expr)
  "Return T if EVAL-WHEN EXPR has :execute in its situation list."
  (when (and (consp expr) (consp (second expr)))
    (some (lambda (s)
            (and (stringp s)
                 (string-equal (utils:symbol-name-from-string s) "EXECUTE")))
          (second expr))))

(defmethod base:check-form ((rule intern-usage-rule) form file)
  "Check FORM from FILE for prohibited intern function usage."
  (check-type form parser:form)
  (check-type file pathname)
  ;; Build (or reuse cached) package context for this file.
  (let* ((cache (intern-usage-rule-context-cache rule))
         (context (if (and cache (equal (car cache) file))
                      (cdr cache)
                      (let ((ctx (handler-case
                                     (let* ((text (uiop:read-file-string file))
                                            (all-forms (parser:parse-forms text file)))
                                       (build-package-context all-forms))
                                   (error ()
                                     (make-package-context)))))
                        (setf (intern-usage-rule-context-cache rule) (cons file ctx))
                        ctx))))
    (let ((*intern-check-context* context))
      (base:check-form-recursive rule
                                 (parser:form-expr form)
                                 file
                                 (parser:form-line form)
                                 (parser:form-column form)
                                 nil
                                 (parser:form-position-map form)))))

(defmethod base:check-form-recursive
    ((rule intern-usage-rule) expr file line column &optional function-name position-map)
  "Recursively check EXPR for prohibited intern function usage."
  (declare (ignore function-name))
  (let ((context *intern-check-context*)
        (violations '())
        (visited (make-hash-table :test 'eq)))

    (labels ((make-intern-violation (vline vcol pattern-desc)
               (make-instance 'violation:violation
                              :rule :intern-usage
                              :file file
                              :line vline
                              :column vcol
                              :severity (base:rule-severity rule)
                              :message (format nil "Avoid using ~A at runtime" pattern-desc)
                              :fix nil))

             (check-expr (current-expr fallback-line fallback-column)
               (base:with-safe-code-expr (current-expr visited)
                 (multiple-value-bind (actual-line actual-column)
                     (base:find-actual-position
                      current-expr position-map fallback-line fallback-column)
                   (let ((head (first current-expr))
                         (rest-args (rest current-expr)))
                     (cond
                       ;; DEFMACRO: skip entirely (macro expansion code, not runtime)
                       ((form-head-name-matches-p head "DEFMACRO")
                        nil)

                       ;; EVAL-WHEN: only recurse into body if :execute is present
                       ((form-head-name-matches-p head "EVAL-WHEN")
                        (when (eval-when-has-execute-p current-expr)
                          (a:nconcf violations
                                    (base:collect-violations-from-subexprs
                                     rule (cddr current-expr) file
                                     actual-line actual-column position-map))))

                       (t
                        ;; Direct call: (intern ...) / (a:symbolicate ...) etc.
                        ;; Use intern-callee-p so that already-interned CL symbol heads
                        ;; (e.g. the symbol CL:INTERN rather than the string "CL:INTERN")
                        ;; are also detected, not just string heads.
                        (let ((display (intern-callee-p head context)))
                          (when (and display (base:should-create-violation-p rule))
                            (push (make-intern-violation actual-line actual-column display)
                                  violations)))

                        ;; (funcall #'intern ...) or (funcall 'intern ...)
                        (when (and (form-head-name-matches-p head "FUNCALL")
                                   (consp rest-args))
                          (let ((display (intern-callee-p (first rest-args) context)))
                            (when (and display (base:should-create-violation-p rule))
                              (push (make-intern-violation
                                     actual-line actual-column
                                     (format nil "~A via funcall" display))
                                    violations))))

                        ;; (apply #'intern ...) or (apply 'intern ...)
                        (when (and (form-head-name-matches-p head "APPLY")
                                   (consp rest-args))
                          (let ((display (intern-callee-p (first rest-args) context)))
                            (when (and display (base:should-create-violation-p rule))
                              (push (make-intern-violation
                                     actual-line actual-column
                                     (format nil "~A via apply" display))
                                    violations))))

                        ;; Recurse into subexpressions
                        (a:nconcf violations
                                  (base:collect-violations-from-subexprs
                                   rule head file actual-line actual-column position-map))
                        (a:nconcf violations
                                  (base:collect-violations-from-subexprs
                                   rule rest-args file actual-line actual-column
                                   position-map)))))))))

      (check-expr expr line column))

    violations))
