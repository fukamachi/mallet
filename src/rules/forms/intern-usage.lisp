(defpackage #:mallet/rules/forms/intern-usage
  (:use #:cl)
  (:local-nicknames
   (#:utils #:mallet/utils)
   (#:parser #:mallet/parser))
  (:export #:*prohibited-intern-functions*
           #:prohibited-intern-function-p
           #:package-context
           #:make-package-context
           #:package-context-local-nicknames
           #:package-context-imported-symbols
           #:build-package-context
           #:resolve-intern-usage))
(in-package #:mallet/rules/forms/intern-usage)

;;; Target Function Registry
;;;
;;; The six prohibited symbol-interning functions, identified by their canonical
;;; package-qualified names. Package names are stored as full package names (not
;;; nicknames) to match how the Eclector parser represents them.

(defparameter *prohibited-intern-functions*
  '((:name "INTERN"          :package "COMMON-LISP" :display "cl:intern")
    (:name "UNINTERN"        :package "COMMON-LISP" :display "cl:unintern")
    (:name "INTERN*"         :package "UIOP"        :display "uiop:intern*")
    (:name "SYMBOLICATE"     :package "ALEXANDRIA"  :display "alexandria:symbolicate")
    (:name "FORMAT-SYMBOL"   :package "ALEXANDRIA"  :display "alexandria:format-symbol")
    (:name "MAKE-KEYWORD"    :package "ALEXANDRIA"  :display "alexandria:make-keyword"))
  "List of prohibited symbol-interning functions with their canonical packages.
Each entry is a plist with :name (symbol name, uppercase), :package (full package name),
and :display (human-readable canonical form).")

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
                  (string-equal package (getf entry :package)))
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
2. If SYMBOL-STRING has a package prefix that is NOT a local nickname,
   delegate to PROHIBITED-INTERN-FUNCTION-P (handles known canonical packages).
3. If SYMBOL-STRING has no package prefix and the symbol name appears in the
   import-from mappings of CONTEXT, check with the mapped package.
4. If SYMBOL-STRING has no package prefix and is not in imports, fall back
   to PROHIBITED-INTERN-FUNCTION-P (name-only conservative match)."
  (unless (stringp symbol-string)
    (return-from resolve-intern-usage nil))
  (let* ((name    (string-upcase (utils:symbol-name-from-string symbol-string)))
         (prefix  (extract-package-from-symbol-string symbol-string)))
    (if prefix
        ;; Has a package prefix
        (let* ((prefix-upper (string-upcase prefix))
               (canonical-pkg (gethash prefix-upper
                                       (package-context-local-nicknames context))))
          (if canonical-pkg
              ;; Prefix is a local nickname → resolve to canonical package
              (prohibited-intern-function-p
               (concatenate 'string canonical-pkg ":" name))
              ;; Not a nickname → try direct check (handles COMMON-LISP:, UIOP:, etc.)
              (prohibited-intern-function-p symbol-string)))
        ;; No package prefix
        (let ((imported-pkg (gethash name
                                     (package-context-imported-symbols context))))
          (if imported-pkg
              ;; Symbol was explicitly imported → check with known source package
              (prohibited-intern-function-p
               (concatenate 'string imported-pkg ":" name))
              ;; Not in imports → name-only conservative fallback
              (prohibited-intern-function-p symbol-string))))))
