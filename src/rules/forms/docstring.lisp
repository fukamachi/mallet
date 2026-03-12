(defpackage #:mallet/rules/forms/docstring
  (:use #:cl)
  (:local-nicknames
   (#:base #:mallet/rules/base)
   (#:utils #:mallet/utils)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)
   (#:pkg-exports #:mallet/rules/forms/package-exports))
  (:export
   #:definition-form-p
   #:checkable-definition-p
   #:has-docstring-p
   #:definition-name
   #:missing-docstring-rule
   #:missing-exported-docstring-rule))
(in-package #:mallet/rules/forms/docstring)

;;; Docstring Detection Utilities
;;;
;;; Shared utilities for detecting docstrings across definition form types.
;;; CL spec: defun/defmacro docstring is the first string in body, ONLY IF more
;;; body forms follow (a single-string body is a return value, not a docstring).
;;; defgeneric and defclass use (:documentation "string") options instead.

;;; --- Form type predicates ---

(defun definition-form-p (expr)
  "Return T if EXPR is a top-level definition form.
Recognized forms: defun, defmacro, defgeneric, defclass, defmethod."
  (and (consp expr)
       (stringp (first expr))
       (or (base:symbol-matches-p (first expr) "DEFUN")
           (base:symbol-matches-p (first expr) "DEFMACRO")
           (base:symbol-matches-p (first expr) "DEFGENERIC")
           (base:symbol-matches-p (first expr) "DEFCLASS")
           (base:symbol-matches-p (first expr) "DEFMETHOD"))))

(defun checkable-definition-p (expr)
  "Return T if EXPR is a definition form that should be checked for docstrings.
Includes defun, defmacro, defgeneric, defclass.
Excludes defmethod (methods inherit docs from the generic function)."
  (and (consp expr)
       (stringp (first expr))
       (or (base:symbol-matches-p (first expr) "DEFUN")
           (base:symbol-matches-p (first expr) "DEFMACRO")
           (base:symbol-matches-p (first expr) "DEFGENERIC")
           (base:symbol-matches-p (first expr) "DEFCLASS"))))

(defun defun-or-defmacro-p (expr)
  "Return T if EXPR is a defun or defmacro form."
  (and (consp expr)
       (stringp (first expr))
       (or (base:symbol-matches-p (first expr) "DEFUN")
           (base:symbol-matches-p (first expr) "DEFMACRO"))))

(defun defgeneric-p (expr)
  "Return T if EXPR is a defgeneric form."
  (and (consp expr)
       (stringp (first expr))
       (base:symbol-matches-p (first expr) "DEFGENERIC")))

(defun defclass-p (expr)
  "Return T if EXPR is a defclass form."
  (and (consp expr)
       (stringp (first expr))
       (base:symbol-matches-p (first expr) "DEFCLASS")))

(defun defmethod-p (expr)
  "Return T if EXPR is a defmethod form."
  (and (consp expr)
       (stringp (first expr))
       (base:symbol-matches-p (first expr) "DEFMETHOD")))

;;; --- Docstring detection ---

(defun documentation-option-p (option)
  "Return T if OPTION is a (:documentation \"string\") form."
  (and (consp option)
       (stringp (first option))
       (base:symbol-matches-p (first option) "DOCUMENTATION")
       (consp (rest option))
       (stringp (second option))))

(defun defun-body-has-docstring-p (body)
  "Return T if BODY (forms after lambda list in defun/defmacro) starts with a docstring.
Per the CL spec, a string is only a docstring when additional body forms follow it.
Note: The parser represents unqualified symbols as plain strings (no colon), so a bare
symbol as the first body form (e.g., a variable reference) will pass (stringp ...) here.
This is an accepted limitation: such patterns are rare in practice and the rule layer
should not flag something it cannot confirm is truly missing a docstring."
  (and (consp body)
       (stringp (first body))
       (consp (rest body))))

(defun options-have-documentation-p (options)
  "Return T if OPTIONS list contains a (:documentation \"string\") entry."
  (loop for option in options
        when (documentation-option-p option)
          return t))

(defun has-docstring-p (expr)
  "Return T if EXPR (a definition form) has a docstring.
For defun/defmacro: first form after lambda list is a string AND more body forms follow.
For defgeneric/defclass: body options contain (:documentation \"string\").
For defmethod: always returns T (not checked).
Returns NIL for non-definition forms or definitions without docstrings."
  (cond
    ((defmethod-p expr) t)
    ((defun-or-defmacro-p expr)
     (defun-body-has-docstring-p (cdddr expr)))
    ((defgeneric-p expr)
     (options-have-documentation-p (cdddr expr)))
    ((defclass-p expr)
     (options-have-documentation-p (cddddr expr)))
    (t nil)))

;;; --- Name extraction ---

(defun definition-name (expr)
  "Return the name of the entity defined by EXPR as a string.
Works for defun, defmacro, defgeneric, defclass, defmethod forms.
Returns NIL if the name cannot be extracted.
For setf functions like (defun (setf foo) ...), returns \"(setf foo)\"."
  (when (and (consp expr) (consp (cdr expr)))
    (let ((name (second expr)))
      (cond
        ((stringp name)
         (utils:symbol-name-from-string name))
        ((and (consp name) (= (length name) 2)
              (stringp (first name))
              (base:symbol-matches-p (first name) "SETF")
              (stringp (second name)))
         (format nil "(setf ~A)" (utils:symbol-name-from-string (second name))))
        (t nil)))))

;;; --- missing-docstring Rule ---

(defclass missing-docstring-rule (base:rule)
  ()
  (:default-initargs
   :name :missing-docstring
   :description "Top-level definitions should have docstrings"
   :severity :info
   :category :style
   :type :form)
  (:documentation "Rule to detect top-level definitions lacking docstrings.
Checks defun, defmacro, defgeneric, and defclass forms. Skips defmethod,
which inherits its documentation from the generic function."))

(defun form-type-string (expr)
  "Return uppercase form type name for EXPR (e.g., \"DEFUN\", \"DEFCLASS\")."
  (when (and (consp expr) (stringp (first expr)))
    (string-upcase (utils:symbol-name-from-string (first expr)))))

(defmethod base:check-form ((rule missing-docstring-rule) form file)
  "Check FORM for missing docstring on a top-level definition."
  (check-type form parser:form)
  (check-type file pathname)
  (let ((expr (parser:form-expr form)))
    (when (and (checkable-definition-p expr)
               (not (has-docstring-p expr))
               (base:should-create-violation-p rule))
      (let ((form-type (form-type-string expr))
            (name (definition-name expr)))
        (when (and form-type name)
          (list (make-instance 'violation:violation
                               :rule (base:rule-name rule)
                               :file file
                               :line (parser:form-line form)
                               :column (parser:form-column form)
                               :severity (base:rule-severity rule)
                               :message (format nil "~A ~A is missing a docstring"
                                                form-type name)
                               :fix nil)))))))

;;; --- missing-exported-docstring-rule ---

(defun in-package-form-p (expr)
  "Return T if EXPR is an (in-package ...) form."
  (and (consp expr)
       (stringp (first expr))
       (base:symbol-matches-p (first expr) "IN-PACKAGE")))

(defun export-lookup-name (name)
  "Return the symbol name to use for export lookup from a definition name.
For setf names like \"(setf foo)\", returns \"foo\".
For plain names, returns NAME unchanged."
  (cond
    ((null name) nil)
    ((and (> (length name) 7)
          (string-equal (subseq name 0 6) "(setf "))
     (string-right-trim ")" (subseq name 6)))
    (t name)))

(defclass missing-exported-docstring-rule (base:rule)
  ((current-file
    :initform nil
    :accessor rule-current-file
    :documentation "Last file seen, for detecting file transitions.")
   (current-package
    :initform nil
    :accessor rule-current-package
    :documentation "Package name from the most recent in-package form.")
   (cached-project-root
    :initform nil
    :accessor rule-cached-project-root
    :documentation "Project root pathname, cached on the first file transition to avoid
repeated filesystem walks. find-project-root-for-file performs directory traversal
on every call; caching it reduces cost from O(violations) to O(unique files)."))
  (:default-initargs
   :name :missing-exported-docstring
   :description "Exported definitions should have docstrings"
   :severity :warning
   :category :practice
   :type :form)
  (:documentation "Rule to detect exported definitions lacking docstrings.
Only checks definitions whose symbol is in the :export list of the current
package. Uses a cross-file package export index for accurate lookup.
Tracks the current package via in-package forms during traversal."))

(defmethod base:check-form ((rule missing-exported-docstring-rule) form file)
  "Emit a violation when an exported definition is missing a docstring."
  (check-type form parser:form)
  (check-type file pathname)

  ;; Reset package tracking on file transitions; update cached project root
  (unless (equal file (rule-current-file rule))
    (setf (rule-current-file rule) file
          (rule-current-package rule) nil
          (rule-cached-project-root rule) (pkg-exports:find-project-root-for-file file)))

  (let ((expr (parser:form-expr form)))
    (cond
      ;; Track current package from in-package forms
      ((in-package-form-p expr)
       (let ((pkg-arg (second expr)))
         (when pkg-arg
           (setf (rule-current-package rule)
                 (utils:symbol-name-from-string pkg-arg))))
       nil)

      ;; Check definition forms when inside a known package
      ((and (checkable-definition-p expr)
            (rule-current-package rule)
            (base:should-create-violation-p rule))
       (let ((name (definition-name expr)))
         (when (and name (not (has-docstring-p expr)))
           (let ((project-root (rule-cached-project-root rule))
                 (lookup-name (export-lookup-name name)))
             (when (and lookup-name
                        (pkg-exports:exported-symbol-p
                         project-root (rule-current-package rule) lookup-name))
               (let ((form-head (string-upcase
                                 (utils:symbol-name-from-string (first expr)))))
                 (list (make-instance 'violation:violation
                                      :rule (base:rule-name rule)
                                      :file file
                                      :line (parser:form-line form)
                                      :column (parser:form-column form)
                                      :severity (base:rule-severity rule)
                                      :message (format nil "Exported ~A ~A is missing a docstring"
                                                       form-head name)
                                      :fix nil))))))))
      (t nil))))
