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
   ;; New form-type predicates
   #:deftype-p
   #:define-condition-p
   #:defvar-p
   #:defparameter-p
   #:defstruct-p
   ;; New docstring detection functions
   #:deftype-can-have-docstring-p
   #:variable-has-docstring-p
   #:defstruct-has-docstring-p
   #:defstruct-name
   #:defpackage-has-docstring-p
   ;; Exported-only mixin
   #:exported-only-mixin
   #:rule-exported-only-p
   #:update-package-tracking
   #:should-check-definition-p
   ;; Rule classes
   #:missing-docstring-rule
   #:missing-package-docstring-rule))
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
Recognized forms: defun, defmacro, defgeneric, defclass, defmethod, deftype, define-condition."
  (and (consp expr)
       (stringp (first expr))
       (or (base:symbol-matches-p (first expr) "DEFUN")
           (base:symbol-matches-p (first expr) "DEFMACRO")
           (base:symbol-matches-p (first expr) "DEFGENERIC")
           (base:symbol-matches-p (first expr) "DEFCLASS")
           (base:symbol-matches-p (first expr) "DEFMETHOD")
           (base:symbol-matches-p (first expr) "DEFTYPE")
           (base:symbol-matches-p (first expr) "DEFINE-CONDITION"))))

(defun checkable-definition-p (expr)
  "Return T if EXPR is a definition form that should be checked for docstrings.
Includes defun, defmacro, defgeneric, defclass, deftype, define-condition.
Excludes defmethod (methods inherit docs from the generic function)."
  (and (consp expr)
       (stringp (first expr))
       (or (base:symbol-matches-p (first expr) "DEFUN")
           (base:symbol-matches-p (first expr) "DEFMACRO")
           (base:symbol-matches-p (first expr) "DEFGENERIC")
           (base:symbol-matches-p (first expr) "DEFCLASS")
           (base:symbol-matches-p (first expr) "DEFTYPE")
           (base:symbol-matches-p (first expr) "DEFINE-CONDITION"))))

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

(defun deftype-p (expr)
  "Return T if EXPR is a deftype form."
  (and (consp expr)
       (stringp (first expr))
       (base:symbol-matches-p (first expr) "DEFTYPE")))

(defun define-condition-p (expr)
  "Return T if EXPR is a define-condition form."
  (and (consp expr)
       (stringp (first expr))
       (base:symbol-matches-p (first expr) "DEFINE-CONDITION")))

(defun defvar-p (expr)
  "Return T if EXPR is a defvar form."
  (and (consp expr)
       (stringp (first expr))
       (base:symbol-matches-p (first expr) "DEFVAR")))

(defun defparameter-p (expr)
  "Return T if EXPR is a defparameter form."
  (and (consp expr)
       (stringp (first expr))
       (base:symbol-matches-p (first expr) "DEFPARAMETER")))

(defun defstruct-p (expr)
  "Return T if EXPR is a defstruct form."
  (and (consp expr)
       (stringp (first expr))
       (base:symbol-matches-p (first expr) "DEFSTRUCT")))

;;; defpackage-or-define-package-p is provided by mallet/rules/forms/package-exports
;;; and accessible here via the pkg-exports local nickname.

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
For defun/defmacro/deftype: first form after lambda list is a string AND more body forms follow.
For defgeneric/defclass/define-condition: body options contain (:documentation \"string\").
For defmethod: always returns T (not checked).
Returns NIL for non-definition forms or definitions without docstrings."
  (cond
    ((defmethod-p expr) t)
    ((or (defun-or-defmacro-p expr) (deftype-p expr))
     (defun-body-has-docstring-p (cdddr expr)))
    ((defgeneric-p expr)
     (options-have-documentation-p (cdddr expr)))
    ((or (defclass-p expr) (define-condition-p expr))
     (options-have-documentation-p (cddddr expr)))
    (t nil)))

(defun deftype-can-have-docstring-p (expr)
  "Return T if deftype EXPR's body can contain a docstring.
A deftype with a single body form uses that as the type expansion, not a docstring.
Returns T only when the body (after lambda list, i.e. cdddr) contains at least 2 forms."
  (let ((body (cdddr expr)))
    (and (consp body)
         (consp (cdr body)))))

(defun variable-has-docstring-p (expr)
  "Return T if EXPR (a defvar or defparameter form) has a docstring.
For defvar without an init value (no third element): returns T (not checkable, skip).
The docstring is the 4th element: (defvar *name* value \"doc\")."
  (if (and (defvar-p expr) (null (cddr expr)))
      t
      (and (consp (cdddr expr))
           (stringp (fourth expr)))))

(defun defstruct-has-docstring-p (expr)
  "Return T if defstruct EXPR has a docstring.
Checks both (:documentation \"string\") in name-and-options and a plain string in the body."
  (when (defstruct-p expr)
    (let ((name-and-opts (second expr))
          (body (cddr expr)))
      (or
       ;; Check (:documentation "string") in name-and-options list
       (and (consp name-and-opts)
            (options-have-documentation-p (cdr name-and-opts)))
       ;; Check for plain string as first body element
       (and (consp body)
            (stringp (first body)))))))

(defun defpackage-has-docstring-p (expr)
  "Return T if defpackage or define-package EXPR has a :documentation option."
  (and (pkg-exports:defpackage-or-define-package-p expr)
       (options-have-documentation-p (cddr expr))))

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

(defun defstruct-name (expr)
  "Return the name of the structure defined by defstruct EXPR as a string.
Handles both symbol name (defstruct foo ...) and list name-and-options (defstruct (foo ...) ...)."
  (when (and (consp expr) (consp (cdr expr)))
    (let ((name-and-opts (second expr)))
      (cond
        ((stringp name-and-opts)
         (utils:symbol-name-from-string name-and-opts))
        ((consp name-and-opts)
         (let ((name (first name-and-opts)))
           (when (stringp name)
             (utils:symbol-name-from-string name))))
        (t nil)))))

;;; --- Package tracking helpers ---

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

;;; --- exported-only-mixin ---

(defclass exported-only-mixin ()
  ((exported-only
    :initarg :exported-only
    :initform nil
    :reader rule-exported-only-p
    :documentation "When non-nil, only check exported definitions.")
   (current-file
    :initform nil
    :accessor mixin-current-file
    :documentation "Last file seen, for detecting file transitions.")
   (current-package
    :initform nil
    :accessor mixin-current-package
    :documentation "Package name from the most recent in-package form.")
   (cached-project-root
    :initform nil
    :accessor mixin-cached-project-root
    :documentation "Project root pathname, cached on first file transition to avoid
repeated filesystem walks."))
  (:documentation "Mixin that adds exported-only filtering for docstring rules.
When exported-only is non-nil, definitions are only checked if their symbol is
exported from the current package. Tracks the current package via in-package forms."))

(defmethod initialize-instance :after ((mixin exported-only-mixin) &rest initargs)
  "Auto-upgrade severity to :warning when exported-only is set and no explicit severity provided.
Note: :default-initargs values in a subclass are included in INITARGS, so this upgrade
fires only when the mixin is instantiated standalone (no :severity in :default-initargs)
or when the subclass inherits no :severity default. Rule classes that declare their own
:severity in :default-initargs will suppress this auto-upgrade, which is correct."
  (unless (or (not (rule-exported-only-p mixin))
              (member :severity initargs))
    (setf (base:rule-severity mixin) :warning)))

(defgeneric update-package-tracking (mixin form file)
  (:documentation "Update package tracking state for FILE/FORM.
Handles file transitions (reset current-package, cache project root) and
in-package form tracking. Returns T if FORM is an in-package form (caller should skip it)."))

(defmethod update-package-tracking ((mixin exported-only-mixin) form file)
  "Update tracking state and return T if FORM is an in-package form."
  (check-type form parser:form)
  (unless (equal file (mixin-current-file mixin))
    (setf (mixin-current-file mixin) file
          (mixin-current-package mixin) nil
          (mixin-cached-project-root mixin) (pkg-exports:find-project-root-for-file file)))
  (let ((expr (parser:form-expr form)))
    (when (in-package-form-p expr)
      (let ((pkg-arg (second expr)))
        (when pkg-arg
          (setf (mixin-current-package mixin)
                (utils:symbol-name-from-string pkg-arg))))
      t)))

(defgeneric should-check-definition-p (mixin name)
  (:documentation "Return T if a definition with NAME should be checked.
When exported-only is nil: always returns T.
When exported-only is non-nil: checks export status via pkg-exports."))

(defmethod should-check-definition-p ((mixin exported-only-mixin) name)
  "Return T if NAME should be checked based on exported-only setting."
  (or (not (rule-exported-only-p mixin))
      (let ((lookup-name (export-lookup-name name))
            (project-root (mixin-cached-project-root mixin))
            (pkg (mixin-current-package mixin)))
        (and lookup-name pkg project-root
             (pkg-exports:exported-symbol-p project-root pkg lookup-name)))))

;;; --- missing-docstring Rule ---

(defclass missing-docstring-rule (exported-only-mixin base:rule)
  ()
  (:default-initargs
   :name :missing-docstring
   :description "Top-level definitions should have docstrings"
   :severity :info
   :category :style
   :type :form)
  (:documentation "Rule to detect top-level definitions lacking docstrings.
Checks defun, defmacro, defgeneric, defclass, deftype, and define-condition forms.
Skips defmethod, which inherits its documentation from the generic function.
When :exported-only t is set, only flags definitions whose symbol is exported
from the current package."))

(defun form-type-string (expr)
  "Return uppercase form type name for EXPR (e.g., \"DEFUN\", \"DEFCLASS\")."
  (when (and (consp expr) (stringp (first expr)))
    (string-upcase (utils:symbol-name-from-string (first expr)))))

(defmethod base:check-form ((rule missing-docstring-rule) form file)
  "Check FORM for missing docstring on a top-level definition."
  (check-type form parser:form)
  (check-type file pathname)
  ;; Track package context; skip in-package forms themselves
  (unless (update-package-tracking rule form file)
    (let ((expr (parser:form-expr form)))
      (when (and (checkable-definition-p expr)
                 ;; deftype with single body form is a type expansion, not checkable
                 (or (not (deftype-p expr))
                     (deftype-can-have-docstring-p expr))
                 (not (has-docstring-p expr))
                 (base:should-create-violation-p rule))
        (let ((name (definition-name expr)))
          (when (and name (should-check-definition-p rule name))
            (let* ((form-type (form-type-string expr))
                   (msg (if (rule-exported-only-p rule)
                            (format nil "Exported ~A ~A is missing a docstring"
                                    form-type name)
                            (format nil "~A ~A is missing a docstring"
                                    form-type name))))
              (list (make-instance 'violation:violation
                                   :rule (base:rule-name rule)
                                   :file file
                                   :line (parser:form-line form)
                                   :column (parser:form-column form)
                                   :severity (base:rule-severity rule)
                                   :message msg
                                   :fix nil)))))))))

;;; --- missing-package-docstring-rule ---

(defclass missing-package-docstring-rule (base:rule)
  ()
  (:default-initargs
   :name :missing-package-docstring
   :description "Package definitions should have docstrings"
   :severity :info
   :category :style
   :type :form)
  (:documentation "Rule to detect defpackage and define-package forms lacking docstrings.
A package docstring appears as a (:documentation \"string\") option in the body.
Documents the package's purpose and public API."))

(defmethod base:check-form ((rule missing-package-docstring-rule) form file)
  "Emit a violation when a defpackage or define-package is missing a :documentation option."
  (check-type form parser:form)
  (check-type file pathname)
  (let ((expr (parser:form-expr form)))
    (when (and (pkg-exports:defpackage-or-define-package-p expr)
               (not (defpackage-has-docstring-p expr))
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
