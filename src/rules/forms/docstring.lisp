(defpackage #:mallet/rules/forms/docstring
  (:use #:cl)
  (:local-nicknames
   (#:base #:mallet/rules/base)
   (#:utils #:mallet/utils))
  (:export
   #:definition-form-p
   #:checkable-definition-p
   #:has-docstring-p
   #:definition-name))
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
