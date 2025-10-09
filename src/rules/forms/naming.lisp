(defpackage #:malo/rules/forms/naming
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:malo/rules/base)
   (#:violation #:malo/violation)
   (#:parser #:malo/parser))
  (:export #:special-variable-naming-rule
           #:constant-naming-rule))
(in-package #:malo/rules/forms/naming)

;;; Special Variable Naming Rule

(defclass special-variable-naming-rule (base:rule)
  ()
  (:default-initargs
   :name :special-variable-naming
   :description "Special variables should be named *foo*"
   :severity :convention
   :type :form))

(defmethod base:check-form ((rule special-variable-naming-rule) form file)
  "Check that special variables follow *foo* naming convention."
  (let ((expr (parser:form-expr form)))
    (when (and (consp expr)
               (stringp (first expr)))
      (let ((operator (base:symbol-name-from-string (first expr))))
        (when (or (base:symbol-matches-p operator "DEFVAR")
                  (base:symbol-matches-p operator "DEFPARAMETER"))
          (check-earmuff-name expr form file (base:rule-severity rule) "Special variable"))))))

;;; Constant Naming Rule

(defclass constant-naming-rule (base:rule)
  ()
  (:default-initargs
   :name :constant-naming
   :description "Constants should be named +foo+"
   :severity :info
   :type :form))

(defmethod base:check-form ((rule constant-naming-rule) form file)
  "Check that constants follow +foo+ naming convention."
  (let ((expr (parser:form-expr form)))
    (when (and (consp expr)
               (stringp (first expr)))
      (let ((operator (base:symbol-name-from-string (first expr))))
        (when (or (base:symbol-matches-p operator "DEFCONSTANT")
                  (base:symbol-matches-p operator "DEFINE-CONSTANT"))
          (check-plus-name expr form file (base:rule-severity rule) "Constant"))))))

;;; Helper functions

(defun check-earmuff-name (expr form file severity label)
  "Check that variable name follows *foo* convention."
  (when (and (consp expr) (>= (length expr) 2))
    (let* ((var-name-expr (second expr))
           (var-name (base:symbol-name-from-string var-name-expr))
           (position-map (parser:form-position-map form)))
      (when (and var-name
                 (plusp (length var-name))
                 (not (and (char= (char var-name 0) #\*)
                           (char= (char var-name (1- (length var-name))) #\*))))
        (multiple-value-bind (line column)
            (if position-map
                (parser:find-position var-name-expr position-map
                                     (parser:form-line form)
                                     (parser:form-column form))
                (values (parser:form-line form) (parser:form-column form)))
          (list (make-instance 'violation:violation
                               :rule :special-variable-naming
                               :file file
                               :line line
                               :column column
                               :severity severity
                               :message (format nil "~A '~A' should be named *~A*"
                                              label var-name var-name))))))))

(defun check-plus-name (expr form file severity label)
  "Check that constant name follows +foo+ convention."
  (when (and (consp expr) (>= (length expr) 2))
    (let* ((const-name-expr (second expr))
           (const-name (base:symbol-name-from-string const-name-expr))
           (position-map (parser:form-position-map form)))
      (when (and const-name
                 (plusp (length const-name))
                 (not (and (char= (char const-name 0) #\+)
                           (char= (char const-name (1- (length const-name))) #\+))))
        (multiple-value-bind (line column)
            (if position-map
                (parser:find-position const-name-expr position-map
                                     (parser:form-line form)
                                     (parser:form-column form))
                (values (parser:form-line form) (parser:form-column form)))
          (list (make-instance 'violation:violation
                               :rule :constant-naming
                               :file file
                               :line line
                               :column column
                               :severity severity
                               :message (format nil "~A '~A' should be named +~A+"
                                              label const-name const-name))))))))
