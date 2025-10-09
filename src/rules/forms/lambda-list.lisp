(defpackage #:malo/rules/forms/lambda-list
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:malo/rules/base)
   (#:violation #:malo/violation)
   (#:parser #:malo/parser))
  (:export #:mixed-optional-and-key-rule))
(in-package #:malo/rules/forms/lambda-list)

;;; Mixed &optional and &key Rule

(defclass mixed-optional-and-key-rule (base:rule)
  ()
  (:default-initargs
   :name :mixed-optional-and-key
   :description "Don't mix &optional and &key in lambda lists"
   :severity :warning
   :type :form))

(defmethod base:check-form ((rule mixed-optional-and-key-rule) form file)
  "Check that lambda lists don't mix &optional and &key."
  (let ((expr (parser:form-expr form)))
    (when (and (consp expr)
               (stringp (first expr)))
      (let ((operator (base:symbol-name-from-string (first expr))))
        (cond
          ;; defun (defun name lambda-list ...)
          ((base:symbol-matches-p operator "DEFUN")
           (when (>= (length expr) 3)
             (check-lambda-list (third expr) form file (base:rule-severity rule) "defun")))
          ;; defmethod (defmethod name qualifiers* (specializers) ...)
          ((base:symbol-matches-p operator "DEFMETHOD")
           (check-defmethod-lambda-lists expr form file (base:rule-severity rule)))
          ;; lambda (lambda lambda-list ...)
          ((base:symbol-matches-p operator "LAMBDA")
           (when (>= (length expr) 2)
             (check-lambda-list (second expr) form file (base:rule-severity rule) "lambda")))
          ;; defmacro (defmacro name lambda-list ...)
          ((base:symbol-matches-p operator "DEFMACRO")
           (when (>= (length expr) 3)
             (check-lambda-list (third expr) form file (base:rule-severity rule) "defmacro")))
          ;; flet and labels
          ((or (base:symbol-matches-p operator "FLET")
               (base:symbol-matches-p operator "LABELS"))
           (when (>= (length expr) 2)
             (check-flet-functions (second expr) form file (base:rule-severity rule)))))))))

;;; Helper functions

(defun check-lambda-list (lambda-list form file severity context)
  "Check a single lambda list for mixed &optional and &key."
  (when (consp lambda-list)
    (let ((has-optional nil)
          (has-key nil))
      (dolist (item lambda-list)
        (when (stringp item)
          (let ((name (base:symbol-name-from-string item)))
            (cond
              ((string-equal name "&OPTIONAL")
               (setf has-optional t))
              ((string-equal name "&KEY")
               (setf has-key t))))))
      (when (and has-optional has-key)
        (multiple-value-bind (line column)
            (let ((position-map (parser:form-position-map form)))
              (if position-map
                  (parser:find-position lambda-list position-map
                                       (parser:form-line form)
                                       (parser:form-column form))
                  (values (parser:form-line form) (parser:form-column form))))
          (list (make-instance 'violation:violation
                               :rule :mixed-optional-and-key
                               :file file
                               :line line
                               :column column
                               :severity severity
                               :message (format nil "Lambda list in ~A mixes &optional and &key (confusing and problematic)"
                                              context))))))))

(defun check-defmethod-lambda-lists (expr form file severity)
  "Check defmethod specialized lambda list for mixed &optional and &key."
  (when (>= (length expr) 3)
    ;; defmethod can have qualifiers, so we need to find the lambda list
    ;; (defmethod name qualifier* (specializers) ...)
    ;; The lambda list is the first list after the name
    (loop for item in (cddr expr)
          when (consp item)
          return (check-lambda-list item form file severity "defmethod"))))

(defun check-flet-functions (bindings form file severity)
  "Check flet/labels function bindings for mixed &optional and &key."
  (when (consp bindings)
    (let ((violations '()))
      (dolist (binding bindings)
        ;; Each binding is (name lambda-list ...)
        (when (and (consp binding) (>= (length binding) 2))
          (let ((result (check-lambda-list (second binding) form file severity "flet/labels function")))
            (when result
              (setf violations (nconc violations result))))))
      violations)))
