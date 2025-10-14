(defpackage #:mallet/rules/forms/lambda-list
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:mallet/rules/base)
   (#:violation #:mallet/violation)
   (#:parser #:mallet/parser))
  (:export #:mixed-optional-and-key-rule))
(in-package #:mallet/rules/forms/lambda-list)

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
  (base:check-form-recursive rule
                             (parser:form-expr form)
                             file
                             (parser:form-line form)
                             (parser:form-column form)
                             nil  ; function-name
                             (parser:form-position-map form)))

(defmethod base:check-form-recursive ((rule mixed-optional-and-key-rule) expr file line column &optional function-name position-map)
  "Recursively check for mixed &optional and &key violations."
  (declare (ignore function-name))
  (let ((violations '())
        (visited (make-hash-table :test 'eq)))
    (labels ((check-expr (current-expr fallback-line fallback-column)
               (base:with-safe-code-expr (current-expr visited)
                 ;; Check if this form has a lambda list
                 (when (stringp (first current-expr))
                   (let ((operator (base:symbol-name-from-string (first current-expr))))
                     (cond
                       ;; defun (defun name lambda-list ...)
                       ((base:symbol-matches-p operator "DEFUN")
                        (when (>= (length current-expr) 3)
                          (let* ((lambda-list (third current-expr))
                                 (result (check-lambda-list-mixed lambda-list position-map fallback-line fallback-column "defun")))
                            (when (and result
                                       (base:should-create-violation-p rule))
                              (push (make-instance 'violation:violation
                                                   :rule :mixed-optional-and-key
                                                   :file file
                                                   :line (first result)
                                                   :column (second result)
                                                   :severity (base:rule-severity rule)
                                                   :message (third result))
                                    violations)))))
                       ;; defmethod (defmethod name qualifiers* (specializers) ...)
                       ((base:symbol-matches-p operator "DEFMETHOD")
                        (when (>= (length current-expr) 3)
                          (loop for item in (cddr current-expr)
                                when (consp item)
                                do (let ((result (check-lambda-list-mixed item position-map fallback-line fallback-column "defmethod")))
                                     (when (and result
                                                (base:should-create-violation-p rule))
                                       (push (make-instance 'violation:violation
                                                            :rule :mixed-optional-and-key
                                                            :file file
                                                            :line (first result)
                                                            :column (second result)
                                                            :severity (base:rule-severity rule)
                                                            :message (third result))
                                             violations)))
                                   (return))))
                       ;; lambda (lambda lambda-list ...)
                       ((base:symbol-matches-p operator "LAMBDA")
                        (when (>= (length current-expr) 2)
                          (let* ((lambda-list (second current-expr))
                                 (result (check-lambda-list-mixed lambda-list position-map fallback-line fallback-column "lambda")))
                            (when (and result
                                       (base:should-create-violation-p rule))
                              (push (make-instance 'violation:violation
                                                   :rule :mixed-optional-and-key
                                                   :file file
                                                   :line (first result)
                                                   :column (second result)
                                                   :severity (base:rule-severity rule)
                                                   :message (third result))
                                    violations)))))
                       ;; defmacro (defmacro name lambda-list ...)
                       ((base:symbol-matches-p operator "DEFMACRO")
                        (when (>= (length current-expr) 3)
                          (let* ((lambda-list (third current-expr))
                                 (result (check-lambda-list-mixed lambda-list position-map fallback-line fallback-column "defmacro")))
                            (when (and result
                                       (base:should-create-violation-p rule))
                              (push (make-instance 'violation:violation
                                                   :rule :mixed-optional-and-key
                                                   :file file
                                                   :line (first result)
                                                   :column (second result)
                                                   :severity (base:rule-severity rule)
                                                   :message (third result))
                                    violations)))))
                       ;; flet and labels
                       ((or (base:symbol-matches-p operator "FLET")
                            (base:symbol-matches-p operator "LABELS"))
                        (when (>= (length current-expr) 2)
                          (let ((bindings (second current-expr)))
                            (when (consp bindings)
                              (dolist (binding bindings)
                                (when (and (consp binding) (>= (length binding) 2))
                                  (let* ((lambda-list (second binding))
                                         (result (check-lambda-list-mixed lambda-list position-map fallback-line fallback-column "flet/labels function")))
                                    (when (and result
                                               (base:should-create-violation-p rule))
                                      (push (make-instance 'violation:violation
                                                           :rule :mixed-optional-and-key
                                                           :file file
                                                           :line (first result)
                                                           :column (second result)
                                                           :severity (base:rule-severity rule)
                                                           :message (third result))
                                            violations)))))))))
                       ;; destructuring-bind
                       ((base:symbol-matches-p operator "DESTRUCTURING-BIND")
                        (when (>= (length current-expr) 3)
                          (let* ((lambda-list (second current-expr))
                                 (result (check-lambda-list-mixed lambda-list position-map fallback-line fallback-column "destructuring-bind")))
                            (when (and result
                                       (base:should-create-violation-p rule))
                              (push (make-instance 'violation:violation
                                                   :rule :mixed-optional-and-key
                                                   :file file
                                                   :line (first result)
                                                   :column (second result)
                                                   :severity (base:rule-severity rule)
                                                   :message (third result))
                                    violations))))))))

                 ;; Recursively check nested forms
                 ;; Only recurse on proper lists to avoid issues with dotted lists (e.g., loop destructuring)
                 (when (and (consp current-expr)
                            (a:proper-list-p current-expr))
                   (dolist (subexpr current-expr)
                     (when (consp subexpr)
                       (let ((nested-violations (base:check-form-recursive rule subexpr file
                                                                           fallback-line fallback-column)))
                         (setf violations (nconc violations nested-violations)))))))))
      (check-expr expr line column))
    violations))

;;; Helper functions

(defun check-lambda-list-mixed (lambda-list position-map fallback-line fallback-column context)
  "Check a single lambda list for mixed &optional and &key.
Returns (line column message) if violation found, NIL otherwise."
  ;; Only check proper lists - dotted lists in destructuring-bind can't have &optional/&key
  (when (and (consp lambda-list)
             (a:proper-list-p lambda-list))
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
            (if position-map
                (parser:find-position lambda-list position-map
                                     fallback-line fallback-column)
                (values fallback-line fallback-column))
          (list line column
                (format nil "Lambda list in ~A mixes &optional and &key (confusing and problematic)"
                        context)))))))
