(defpackage #:mallet/rules/forms/lambda-list
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:mallet/rules/base)
   (#:violation #:mallet/violation)
   (#:parser #:mallet/parser))
  (:export #:mixed-optional-and-key-rule
           #:allow-other-keys-rule))
(in-package #:mallet/rules/forms/lambda-list)

;;; Mixed &optional and &key Rule

(defclass mixed-optional-and-key-rule (base:rule)
  ()
  (:default-initargs
   :name :mixed-optional-and-key
   :description "Don't mix &optional and &key in lambda lists"
   :severity :error
   :category :correctness
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
                   (let ((operator (first current-expr)))
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
            (base:find-actual-position lambda-list position-map fallback-line fallback-column)
          (list line column
                (format nil "Lambda list in ~A mixes &optional and &key (confusing and problematic)"
                        context)))))))

(defun check-lambda-list-allow-other-keys (lambda-list position-map fallback-line fallback-column context)
  "Check a single lambda list for &allow-other-keys.
Returns (line column message) if violation found, NIL otherwise."
  (when (and (consp lambda-list)
             (a:proper-list-p lambda-list))
    (let ((has-allow-other-keys nil))
      (dolist (item lambda-list)
        (when (stringp item)
          (when (string-equal (base:symbol-name-from-string item) "&ALLOW-OTHER-KEYS")
            (setf has-allow-other-keys t))))
      (when has-allow-other-keys
        (multiple-value-bind (line column)
            (base:find-actual-position lambda-list position-map fallback-line fallback-column)
          (list line column
                (format nil "Lambda list in ~A uses &allow-other-keys, which silently ignores unknown keyword arguments"
                        context)))))))

;;; Allow-Other-Keys Rule

(defclass allow-other-keys-rule (base:rule)
  ()
  (:default-initargs
   :name :allow-other-keys
   :description "Avoid &allow-other-keys in lambda lists (silently ignores unknown keywords)"
   :severity :warning
   :category :practice
   :type :form))

(defmethod base:check-form ((rule allow-other-keys-rule) form file)
  "Check that lambda lists don't use &allow-other-keys."
  (base:check-form-recursive rule
                             (parser:form-expr form)
                             file
                             (parser:form-line form)
                             (parser:form-column form)
                             nil  ; function-name
                             (parser:form-position-map form)))

(defmethod base:check-form-recursive ((rule allow-other-keys-rule) expr file line column &optional function-name position-map)
  "Recursively check for &allow-other-keys violations."
  (declare (ignore function-name))
  (let ((violations '())
        (visited (make-hash-table :test 'eq)))
    (labels ((make-violation (vline vcol msg)
               (make-instance 'violation:violation
                              :rule :allow-other-keys
                              :file file
                              :line vline
                              :column vcol
                              :severity (base:rule-severity rule)
                              :message msg))

             (check-expr (current-expr fallback-line fallback-column)
               (base:with-safe-code-expr (current-expr visited)
                 (when (stringp (first current-expr))
                   (let ((operator (first current-expr)))
                     (cond
                       ;; defun (defun name lambda-list ...)
                       ((base:symbol-matches-p operator "DEFUN")
                        (when (>= (length current-expr) 3)
                          (let ((result (check-lambda-list-allow-other-keys
                                         (third current-expr) position-map fallback-line fallback-column "defun")))
                            (when (and result (base:should-create-violation-p rule))
                              (push (make-violation (first result) (second result) (third result))
                                    violations)))))
                       ;; defmethod (defmethod name qualifiers* (specializers) ...)
                       ((base:symbol-matches-p operator "DEFMETHOD")
                        (when (>= (length current-expr) 3)
                          (loop for item in (cddr current-expr)
                                when (consp item)
                                  do (let ((result (check-lambda-list-allow-other-keys
                                                     item position-map fallback-line fallback-column "defmethod")))
                                       (when (and result (base:should-create-violation-p rule))
                                         (push (make-violation (first result) (second result) (third result))
                                               violations)))
                                     (return))))
                       ;; lambda (lambda lambda-list ...)
                       ((base:symbol-matches-p operator "LAMBDA")
                        (when (>= (length current-expr) 2)
                          (let ((result (check-lambda-list-allow-other-keys
                                         (second current-expr) position-map fallback-line fallback-column "lambda")))
                            (when (and result (base:should-create-violation-p rule))
                              (push (make-violation (first result) (second result) (third result))
                                    violations)))))
                       ;; defmacro (defmacro name lambda-list ...)
                       ((base:symbol-matches-p operator "DEFMACRO")
                        (when (>= (length current-expr) 3)
                          (let ((result (check-lambda-list-allow-other-keys
                                         (third current-expr) position-map fallback-line fallback-column "defmacro")))
                            (when (and result (base:should-create-violation-p rule))
                              (push (make-violation (first result) (second result) (third result))
                                    violations)))))
                       ;; flet and labels
                       ((or (base:symbol-matches-p operator "FLET")
                            (base:symbol-matches-p operator "LABELS"))
                        (when (>= (length current-expr) 2)
                          (let ((bindings (second current-expr)))
                            (when (consp bindings)
                              (dolist (binding bindings)
                                (when (and (consp binding) (>= (length binding) 2))
                                  (let ((result (check-lambda-list-allow-other-keys
                                                  (second binding) position-map fallback-line fallback-column "flet/labels function")))
                                    (when (and result (base:should-create-violation-p rule))
                                      (push (make-violation (first result) (second result) (third result))
                                            violations)))))))))))

                 ;; Recursively check nested forms
                 (when (and (consp current-expr)
                            (a:proper-list-p current-expr))
                   (dolist (subexpr current-expr)
                     (when (consp subexpr)
                       (let ((nested-violations (base:check-form-recursive rule subexpr file
                                                                           fallback-line fallback-column)))
                         (setf violations (nconc violations nested-violations))))))))))
      (check-expr expr line column))
    violations))
