(defpackage #:mallet/rules/forms/naming
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:mallet/rules/base)
   (#:violation #:mallet/violation)
   (#:parser #:mallet/parser))
  (:export #:special-variable-naming-rule
           #:constant-naming-rule))
(in-package #:mallet/rules/forms/naming)

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
  (base:check-form-recursive rule
                             (parser:form-expr form)
                             file
                             (parser:form-line form)
                             (parser:form-column form)
                             nil  ; function-name
                             (parser:form-position-map form)))

(defmethod base:check-form-recursive ((rule special-variable-naming-rule) expr file line column &optional function-name position-map)
  "Recursively check for special variable naming violations."
  (declare (ignore function-name))
  (let ((violations '())
        (visited (make-hash-table :test 'eq)))
    (labels ((check-expr (current-expr fallback-line fallback-column)
               (when (and (consp current-expr)
                          (not (gethash current-expr visited)))
                 (setf (gethash current-expr visited) t)

                 ;; Check if this form is a defvar/defparameter
                 (when (stringp (first current-expr))
                   (let ((operator (base:symbol-name-from-string (first current-expr))))
                     (when (or (base:symbol-matches-p operator "DEFVAR")
                               (base:symbol-matches-p operator "DEFPARAMETER"))
                       ;; Found a defvar/defparameter - check naming
                       (when (>= (length current-expr) 2)
                         (let* ((var-name-expr (second current-expr))
                                (var-name (base:symbol-name-from-string var-name-expr)))
                           (when (and var-name
                                      (plusp (length var-name))
                                      (not (and (char= (char var-name 0) #\*)
                                                (char= (char var-name (1- (length var-name))) #\*)))
                                      (not (and (char= (char var-name 0) #\+)
                                                (char= (char var-name (1- (length var-name))) #\+))))
                             (when (base:should-create-violation-p rule)
                               (multiple-value-bind (var-line var-column)
                                   (if position-map
                                       (parser:find-position var-name-expr position-map
                                                            fallback-line fallback-column)
                                       (values fallback-line fallback-column))
                                 (push (make-instance 'violation:violation
                                                    :rule :special-variable-naming
                                                    :file file
                                                    :line var-line
                                                    :column var-column
                                                    :severity (base:rule-severity rule)
                                                    :message (format nil "Special variable '~A' should be named *~A*"
                                                                     var-name
                                                                     (string-trim '(#\*) var-name)))
                                       violations)))))))))

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
  (base:check-form-recursive rule
                             (parser:form-expr form)
                             file
                             (parser:form-line form)
                             (parser:form-column form)
                             nil  ; function-name
                             (parser:form-position-map form)))

(defmethod base:check-form-recursive ((rule constant-naming-rule) expr file line column &optional function-name position-map)
  "Recursively check for constant naming violations."
  (declare (ignore function-name))
  (let ((violations '())
        (visited (make-hash-table :test 'eq)))
    (labels ((check-expr (current-expr fallback-line fallback-column)
               (when (and (consp current-expr)
                          (not (gethash current-expr visited)))
                 (setf (gethash current-expr visited) t)

                 ;; Check if this form is a defconstant/define-constant
                 (when (stringp (first current-expr))
                   (let ((operator (base:symbol-name-from-string (first current-expr))))
                     (when (or (base:symbol-matches-p operator "DEFCONSTANT")
                               (base:symbol-matches-p operator "DEFINE-CONSTANT"))
                       ;; Found a defconstant - check naming
                       (when (>= (length current-expr) 2)
                         (let* ((const-name-expr (second current-expr))
                                (const-name (base:symbol-name-from-string const-name-expr)))
                           (when (and const-name
                                      (plusp (length const-name))
                                      (not (and (char= (char const-name 0) #\+)
                                                (char= (char const-name (1- (length const-name))) #\+))))
                             (when (base:should-create-violation-p rule)
                               (multiple-value-bind (const-line const-column)
                                   (if position-map
                                       (parser:find-position const-name-expr position-map
                                                            fallback-line fallback-column)
                                       (values fallback-line fallback-column))
                                 (push (make-instance 'violation:violation
                                                    :rule :constant-naming
                                                    :file file
                                                    :line const-line
                                                    :column const-column
                                                    :severity (base:rule-severity rule)
                                                    :message (format nil "Constant '~A' should be named +~A+"
                                                                     const-name
                                                                     (string-trim '(#\+) const-name)))
                                       violations)))))))))

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

