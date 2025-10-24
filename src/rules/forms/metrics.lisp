(defpackage #:mallet/rules/forms/metrics
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation))
  (:export #:function-length-rule
           #:cyclomatic-complexity-rule))
(in-package #:mallet/rules/forms/metrics)

;;; Function-length rule

(defclass function-length-rule (base:rule)
  ((max-lines
    :initarg :max-lines
    :initform 50
    :accessor max-lines))
  (:default-initargs
   :name :function-length
   :description "Function exceeds maximum length"
   :severity :info
   :type :form))

(defmethod base:check-form ((rule function-length-rule) form file)
  "Check function length."
  (check-type form parser:form)
  (check-type file pathname)

  ;; Call recursive checker
  (base:check-form-recursive rule
                             (parser:form-expr form)
                             file
                             (parser:form-line form)
                             (parser:form-column form)
                             nil
                             (parser:form-position-map form)))

(defmethod base:check-form-recursive ((rule function-length-rule) expr file line column
                                      &optional function-name position-map)
  (declare (ignore function-name))

  (let ((violations '())
        (visited (make-hash-table :test 'eq)))

    (labels ((check-expr (current-expr fallback-line fallback-column)
               (base:with-safe-cons-expr (current-expr visited)
                 (multiple-value-bind (actual-line actual-column)
                     (base:find-actual-position current-expr position-map
                                                fallback-line fallback-column)
                   (let ((head (first current-expr)))

                     ;; Check if this is a function definition
                     (when (is-function-definition-p head)
                       (let ((length (calculate-function-length
                                      current-expr position-map
                                      actual-line actual-column)))
                         (when (and (> length (max-lines rule))
                                    (base:should-create-violation-p rule))
                           (push (make-function-length-violation
                                  rule file actual-line actual-column
                                  length (get-function-name current-expr))
                                 violations))))

                     ;; Recurse into nested forms
                     ;; Special handling for flet/labels to check inner functions
                     (cond
                       ;; flet/labels: check both inner functions and body
                       ((or (base:symbol-matches-p head "FLET")
                            (base:symbol-matches-p head "LABELS"))
                        (when (and (consp (rest current-expr))
                                   (a:proper-list-p (second current-expr)))
                          ;; Check each inner function definition
                          ;; Inner functions have structure: (name lambda-list . body)
                          (dolist (func-def (second current-expr))
                            (when (consp func-def)
                              (multiple-value-bind (func-line func-column)
                                  (base:find-actual-position func-def position-map
                                                             actual-line actual-column)
                                (let ((length (calculate-function-length
                                               func-def position-map
                                               func-line func-column))
                                      (func-name (get-inner-function-name func-def)))
                                  (when (and (> length (max-lines rule))
                                             (base:should-create-violation-p rule))
                                    (push (make-function-length-violation
                                           rule file func-line func-column
                                           length func-name)
                                          violations))))))
                          ;; Check body forms
                          (dolist (body-form (cddr current-expr))
                            (when (consp body-form)
                              (check-expr body-form actual-line actual-column)))))

                       ;; Other forms: recurse normally
                       (t
                        (dolist (subexpr (rest current-expr))
                          (when (consp subexpr)
                            (check-expr subexpr actual-line actual-column))))))))))

      (check-expr expr line column))

    violations))

(defun is-function-definition-p (head)
  "Check if HEAD is a function-defining form."
  (and (stringp head)
       (member (base:symbol-name-from-string head)
               '("DEFUN" "DEFMETHOD" "DEFMACRO" "DEFGENERIC")
               :test #'string-equal)))

(defun calculate-function-length (expr position-map start-line start-column)
  "Calculate the length of a function definition in lines.
   V1: Simple line counting (end-line - start-line + 1)."
  (declare (ignore start-column))

  ;; Find end position from position-map
  ;; The position-map stores (line . column) for the START of each expression
  ;; We need to find the end by looking at the entire form's extent

  ;; For now, use a simple heuristic: find the maximum line number
  ;; of any sub-expression in this function
  (let ((max-line start-line))
    (maphash (lambda (k v)
               (when (and (eq-subexpr-p k expr)
                          (> (car v) max-line))
                 (setf max-line (car v))))
             position-map)
    ;; Length = end-line - start-line + 1
    (- max-line start-line -1)))

(defun eq-subexpr-p (needle haystack)
  "Check if NEEDLE is EQ to HAYSTACK or any sub-expression in HAYSTACK."
  (cond
    ((eq needle haystack) t)
    ((not (consp haystack)) nil)
    (t (or (eq-subexpr-p needle (car haystack))
           (eq-subexpr-p needle (cdr haystack))))))

(defun get-function-name (expr)
  "Extract function name from definition form."
  (when (consp expr)
    (let ((name (second expr)))
      (cond
        ((symbolp name) (symbol-name name))
        ((stringp name) (base:symbol-name-from-string name))
        (t "CL:LAMBDA")))))

(defun get-inner-function-name (func-def)
  "Extract function name from flet/labels inner function definition.
   Inner functions have structure: (name lambda-list . body)"
  (when (consp func-def)
    (let ((name (first func-def)))
      (cond
        ((symbolp name) (symbol-name name))
        ((stringp name) (base:symbol-name-from-string name))
        (t "CL:LAMBDA")))))

(defun make-function-length-violation (rule file line column length func-name)
  "Create a violation for function length."
  (make-instance 'violation:violation
                 :rule :function-length
                 :file file
                 :line line
                 :column column
                 :severity (base:rule-severity rule)
                 :message (format nil "Function '~A' is ~D lines (max: ~D)"
                                  func-name length (max-lines rule))
                 :fix nil))

;;; Cyclomatic-complexity rule (to be implemented)
