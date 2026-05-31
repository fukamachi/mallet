(defpackage #:mallet/tests/rules/special-forms
  (:use #:cl #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/special-forms)

(defparameter *special-form-named-like-keyword-cases*
  '(("DEFSTRUCT"
     "(defstruct my-struct
        (do #'identity :type function)
        (variable #'list :type function))")
    ("DEFCLASS"
     "(defclass my-class ()
        ((do :initarg :do :accessor get-do)
         (variable :initarg :var)))")
    ("DEFINE-CONDITION"
     "(define-condition my-condition (error)
        ((do :initarg :do :reader condition-do)
         (variable :initarg :var)))")
    ("DEFPACKAGE"
     "(defpackage #:test-pkg
        (:use #:cl)
        (:export #:do #:variable))")
    ("DEFTYPE"
     "(deftype my-type (do variable)
        'integer)")
    ("DEFGENERIC"
     "(defgeneric compute (do variable)
        (:documentation \"Compute something\"))")
    ("DEFMETHOD"
     "(defmethod compute ((do my-class) variable)
        42)")
    ("DEFINE-COMPILER-MACRO"
     "(define-compiler-macro my-macro (do &optional variable)
        42)")
    ("DEFSETF"
     "(defsetf my-accessor (do variable) (new-value)
        `(set-accessor ,do ,variable ,new-value))")
    ("DEFINE-MODIFY-MACRO"
     "(define-modify-macro my-incf (do &optional (variable 1))
        +)")
    ("DEFINE-SETF-EXPANDER"
     "(define-setf-expander my-place (do variable)
        (values do variable nil nil nil))")))

(defparameter *special-form-body-unused-variable-cases*
  '(("DEFINE-CONDITION"
     "(define-condition my-error (error)
        ((slot1 :initarg :slot1))
        (:report (lambda (condition stream)
                   (let ((unused-in-report 3))
                     (format stream \"Error: ~A\" (slot1 condition))))))"
     "Variable 'unused-in-report' is unused")
    ("DEFGENERIC"
     "(defgeneric foo (a b)
        (:method ((a integer) b)
          (let ((unused-var 1))
            b)))"
     "Variable 'unused-var' is unused")
    ("DEFMETHOD"
     "(defmethod compute ((a my-class) b)
        (let ((unused-var 1))
          (values a b)))"
     "Variable 'unused-var' is unused")
    ("DEFINE-COMPILER-MACRO"
     "(define-compiler-macro my-macro (a &optional b)
        (let ((unused-var 2))
          `(+ ,a ,b)))"
     "Variable 'unused-var' is unused")))

(defun unused-variable-violations (code)
  (let* ((forms (parser:parse-forms code #P"/tmp/test.lisp"))
         (rule (make-instance 'rules:unused-variables-rule)))
    (rules:check-form rule (first forms) #P"/tmp/test.lisp")))

(deftest special-form-named-like-keyword-no-violations
  (dolist (case *special-form-named-like-keyword-cases*)
    (destructuring-bind (head code) case
      (testing (format nil "~A slot/parameter names do not create violations" head)
        (ok (null (unused-variable-violations code))
            (format nil "No unused-variable violations for ~A" head))))))

(deftest special-form-body-unused-variable-violations
  (dolist (case *special-form-body-unused-variable-cases*)
    (destructuring-bind (head code expected-message) case
      (testing (format nil "~A body reports unused variables" head)
        (let ((violations (unused-variable-violations code)))
          (ok (= (length violations) 1)
              (format nil "One violation for unused variable in ~A body" head))
          (ok (eq (violation:violation-rule (first violations))
                  :unused-variables))
          (ok (string= (violation:violation-message (first violations))
                       expected-message)
              "Correct violation message"))))))
