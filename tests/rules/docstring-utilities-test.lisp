(defpackage #:mallet/tests/rules/docstring-utilities
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:utils #:mallet/rules/forms/docstring)
   (#:parser #:mallet/parser)))
(in-package #:mallet/tests/rules/docstring-utilities)

;;; Helper: parse first form's expr from code string

(defun parse-expr (code)
  "Parse CODE and return the expr of the first form."
  (let ((forms (parser:parse-forms code #p"test.lisp")))
    (when forms
      (parser:form-expr (first forms)))))

;;; definition-form-p

(deftest definition-form-p-tests
  (testing "defun is a definition form"
    (ok (utils:definition-form-p (parse-expr "(defun foo (x) x)"))))

  (testing "defmacro is a definition form"
    (ok (utils:definition-form-p (parse-expr "(defmacro when+ (c) c)"))))

  (testing "defgeneric is a definition form"
    (ok (utils:definition-form-p (parse-expr "(defgeneric foo (x))"))))

  (testing "defclass is a definition form"
    (ok (utils:definition-form-p (parse-expr "(defclass foo () ())"))))

  (testing "defmethod is a definition form"
    (ok (utils:definition-form-p (parse-expr "(defmethod foo (x) x)"))))

  (testing "defvar is not a definition form"
    (ok (null (utils:definition-form-p (parse-expr "(defvar *x* 1)")))))

  (testing "let is not a definition form"
    (ok (null (utils:definition-form-p (parse-expr "(let ((x 1)) x)")))))

  (testing "nil is not a definition form"
    (ok (null (utils:definition-form-p nil)))))

;;; checkable-definition-p

(deftest checkable-definition-p-tests
  (testing "defun is checkable"
    (ok (utils:checkable-definition-p (parse-expr "(defun foo (x) x)"))))

  (testing "defmacro is checkable"
    (ok (utils:checkable-definition-p (parse-expr "(defmacro when+ (c) c)"))))

  (testing "defgeneric is checkable"
    (ok (utils:checkable-definition-p (parse-expr "(defgeneric foo (x))"))))

  (testing "defclass is checkable"
    (ok (utils:checkable-definition-p (parse-expr "(defclass foo () ())"))))

  (testing "defmethod is NOT checkable (always skipped)"
    (ok (null (utils:checkable-definition-p (parse-expr "(defmethod foo (x) x)")))))

  (testing "defvar is not checkable"
    (ok (null (utils:checkable-definition-p (parse-expr "(defvar *x* 1)"))))))

;;; has-docstring-p for defun/defmacro

(deftest has-docstring-defun-tests
  (testing "defun with docstring and body returns T"
    (ok (utils:has-docstring-p
         (parse-expr "(defun add (x y) \"Add X and Y.\" (+ x y))"))))

  (testing "defun without docstring returns NIL"
    (ok (null (utils:has-docstring-p
               (parse-expr "(defun add (x y) (+ x y))")))))

  (testing "defun with single string body (return value, not docstring) returns NIL"
    (ok (null (utils:has-docstring-p
               (parse-expr "(defun greeting () \"hello\")")))))

  (testing "defun with no body returns NIL"
    (ok (null (utils:has-docstring-p
               (parse-expr "(defun foo ())")))))

  (testing "defmacro with docstring and body returns T"
    (ok (utils:has-docstring-p
         (parse-expr "(defmacro when+ (c &body body) \"Execute BODY when C.\" `(when ,c ,@body))"))))

  (testing "defmacro without docstring returns NIL"
    (ok (null (utils:has-docstring-p
               (parse-expr "(defmacro when+ (c &body body) `(when ,c ,@body))")))))

  (testing "defun with docstring containing a colon returns T"
    (ok (utils:has-docstring-p
         (parse-expr "(defun f (x) \"Foo: returns x.\" x)")))))

;;; has-docstring-p for defgeneric

(deftest has-docstring-defgeneric-tests
  (testing "defgeneric with :documentation option returns T"
    (ok (utils:has-docstring-p
         (parse-expr "(defgeneric serialize (object)
  (:documentation \"Serialize OBJECT to string.\"))"))))

  (testing "defgeneric without :documentation returns NIL"
    (ok (null (utils:has-docstring-p
               (parse-expr "(defgeneric serialize (object) (:method-combination +))")))))

  (testing "defgeneric with only (:generic-function-class ...) returns NIL"
    (ok (null (utils:has-docstring-p
               (parse-expr "(defgeneric foo (x) (:generic-function-class my-gf))")))))

  (testing "defgeneric with both :documentation and other options returns T"
    (ok (utils:has-docstring-p
         (parse-expr "(defgeneric foo (x)
  (:method-combination +)
  (:documentation \"Foo.\"))")))))

;;; has-docstring-p for defclass

(deftest has-docstring-defclass-tests
  (testing "defclass with :documentation option returns T"
    (ok (utils:has-docstring-p
         (parse-expr "(defclass point ()
  ((x :initarg :x)
   (y :initarg :y))
  (:documentation \"A 2D point.\"))"))))

  (testing "defclass without :documentation returns NIL"
    (ok (null (utils:has-docstring-p
               (parse-expr "(defclass point ()
  ((x :initarg :x)))")))))

  (testing "defclass with non-empty superclass list and :documentation returns T"
    (ok (utils:has-docstring-p
         (parse-expr "(defclass child (parent)
  ()
  (:documentation \"A child class.\"))"))))

  (testing "defclass with non-empty superclass list without :documentation returns NIL"
    (ok (null (utils:has-docstring-p
               (parse-expr "(defclass child (parent) () ())")))))

;;; has-docstring-p for defmethod

(deftest has-docstring-defmethod-tests
  (testing "defmethod always returns T (never checked)"
    (ok (utils:has-docstring-p
         (parse-expr "(defmethod add ((x integer) (y integer)) (+ x y))"))))

  (testing "defmethod without docstring still returns T"
    (ok (utils:has-docstring-p
         (parse-expr "(defmethod foo :before (x) (bar x))")))))

;;; definition-name

(deftest definition-name-tests
  (testing "defun name is extracted"
    (let ((name (utils:definition-name (parse-expr "(defun my-func (x) x)"))))
      (ok (stringp name))
      (ok (string-equal name "my-func"))))

  (testing "defmacro name is extracted"
    (let ((name (utils:definition-name (parse-expr "(defmacro my-macro (x) x)"))))
      (ok (stringp name))
      (ok (string-equal name "my-macro"))))

  (testing "defgeneric name is extracted"
    (let ((name (utils:definition-name (parse-expr "(defgeneric my-generic (x))"))))
      (ok (stringp name))
      (ok (string-equal name "my-generic"))))

  (testing "defclass name is extracted"
    (let ((name (utils:definition-name (parse-expr "(defclass my-class () ())"))))
      (ok (stringp name))
      (ok (string-equal name "my-class"))))

  (testing "defmethod name is extracted"
    (let ((name (utils:definition-name (parse-expr "(defmethod my-method (x) x)"))))
      (ok (stringp name))
      (ok (string-equal name "my-method"))))

  (testing "setf function name is extracted"
    (let ((name (utils:definition-name (parse-expr "(defun (setf my-slot) (v o) v)"))))
      (ok (stringp name))
      (ok (string-equal name "(setf my-slot)")))))
