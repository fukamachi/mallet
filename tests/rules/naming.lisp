(defpackage #:mallet/tests/rules/naming
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/naming)

(defun check-naming-code (rule code)
  (let* ((file (uiop:parse-native-namestring "test.lisp"))
         (forms (parser:parse-forms code file)))
    (rules:check-form rule (first forms) file)))

;;; Special variable naming tests

(deftest special-variable-naming-valid
  (testing "Valid special variable names with earmuffs"
    (let ((rule (make-instance 'rules:special-variable-naming-rule)))
      (dolist (case '(("defvar" "(defvar *my-var* 42)")
                      ("defparameter" "(defparameter *config* nil)")
                      ("sb-ext:defglobal with earmuffs" "(sb-ext:defglobal *foo* 42)")
                      ("sb-ext:defglobal with plus signs" "(sb-ext:defglobal +foo+ 42)")))
        (destructuring-bind (description code) case
          (testing description
            (ok (null (check-naming-code rule code))))))))

  (testing "Non-special variables are ignored"
    (let ((rule (make-instance 'rules:special-variable-naming-rule)))

      (testing "defun is ignored"
        (ok (null (check-naming-code rule "(defun foo () 42)"))))

      (testing "defconstant is ignored"
        (ok (null (check-naming-code rule "(defconstant +foo+ 42)")))))))

(deftest special-variable-naming-invalid
  (testing "Invalid special variable names without earmuffs"
    (let ((rule (make-instance 'rules:special-variable-naming-rule)))
      (dolist (case '(("defvar bare name" "(defvar my-var 42)"
                       "Special variable 'my-var' should be named *my-var*")
                      ("defvar leading star only" "(defvar *config 42)" nil)
                      ("defvar trailing star only" "(defvar config* 42)" nil)
                      ("defparameter bare name" "(defparameter config nil)" nil)
                      ("defparameter leading star only" "(defparameter *config nil)" nil)
                      ("defparameter trailing star only" "(defparameter config* nil)" nil)
                      ("sb-ext:defglobal bare name" "(sb-ext:defglobal foo 42)" nil)
                      ("sb-ext:defglobal leading star only" "(sb-ext:defglobal *foo 42)" nil)
                      ("sb-ext:defglobal trailing star only" "(sb-ext:defglobal foo* 42)" nil)))
        (destructuring-bind (description code expected-message) case
          (testing description
            (let ((violations (check-naming-code rule code)))
              (ok (= (length violations) 1))
              (ok (eq (violation:violation-rule (first violations))
                      :special-variable-naming))
              (when expected-message
                (ok (string= (violation:violation-message (first violations))
                             expected-message))))))))))

;;; Constant naming tests

(deftest constant-naming-valid
  (testing "Valid constant names with plus signs"
    (let ((rule (make-instance 'rules:constant-naming-rule)))
      (dolist (case '(("defconstant" "(defconstant +my-const+ 42)")
                      ("define-constant" "(define-constant +config+ 42)")))
        (destructuring-bind (description code) case
          (testing description
            (ok (null (check-naming-code rule code))))))))

  (testing "Non-constants are ignored"
    (let ((rule (make-instance 'rules:constant-naming-rule)))

      (testing "defvar is ignored"
        (ok (null (check-naming-code rule "(defvar *foo* 42)")))))))

(deftest constant-naming-invalid
  (testing "Invalid constant names without plus signs"
    (let ((rule (make-instance 'rules:constant-naming-rule)))
      (dolist (case '(("defconstant bare name" "(defconstant my-const 42)"
                       "Constant 'my-const' should be named +my-const+")
                      ("defconstant leading plus only" "(defconstant +config 42)" nil)
                      ("defconstant trailing plus only" "(defconstant config+ 42)" nil)
                      ("define-constant bare name" "(define-constant config 42)" nil)
                      ("define-constant leading plus only" "(define-constant +config 42)" nil)
                      ("define-constant trailing plus only" "(define-constant config+ 42)" nil)))
        (destructuring-bind (description code expected-message) case
          (testing description
            (let ((violations (check-naming-code rule code)))
              (ok (= (length violations) 1))
              (ok (eq (violation:violation-rule (first violations))
                      :constant-naming))
              (when expected-message
                (ok (string= (violation:violation-message (first violations))
                             expected-message))))))))))
