(defpackage #:malo/tests/rules/naming
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:malo/rules)
   (#:parser #:malo/parser)
   (#:violation #:malo/violation)))
(in-package #:malo/tests/rules/naming)

;;; Special variable naming tests

(deftest special-variable-naming-valid
  (testing "Valid special variable names with earmuffs"
    (let ((rule (make-instance 'rules:special-variable-naming-rule))
          (file (uiop:parse-native-namestring "test.lisp")))

      (testing "*name* pattern is valid"
        (let* ((text "(defvar *my-var* 42)")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (null violations))))

      (testing "defparameter with *name* is valid"
        (let* ((text "(defparameter *config* nil)")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (null violations))))))

  (testing "Non-special variables are ignored"
    (let ((rule (make-instance 'rules:special-variable-naming-rule))
          (file (uiop:parse-native-namestring "test.lisp")))

      (testing "defun is ignored"
        (let* ((text "(defun foo () 42)")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (null violations))))

      (testing "defconstant is ignored"
        (let* ((text "(defconstant +foo+ 42)")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (null violations)))))))

(deftest special-variable-naming-invalid
  (testing "Invalid special variable names without earmuffs"
    (let ((rule (make-instance 'rules:special-variable-naming-rule))
          (file (uiop:parse-native-namestring "test.lisp")))

      (testing "defvar without *name* violates"
        (let* ((text "(defvar my-var 42)")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1))
          (ok (eq (violation:violation-rule (first violations)) :special-variable-naming))
          (ok (string= (violation:violation-message (first violations))
                       "Special variable 'my-var' should be named *my-var*"))))

      (testing "defparameter without *name* violates"
        (let* ((text "(defparameter config nil)")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1))
          (ok (eq (violation:violation-rule (first violations)) :special-variable-naming))))

      (testing "Only leading * violates"
        (let* ((text "(defvar *config 42)")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1))))

      (testing "Only trailing * violates"
        (let* ((text "(defvar config* 42)")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1)))))))

;;; Constant naming tests

(deftest constant-naming-valid
  (testing "Valid constant names with plus signs"
    (let ((rule (make-instance 'rules:constant-naming-rule))
          (file (uiop:parse-native-namestring "test.lisp")))

      (testing "+name+ pattern is valid"
        (let* ((text "(defconstant +my-const+ 42)")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (null violations))))

      (testing "define-constant with +name+ is valid"
        (let* ((text "(define-constant +config+ 42)")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (null violations))))))

  (testing "Non-constants are ignored"
    (let ((rule (make-instance 'rules:constant-naming-rule))
          (file (uiop:parse-native-namestring "test.lisp")))

      (testing "defvar is ignored"
        (let* ((text "(defvar *foo* 42)")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (null violations)))))))

(deftest constant-naming-invalid
  (testing "Invalid constant names without plus signs"
    (let ((rule (make-instance 'rules:constant-naming-rule))
          (file (uiop:parse-native-namestring "test.lisp")))

      (testing "defconstant without +name+ violates"
        (let* ((text "(defconstant my-const 42)")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1))
          (ok (eq (violation:violation-rule (first violations)) :constant-naming))
          (ok (string= (violation:violation-message (first violations))
                       "Constant 'my-const' should be named +my-const+"))))

      (testing "define-constant without +name+ violates"
        (let* ((text "(define-constant config 42)")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1))
          (ok (eq (violation:violation-rule (first violations)) :constant-naming))))

      (testing "Only leading + violates"
        (let* ((text "(defconstant +config 42)")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1))))

      (testing "Only trailing + violates"
        (let* ((text "(defconstant config+ 42)")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1)))))))
