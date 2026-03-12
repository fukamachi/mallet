(defpackage #:mallet/tests/rules/missing-docstring
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/missing-docstring)

(defun check-missing-docstring (code)
  "Check CODE for missing-docstring violations."
  (let ((forms (parser:parse-forms code #p"test.lisp"))
        (rule (make-instance 'rules:missing-docstring-rule)))
    (mapcan (lambda (form)
              (rules:check-form rule form #p"test.lisp"))
            forms)))

;;; Valid cases (no violations)

(deftest missing-docstring-valid
  (testing "defun with docstring is not flagged"
    (ok (null (check-missing-docstring
               "(defun add (x y) \"Add X and Y.\" (+ x y))"))))

  (testing "defmacro with docstring is not flagged"
    (ok (null (check-missing-docstring
               "(defmacro when+ (c &body b) \"Execute B when C.\" `(when ,c ,@b))"))))

  (testing "defgeneric with :documentation is not flagged"
    (ok (null (check-missing-docstring
               "(defgeneric serialize (obj)
  (:documentation \"Serialize OBJ.\"))"))))

  (testing "defclass with :documentation is not flagged"
    (ok (null (check-missing-docstring
               "(defclass point ()
  ((x :initarg :x))
  (:documentation \"A 2D point.\"))"))))

  (testing "defmethod is never flagged (skip entirely)"
    (ok (null (check-missing-docstring
               "(defmethod add ((x integer) (y integer)) (+ x y))"))))

  (testing "defmethod without docstring is still not flagged"
    (ok (null (check-missing-docstring
               "(defmethod foo :before (x) (bar x))"))))

  (testing "defvar is not a definition form - not flagged"
    (ok (null (check-missing-docstring
               "(defvar *x* 42)")))))

;;; Invalid cases (violations expected)

(deftest missing-docstring-invalid
  (testing "defun with single-string body is flagged (string is return value, not docstring)"
    (let ((violations (check-missing-docstring "(defun greeting () \"hello\")")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :missing-docstring))))

  (testing "defun with no body is flagged (no docstring possible)"
    (let ((violations (check-missing-docstring "(defun foo ())")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :missing-docstring))))

  (testing "defun without docstring is flagged"
    (let ((violations (check-missing-docstring
                       "(defun add (x y) (+ x y))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :missing-docstring))
      (ok (eq (violation:violation-severity (first violations)) :info))))

  (testing "defmacro without docstring is flagged"
    (let ((violations (check-missing-docstring
                       "(defmacro when+ (c &body b) `(when ,c ,@b))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :missing-docstring))))

  (testing "defgeneric without :documentation is flagged"
    (let ((violations (check-missing-docstring
                       "(defgeneric serialize (obj))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :missing-docstring))))

  (testing "defclass without :documentation is flagged"
    (let ((violations (check-missing-docstring
                       "(defclass point ()
  ((x :initarg :x)))")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :missing-docstring)))))

;;; Violation message format

(deftest missing-docstring-message-format
  (testing "defun violation message includes DEFUN and function name"
    (let ((violations (check-missing-docstring
                       "(defun my-func (x) (+ x 1))")))
      (ok (= (length violations) 1))
      (let ((msg (violation:violation-message (first violations))))
        (ok (stringp msg))
        (ok (search "DEFUN" msg))
        (ok (search "my-func" msg)))))

  (testing "defmacro violation message includes DEFMACRO and macro name"
    (let ((violations (check-missing-docstring
                       "(defmacro my-macro (x) x)")))
      (ok (= (length violations) 1))
      (let ((msg (violation:violation-message (first violations))))
        (ok (search "DEFMACRO" msg))
        (ok (search "my-macro" msg)))))

  (testing "defgeneric violation message includes DEFGENERIC and name"
    (let ((violations (check-missing-docstring
                       "(defgeneric my-gf (x))")))
      (ok (= (length violations) 1))
      (let ((msg (violation:violation-message (first violations))))
        (ok (search "DEFGENERIC" msg))
        (ok (search "my-gf" msg)))))

  (testing "defclass violation message includes DEFCLASS and name"
    (let ((violations (check-missing-docstring
                       "(defclass my-class () ())")))
      (ok (= (length violations) 1))
      (let ((msg (violation:violation-message (first violations))))
        (ok (search "DEFCLASS" msg))
        (ok (search "my-class" msg))))))

;;; Location reporting

(deftest missing-docstring-location
  (testing "violation reports line 1 for single-line form"
    (let ((violations (check-missing-docstring
                       "(defun foo (x) x)")))
      (ok (= (length violations) 1))
      (ok (= (violation:violation-line (first violations)) 1))))

  (testing "violation reports correct line for multi-line code"
    (let ((violations (check-missing-docstring
                       "(defun foo (x) x)
(defun bar (y)
  y)")))
      (ok (= (length violations) 2))
      (ok (= (violation:violation-line (first violations)) 1))
      (ok (= (violation:violation-line (second violations)) 2)))))

;;; Multiple forms

(deftest missing-docstring-multiple-forms
  (testing "multiple missing docstrings all flagged"
    (let ((violations (check-missing-docstring
                       "(defun foo (x) x)
(defun bar (y) y)
(defun baz (z) z)")))
      (ok (= (length violations) 3))))

  (testing "mix of documented and undocumented"
    (let ((violations (check-missing-docstring
                       "(defun foo (x) \"Documented.\" x)
(defun bar (y) y)")))
      (ok (= (length violations) 1))
      (ok (search "bar" (violation:violation-message (first violations)))))))
