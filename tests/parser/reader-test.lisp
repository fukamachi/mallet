(defpackage #:malvolio/tests/parser/reader
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:reader #:malvolio/parser/reader)
   (#:parser #:malvolio/parser)))
(in-package #:malvolio/tests/parser/reader)

(deftest parse-simple-forms
  (testing "Single number"
    (let* ((text "42")
           (forms (reader:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (ok (eql 42 (parser:form-expr (first forms))))
      (ok (= 1 (parser:form-line (first forms))))))

  (testing "Single symbol"
    (let* ((text "foo")
           (forms (reader:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (ok (eq 'foo (parser:form-expr (first forms))))))

  (testing "Simple list"
    (let* ((text "(+ 1 2)")
           (forms (reader:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (let ((form (first forms)))
        (ok (consp (parser:form-expr form)))
        (ok (eq '+ (first (parser:form-expr form))))
        (ok (= 1 (parser:form-line form)))
        (ok (= 0 (parser:form-column form)))))))

(deftest parse-multiple-forms
  (failing "Two forms on separate lines"
    (let* ((text "(defun foo ())\n(defun bar ())")
           (forms (reader:parse-forms text #P"test.lisp")))
      (ok (= 2 (length forms)))
      (ok (= 1 (parser:form-line (first forms))))
      (ok (= 2 (parser:form-line (second forms))))))

  (failing "Multiple forms with comments"
    (let* ((text "; comment\n(+ 1 2)\n;;; section\n(* 3 4)")
           (forms (reader:parse-forms text #P"test.lisp")))
      (ok (= 2 (length forms)))
      (ok (= 2 (parser:form-line (first forms))))
      (ok (= 4 (parser:form-line (second forms)))))))

(deftest parse-nested-forms
  (testing "Nested lists"
    (let* ((text "(defun foo () (+ 1 2))")
           (forms (reader:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (let* ((form (first forms))
             (expr (parser:form-expr form)))
        (ok (eq 'defun (first expr)))
        (ok (eq 'foo (second expr)))
        (ok (consp (fourth expr)))))))

(deftest parse-special-forms
  (testing "Quoted form"
    (let* ((text "'(1 2 3)")
           (forms (reader:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (let ((expr (parser:form-expr (first forms))))
        (ok (eq 'quote (first expr))))))

  (testing "Backquoted form"
    (let* ((text "`(a ,b ,@c)")
           (forms (reader:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (ok (consp (parser:form-expr (first forms)))))))

(deftest parse-strings-and-numbers
  (testing "String literal"
    (let* ((text "\"hello world\"")
           (forms (reader:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (ok (string= "hello world" (parser:form-expr (first forms))))))

  (testing "Various number types"
    (let* ((text "42 3.14 1/2 #x2A")
           (forms (reader:parse-forms text #P"test.lisp")))
      (ok (= 4 (length forms)))
      (ok (eql 42 (parser:form-expr (first forms))))
      (ok (= 3.14 (parser:form-expr (second forms))))
      (ok (eql 1/2 (parser:form-expr (third forms))))
      (ok (eql 42 (parser:form-expr (fourth forms)))))))

(deftest parse-with-source-tracking
  (testing "Source text extraction"
    (let* ((text "(defun foo ())")
           (forms (reader:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (let ((form (first forms)))
        (ok (stringp (parser:form-source form)))
        (ok (string= "(defun foo ())" (parser:form-source form)))))))
