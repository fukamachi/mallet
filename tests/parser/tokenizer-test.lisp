(defpackage #:malo/tests/parser/tokenizer
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:parser #:malo/parser)))
(in-package #:malo/tests/parser/tokenizer)

(deftest tokenize-simple-comment
  (testing "Single semicolon inline comment"
    (let* ((text "; inline comment")
           (tokens (parser:tokenize text #P"test.lisp")))
      (ok (= 1 (length tokens)))
      (ok (eq :comment-inline (parser:token-type (first tokens))))
      (ok (string= "inline comment" (parser:token-value (first tokens))))
      (ok (= 1 (parser:token-line (first tokens))))
      (ok (= 0 (parser:token-column (first tokens))))))

  (testing "Double semicolon line comment"
    (let* ((text ";; line comment")
           (tokens (parser:tokenize text #P"test.lisp")))
      (ok (= 1 (length tokens)))
      (ok (eq :comment-line (parser:token-type (first tokens))))
      (ok (string= "line comment" (parser:token-value (first tokens))))))

  (testing "Triple semicolon section comment"
    (let* ((text ";;; section comment")
           (tokens (parser:tokenize text #P"test.lisp")))
      (ok (= 1 (length tokens)))
      (ok (eq :comment-section (parser:token-type (first tokens))))
      (ok (string= "section comment" (parser:token-value (first tokens))))))

  (testing "Quadruple semicolon file comment"
    (let* ((text ";;;; file comment")
           (tokens (parser:tokenize text #P"test.lisp")))
      (ok (= 1 (length tokens)))
      (ok (eq :comment-file (parser:token-type (first tokens))))
      (ok (string= "file comment" (parser:token-value (first tokens)))))))

(deftest tokenize-parentheses
  (testing "Open and close parens"
    (let* ((text "()")
           (tokens (parser:tokenize text #P"test.lisp")))
      (ok (= 2 (length tokens)))
      (ok (eq :open-paren (parser:token-type (first tokens))))
      (ok (eq :close-paren (parser:token-type (second tokens)))))))

(deftest tokenize-simple-form
  (testing "Simple list"
    (let* ((text "(+ 1 2)")
           (tokens (parser:tokenize text #P"test.lisp")))
      (ok (>= (length tokens) 5))
      (ok (eq :open-paren (parser:token-type (first tokens))))
      (ok (eq :close-paren (parser:token-type (car (last tokens))))))))

(deftest tokenize-with-comments
  (testing "Code with inline comment"
    (let* ((text "(defun foo () 42) ; inline")
           (tokens (parser:tokenize text #P"test.lisp")))
      (ok (find :comment-inline tokens :key #'parser:token-type))))

  (testing "Code with section comment"
    (let* ((text ";;; Section\n(defun foo ())")
           (tokens (parser:tokenize text #P"test.lisp")))
      (ok (find :comment-section tokens :key #'parser:token-type)))))
