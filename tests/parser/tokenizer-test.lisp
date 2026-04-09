(defpackage #:mallet/tests/parser/tokenizer
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:parser #:mallet/parser)))
(in-package #:mallet/tests/parser/tokenizer)

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

(deftest tokenize-single-escape
  (testing "Character literal #\\\" is a single token"
    (let* ((text "(char= ch #\\\")")
           (tokens (parser:tokenize text #P"test.lisp"))
           (types (mapcar #'parser:token-type tokens)))
      (ok (equal types '(:open-paren :symbol :symbol :symbol :close-paren)))
      (ok (string= "#\\\"" (parser:token-raw (fourth tokens))))))

  (testing "Character literal #\\( is a single token"
    (let* ((text "(char= ch #\\()")
           (tokens (parser:tokenize text #P"test.lisp"))
           (types (mapcar #'parser:token-type tokens)))
      (ok (equal types '(:open-paren :symbol :symbol :symbol :close-paren)))
      (ok (string= "#\\(" (parser:token-raw (fourth tokens)))))))

(deftest tokenize-escaped-quotes-in-strings
  (testing "Escaped quote within string"
    (let* ((text "(format nil \"hello \\\"world\\\"\")")
           (tokens (parser:tokenize text #P"test.lisp"))
           (string-tokens (remove-if-not (lambda (tok)
                                           (eq (parser:token-type tok) :string))
                                         tokens)))
      (ok (= 1 (length string-tokens)))))

  (testing "Escaped backslash before closing quote"
    (let* ((text "(print \"ends with backslash\\\\\")")
           (tokens (parser:tokenize text #P"test.lisp"))
           (string-tokens (remove-if-not (lambda (tok)
                                           (eq (parser:token-type tok) :string))
                                         tokens)))
      (ok (= 1 (length string-tokens))))))

(deftest tokenize-multiline-string-line-tracking
  (testing "Tokens after a multi-line string report correct line numbers"
    ;; Regression test: previously the tokenizer did not update the line
    ;; counter for newlines inside string literals, causing every token
    ;; after a multi-line docstring to report a too-low line number.
    (let* ((text (format nil "(defvar *x* \"line1~%line2~%line3\")~%foo"))
           (tokens (parser:tokenize text #P"test.lisp"))
           (foo (find-if (lambda (tok)
                           (and (eq (parser:token-type tok) :symbol)
                                (string= (parser:token-raw tok) "foo")))
                         tokens)))
      (ok foo)
      (ok (= 4 (parser:token-line foo)))
      (ok (= 0 (parser:token-column foo)))))

  (testing "Column resets correctly after multi-line string"
    (let* ((text (format nil "\"a~%bc\" sym"))
           (tokens (parser:tokenize text #P"test.lisp"))
           (sym (find-if (lambda (tok)
                           (and (eq (parser:token-type tok) :symbol)
                                (string= (parser:token-raw tok) "sym")))
                         tokens)))
      (ok sym)
      (ok (= 2 (parser:token-line sym)))
      ;; "bc" + closing quote = 3 chars on line 2, then space, then sym at col 4
      (ok (= 4 (parser:token-column sym))))))
