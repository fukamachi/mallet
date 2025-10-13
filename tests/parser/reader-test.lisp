(defpackage #:mallet/tests/parser/reader
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:parser #:mallet/parser)))
(in-package #:mallet/tests/parser/reader)

(deftest parse-simple-forms
  (testing "Single number"
    (let* ((text "42")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (ok (eql 42 (parser:form-expr (first forms))))
      (ok (= 1 (parser:form-line (first forms))))))

  (testing "Single symbol"
    (let* ((text "foo")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (ok (stringp (parser:form-expr (first forms))))
      (ok (search "foo" (string-downcase (parser:form-expr (first forms)))))))

  (testing "Simple list"
    (let* ((text "(+ 1 2)")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (let ((form (first forms)))
        (ok (consp (parser:form-expr form)))
        (ok (stringp (first (parser:form-expr form))))
        (ok (search "+" (string-downcase (first (parser:form-expr form)))))
        (ok (= 1 (parser:form-line form)))
        (ok (= 0 (parser:form-column form)))))))

(deftest parse-multiple-forms
  (testing "Two forms on separate lines"
    (let* ((text "(defun foo ())
(defun bar ())")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 2 (length forms)))
      (ok (= 1 (parser:form-line (first forms))))
      (ok (= 2 (parser:form-line (second forms))))))

  (testing "Multiple forms with comments"
    (let* ((text "; comment
(+ 1 2)
;;; section
(* 3 4)")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 2 (length forms)))
      (ok (= 2 (parser:form-line (first forms))))
      (ok (= 4 (parser:form-line (second forms)))))))

(deftest parse-nested-forms
  (testing "Nested lists"
    (let* ((text "(defun foo () (+ 1 2))")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (let* ((form (first forms))
             (expr (parser:form-expr form)))
        (ok (stringp (first expr)))
        (ok (search "defun" (string-downcase (first expr))))
        (ok (stringp (second expr)))
        (ok (search "foo" (string-downcase (second expr))))
        (ok (consp (fourth expr)))))))

(deftest parse-special-forms
  (testing "Quoted form"
    (let* ((text "'(1 2 3)")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (let ((expr (parser:form-expr (first forms))))
        (ok (eq 'quote (first expr))))))

  (testing "Backquoted form"
    (let* ((text "`(a ,b ,@c)")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (ok (consp (parser:form-expr (first forms)))))))

(deftest parse-strings-and-numbers
  (testing "String literal"
    (let* ((text "\"hello world\"")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (ok (string= "hello world" (parser:form-expr (first forms))))))

  (testing "Various number types"
    (let* ((text "42 3.14 1/2 #x2A")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 4 (length forms)))
      (ok (eql 42 (parser:form-expr (first forms))))
      (ok (= 3.14 (parser:form-expr (second forms))))
      (ok (eql 1/2 (parser:form-expr (third forms))))
      (ok (eql 42 (parser:form-expr (fourth forms)))))))

(deftest parse-with-source-tracking
  (testing "Source text extraction"
    (let* ((text "(defun foo ())")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (let ((form (first forms)))
        (ok (stringp (parser:form-source form)))
        (ok (string= "(defun foo ())" (parser:form-source form)))))))

(deftest parse-sbcl-character-names
  (testing "ASCII control characters (codes 0-31)"
    ;; Test NUL variants
    (let* ((text "(list #\\Nul #\\Null)")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (let ((expr (parser:form-expr (first forms))))
        (ok (consp expr))
        (ok (string-equal "CURRENT:list" (first expr)))
        (ok (characterp (second expr)))
        (ok (= 0 (char-code (second expr))))
        (ok (characterp (third expr)))
        (ok (= 0 (char-code (third expr))))))

    ;; Test Soh through Bel (codes 1-7)
    (let* ((text "(list #\\Soh #\\Stx #\\Etx #\\Eot #\\Enq #\\Ack #\\Bel)")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (let ((expr (parser:form-expr (first forms))))
        (ok (consp expr))
        (ok (string-equal "CURRENT:list" (first expr)))
        (loop for i from 1 to 7
              for char in (rest expr)
              do (progn
                   (ok (characterp char))
                   (ok (= i (char-code char)))))))

    ;; Test Vt, Ff, Cr, So, Si (codes 11-15)
    (let* ((text "(list #\\Vt #\\Ff #\\Cr #\\So #\\Si)")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (let ((expr (parser:form-expr (first forms))))
        (ok (consp expr))
        (ok (string-equal "CURRENT:list" (first expr)))
        (ok (= 11 (char-code (second expr))))
        (ok (= 12 (char-code (third expr))))
        (ok (= 13 (char-code (fourth expr))))
        (ok (= 14 (char-code (fifth expr))))
        (ok (= 15 (char-code (sixth expr))))))

    ;; Test Dle through Sub (codes 16-26)
    (let* ((text "(list #\\Dle #\\Dc1 #\\Dc2 #\\Dc3 #\\Dc4 #\\Nak #\\Syn #\\Etb #\\Can #\\Em #\\Sub)")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (let ((expr (parser:form-expr (first forms))))
        (ok (consp expr))
        (ok (string-equal "CURRENT:list" (first expr)))
        (loop for i from 16 to 26
              for char in (rest expr)
              do (progn
                   (ok (characterp char))
                   (ok (= i (char-code char)))))))

    ;; Test ESC variants (code 27)
    (let* ((text "(list #\\Esc #\\Escape)")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (let ((expr (parser:form-expr (first forms))))
        (ok (consp expr))
        (ok (string-equal "CURRENT:list" (first expr)))
        (ok (= 27 (char-code (second expr))))
        (ok (= 27 (char-code (third expr))))))

    ;; Test Fs, Gs, Rs, Us (codes 28-31)
    (let* ((text "(list #\\Fs #\\Gs #\\Rs #\\Us)")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (let ((expr (parser:form-expr (first forms))))
        (ok (consp expr))
        (ok (string-equal "CURRENT:list" (first expr)))
        (ok (= 28 (char-code (second expr))))
        (ok (= 29 (char-code (third expr))))
        (ok (= 30 (char-code (fourth expr))))
        (ok (= 31 (char-code (fifth expr)))))))

  (testing "Standard character names"
    (let* ((text "(list #\\Backspace #\\Tab #\\Newline #\\Page #\\Return #\\Space #\\Rubout)")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (let ((expr (parser:form-expr (first forms))))
        (ok (consp expr))
        (ok (string-equal "CURRENT:list" (first expr)))
        (ok (= 8 (char-code (second expr))))
        (ok (= 9 (char-code (third expr))))
        (ok (= 10 (char-code (fourth expr))))
        (ok (= 12 (char-code (fifth expr))))
        (ok (= 13 (char-code (sixth expr))))
        (ok (= 32 (char-code (seventh expr))))
        (ok (= 127 (char-code (eighth expr)))))))

  (testing "Unicode characters with variable-length hex codes"
    ;; Test 2-digit hex
    (let* ((text "(list #\\u80 #\\uFF)")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (let ((expr (parser:form-expr (first forms))))
        (ok (consp expr))
        (ok (string-equal "CURRENT:list" (first expr)))
        (ok (= #x80 (char-code (second expr))))
        (ok (= #xFF (char-code (third expr))))))

    ;; Test 3-digit hex
    (let* ((text "(list #\\u100 #\\ufff)")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (let ((expr (parser:form-expr (first forms))))
        (ok (consp expr))
        (ok (string-equal "CURRENT:list" (first expr)))
        (ok (= #x100 (char-code (second expr))))
        (ok (= #xfff (char-code (third expr))))))

    ;; Test 4-digit hex (umbrella ☂)
    (let* ((text "(list #\\u2602 #\\u00A0)")
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (let ((expr (parser:form-expr (first forms))))
        (ok (consp expr))
        (ok (string-equal "CURRENT:list" (first expr)))
        (ok (= #x2602 (char-code (second expr))))
        (ok (= #xA0 (char-code (third expr))))))

    ;; Test uppercase U with 8-digit hex
    (let* ((text "#\\U0001F600")  ; 😀 emoji
           (forms (parser:parse-forms text #P"test.lisp")))
      (ok (= 1 (length forms)))
      (let ((expr (parser:form-expr (first forms))))
        (ok (characterp expr))
        (ok (= #x1F600 (char-code expr)))))))
