(defpackage #:malvolio/tests/rules/comment-level
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:malvolio/rules)
   (#:parser #:malvolio/parser)
   (#:violation #:malvolio/violation)))
(in-package #:malvolio/tests/rules/comment-level)

(deftest inline-comments
  (failing "Single semicolon inline comment is correct"
    (let ((text "(defun foo () 42) ; inline comment")
          (file #p"/tmp/test.lisp")
          (rule (make-instance 'rules:comment-level-rule)))
      (let ((tokens (parser:tokenize text file)))
        (let ((violations (rules:check-tokens rule tokens file)))
          (ok (null violations))))))

  (failing "Two semicolons inline is incorrect"
    (let ((text "(defun foo () 42) ;; wrong inline comment")
          (file #p"/tmp/test.lisp")
          (rule (make-instance 'rules:comment-level-rule)))
      (let ((tokens (parser:tokenize text file)))
        (let ((violations (rules:check-tokens rule tokens file)))
          (ok (= 1 (length violations)))
          (let ((v (first violations)))
            (ok (eq :comment-level (violation:violation-rule v)))))))))

(deftest line-comments
  (failing "Two semicolons for line comment is correct"
    (let ((text "(defun foo ()
  ;; This is a line comment
  42)")
          (file #p"/tmp/test.lisp")
          (rule (make-instance 'rules:comment-level-rule)))
      (let ((tokens (parser:tokenize text file)))
        (let ((violations (rules:check-tokens rule tokens file)))
          (ok (null violations))))))

  (failing "Single semicolon for line comment is incorrect"
    (let ((text "(defun foo ()
  ; wrong line comment
  42)")
          (file #p"/tmp/test.lisp")
          (rule (make-instance 'rules:comment-level-rule)))
      (let ((tokens (parser:tokenize text file)))
        (let ((violations (rules:check-tokens rule tokens file)))
          (ok (= 1 (length violations)))
          (let ((v (first violations)))
            (ok (eq :comment-level (violation:violation-rule v)))))))))

(deftest section-comments
  (failing "Three semicolons for section comment is correct"
    (let ((text ";;; Section comment

(defun foo () 42)")
          (file #p"/tmp/test.lisp")
          (rule (make-instance 'rules:comment-level-rule)))
      (let ((tokens (parser:tokenize text file)))
        (let ((violations (rules:check-tokens rule tokens file)))
          (ok (null violations))))))

  (failing "Two semicolons for section comment is incorrect"
    (let ((text ";; wrong section comment

(defun foo () 42)")
          (file #p"/tmp/test.lisp")
          (rule (make-instance 'rules:comment-level-rule)))
      (let ((tokens (parser:tokenize text file)))
        (let ((violations (rules:check-tokens rule tokens file)))
          (ok (= 1 (length violations))))))))

(deftest file-level-comments
  (failing "Four semicolons for file-level comment is correct"
    (let ((text ";;;; File-level comment

(defpackage #:foo (:use #:cl))")
          (file #p"/tmp/test.lisp")
          (rule (make-instance 'rules:comment-level-rule)))
      (let ((tokens (parser:tokenize text file)))
        (let ((violations (rules:check-tokens rule tokens file)))
          (ok (null violations)))))))
