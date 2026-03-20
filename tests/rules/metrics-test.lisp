(defpackage #:mallet/tests/rules/metrics
  (:use #:cl #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules/forms/metrics)
   (#:all-rules #:mallet/rules)
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/metrics)

;;; Test infrastructure helpers

(defun test-no-violation (rule code)
  "Test that CODE does not violate RULE."
  (let* ((forms (parser:parse-forms code #P"test.lisp"))
         (form (first forms))
         (violations (base:check-form rule form #P"test.lisp")))
    (ok (null violations))))

(defun test-violation (rule code expected-message-fragment)
  "Test that CODE violates RULE with message containing EXPECTED-MESSAGE-FRAGMENT."
  (let* ((forms (parser:parse-forms code #P"test.lisp"))
         (form (first forms))
         (violations (base:check-form rule form #P"test.lisp")))
    (ok (= 1 (length violations)))
    (when (first violations)
      (ok (search expected-message-fragment
                  (violation:violation-message (first violations)))))))

;;; Function-length tests

(deftest function-length-one-line
  (testing "One-line function should have length 1"
    (let* ((rule (make-instance 'rules:function-length-rule :max 0))
           (code "(defun foo (x) (+ x 1))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      (ok (= 1 (length violations)))
      (ok (search "1 lines" (violation:violation-message (first violations)))))))

(deftest function-length-empty
  (testing "Empty function should have length 1"
    (let* ((rule (make-instance 'rules:function-length-rule :max 0))
           (code "(defun empty ())")
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      (ok (= 1 (length violations)))
      (ok (search "1 lines" (violation:violation-message (first violations)))))))

(deftest function-length-short-defun
  (testing "Short function (10 lines) should not violate"
    (let* ((rule (make-instance 'rules:function-length-rule))
           (code "(defun foo (x)
  \"Docstring.\"
  (let ((y (* x 2)))
    (when (> y 0)
      (print y)
      (+ y 1))))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      (ok (null violations)))))

(deftest function-length-long-defun
  (testing "Long function (60 lines) should violate"
    (let* ((rule (make-instance 'rules:function-length-rule :max 50))
           ;; Generate a function with 60 lines
           (code (format nil "(defun long-function (x)~%~{  (print ~D)~%~})"
                         (loop for i from 1 to 59 collect i)))
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      (ok (= 1 (length violations)))
      (ok (search "60 lines" (violation:violation-message (first violations)))))))

(deftest function-length-at-limit
  (testing "Function exactly at limit (50 lines) should not violate"
    (let* ((rule (make-instance 'rules:function-length-rule :max 50))
           ;; Generate a function with exactly 50 lines
           (code (format nil "(defun at-limit (x)~%~{  (print ~D)~%~})"
                         (loop for i from 1 to 49 collect i)))
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      (ok (null violations)))))

(deftest function-length-defmethod
  (testing "defmethod is counted"
    (let* ((rule (make-instance 'rules:function-length-rule :max 5))
           (code "(defmethod process ((x integer))
  (print x)
  (print x)
  (print x)
  (print x)
  (print x))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      (ok (= 1 (length violations)))
      (ok (search "process" (violation:violation-message (first violations)))))))

(deftest function-length-nested-flet
  (testing "flet inner functions are counted separately"
    (let* ((rule (make-instance 'rules:function-length-rule :max 5))
           ;; Outer function: 6 lines (VIOLATE)
           ;; Inner function: 4 lines (OK)
           (code "(defun outer (x)
  (flet ((inner (y)
           (print y)
           (print y)
           (print y)))
    (inner x)))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      ;; Should have 1 violation for outer function (6 lines > 5 max)
      (ok (= 1 (length violations)))
      (ok (search "outer" (violation:violation-message (first violations))))
      (ok (search "6 lines" (violation:violation-message (first violations)))))))

(deftest function-length-custom-max
  (testing "Custom max configuration"
    (let* ((rule (make-instance 'rules:function-length-rule :max 75))
           (code (format nil "(defun custom-max (x)~%~{  (print ~D)~%~})"
                         (loop for i from 1 to 70 collect i)))
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      ;; 71 lines, max 75 - should not violate
      (ok (null violations)))))

;;; Cyclomatic-complexity tests

(defun test-complexity (code expected-complexity &optional (max nil) (variant :standard))
  "Test that CODE has EXPECTED-COMPLEXITY.
   If MAX is not provided, use (1- expected-complexity) to trigger violation.
   VARIANT defaults to :standard for backward compatibility with existing tests."
  (let* ((rule (make-instance 'rules:cyclomatic-complexity-rule
                              :max (or max (1- expected-complexity))
                              :variant variant))
         (forms (parser:parse-forms code #P"test.lisp"))
         (form (first forms))
         (violations (base:check-form rule form #P"test.lisp")))
    (ok (= 1 (length violations)))
    (when (first violations)
      (ok (search (format nil "complexity of ~D" expected-complexity)
                  (violation:violation-message (first violations)))))))

(defun test-no-complexity-violation (code max &optional (variant :standard))
  "Test that CODE does not violate with given MAX.
   VARIANT defaults to :standard for backward compatibility with existing tests."
  (let* ((rule (make-instance 'rules:cyclomatic-complexity-rule
                              :max max
                              :variant variant))
         (forms (parser:parse-forms code #P"test.lisp"))
         (form (first forms))
         (violations (base:check-form rule form #P"test.lisp")))
    (ok (null violations))))

(deftest complexity-base
  (testing "Empty function has complexity 1"
    (test-complexity "(defun empty () nil)" 1)))

(deftest complexity-if
  (testing "IF adds 1 to complexity"
    (test-complexity "(defun foo (x) (if (> x 0) 1 2))" 2)))

(deftest complexity-when
  (testing "WHEN adds 1 to complexity"
    (test-complexity "(defun foo (x) (when (> x 0) 1))" 2)))

(deftest complexity-unless
  (testing "UNLESS adds 1 to complexity"
    (test-complexity "(defun foo (x) (unless (> x 0) 1))" 2)))

(deftest complexity-multiple-ifs
  (testing "Multiple IFs add to complexity"
    (test-complexity
     "(defun foo (x y)
        (if (> x 0) 1 2)
        (if (> y 0) 3 4))" 3)))

(deftest complexity-cond
  (testing "COND with 3 clauses (2 conditionals + 1 else) adds 2"
    (test-complexity
     "(defun foo (x)
        (cond
          ((< x 0) -1)
          ((> x 0) 1)
          (t 0)))" 3)))

(deftest complexity-cond-no-else
  (testing "COND with 3 clauses (all conditionals) adds 3"
    (test-complexity
     "(defun foo (x)
        (cond
          ((< x 0) -1)
          ((= x 0) 0)
          ((> x 0) 1)))" 4)))

(deftest complexity-cond-otherwise
  (testing "COND with otherwise instead of t also excludes it"
    (test-complexity
     "(defun foo (x)
        (cond
          ((< x 0) -1)
          ((> x 0) 1)
          (otherwise 0)))" 3)))

(deftest complexity-case
  (testing "CASE with 3 cases + otherwise adds 3"
    (test-complexity
     "(defun foo (x)
        (case x
          (a 1)
          (b 2)
          (c 3)
          (otherwise 4)))" 4)))

(deftest complexity-case-no-otherwise
  (testing "CASE with 4 cases (no otherwise) adds 4"
    (test-complexity
     "(defun foo (x)
        (case x
          (a 1)
          (b 2)
          (c 3)
          (d 4)))" 5)))

(deftest complexity-typecase
  (testing "TYPECASE with 2 types + otherwise adds 2"
    (test-complexity
     "(defun foo (x)
        (typecase x
          (integer 1)
          (string 2)
          (otherwise 3)))" 3)))

(deftest complexity-ecase
  (testing "ECASE with 3 cases adds 3 (no otherwise needed)"
    (test-complexity
     "(defun foo (x)
        (ecase x
          (a 1)
          (b 2)
          (c 3)))" 4)))

(deftest complexity-etypecase
  (testing "ETYPECASE with 2 types adds 2 (no otherwise needed)"
    (test-complexity
     "(defun foo (x)
        (etypecase x
          (integer 1)
          (string 2)))" 3)))

;; Phase 3B: Loops

(deftest complexity-dotimes
  (testing "DOTIMES adds 0 (simple iteration)"
    (test-no-complexity-violation
     "(defun foo (n)
        (dotimes (i n)
          (print i)))" 10)))

(deftest complexity-dolist
  (testing "DOLIST adds 0 (simple iteration)"
    (test-no-complexity-violation
     "(defun foo (list)
        (dolist (x list)
          (print x)))" 10)))

(deftest complexity-do
  (testing "DO adds 1 (has end-test condition)"
    (test-complexity
     "(defun foo (n)
        (do ((i 0 (1+ i)))
            ((>= i n))
          (print i)))" 2)))

(deftest complexity-do-star
  (testing "DO* adds 1 (has end-test condition)"
    (test-complexity
     "(defun foo (n)
        (do* ((i 0 (1+ i)))
             ((>= i n))
          (print i)))" 2)))

(deftest complexity-loop-simple
  (testing "Simple LOOP adds 0 (no conditionals)"
    (test-no-complexity-violation
     "(defun foo (list)
        (loop for x in list collect x))" 10)))

(deftest complexity-loop-when
  (testing "LOOP with WHEN adds 1"
    (test-complexity
     "(defun foo (list)
        (loop for x in list
              when (evenp x)
                collect x))" 2)))

(deftest complexity-loop-multiple-conditionals
  (testing "LOOP with multiple conditionals"
    (test-complexity
     "(defun foo (list)
        (loop for x in list
              when (evenp x)
                collect x
              unless (zerop x)
                sum x))" 3)))

;; Phase 3C: Advanced forms

(deftest complexity-and
  (testing "AND adds 1 (regardless of argument count)"
    (test-complexity
     "(defun foo (a b)
        (and a b))" 2)))

(deftest complexity-and-many-args
  (testing "AND with 4 arguments still adds 1"
    (test-complexity
     "(defun validate (a b c d)
        (and a b c d))" 2)))

(deftest complexity-or
  (testing "OR adds 1 (regardless of argument count)"
    (test-complexity
     "(defun foo (a b c)
        (or a b c))" 2)))

(deftest complexity-nested-and-or
  (testing "Nested AND/OR each adds 1"
    (test-complexity
     "(defun foo (x y z)
        (and x (or y z)))" 3)))

(deftest complexity-ignore-errors
  (testing "IGNORE-ERRORS adds 1"
    (test-complexity
     "(defun foo ()
        (ignore-errors
          (risky-operation)))" 2)))

(deftest complexity-handler-case-two
  (testing "HANDLER-CASE with 2 handlers adds 2"
    (test-complexity
     "(defun foo ()
        (handler-case
            (risky-operation)
          (error (e) (log-error e))
          (warning (w) (log-warning w))))" 3)))

;; Complex example

(deftest complexity-violation-example
  (testing "Violation example (9 conditions + 1 else = 9 added, total 10)"
    (test-complexity
     "(defun handle-command (cmd args)
        (cond
          ((string= cmd \"start\") (start-server))
          ((string= cmd \"stop\") (stop-server))
          ((string= cmd \"restart\") (restart-server))
          ((string= cmd \"status\") (show-status))
          ((string= cmd \"config\") (show-config))
          ((string= cmd \"init\") (initialize))
          ((string= cmd \"destroy\") (destroy))
          ((string= cmd \"pause\") (pause-server))
          ((string= cmd \"resume\") (resume-server))
          (t (error \"Unknown command\"))))" 10)))

;; Modified variant tests

(deftest complexity-case-modified-variant
  (testing "CASE with modified variant counts as +1 total"
    (let* ((rule (make-instance 'rules:cyclomatic-complexity-rule
                                :max 1
                                :variant :modified))
           (code "(defun foo (x)
                    (case x
                      (a 1)
                      (b 2)
                      (c 3)
                      (otherwise 4)))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      ;; Complexity = 1 (base) + 1 (case) = 2 with modified variant
      (ok (= 1 (length violations)))
      (ok (search "complexity of 2" (violation:violation-message (first violations)))))))

(deftest complexity-case-standard-variant
  (testing "CASE with standard variant counts per clause"
    (let* ((rule (make-instance 'rules:cyclomatic-complexity-rule
                                :max 1
                                :variant :standard))
           (code "(defun foo (x)
                    (case x
                      (a 1)
                      (b 2)
                      (c 3)
                      (otherwise 4)))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      ;; Complexity = 1 (base) + 3 (case clauses) = 4 with standard variant
      (ok (= 1 (length violations)))
      (ok (search "complexity of 4" (violation:violation-message (first violations)))))))

(deftest complexity-typecase-modified-variant
  (testing "TYPECASE with modified variant counts as +1 total"
    (let* ((rule (make-instance 'rules:cyclomatic-complexity-rule
                                :max 1
                                :variant :modified))
           (code "(defun foo (x)
                    (typecase x
                      (integer 1)
                      (string 2)
                      (otherwise 3)))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      ;; Complexity = 1 (base) + 1 (typecase) = 2 with modified variant
      (ok (= 1 (length violations)))
      (ok (search "complexity of 2" (violation:violation-message (first violations)))))))

;; Third-party macros: Alexandria

(deftest complexity-alexandria-if-let
  (testing "Alexandria IF-LET adds 1"
    (test-complexity
     "(defun foo (x)
        (if-let ((y (find x list)))
          (process y)
          (error \"not found\")))" 2)))

(deftest complexity-alexandria-when-let
  (testing "Alexandria WHEN-LET adds 1"
    (test-complexity
     "(defun foo (x)
        (when-let ((y (find x list)))
          (process y)))" 2)))

(deftest complexity-alexandria-when-let-star
  (testing "Alexandria WHEN-LET* adds 1"
    (test-complexity
     "(defun foo (x)
        (when-let* ((y (find x list))
                    (z (lookup y)))
          (process z)))" 2)))

(deftest complexity-alexandria-xor
  (testing "Alexandria XOR adds 1 (like OR)"
    (test-complexity
     "(defun foo (a b)
        (xor a b))" 2)))

(deftest complexity-alexandria-destructuring-case-standard
  (testing "Alexandria DESTRUCTURING-CASE with standard variant counts per clause"
    (test-complexity
     "(defun foo (cmd)
        (destructuring-case cmd
          ((:start x) (start x))
          ((:stop) (stop))
          ((:restart x y) (restart x y))
          (otherwise (error \"unknown\"))))" 4)))

(deftest complexity-alexandria-destructuring-case-modified
  (testing "Alexandria DESTRUCTURING-CASE with modified variant counts as +1 total"
    (let* ((rule (make-instance 'rules:cyclomatic-complexity-rule
                                :max 1
                                :variant :modified))
           (code "(defun foo (cmd)
                    (destructuring-case cmd
                      ((:start x) (start x))
                      ((:stop) (stop))
                      ((:restart x y) (restart x y))
                      (otherwise (error \"unknown\"))))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      ;; Complexity = 1 (base) + 1 (destructuring-case) = 2
      (ok (= 1 (length violations)))
      (ok (search "complexity of 2" (violation:violation-message (first violations)))))))

(deftest complexity-alexandria-destructuring-ecase
  (testing "Alexandria DESTRUCTURING-ECASE counts all clauses"
    (test-complexity
     "(defun foo (cmd)
        (destructuring-ecase cmd
          ((:start x) (start x))
          ((:stop) (stop))
          ((:restart x y) (restart x y))))" 4)))

;; Third-party macros: Trivia

(deftest complexity-trivia-match-standard
  (testing "Trivia MATCH with standard variant counts per clause"
    (test-complexity
     "(defun foo (x)
        (match x
          (0 'zero)
          (1 'one)
          (2 'two)
          (_ 'other)))" 4)))

(deftest complexity-trivia-match-modified
  (testing "Trivia MATCH with modified variant counts as +1 total"
    (let* ((rule (make-instance 'rules:cyclomatic-complexity-rule
                                :max 1
                                :variant :modified))
           (code "(defun foo (x)
                    (match x
                      (0 'zero)
                      (1 'one)
                      (2 'two)
                      (_ 'other)))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      ;; Complexity = 1 (base) + 1 (match) = 2
      (ok (= 1 (length violations)))
      (ok (search "complexity of 2" (violation:violation-message (first violations)))))))

(deftest complexity-trivia-ematch
  (testing "Trivia EMATCH counts all clauses (no default)"
    (test-complexity
     "(defun foo (x)
        (ematch x
          (0 'zero)
          (1 'one)
          (2 'two)))" 4)))

(deftest complexity-trivia-if-match
  (testing "Trivia IF-MATCH adds 1"
    (test-complexity
     "(defun foo (x)
        (if-match (cons a b) x
          (list a b)
          nil))" 2)))

(deftest complexity-trivia-when-match
  (testing "Trivia WHEN-MATCH adds 1"
    (test-complexity
     "(defun foo (x)
        (when-match (cons a b) x
          (list a b)))" 2)))

(deftest complexity-trivia-unless-match
  (testing "Trivia UNLESS-MATCH adds 1"
    (test-complexity
     "(defun foo (x)
        (unless-match nil x
          (process x)))" 2)))

(deftest complexity-trivia-match-star
  (testing "Trivia MATCH* with standard variant counts per clause"
    (test-complexity
     "(defun foo (x y)
        (match* (x y)
          ((0 0) 'origin)
          ((0 _) 'y-axis)
          ((_ 0) 'x-axis)
          (_ 'elsewhere)))" 4)))

(deftest complexity-trivia-multiple-value-match
  (testing "Trivia MULTIPLE-VALUE-MATCH with standard variant counts per clause"
    (test-complexity
     "(defun foo (x)
        (multiple-value-match (values-fn x)
          ((a b) (process a b))
          ((a) (process-one a))
          (_ (error \"bad\"))))" 3)))

(deftest complexity-trivia-multiple-value-ematch
  (testing "Trivia MULTIPLE-VALUE-EMATCH counts all clauses"
    (test-complexity
     "(defun foo (x)
        (multiple-value-ematch (values-fn x)
          ((a b) (process a b))
          ((a) (process-one a))))" 3)))

;; Third-party macros: string-case

(deftest complexity-string-case-standard
  (testing "STRING-CASE with standard variant counts per clause"
    (test-complexity
     "(defun foo (cmd)
        (string-case cmd
          (\"start\" (start))
          (\"stop\" (stop))
          (\"restart\" (restart))
          (otherwise (error \"unknown\"))))" 4)))

;;; Comment-ratio calculation tests

(defun test-comment-ratio (code &key include-docstrings (min-lines 3))
  "Parse CODE and call calculate-comment-ratio-from-source.
   Returns the plist (:ratio :comment-lines :code-lines) or NIL."
  (let* ((forms (parser:parse-forms code #P"test.lisp"))
         (form (first forms))
         (expr (parser:form-expr form))
         (position-map (parser:form-position-map form))
         (source-lines (mallet/rules/forms/metrics::parse-source-to-lines code))
         (start-line (parser:form-line form))
         (start-column (parser:form-column form)))
    (mallet/rules/forms/metrics::calculate-comment-ratio-from-source
     expr position-map source-lines start-line start-column
     :include-docstrings include-docstrings
     :min-lines min-lines)))

(deftest comment-ratio-no-comments
  (testing "Function with no comments has ratio 0"
    (let ((result (test-comment-ratio
                   "(defun foo (x)
  (let ((y (* x 2)))
    (when (> y 0)
      (print y)
      (+ y 1))))")))
      (ok (not (null result)))
      (ok (= 0.0d0 (getf result :ratio)))
      (ok (= 0 (getf result :comment-lines)))
      (ok (= 5 (getf result :code-lines))))))

(deftest comment-ratio-all-comments
  (testing "Function with all comment body lines has high ratio"
    (let ((result (test-comment-ratio
                   "(defun foo ()
  ;; comment 1
  ;; comment 2
  ;; comment 3
  nil)"
                   :min-lines 1)))
      (ok (not (null result)))
      (ok (= 3 (getf result :comment-lines)))
      ;; ratio = 3/(3+1) = 0.75
      (ok (< (abs (- (getf result :ratio) 0.75d0)) 0.001d0)))))

(deftest comment-ratio-mixed
  (testing "Function with mixed comments and code"
    (let ((result (test-comment-ratio
                   "(defun process (x)
  ;; Validate input
  (check-type x integer)
  ;; Transform
  (let ((y (* x 2)))
    ;; Return result
    (+ y 1)))")))
      (ok (not (null result)))
      (ok (= 3 (getf result :comment-lines)))
      (ok (= 4 (getf result :code-lines)))
      ;; ratio = 3/7
      (ok (< (abs (- (getf result :ratio) (/ 3.0d0 7.0d0))) 0.001d0)))))

(deftest comment-ratio-below-min-lines
  (testing "Function below min-lines returns nil"
    (let ((result (test-comment-ratio
                   "(defun tiny (x) (+ x 1))"
                   :min-lines 3)))
      (ok (null result)))))

(deftest comment-ratio-at-min-lines
  (testing "Function at exactly min-lines returns result"
    (let ((result (test-comment-ratio
                   "(defun foo (x)
  (print x)
  (+ x 1))"
                   :min-lines 3)))
      (ok (not (null result)))
      (ok (= 0.0d0 (getf result :ratio))))))

(deftest comment-ratio-docstrings-excluded-by-default
  (testing "Docstring lines are not counted as comments by default"
    (let ((result (test-comment-ratio
                   "(defun foo (x)
  \"This is a docstring.\"
  (print x)
  (+ x 1)
  (* x 2))")))
      (ok (not (null result)))
      ;; docstring line excluded from both counts
      (ok (= 0 (getf result :comment-lines)))
      (ok (= 4 (getf result :code-lines))))))

(deftest comment-ratio-docstrings-included
  (testing "Docstring lines counted as comments when include-docstrings is t"
    (let ((result (test-comment-ratio
                   "(defun foo (x)
  \"This is a docstring.\"
  (print x)
  (+ x 1)
  (* x 2))"
                   :include-docstrings t)))
      (ok (not (null result)))
      (ok (= 1 (getf result :comment-lines)))
      (ok (= 4 (getf result :code-lines))))))

(deftest comment-ratio-multiline-docstring-included
  (testing "Multi-line docstring counted as comments when included"
    (let ((result (test-comment-ratio
                   "(defun foo (x)
  \"This is a
  multi-line
  docstring.\"
  (print x)
  (+ x 1)
  (* x 2))"
                   :include-docstrings t)))
      (ok (not (null result)))
      ;; 3 docstring lines counted as comments
      (ok (= 3 (getf result :comment-lines)))
      (ok (= 4 (getf result :code-lines))))))

(deftest comment-ratio-block-comments
  (testing "Block comment lines counted as comments"
    (let ((result (test-comment-ratio
                   "(defun foo (x)
  #|
  This is a block comment
  spanning multiple lines
  |#
  (print x)
  (+ x 1)
  (* x 2))")))
      (ok (not (null result)))
      ;; 4 block comment lines (including #| and |# lines)
      (ok (= 4 (getf result :comment-lines)))
      (ok (= 4 (getf result :code-lines))))))

;;; Comment-ratio rule tests

(defun test-comment-ratio-violation (code &key (max 0.3d0) (min-lines 3) include-docstrings)
  "Test CODE with comment-ratio-rule and return violations."
  (let* ((rule (make-instance 'rules:comment-ratio-rule
                              :max max
                              :min-lines min-lines
                              :include-docstrings include-docstrings))
         (forms (parser:parse-forms code #P"test.lisp"))
         (form (first forms)))
    (base:check-form rule form #P"test.lisp")))

(deftest comment-ratio-rule-violation
  (testing "Function exceeding max ratio produces violation"
    (let ((violations (test-comment-ratio-violation
                       "(defun foo (x)
  ;; comment 1
  ;; comment 2
  ;; comment 3
  (print x)
  (+ x 1)
  (* x 2))"
                       :max 0.2d0)))
      (ok (= 1 (length violations)))
      (ok (search "foo" (violation:violation-message (first violations))))
      (ok (search "comment ratio" (violation:violation-message (first violations)))))))

(deftest comment-ratio-rule-no-violation
  (testing "Function under max ratio produces no violation"
    (let ((violations (test-comment-ratio-violation
                       "(defun foo (x)
  ;; one comment
  (print x)
  (+ x 1)
  (* x 2)
  (- x 3))"
                       :max 0.3d0)))
      (ok (null violations)))))

(deftest comment-ratio-rule-min-lines-skip
  (testing "Function below min-lines is skipped"
    (let ((violations (test-comment-ratio-violation
                       "(defun tiny (x) (+ x 1))"
                       :max 0.0d0
                       :min-lines 3)))
      (ok (null violations)))))

(deftest comment-ratio-rule-message-format
  (testing "Violation message has expected format"
    (let* ((violations (test-comment-ratio-violation
                        "(defun bar (x)
  ;; comment 1
  ;; comment 2
  ;; comment 3
  (print x)
  (+ x 1)
  (* x 2))"
                        :max 0.1d0))
           (msg (violation:violation-message (first violations))))
      (ok (search "bar" msg))
      (ok (search "max: 0.10" msg))
      (ok (search "3 comment lines out of 7 non-blank lines" msg)))))

(deftest comment-ratio-rule-flet-inner
  (testing "flet inner functions checked independently"
    (let ((violations (test-comment-ratio-violation
                       "(defun outer (x)
  (flet ((inner (y)
           ;; lots of comments
           ;; more comments
           ;; even more
           (print y)))
    (inner x)))"
                       :max 0.3d0
                       :min-lines 1)))
      ;; inner function has 3 comments out of 4 non-blank = 0.75 > 0.3
      ;; outer function also checked
      (ok (<= 1 (length violations)))
      ;; At least one violation mentions inner function behavior
      (ok (some (lambda (v) (search "inner" (violation:violation-message v)))
                violations)))))

(deftest comment-ratio-rule-include-docstrings
  (testing "include-docstrings option affects violation"
    ;; Without include-docstrings: 0 comment lines, no violation
    (let ((violations-without (test-comment-ratio-violation
                               "(defun foo (x)
  \"A docstring.\"
  (print x)
  (+ x 1)
  (* x 2))"
                               :max 0.0d0
                               :include-docstrings nil)))
      (ok (null violations-without)))
    ;; With include-docstrings: 1 comment line out of 5 non-blank = 0.2 > 0.0
    (let ((violations-with (test-comment-ratio-violation
                            "(defun foo (x)
  \"A docstring.\"
  (print x)
  (+ x 1)
  (* x 2))"
                            :max 0.0d0
                            :include-docstrings t
                            :min-lines 1)))
      (ok (= 1 (length violations-with))))))

(deftest comment-ratio-rule-via-make-rule
  (testing "comment-ratio rule can be created via make-rule factory"
    (let ((rule (all-rules:make-rule :comment-ratio :max 0.2d0 :min-lines 5)))
      (ok (typep rule 'rules:comment-ratio-rule))
      (ok (= 0.2d0 (rules::max-ratio rule)))
      (ok (= 5 (rules::rule-min-lines rule))))))

(deftest complexity-string-case-modified
  (testing "STRING-CASE with modified variant counts as +1 total"
    (let* ((rule (make-instance 'rules:cyclomatic-complexity-rule
                                :max 1
                                :variant :modified))
           (code "(defun foo (cmd)
                    (string-case cmd
                      (\"start\" (start))
                      (\"stop\" (stop))
                      (\"restart\" (restart))
                      (otherwise (error \"unknown\"))))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (violations (base:check-form rule form #P"test.lisp")))
      ;; Complexity = 1 (base) + 1 (string-case) = 2
      (ok (= 1 (length violations)))
      (ok (search "complexity of 2" (violation:violation-message (first violations)))))))

;;; calculate-comment-ratio public API tests

(deftest comment-ratio-rule-labels-inner
  (testing "labels inner functions checked independently"
    (let ((violations (test-comment-ratio-violation
                       "(defun outer (x)
  (labels ((helper (y)
             ;; lots of comments
             ;; more comments
             ;; even more
             (print y)))
    (helper x)))"
                       :max 0.3d0
                       :min-lines 1)))
      ;; inner function has 3 comments out of 4+1 non-blank = 0.60 > 0.3
      (ok (<= 1 (length violations)))
      (ok (some (lambda (v) (search "helper" (violation:violation-message v)))
                violations)))))

(deftest comment-ratio-rule-severity
  (testing "comment-ratio rule has :info severity"
    (let ((rule (make-instance 'rules:comment-ratio-rule)))
      (ok (eq :info (base:rule-severity rule))))))

(deftest comment-ratio-rule-category
  (testing "comment-ratio rule has :metrics category"
    (let ((rule (make-instance 'rules:comment-ratio-rule)))
      (ok (eq :metrics (base:rule-category rule))))))

(deftest comment-ratio-rule-disabled-by-default
  (testing "comment-ratio is not in default config"
    (let* ((config (mallet/config:get-built-in-config :default))
           (rule-names (mapcar #'base:rule-name (mallet/config:config-rules config))))
      (ok (not (member :comment-ratio rule-names))))))

(deftest comment-ratio-rule-in-all-preset
  (testing "comment-ratio is in :all preset"
    (let* ((config (mallet/config:get-built-in-config :all))
           (rule-names (mapcar #'base:rule-name (mallet/config:config-rules config))))
      (ok (member :comment-ratio rule-names)))))

(deftest comment-ratio-blank-lines-excluded
  (testing "Blank lines do not affect ratio calculation"
    (let ((result-with-blanks (test-comment-ratio
                                "(defun foo (x)
  ;; comment

  (print x)

  (+ x 1)
  (* x 2))")))
      (ok (not (null result-with-blanks)))
      ;; 1 comment, 4 code lines. Blank lines excluded.
      (ok (= 1 (getf result-with-blanks :comment-lines)))
      (ok (= 4 (getf result-with-blanks :code-lines))))))

(deftest comment-ratio-api-returns-float
  (testing "calculate-comment-ratio returns a float, not a plist"
    (let ((result (rules:calculate-comment-ratio
                   "(defun foo (x)
  ;; comment
  (print x)
  (+ x 1)
  (* x 2))"
                   :min-lines 1)))
      (ok (not (null result)))
      (ok (typep result 'double-float))
      (ok (not (consp result))))))

(deftest comment-ratio-api-string-input
  (testing "calculate-comment-ratio accepts string input"
    (let ((result (rules:calculate-comment-ratio
                   "(defun foo (x)
  ;; comment one
  ;; comment two
  (+ x 1))"
                   :min-lines 1)))
      (ok (not (null result)))
      (ok (< 0.49d0 result 0.51d0)))))

(deftest comment-ratio-api-form-input
  (testing "calculate-comment-ratio accepts parser:form input"
    (let* ((code "(defun foo (x)
  ;; a comment
  ;; another comment
  (+ x 1))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (form (first forms))
           (result (rules:calculate-comment-ratio form :min-lines 1)))
      (ok (not (null result)))
      (ok (< 0.49d0 result 0.51d0)))))

(deftest comment-ratio-api-no-comments
  (testing "calculate-comment-ratio returns zero ratio for code without comments"
    (let ((result (rules:calculate-comment-ratio
                   "(defun foo (x)
  (let ((y (* x 2)))
    (+ y 1)))"
                   :min-lines 1)))
      (ok (not (null result)))
      (ok (< result 0.01d0)))))

(deftest comment-ratio-api-below-min-lines
  (testing "calculate-comment-ratio returns NIL when below min-lines"
    (let ((result (rules:calculate-comment-ratio
                   "(defun foo (x) (+ x 1))"
                   :min-lines 5)))
      (ok (null result)))))

(deftest comment-ratio-api-include-docstrings
  (testing "calculate-comment-ratio counts docstrings when include-docstrings is true"
    (let* ((code "(defun foo (x)
  \"A docstring.\"
  ;; a comment
  (+ x 1))")
           (without (rules:calculate-comment-ratio code :include-docstrings nil :min-lines 1))
           (with (rules:calculate-comment-ratio code :include-docstrings t :min-lines 1)))
      ;; Without docstrings: 1 comment line out of 3 non-blank = ratio ~0.33
      (ok (not (null without)))
      (ok (< without 0.5d0))
      ;; With docstrings: docstring line also counted, ratio higher
      (ok (not (null with)))
      (ok (< without with)))))

(deftest analyze-metrics-includes-comment-ratio
  (testing "analyze-function-metrics includes :comment-ratio key"
    (let ((result (rules:analyze-function-metrics
                   "(defun foo (x)
  ;; comment
  (+ x 1))"
                   :min-lines 1)))
      (ok (not (null (getf result :length))))
      (ok (not (null (getf result :complexity))))
      (ok (not (null (getf result :comment-ratio))))
      (ok (floatp (getf result :comment-ratio))))))

;;; Coalton function-length tests

(deftest function-length-rule-coalton-aware-p
  (testing "function-length-rule opts into Coalton form processing"
    (let ((rule (make-instance 'rules:function-length-rule)))
      (ok (base:coalton-aware-p rule)))))

(deftest function-length-rule-skips-coalton-value-define
  (testing "function-length rule ignores value defines (define x 42) in coalton-toplevel"
    (let* ((rule (make-instance 'rules:function-length-rule :max 0))
           (code "(coalton-toplevel (define x 42))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (null violations)))))

(deftest function-length-rule-short-coalton-define-passes
  (testing "function-length rule does not flag short coalton function defines"
    (let* ((rule (make-instance 'rules:function-length-rule :max 50))
           (code "(coalton-toplevel
  (define (foo x)
    (+ x 1)))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (null violations)))))

(deftest function-length-rule-detects-long-coalton-define
  (testing "function-length rule detects long coalton function defines"
    (let* ((rule (make-instance 'rules:function-length-rule :max 1))
           (code (format nil "(coalton-toplevel~%  (define (foo x)~%~{    (bar ~D)~%~}    x))"
                         (loop for i from 1 to 5 collect i)))
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (= 1 (length violations)))
      (ok (search "FOO" (violation:violation-message (first violations)))))))

(deftest function-length-rule-coalton-reports-function-name
  (testing "function-length violation for coalton define includes function name"
    (let* ((rule (make-instance 'rules:function-length-rule :max 0))
           (code "(coalton-toplevel
  (define (my-function x)
    (+ x 1)))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (= 1 (length violations)))
      (ok (search "MY-FUNCTION" (violation:violation-message (first violations)))))))

(deftest function-length-rule-cl-forms-unaffected-by-coalton-aware
  (testing "function-length rule still processes CL defun forms after opting into Coalton"
    (let* ((rule (make-instance 'rules:function-length-rule :max 0))
           (code "(defun foo (x) (+ x 1))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (= 1 (length violations))))))

;;; Coalton comment-ratio tests

(deftest comment-ratio-rule-coalton-aware-p
  (testing "comment-ratio-rule opts into Coalton form processing"
    (let ((rule (make-instance 'rules:comment-ratio-rule)))
      (ok (base:coalton-aware-p rule)))))

(deftest comment-ratio-rule-skips-coalton-value-define
  (testing "comment-ratio rule ignores value defines (define x 42) in coalton-toplevel"
    (let* ((rule (make-instance 'rules:comment-ratio-rule :max 0.0d0 :min-lines 1))
           (code "(coalton-toplevel (define x 42))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (null violations)))))

(deftest comment-ratio-rule-no-violation-coalton-define-clean
  (testing "comment-ratio rule does not flag coalton function defines with low comment ratio"
    (let* ((rule (make-instance 'rules:comment-ratio-rule :max 0.5d0 :min-lines 1))
           (code "(coalton-toplevel
  (define (foo x)
    (+ x 1)))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (null violations)))))

(deftest comment-ratio-rule-detects-coalton-define-with-high-ratio
  (testing "comment-ratio rule detects high comment ratio in coalton function defines"
    (let* ((rule (make-instance 'rules:comment-ratio-rule :max 0.3d0 :min-lines 1))
           (code "(coalton-toplevel
  (define (foo x)
    ;; lots of comments
    ;; more comments
    ;; even more
    x))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (= 1 (length violations)))
      (ok (search "FOO" (violation:violation-message (first violations)))))))

(deftest comment-ratio-rule-coalton-reports-function-name
  (testing "comment-ratio violation for coalton define includes function name"
    (let* ((rule (make-instance 'rules:comment-ratio-rule :max 0.0d0 :min-lines 1))
           (code "(coalton-toplevel
  (define (my-function x)
    ;; a comment
    x))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (= 1 (length violations)))
      (ok (search "MY-FUNCTION" (violation:violation-message (first violations)))))))

(deftest comment-ratio-rule-cl-forms-unaffected-by-coalton-aware
  (testing "comment-ratio rule still processes CL defun forms after opting into Coalton"
    (let* ((rule (make-instance 'rules:comment-ratio-rule :max 0.0d0 :min-lines 1))
           (code "(defun foo (x)
  ;; comment
  (+ x 1))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (= 1 (length violations))))))

;;; Coalton function-length edge cases

(deftest function-length-coalton-at-exact-threshold
  (testing "Coalton define at exactly the max threshold produces no violation"
    (let* ((rule (make-instance 'rules:function-length-rule :max 3))
           ;; 3 code lines: (define (foo x), (+ x 1), closing paren line
           (code "(coalton-toplevel
  (define (foo x)
    (+ x 1)
    x))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (null violations)))))

(deftest function-length-coalton-one-over-threshold
  (testing "Coalton define one line over the max threshold produces a violation"
    (let* ((rule (make-instance 'rules:function-length-rule :max 2))
           ;; 3 code lines > max 2
           (code "(coalton-toplevel
  (define (foo x)
    (+ x 1)
    x))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (= 1 (length violations)))
      (ok (search "FOO" (violation:violation-message (first violations))))
      (ok (search "3 lines" (violation:violation-message (first violations)))))))

(deftest function-length-coalton-nested-let
  (testing "Coalton define with nested let counts correctly"
    (let* ((rule (make-instance 'rules:function-length-rule :max 2))
           (code "(coalton-toplevel
  (define (foo x)
    (let ((y (+ x 1)))
      (let ((z (* y 2)))
        (+ y z)))))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      ;; The function has more than 2 code lines
      (ok (= 1 (length violations)))
      (ok (search "FOO" (violation:violation-message (first violations)))))))

(deftest function-length-coalton-multiple-defines-only-long-triggers
  (testing "Multiple defines in one coalton-toplevel: only the long one triggers"
    (let* ((rule (make-instance 'rules:function-length-rule :max 3))
           (code (format nil "(coalton-toplevel
  (define (short-fn x)
    (+ x 1))
  (define (long-fn x)
~{    (bar ~D)~%~}    x))"
                         (loop for i from 1 to 5 collect i)))
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      ;; Only long-fn should trigger
      (ok (= 1 (length violations)))
      (ok (search "LONG-FN" (violation:violation-message (first violations))))
      ;; short-fn should NOT appear
      (ok (not (some (lambda (v)
                       (search "SHORT-FN" (violation:violation-message v)))
                     violations))))))

(deftest function-length-coalton-value-define-variants-no-violation
  (testing "Value defines like (define pi 3.14) and (define x 42) produce no violations"
    (let* ((rule (make-instance 'rules:function-length-rule :max 0))
           (code "(coalton-toplevel
  (define x 42)
  (define pi 3.14)
  (define greeting \"hello\"))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (null violations)))))

(deftest function-length-coalton-define-type-ignored
  (testing "define-type is NOT treated as a function definition"
    (let* ((rule (make-instance 'rules:function-length-rule :max 0))
           (code "(coalton-toplevel
  (define-type (Optional :a)
    (Some :a)
    None))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (null violations)))))

(deftest function-length-coalton-define-instance-ignored
  (testing "define-instance is NOT treated as a function definition"
    (let* ((rule (make-instance 'rules:function-length-rule :max 0))
           (code "(coalton-toplevel
  (define-instance (Eq MyType)
    (define (== a b)
      (match a
        ((MyType x) (match b
                      ((MyType y) (== x y))))))))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      ;; define-instance itself should not be treated as a function definition.
      ;; The inner (define (== a b) ...) IS a function define and may trigger,
      ;; but the define-instance wrapper must not contribute a separate violation.
      ;; With max 0, the inner define should trigger exactly once.
      (ok (<= (length violations) 1))
      ;; If there is a violation, it should be for the inner define, not define-instance
      (when violations
        (ok (search "==" (violation:violation-message (first violations))))))))

(deftest function-length-coalton-violation-metadata
  (testing "Coalton function-length violation has correct severity and line info"
    (let* ((rule (make-instance 'rules:function-length-rule :max 1))
           (code "(coalton-toplevel
  (define (foo x)
    (+ x 1)
    x))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (= 1 (length violations)))
      (let ((v (first violations)))
        ;; Severity should be :info (metrics rules are informational)
        (ok (eq :info (violation:violation-severity v)))
        ;; Line should be > 0 (the define starts on line 2)
        (ok (> (violation:violation-line v) 0))
        ;; Column should be >= 0
        (ok (>= (violation:violation-column v) 0))
        ;; Message should include function name and line count
        (ok (search "FOO" (violation:violation-message v)))
        (ok (search "lines" (violation:violation-message v)))
        (ok (search "max:" (violation:violation-message v)))))))

(deftest function-length-coalton-all-short-defines-clean
  (testing "coalton-toplevel with only short clean defines produces zero violations"
    (let* ((rule (make-instance 'rules:function-length-rule))  ; default max 50
           (code "(coalton-toplevel
  (define (add x y)
    (+ x y))
  (define (sub x y)
    (- x y))
  (define (mul x y)
    (* x y)))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (null violations)))))

;;; Coalton comment-ratio edge cases

(deftest comment-ratio-coalton-at-exact-threshold
  (testing "Coalton define at exactly the max ratio produces no violation"
    (let* ((rule (make-instance 'rules:comment-ratio-rule :max 0.5d0 :min-lines 1))
           ;; 2 comment lines, 2 code lines = ratio 0.5 which is NOT > 0.5
           (code "(coalton-toplevel
  (define (foo x)
    ;; comment one
    ;; comment two
    (+ x 1)
    x))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (null violations)))))

(deftest comment-ratio-coalton-one-over-threshold
  (testing "Coalton define slightly over max ratio produces a violation"
    (let* ((rule (make-instance 'rules:comment-ratio-rule :max 0.3d0 :min-lines 1))
           ;; 3 comment lines, 2 code lines = ratio 0.6 > 0.3
           (code "(coalton-toplevel
  (define (foo x)
    ;; comment one
    ;; comment two
    ;; comment three
    (+ x 1)
    x))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (= 1 (length violations)))
      (ok (search "FOO" (violation:violation-message (first violations))))
      (ok (search "comment ratio" (violation:violation-message (first violations)))))))

(deftest comment-ratio-coalton-value-define-variants-no-violation
  (testing "Value defines like (define pi 3.14) produce no comment-ratio violations"
    (let* ((rule (make-instance 'rules:comment-ratio-rule :max 0.0d0 :min-lines 1))
           (code "(coalton-toplevel
  (define x 42)
  (define pi 3.14))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (null violations)))))

(deftest comment-ratio-coalton-define-type-ignored
  (testing "define-type does not produce comment-ratio violations"
    (let* ((rule (make-instance 'rules:comment-ratio-rule :max 0.0d0 :min-lines 1))
           (code "(coalton-toplevel
  (define-type (Optional :a)
    ;; comment inside define-type
    (Some :a)
    None))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (null violations)))))

(deftest comment-ratio-coalton-define-instance-ignored
  (testing "define-instance does not produce comment-ratio violations"
    (let* ((rule (make-instance 'rules:comment-ratio-rule :max 0.0d0 :min-lines 1))
           (code "(coalton-toplevel
  (define-instance (Eq MyType)
    ;; instance methods
    (define (== a b)
      (== a b))))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      ;; define-instance itself should not trigger a violation.
      ;; The inner define may trigger if it has comments, but define-instance must not.
      ;; In this case the inner define has no comments relative to its code,
      ;; so no violations expected from the inner define either.
      (ok (null violations)))))

(deftest comment-ratio-coalton-violation-metadata
  (testing "Coalton comment-ratio violation has correct severity and metadata"
    (let* ((rule (make-instance 'rules:comment-ratio-rule :max 0.0d0 :min-lines 1))
           (code "(coalton-toplevel
  (define (bar x)
    ;; a comment
    x))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (= 1 (length violations)))
      (let ((v (first violations)))
        ;; Severity should be :info
        (ok (eq :info (violation:violation-severity v)))
        ;; Line should be > 0
        (ok (> (violation:violation-line v) 0))
        ;; Column should be >= 0
        (ok (>= (violation:violation-column v) 0))
        ;; Message should include function name
        (ok (search "BAR" (violation:violation-message v)))
        ;; Message should mention comment ratio and max
        (ok (search "comment ratio" (violation:violation-message v)))
        (ok (search "max:" (violation:violation-message v)))))))

(deftest comment-ratio-coalton-all-short-defines-clean
  (testing "coalton-toplevel with only clean defines produces zero comment-ratio violations"
    (let* ((rule (make-instance 'rules:comment-ratio-rule :min-lines 1))  ; default max 0.5
           (code "(coalton-toplevel
  (define (add x y)
    (+ x y))
  (define (sub x y)
    (- x y))
  (define (mul x y)
    (* x y)))")
           (forms (parser:parse-forms code #P"test.lisp"))
           (violations (mapcan (lambda (f) (base:check-form rule f #P"test.lisp")) forms)))
      (ok (null violations)))))
