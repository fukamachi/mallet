(defpackage #:mallet/tests/rules/unused-variables
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/unused-variables)

(deftest unused-variables-valid
  (testing "Valid: all parameters used"
    (let* ((code "(defun add (x y)
                     (+ x y))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: parameter with declare ignore"
    (let* ((code "(defun add (x y)
                     (declare (ignore y))
                     (+ x 1))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: parameter with underscore prefix"
    (let* ((code "(defun add (x _y)
                     (+ x 1))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: parameter named underscore"
    (let* ((code "(lambda (_ x)
                     (+ x 1))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: all let bindings used"
    (let* ((code "(let ((x 1) (y 2))
                     (+ x y))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: let binding with declare ignore"
    (let* ((code "(let ((x 1) (y 2))
                     (declare (ignore y))
                     (+ x 1))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)))))

(deftest unused-variables-invalid
  (testing "Invalid: unused function parameter"
    (let* ((code "(defun add (x y)
                     (+ x 1))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :unused-variables))
      (ok (search "Variable 'y' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Invalid: multiple unused parameters"
    (let* ((code "(defun foo (x y z)
                     (+ x 1))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 2))
      (ok (every (lambda (v)
                   (eq (violation:violation-rule v) :unused-variables))
                 violations))))

  (testing "Invalid: unused lambda parameter"
    (let* ((code "(lambda (x y)
                     (+ x 1))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (search "Variable 'y' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Invalid: unused let binding"
    (let* ((code "(let ((x 1) (y 2))
                     (+ x 1))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (search "Variable 'y' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Invalid: unused let* binding"
    (let* ((code "(let* ((x 1) (y 2))
                     (+ x 1))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (search "Variable 'y' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Invalid: unused loop variable"
    (let* ((code "(loop for item in list
                        for index from 0
                        collect item)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-loop-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (search "Variable 'index' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Invalid: nested unused variable"
    (let* ((code "(defun outer (x)
                     (let ((y 1))
                       (+ x 1)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (search "Variable 'y' is unused"
                  (violation:violation-message (first violations)))))))

(deftest let-star-sequential-bindings
  (testing "Valid: LET* variable used in nested let* (tokenizer case)"
    (let* ((code "(let* ((start-pos 10)
                         (start-column 20))
                     (loop while t do (print x))
                     (let* ((raw (+ start-pos 5)))
                       (print raw)
                       (print start-column)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: LET* inside COND clause (exact tokenizer structure)"
    (let* ((code "(loop while t do
                     (let ((ch x))
                       (cond
                         ((constituent-char-p ch)
                          (let* ((start-pos pos)
                                 (start-column column))
                            (loop while t do (incf pos))
                            (let* ((raw (subseq text start-pos pos))
                                   (type :number))
                              (push (make-token type raw file line start-column raw) tokens)
                              (incf column (length raw))))))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Filter out violations for other undefined variables (pos, text, etc)
      ;; We only care about start-pos and start-column
      (let ((start-pos-violations (remove-if-not
                                   (lambda (v)
                                     (search "start-pos" (violation:violation-message v)))
                                   violations)))
        (ok (null start-pos-violations)))))

  (testing "Valid: LET* variable used in subsequent binding"
    (let* ((code "(let* ((a 1)
                         (b (1+ a))
                         (c (+ a b)))
                     (+ a c))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Invalid: LET* variable unused in binding or body"
    (let* ((code "(let* ((a 1)
                         (b (1+ a))
                         (c a))
                     (+ a b))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (search "Variable 'c' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Valid: LET* complex sequential usage"
    (let* ((code "(let ((a 1))
                     (let* ((a a)
                            (b (1+ a))
                            (c a))
                       (+ a c)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (search "Variable 'b' is unused"
                  (violation:violation-message (first violations)))))))

(deftest variable-scope-handling
  (testing "Valid: same name in different scopes"
    (let* ((code "(let ((x 1))
                     (let ((x 2))
                       (+ x 3)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Outer x is shadowed and unused, inner x is used
      (ok (= (length violations) 1))
      (ok (search "Variable 'x' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Valid: parameter shadows let binding"
    (let* ((code "(let ((x 1))
                     (funcall (lambda (x) (+ x 1)) x))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Both x variables are used (outer in funcall arg, inner in lambda body)
      (ok (null violations))))

  (testing "Invalid: both variables with same name unused"
    (let* ((code "(let ((x 1))
                     (let ((x 2))
                       (print 'done)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Both x variables should be flagged as unused
      (ok (= (length violations) 2))))

  (testing "Valid: loop variable shadowing"
    (let* ((code "(let ((i 0))
                     (loop for i from 1 to 10
                           collect i))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Outer i is unused (shadowed by loop variable)
      (ok (= (length violations) 1))
      (ok (search "Variable 'i' is unused"
                  (violation:violation-message (first violations)))))))

(deftest destructuring-bind-bindings
  (testing "Invalid: unused destructured variable"
    (let* ((code "(destructuring-bind (a b c) '(1 2 3)
                     (+ a b))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (search "Variable 'c' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Valid: all destructured variables used"
    (let* ((code "(destructuring-bind (x y) '(1 2)
                       (+ x y))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Invalid: nested destructuring with unused"
    (let* ((code "(destructuring-bind ((a b) c) '((1 2) 3)
                       (+ a c))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (search "Variable 'b' is unused"
                  (violation:violation-message (first violations)))))))

(deftest multiple-value-bind-bindings
  (testing "Invalid: unused value binding"
    (let* ((code "(multiple-value-bind (x y z) (values 1 2 3)
                       (+ x y))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (search "Variable 'z' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Valid: all value bindings used"
    (let* ((code "(multiple-value-bind (a b) (values 1 2)
                       (+ a b))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)))))

(deftest init-form-scoping
  (testing "Valid: parameter used in multiple-value-bind init-form"
    ;; Bug fix: init-form evaluated in outer scope, so 'a' should be recognized as used
    (let* ((code "(defun foo (a)
                     (multiple-value-bind (a a2) (bar a)
                       (+ a a2)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Parameter 'a' used in init-form should not be flagged as unused")))

  (testing "Valid: parameter used in destructuring-bind init-form"
    (let* ((code "(defun process (data)
                     (destructuring-bind (x y) (transform data)
                       (+ x y)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Parameter 'data' used in init-form should not be flagged as unused")))

  (testing "Valid: parameter shadows itself in binding but used in init-form"
    ;; This is the exact pattern from the user's bug report
    (let* ((code "(defun test (a)
                     (multiple-value-bind (a b) (values a 2)
                       (+ a b)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Parameter used in init-form then shadowed should not be unused")))

  (testing "Valid: multiple parameters used in init-form"
    (let* ((code "(defun compute (x y)
                     (destructuring-bind (a b c) (list x y (+ x y))
                       (+ a b c)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "All parameters used in init-form should be recognized")))

  (testing "Valid: nested bindings with init-form usage"
    (let* ((code "(defun nested (input)
                     (multiple-value-bind (x y) (split input)
                       (destructuring-bind (a b) (combine x y)
                         (+ a b))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Init-form usage should work across nested bindings")))

  (testing "Invalid: parameter truly unused when not in init-form"
    (let* ((code "(defun test (unused)
                     (multiple-value-bind (a b) (values 1 2)
                       (+ a b)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (search "Variable 'unused' is unused"
                  (violation:violation-message (first violations)))
          "Parameter not used in init-form should be flagged as unused")))

  (testing "Valid: parameter used in complex init-form expression"
    (let* ((code "(defun process (items)
                     (destructuring-bind (first rest)
                         (cons (car items) (cdr items))
                       (list first rest)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Parameter used in complex init-form should be recognized"))))

(deftest defmacro-parameters
  (testing "Invalid: unused macro parameter"
    (let* ((code "(defmacro my-macro (form1 form2 form3)
                     `(progn ,form1 ,form2))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (search "Variable 'form3' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Valid: all macro parameters used"
    (let* ((code "(defmacro with-vars (var value &body body)
                     `(let ((,var ,value))
                        ,@body))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)))))

(deftest nested-lambda-lists
  (testing "Bug: defmacro with nested lambda list and initialization form"
    ;; This was incorrectly extracting '+' from the init form (+ 1 2 3)
    (let* ((code "(defmacro foo (a (&key (b (+ 1 2 3))))
                     `(values ,a ,b))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Should have no violations - both a and b are used
      ;; Bug: was incorrectly reporting '+' as an unused variable
      (ok (null violations))))

  (testing "Valid: defmacro with multiple nested lambda list parameters"
    (let* ((code "(defmacro with-options (name (&key (x 10) (y 20)) &body body)
                     `(let ((,name (list ,x ,y)))
                        ,@body))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; All parameters used: name, x, y, body
      (ok (null violations))))

  (testing "Invalid: defmacro with unused nested lambda list parameter"
    (let* ((code "(defmacro bar (a (&key (b 1) (c 2)))
                     `(list ,a ,b))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; c is unused
      (ok (= (length violations) 1))
      (ok (search "Variable 'c' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Valid: nested lambda list with &optional"
    (let* ((code "(defmacro process (input (&optional (validate t)))
                     `(if ,validate
                          (check ,input)
                          ,input))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: nested lambda list with complex init form"
    (let* ((code "(defmacro compute (x (&key (result (funcall #'+ x 10))))
                     `(list ,x ,result))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Should not extract 'funcall' or '+' from the init form
      (ok (null violations))))

  (testing "Valid: deeply nested lambda lists"
    (let* ((code "(defmacro nested (a (b (&key (c 3))))
                     `(list ,a ,b ,c))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Invalid: deeply nested with unused parameter"
    (let* ((code "(defmacro nested (a (b (&key (c 3) (d 4))))
                     `(list ,a ,b ,c))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; d is unused
      (ok (= (length violations) 1))
      (ok (search "Variable 'd' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Valid: nested lambda list with supplied-p parameter"
    (let* ((code "(defmacro check (name (&optional (value nil value-p)))
                     (if ,value-p
                         `(process ,name ,value)
                         `(default ,name)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)))))

(deftest dolist-bindings
  (testing "Invalid: unused dolist variable"
    (let* ((code "(let ((sum 0))
                     (dolist (item '(1 2 3))
                       (incf sum))
                     sum)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (search "Variable 'item' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Valid: dolist variable used"
    (let* ((code "(dolist (x '(1 2 3))
                     (print x))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: parameter used in dolist list form then shadowed"
    ;; Bug: parameter used in list form (second x) then shadowed by loop variable (first x)
    (let* ((code "(defun foo (x)
                     (dolist (x x)
                       (print x)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Parameter used in list form should not be flagged as unused"))))

(deftest dotimes-bindings
  (testing "Valid: unused dotimes variable (ignorable by convention)"
    (let* ((code "(let ((sum 0))
                     (dotimes (i 5)
                       (incf sum))
                     sum)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; DOTIMES variables are conventionally ignorable - no violation expected
      (ok (null violations))))

  (testing "Valid: dotimes variable used"
    (let* ((code "(dotimes (i 10)
                     (print i))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: parameter used in dotimes count form then shadowed"
    ;; Bug: parameter used in count form (second x) then shadowed by loop variable (first x)
    (let* ((code "(defun baz (x)
                     (dotimes (x x)
                       (print x)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Parameter used in count form should not be flagged as unused"))))

(deftest lambda-list-keywords
  (testing "Valid: lambda-list keywords (&key, &optional, &rest) are not parameters"
    (let* ((code "(destructuring-bind (a &key b c)
                     (list 1 :b 2 :c 3)
                   (print b))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Should report 'a' and 'c' as unused, but NOT '&key'
      (ok (= (length violations) 2))
      (ok (search "Variable 'a' is unused"
                  (violation:violation-message (first violations))))
      (ok (search "Variable 'c' is unused"
                  (violation:violation-message (second violations))))))

  (testing "Valid: defun with &optional and &key"
    (let* ((code "(defun foo (x &optional y &key z)
                   (print x))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Should report 'y' and 'z' as unused, but NOT '&optional' or '&key'
      (ok (= (length violations) 2))
      (ok (some (lambda (v) (search "Variable 'y' is unused"
                                    (violation:violation-message v)))
                violations))
      (ok (some (lambda (v) (search "Variable 'z' is unused"
                                    (violation:violation-message v)))
                violations))))

  (testing "Valid: lambda with &rest"
    (let* ((code "(lambda (x &rest args)
                   (print x))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Should report 'args' as unused, but NOT '&rest'
      (ok (= (length violations) 1))
      (ok (search "Variable 'args' is unused"
                  (violation:violation-message (first violations)))))))

(deftest do-bindings
  (testing "Invalid: unused do variables"
    (let* ((code "(do ((i 0 (1+ i))
                        (j 0 (+ j 2))
                        (k 0 (+ k 3)))
                       ((>= i 10) i)
                     (print i))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; j and k are unused
      (ok (= (length violations) 2))))

  (testing "Valid: all do variables used"
    (let* ((code "(do ((i 0 (1+ i))
                        (sum 0 (+ sum i)))
                       ((>= i 10) sum)
                     (print i))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Invalid: do variable used only in its own step form"
    (let* ((code "(do ((i 0 (1+ i)))
                       ((>= i 10))
                     (print 'done))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; i is used in step form and test form, so it's used
      (ok (null violations))))

  (testing "Invalid: do variable used only in another variable's step"
    (let* ((code "(do ((i 0 (1+ i))
                        (j 0 (+ j i)))
                       ((>= i 10))
                     (print i))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; j is unused (only defined, never read outside step forms)
      (ok (= (length violations) 1))
      (ok (search "Variable 'j' is unused"
                  (violation:violation-message (first violations)))))))

(deftest do*-bindings
  (testing "Invalid: unused do* variables"
    (let* ((code "(do* ((i 0 (1+ i))
                         (j 0 (+ j 2))
                         (k 0 (+ k 3)))
                        ((>= i 10) i)
                      (print i))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; j and k are unused
      (ok (= (length violations) 2))))

  (testing "Valid: all do* variables used"
    (let* ((code "(do* ((i 0 (1+ i))
                         (sum 0 (+ sum i)))
                        ((>= i 10) sum)
                      (print i))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: do* sequential init forms (unlike do)"
    (let* ((code "(do* ((a 1)
                         (b (+ a 1))
                         (c (+ a b)))
                        ((>= c 10) c)
                      (print c))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; a and b are used in subsequent init forms, c is used in test and body
      (ok (null violations))))

  (testing "Invalid: do* variable used only in subsequent init form"
    (let* ((code "(do* ((a 1)
                         (b (+ a 1)))
                        ((>= b 10) b)
                      (print b))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; a is used in b's init form, so no violations
      (ok (null violations))))

  (testing "Invalid: do* variable unused in init forms and body"
    (let* ((code "(do* ((i 0 (1+ i))
                         (j 0 (+ j 2)))
                        ((>= i 10))
                      (print i))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; j is unused
      (ok (= (length violations) 1))
      (ok (search "Variable 'j' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Valid: do* mutual recursion in step forms (like do)"
    (let* ((code "(do* ((i 0 (1+ j))
                         (j 0 (1+ i)))
                        ((>= i 10) (+ i j)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Both i and j are used in each other's step forms and in result
      (ok (null violations)))))

(deftest loop-simple-for
  (testing "LOOP with simple FOR variable"
    (let* ((code "(loop for i from 1 to 10 collect (* i 2))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; i is used in collect
      (ok (= (length violations) 0)))))

(deftest loop-unused-for
  (testing "LOOP with unused FOR variable"
    (let* ((code "(loop for i from 1 to 10 collect 42)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-loop-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; i is unused
      (ok (= (length violations) 1))
      (ok (search "Variable 'i' is unused"
                  (violation:violation-message (first violations)))))))

(deftest loop-with-variable
  (testing "LOOP with WITH variable"
    (let* ((code "(loop with x = 10 for i from 1 to 5 collect (+ i x))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Both x and i are used
      (ok (= (length violations) 0)))))

(deftest loop-unused-with
  (testing "LOOP with unused WITH variable"
    (let* ((code "(loop with x = 10 for i from 1 to 5 collect i)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-loop-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; x is unused
      (ok (= (length violations) 1))
      (ok (search "Variable 'x' is unused"
                  (violation:violation-message (first violations)))))))

(deftest loop-destructuring
  (testing "LOOP with destructuring pattern"
    (let* ((code "(loop for (a b) in '((1 2) (3 4)) collect (+ a b))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Both a and b are used
      (ok (= (length violations) 0)))))

(deftest loop-destructuring-unused
  (testing "LOOP with unused variable in destructuring"
    (let* ((code "(loop for (a b) in '((1 2) (3 4)) collect a)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-loop-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; b is unused
      (ok (= (length violations) 1))
      (ok (search "Variable 'b' is unused"
                  (violation:violation-message (first violations)))))))

(deftest loop-shadowing
  (testing "LOOP variable shadowing outer binding"
    (let* ((code "(let ((x 1)) (loop for x from 1 to 5 collect x) x)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Outer x is used after loop, inner x is used in collect
      ;; No violations
      (ok (= (length violations) 0)))))

(deftest loop-shadowing-unused-outer
  (testing "LOOP variable shadowing unused outer binding"
    (let* ((code "(let ((x 1)) (loop for x from 1 to 5 collect x))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Outer x is unused (shadowed by loop x)
      (ok (= (length violations) 1))
      (ok (search "Variable 'x' is unused"
                  (violation:violation-message (first violations)))))))

(deftest lambda-list-with-defaults
  (testing "Bug: destructuring-bind with &optional and default value"
    (let* ((code "(destructuring-bind (user &optional (pass \"\"))
                     (list \"alice\" \"secret\")
                   (if user
                       (print pass)
                       nil))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Should have no violations - both user and pass are used
      ;; Bug: was reporting Variable '' is unused (empty string from default value)
      (ok (null violations))))

  (testing "Bug: defun with &optional default value"
    (let* ((code "(defun greet (name &optional (greeting \"Hello\"))
                   (format nil \"~A ~A\" greeting name))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Should have no violations - both name and greeting are used
      ;; Bug: was extracting \"Hello\" as a variable name
      (ok (null violations))))

  (testing "Bug: lambda with &key default value"
    (let* ((code "(lambda (x &key (y 10))
                   (+ x y))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Should have no violations - both x and y are used
      ;; Bug: was extracting 10 as a variable name
      (ok (null violations))))

  (testing "Bug: &optional with default and supplied-p"
    (let* ((code "(defun process (data &optional (validate t validate-p))
                   (when validate-p
                     (print validate))
                   data)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Should have no violations - all variables used
      ;; Bug: was extracting t as a variable name
      (ok (null violations))))

  (testing "Bug: &key with keyword and default value"
    (let* ((code "(lambda (x &key ((:name n) \"unknown\"))
                   (format nil \"~A: ~A\" x n))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Should have no violations - x and n are used
      ;; Bug: was extracting :name and \"unknown\" as variable names
      (ok (null violations))))

  (testing "Valid: unused &optional parameter with default"
    (let* ((code "(defun foo (x &optional (y 10))
                   (print x))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; y is unused, should report exactly one violation for 'y'
      ;; Not two violations (one for y, one for default value)
      (ok (= (length violations) 1))
      (ok (search "Variable 'y' is unused"
                  (violation:violation-message (first violations)))
          "Should only report 'y' as unused, not the default value")))

  (testing "Valid: &allow-other-keys is not a variable"
    (let* ((code "(defun foo (x &key y &allow-other-keys)
                   (print x))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Should only report 'y' as unused, NOT '&allow-other-keys'
      (ok (= (length violations) 1))
      (ok (search "Variable 'y' is unused"
                  (violation:violation-message (first violations)))
          "Should report only 'y' as unused")
      (ok (not (some (lambda (v)
                       (search "&allow-other-keys" (violation:violation-message v)))
                     violations))
          "Should NOT report &allow-other-keys as unused variable")))

  (testing "Valid: other lambda-list keywords are not variables"
    (let* ((code "(defmacro foo (x &body body)
                   `(progn ,x))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Should only report 'body' as unused, NOT '&body'
      (ok (= (length violations) 1))
      (ok (search "Variable 'body' is unused"
                  (violation:violation-message (first violations)))
          "Should report only 'body' as unused, not the &body keyword"))))

(deftest loop-do-cond-false-positive
  (testing "Bug: Variable used in LOOP DO COND test clause (trivial-glob pattern.lisp:60)"
    (let* ((code "(defun find-bracket-close (pattern start end)
                     (loop with i = start
                           while (< i end)
                           do (cond
                                ((char= (char pattern i) #\\])
                                 (return i))
                                (t
                                 (incf i)))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; 'pattern' is used in the COND test clause (char pattern i)
      ;; Bug: DO loop handler was matching LOOP DO keyword and stopping search
      (ok (null violations)
          "Should not report 'pattern' as unused when used in LOOP DO COND test")))

  (testing "Valid: Simpler LOOP DO COND case"
    (let* ((code "(defun test-cond (pattern start end)
                     (loop with i = start
                           while (< i end)
                           do (cond
                                ((char= (char pattern i) #\\x)
                                 (return i))
                                (t
                                 (incf i)))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "All parameters should be recognized as used")))

  (testing "Valid: LOOP DO with nested IF"
    (let* ((code "(loop for item in list
                        with pattern = \"test\"
                        do (if (string= item pattern)
                               (return item)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Pattern used in IF test should not be flagged as unused"))))

(deftest let-init-form-false-positive
  (testing "Bug: Variable used only in LET init form (trivial-glob pattern.lisp:70)"
    (let* ((code "(defun char-in-bracket-p (char bracket-content casefold)
                     (let ((negated nil)
                           (content bracket-content))
                       (when (> (length content) 0)
                         (setf negated t))
                       (list char content negated casefold)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; 'bracket-content' is used to initialize 'content'
      ;; Bug: extract-bindings was treating (content bracket-content) as destructuring
      ;; and extracting both as variable names, causing shadowing detection to fail
      (ok (null violations)
          "Should not report 'bracket-content' as unused when used in LET init form")))

  (testing "Valid: Simpler LET init form case"
    (let* ((code "(defun test-let (bracket-content)
                     (let ((content bracket-content))
                       (list content)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Parameter used in init form should not be flagged as unused")))

  (testing "Valid: Multiple LET bindings with init forms"
    (let* ((code "(defun process (input1 input2)
                     (let ((a input1)
                           (b input2))
                       (+ a b)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
           "All parameters used in init forms should be recognized"))

  (testing "Invalid: LET binding where init form is unused"
    (let* ((code "(defun test (unused-param)
                     (let ((x 10))
                       (print x)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; unused-param is truly unused
      (ok (= (length violations) 1))
      (ok (search "Variable 'unused-param' is unused"
                  (violation:violation-message (first violations)))
           "Should report unused-param but not x")))

  (testing "Valid: Chain of LET init forms"
    (let* ((code "(defun chain (input)
                     (let ((a input))
                       (let ((b a))
                         (let ((c b))
                           (print c)))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
           "Chained init forms should all be recognized as uses"))))

(deftest loop-variables-in-subsequent-clauses
  (testing "Bug: LOOP variable used in subsequent FOR clause (Coalton package.lisp case)"
    (let* ((code "(loop :for name :being :the :hash-keys :of result
                        :for data :being :the :hash-values :of result
                        :for real-time := (cdar data)
                        :for value := (coerce (cdr real-time) 'double-float)
                        :collect value)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; 'data' is used in (cdar data), 'real-time' is used in (cdr real-time)
      ;; Bug: parse-loop-clauses was only searching body clauses after DO/COLLECT
      ;; But variables can be used in subsequent FOR/WITH clauses
      (ok (null violations)
          "Variables used in subsequent FOR clauses should not be flagged as unused")))

  (testing "Valid: Simpler case of FOR variable used in next FOR"
    (let* ((code "(loop for x in list
                        for y = (* x 2)
                        collect y)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Variable 'x' used in subsequent FOR clause should not be unused")))

  (testing "Invalid: FOR variable not used anywhere"
    (let* ((code "(loop for x in list
                        for y = 42
                        collect y)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; 'x' is truly unused
      (ok (= (length violations) 1))
      (ok (search "Variable 'x' is unused"
                  (violation:violation-message (first violations)))
          "Should report 'x' as unused")))

  (testing "Valid: WITH variable used in FOR clause"
    (let* ((code "(loop with multiplier = 2
                        for i from 1 to 10
                        for doubled = (* i multiplier)
                        collect doubled)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "WITH variable used in FOR clause should not be unused"))))

(deftest loop-variables-in-accumulation-expressions
  (testing "Bug: LOOP variable used in SUMMING expression (Coalton matrix.lisp case)"
    (let* ((code "(loop :for i :from 0 :below 4
                        :do (loop :for j :from 0 :below 4
                                  :do (setf (aref r (+ (* i 4) j))
                                            (loop :for k :from 0 :below 4
                                                  :summing (* (aref a (+ (* i 4) k))
                                                              (aref b (+ (* k 4) j)))))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; 'k' is used in the SUMMING expression
      ;; Bug: parse-loop-clauses was only searching body after DO/COLLECT keywords
      ;; But SUMMING expression itself should be searched
      (ok (null violations)
          "Variable 'k' used in SUMMING should not be flagged as unused")))

  (testing "Valid: Variable used in COLLECTING expression"
    (let* ((code "(loop for x from 1 to 10
                        collecting (* x 2))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Variable used in COLLECTING expression should not be unused")))

  (testing "Valid: Variable used in APPEND expression"
    (let* ((code "(loop for x in '((1 2) (3 4))
                        append x)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Variable used in APPEND should not be unused")))

  (testing "Valid: Variable used in COUNT expression"
    (let* ((code "(loop for x in list
                        count (evenp x))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Variable used in COUNT should not be unused")))

  (testing "Valid: Variable used in MAXIMIZE expression"
    (let* ((code "(loop for item in list
                        for value = (compute-value item)
                        maximize value)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Variable used in MAXIMIZE should not be unused"))))

(deftest loop-nested-scoping
  (testing "Valid: Nested loops with proper scoping"
    (let* ((code "(loop for i from 1 to 3
                        collect (loop for j from 1 to 3
                                      collect (* i j)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Nested loop variables should have proper scoping")))

  (testing "Valid: Outer loop variable used in inner loop"
    (let* ((code "(loop for limit from 1 to 5
                        collect (loop for i from 1 to limit
                                      collect i))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Outer loop variable used in inner loop should not be unused")))

  (testing "Invalid: Outer loop variable shadowed and unused"
    (let* ((code "(loop for i from 1 to 3
                        collect (loop for i from 1 to 3
                                      collect i))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Outer 'i' is shadowed by inner loop and never used
      (ok (= (length violations) 1))
      (ok (search "Variable 'i' is unused"
                  (violation:violation-message (first violations)))
          "Shadowed outer loop variable should be flagged as unused")))

  (testing "Valid: Triple-nested loops (Coalton matrix.lisp pattern)"
    (let* ((code "(loop for i from 0 below 4
                        do (loop for j from 0 below 4
                                 do (loop for k from 0 below 4
                                          summing (* i j k))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "All variables in triple-nested loops should be recognized as used")))))

(deftest loop-nested-with-hash-value-bug
  (testing "Bug: LET variable used in nested LOOP with :being :the :hash-value"
    (let* ((code "(defun test (data)
                     (let ((my-var 42))
                       (loop :for instance :in data :do
                         (loop :for subitem :being :the :hash-value :of instance
                               :do (print my-var)
                               :do (print subitem)))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Debug: print violation info and position map
      (when violations
        (format t "~%Got ~D violation(s):~%" (length violations))
        (dolist (v violations)
          (format t "  ~A at ~D:~D - ~A~%"
                  (violation:violation-rule v)
                  (violation:violation-line v)
                  (violation:violation-column v)
                  (violation:violation-message v)))
        ;; Print code lines to see what 5:42 actually points to
        (let ((lines (uiop:split-string code :separator '(#\Newline))))
          (format t "~%Code lines:~%")
          (loop for line in lines
                for i from 1
                do (format t "  Line ~D: ~A~%" i line))))
      (ok (null violations)
          "LET variable used in nested LOOP should not be flagged as unused")))
  (testing "Bug: Same with bare LOOP keywords (no colons)"
    (let* ((code "(defun test (data)
                     (let ((my-var 42))
                       (loop for instance in data do
                         (loop for subitem in instance
                               do (print my-var)
                               do (print subitem)))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "LET variable used in nested LOOP with bare keywords should not be flagged as unused"))))

(deftest backquote-unquote-false-positives
  (testing "Bug: LOOP destructuring variable unused but in backquote (from Coalton)"
    ;; This is a false positive - 'value' is truly unused, not a bug
    ;; But documenting the pattern from original investigation
    (let* ((code "(defun test ()
                     (loop :for (name . value) :in data
                           :collect `(use ,name)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-loop-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; 'value' is genuinely unused - this should report a violation
      ;; NOTE: This may actually be correct behavior
      (ok (= (length violations) 1))
      (ok (search "Variable 'value' is unused"
                  (violation:violation-message (first violations)))
          "Variable 'value' is genuinely unused in this case")))

  (testing "Valid: Parameter used before being shadowed by nested LOOP"
    ;; This is the pattern from Coalton codegen-expression.lisp - now fixed!
    (let* ((code "(defun test (node)
                     (process node)
                     (loop :for node :in items
                           :collect node))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Parameter 'node' is used in (process node), then shadowed by LOOP
      (ok (null violations)
          "Parameter should be recognized as used before shadowing")))

  ;; KNOWN FALSE POSITIVES - These tests document remaining issues to be fixed
  ;; Using `failing` to mark tests that should fail until the bugs are fixed

  (testing "Bug: LOOP variable used in subsequent clause and backquote (from Coalton) - FIXED!"
    ;; This was a false positive that is now fixed by the two-phase shadow architecture
    ;; Real Coalton pattern: node used in subsequent FOR clause, name and arity used in backquote
    (let* ((code "(defun test ()
                     (loop :for (name . node) :in bindings
                           :for arity := (get-arity node)
                           :collect `(setf ,name (make-entry ,arity))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; 'name' and 'arity' used in unquotes, 'node' used in subsequent FOR clause
      ;; Fixed: Two-phase architecture correctly handles shadowing in LOOP destructuring
      (ok (null violations)
          "All variables should be recognized as used")))

  (testing "Bug: Deep nesting with COND and parameter shadowing"
    ;; Complex case with LET, COND, LET*, LOOP
    (let* ((code "(defun test (node)
                     (let ((bindings (get-bindings node)))
                       (cond
                         ((condition bindings)
                          (let* ((inner (recurse node))
                                 (result
                                   (loop :for (name . node) :in bindings
                                         :collect `(use ,name ,inner))))
                            result)))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Parameter 'node' is used in (get-bindings node) on line 2
      ;; and in (recurse node) on line 5, but shadowed by LOOP on line 7
      (ok (null violations)
          "Parameter 'node' should be recognized as used despite LOOP shadowing")))

  (testing "Bug: LET* nesting with parameter shadowing"
    ;; Another pattern with LET* sequential bindings
    (let* ((code "(defun test (node)
                     (let* ((x (process node))
                            (y (loop :for node :in items
                                     :collect (cons x node))))
                       y))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Parameter 'node' is used in (process node) before being shadowed
      (ok (null violations)
          "Parameter should be recognized as used in LET* init form"))))

(deftest nested-loop-shadow-bug
  (testing "Bug: Outer LOOP variable used in nested LOOP (Coalton codegen-expression.lisp:521)"
    ;; Simplified version of the Coalton code that triggers the bug:
    ;; Line 521: (loop :for (name . initform) :in scc-bindings
    ;;                 :appending (loop :for arg :in (node-rands initform)
    ;;                                  :collect `(setf ,(setf-accessor ctor-info i name) ...)))
    ;; The bug: 'name' is reported as unused even though it's used in the nested loop's backquote
    (let* ((code "(defun test ()
                     (let ((pairs '((a . (1 2 3)) (b . (4 5 6)))))
                       (loop :for (name . items) :in pairs
                             :appending (loop :for i :in items
                                              :collect `(setf ,(list 'value i name)
                                                              ,(+ i 10))))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; 'name' from outer loop is used in nested loop's backquote: ,(list 'value i name)
      ;; Bug: Nested LOOP creates complete shadow, blocking search for outer variables
      ;; This should pass (no violations) but currently fails (reports name as unused)
      (when violations
        (format t "~%Got ~D violation(s):~%" (length violations))
        (dolist (v violations)
          (format t "  ~A at ~D:~D - ~A~%"
                  (violation:violation-rule v)
                  (violation:violation-line v)
                  (violation:violation-column v)
                  (violation:violation-message v))))
      (ok (null violations)
          "Outer LOOP variable used in nested LOOP should not be flagged as unused")))

  (testing "Bug: Simpler nested LOOP case"
    (let* ((code "(loop :for (name . value) :in pairs
                        :appending (loop :for item :in value
                                         :collect (cons name item)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (when violations
        (format t "~%Simple case violations: ~A~%" violations))
      (ok (null violations)
          "Outer name used in inner loop should not be unused"))))

;; Helper macro for testing suppression with the engine
(defmacro with-temp-lint-file ((code rule-name &optional (file-name "test-suppression.lisp")) &body body)
  `(let* ((test-file (merge-pathnames ,file-name (uiop:temporary-directory)))
          (config (mallet/config:make-config
                   :rules (list (rules:make-rule ,rule-name)))))
     (unwind-protect
          (progn
            (with-open-file (out test-file :direction :output :if-exists :supersede)
              (write-string ,code out))
            (let ((violations (mallet/engine:lint-file test-file :config config)))
              ,@body))
       (when (probe-file test-file)
         (delete-file test-file)))))

(deftest unused-variables-suppression
  (testing "Outer scope suppression propagates to nested LET"
    (with-temp-lint-file ("(defun test-outer-suppression ()
  (declare (mallet:suppress unused-variables))
  (let ((x 1))
    (let ((y 2))
      (let ((z 3))
        nil))))
" :unused-variables)
      (ok (null violations)
          "All nested unused variables should be suppressed")))

  (testing "Mid-level suppression only affects its scope and children"
    (let* ((code "(defun test-mid-suppression ()
                     (let ((a 1))
                       (let ((b 2))
                         (declare (mallet:suppress unused-variables))
                         (let ((c 3))
                           nil))))")
           (forms (let ((*features* (cons :mallet *features*)))
                    (parser:parse-forms code #p"test.lisp")))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; 'a' is not suppressed (outer scope), 'b' and 'c' are suppressed
      (ok (= (length violations) 1))
      (ok (search "Variable 'a' is unused"
                  (violation:violation-message (first violations)))
          "Only outer variable 'a' should be flagged as unused")))

  (testing "No suppression - all violations reported"
    (let* ((code "(defun test-no-suppression ()
                     (let ((x 1))
                       (let ((y 2))
                         nil)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 2))
      (ok (some (lambda (v) (search "Variable 'x' is unused"
                                    (violation:violation-message v)))
                violations))
      (ok (some (lambda (v) (search "Variable 'y' is unused"
                                    (violation:violation-message v)))
                violations))))

  (testing "Suppression in deeply nested LABELS with LET"
    (let* ((code "(defun test-nested-labels ()
                     (declare (mallet:suppress unused-variables))
                     (let ((x 1))
                       (labels ((f ()
                                  (let ((y 2))
                                    (let ((z 3))
                                      nil))))
                         nil)))")
           (forms (let ((*features* (cons :mallet *features*)))
                    (parser:parse-forms code #p"test.lisp")))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Suppression should propagate through LABELS to nested LETs")))

  (testing "Suppression with MULTIPLE-VALUE-BIND and DESTRUCTURING-BIND"
    (let* ((code "(defun test-mvb ()
                     (declare (mallet:suppress unused-variables))
                     (multiple-value-bind (a b c) (values 1 2 3)
                       (destructuring-bind (x y z) '(1 2 3)
                         nil)))")
           (forms (let ((*features* (cons :mallet *features*)))
                    (parser:parse-forms code #p"test.lisp")))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Suppression should work with multiple-value-bind and destructuring-bind")))

  (testing "Suppression with DO and DO*"
    (let* ((code "(defun test-do ()
                     (declare (mallet:suppress unused-variables))
                     (do ((i 0 (1+ i))
                          (j 0 (+ j 2)))
                         ((>= i 10))
                       (print 'done))
                     (do* ((a 0)
                           (b 0))
                          ((>= a 10))
                       (print 'done)))")
           (forms (let ((*features* (cons :mallet *features*)))
                    (parser:parse-forms code #p"test.lisp")))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Suppression should work with do and do* bindings")))

  (testing "Suppression with DOLIST"
    (let* ((code "(defun test-dolist ()
                     (declare (mallet:suppress unused-variables))
                     (let ((sum 0))
                       (dolist (item '(1 2 3))
                         (incf sum))
                       sum))")
           (forms (let ((*features* (cons :mallet *features*)))
                    (parser:parse-forms code #p"test.lisp")))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Suppression should work with dolist")))

  (testing "Partial suppression - mix of suppressed and non-suppressed"
    (let* ((code "(defun test-partial ()
                     (let ((a 1))
                       (let ((b 2))
                         (declare (mallet:suppress unused-variables))
                         (let ((c 3))
                           nil))
                       (let ((d 4))
                         nil)))")
           (forms (let ((*features* (cons :mallet *features*)))
                    (parser:parse-forms code #p"test.lisp")))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; 'a', 'd' not suppressed; 'b', 'c' suppressed
      (ok (= (length violations) 2))
      (ok (some (lambda (v) (search "Variable 'a' is unused"
                                    (violation:violation-message v)))
                violations))
      (ok (some (lambda (v) (search "Variable 'd' is unused"
                                    (violation:violation-message v)))
                violations)))))

(deftest unused-loop-variables-suppression
  (testing "Outer scope suppression propagates to LOOP variables"
    (let* ((code "(defun test-loop-suppression ()
                     (declare (mallet:suppress unused-loop-variables))
                     (loop for i from 1 to 10 collect 42)
                     (loop with x = 10 for j from 1 to 5 collect j))")
           (forms (let ((*features* (cons :mallet *features*)))
                    (parser:parse-forms code #p"test.lisp")))
           (rule (make-instance 'rules:unused-loop-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "All unused loop variables should be suppressed")))

  (testing "Mid-level suppression in nested LOOPs"
    (let* ((code "(defun test-nested-loops ()
                     (loop for i from 1 to 3
                           collect (progn
                                     (declare (mallet:suppress unused-loop-variables))
                                     (loop for j from 1 to 3 collect 42))))")
           (forms (let ((*features* (cons :mallet *features*)))
                    (parser:parse-forms code #p"test.lisp")))
           (rule (make-instance 'rules:unused-loop-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; Outer 'i' is not suppressed (used in collect), inner 'j' is suppressed
      (ok (= (length violations) 1))
      (ok (search "Variable 'i' is unused"
                  (violation:violation-message (first violations)))
          "Only outer loop variable 'i' should be flagged")))

  (testing "No suppression - loop violations reported"
    (let* ((code "(loop for i from 1 to 10 collect 42)")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-loop-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (search "Variable 'i' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Suppression with LOOP destructuring"
    (let* ((code "(defun test-destructuring ()
                     (declare (mallet:suppress unused-loop-variables))
                     (loop for (a b) in '((1 2) (3 4)) collect a))")
           (forms (let ((*features* (cons :mallet *features*)))
                    (parser:parse-forms code #p"test.lisp")))
           (rule (make-instance 'rules:unused-loop-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Suppression should work with loop destructuring")))

  (testing "Suppression with LOOP WITH variable"
    (let* ((code "(defun test-with ()
                     (declare (mallet:suppress unused-loop-variables))
                     (loop with x = 10
                           for i from 1 to 5
                           collect i))")
           (forms (let ((*features* (cons :mallet *features*)))
                    (parser:parse-forms code #p"test.lisp")))
           (rule (make-instance 'rules:unused-loop-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Suppression should work with LOOP WITH variable")))

  (testing "Nested suppression in deeply nested LOOPs"
    (let* ((code "(defun test-triple-nested ()
                     (declare (mallet:suppress unused-loop-variables))
                     (loop for i from 1 to 3
                           collect (loop for j from 1 to 3
                                         collect (loop for k from 1 to 3
                                                       collect 42))))")
           (forms (let ((*features* (cons :mallet *features*)))
                    (parser:parse-forms code #p"test.lisp")))
           (rule (make-instance 'rules:unused-loop-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Suppression should propagate through all nesting levels")))

  (testing "LOOP inside LET with suppression"
    (let* ((code "(defun test-loop-in-let ()
                     (declare (mallet:suppress unused-loop-variables))
                     (let ((sum 0))
                       (loop for i from 1 to 10 do (incf sum))
                       sum))")
           (forms (let ((*features* (cons :mallet *features*)))
                    (parser:parse-forms code #p"test.lisp")))
           (rule (make-instance 'rules:unused-loop-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Suppression should work for LOOP inside LET"))))
;; Add these tests to the end of unused-variables-test.lisp

;; Helper macro for testing suppression with the engine
(defmacro with-temp-lint-file ((code rule-name &optional (file-name "test-suppression.lisp")) &body body)
  `(let* ((test-file (merge-pathnames ,file-name (uiop:temporary-directory)))
          (config (mallet/config:make-config
                   :rules (list (rules:make-rule ,rule-name)))))
     (unwind-protect
          (progn
            (with-open-file (out test-file :direction :output :if-exists :supersede)
              (write-string ,code out))
            (let ((violations (mallet/engine:lint-file test-file :config config)))
              ,@body))
       (when (probe-file test-file)
         (delete-file test-file)))))

(deftest unused-variables-suppression
  (testing "Suppression propagates through nested LETs"
    (with-temp-lint-file ("(defun test ()
  (declare (mallet:suppress unused-variables))
  (let ((x 1))
    (let ((y 2))
      nil)))
" :unused-variables)
      (ok (null violations) "All nested variables should be suppressed")))

  (testing "Mid-level suppression only affects children"
    (with-temp-lint-file ("(defun test ()
  (let ((a 1))
    (let ((b 2))
      (declare (mallet:suppress unused-variables))
      (let ((c 3))
        nil))))
" :unused-variables)
      (ok (= (length violations) 1) "Only outer variable should trigger")
      (ok (search "Variable 'a'" (violation:violation-message (first violations))))))

  (testing "No suppression - all reported"
    (with-temp-lint-file ("(defun test ()
  (let ((x 1))
    (let ((y 2))
      nil)))
" :unused-variables)
      (ok (= (length violations) 2) "Both variables should trigger"))))

(deftest unused-loop-variables-suppression
  (testing "Suppression propagates to loop variables"
    (with-temp-lint-file ("(defun test ()
  (declare (mallet:suppress unused-loop-variables))
  (loop for i from 1 to 10 collect 42))
" :unused-loop-variables)
      (ok (null violations) "Loop variables should be suppressed")))

  (testing "No suppression - loop violations reported"
    (with-temp-lint-file ("(loop for i from 1 to 10 collect 42)
" :unused-loop-variables)
      (ok (= (length violations) 1) "Unused loop variable should trigger")
      (ok (search "Variable 'i'" (violation:violation-message (first violations)))))))

(deftest function-vs-variable-namespace
  ;; KNOWN LIMITATION: Cannot detect unused variables that shadow user-defined functions.
  ;; Without runtime information, we can't distinguish user-defined functions from unknown macros.
  ;; For example, (defun foo (bar) (bar 10)) - 'bar' parameter should be flagged as unused,
  ;; but we conservatively treat it as potentially used to avoid false positives with macros.
  ;; Trade-off: False negatives (miss some unused vars) > False positives (flag used vars).

  (testing "Valid: Variable used in value position (not function call)"
    (let* ((code "(defun process (handler)
                     (funcall handler 42))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Parameter 'handler' is used as a value in funcall")))

  (testing "Valid: Variable used in function position via funcall/apply"
    (let* ((code "(defun call-it (fn arg)
                     (funcall fn arg))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Both parameters are used")))

  (testing "Valid: FLET/LABELS creates function binding, not variable"
    (let* ((code "(defun outer (x)
                     (flet ((helper (y) (+ x y)))
                       (helper 10)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      ;; 'x' is used in helper's body, so no violations
      (ok (null violations)
          "Parameter 'x' is properly used in FLET"))))

(deftest unused-local-functions-flet
  (testing "Invalid: FLET local function is unused"
    (let* ((code "(defun baz ()
                     (flet ((double (x)
                              (* x x)))
                       (print 'ok)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-local-functions-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1)
          "Unused local function 'double' should be flagged")
      (ok (search "Local function 'double' is unused"
                  (violation:violation-message (first violations)))
          "Should report local function as unused")))

  (testing "Valid: FLET local function is used"
    (let* ((code "(defun process (n)
                     (flet ((square (x) (* x x)))
                       (square n)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-local-functions-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Local function 'square' is used")))

  (testing "Valid: FLET local function used via #'"
    (let* ((code "(defun mapper (list)
                     (flet ((doubler (x) (* x 2)))
                       (mapcar #'doubler list)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-local-functions-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Local function used via #' should be detected")))

  (testing "Valid: FLET local function used via FUNCALL"
    (let* ((code "(defun caller (n)
                     (flet ((add-one (x) (1+ x)))
                       (funcall 'add-one n)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-local-functions-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Local function used via funcall should be detected")))

  (testing "Invalid: Multiple FLET functions, one unused"
    (let* ((code "(defun compute (n)
                     (flet ((double (x) (* x 2))
                            (triple (x) (* x 3)))
                       (double n)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-local-functions-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1)
          "One of two local functions should be flagged")
      (ok (search "Local function 'triple' is unused"
                  (violation:violation-message (first violations)))
          "Should report 'triple' as unused"))))

(deftest unused-local-functions-labels
  (testing "Invalid: LABELS local function is unused"
    (let* ((code "(defun factorial-bad (n)
                     (labels ((helper (acc x)
                                (if (zerop x)
                                    acc
                                    (helper (* acc x) (1- x)))))
                       n))")  ; Helper is defined but not used
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-local-functions-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1)
          "Unused local function 'helper' should be flagged")
      (ok (search "Local function 'helper' is unused"
                  (violation:violation-message (first violations)))
          "Should report local function as unused")))

  (testing "Valid: LABELS local function is used"
    (let* ((code "(defun factorial (n)
                     (labels ((helper (acc x)
                                (if (zerop x)
                                    acc
                                    (helper (* acc x) (1- x)))))
                       (helper 1 n)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-local-functions-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Recursive local function should be detected as used")))

  (testing "Valid: LABELS functions can reference each other"
    (let* ((code "(defun even-odd (n)
                     (labels ((is-even (x)
                                (if (zerop x) t (is-odd (1- x))))
                              (is-odd (x)
                                (if (zerop x) nil (is-even (1- x)))))
                       (is-even n)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-local-functions-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Mutually recursive functions should both be detected as used")))

  (testing "Invalid: LABELS with one unused in mutual recursion"
    (let* ((code "(defun test (n)
                     (labels ((is-even (x)
                                (if (zerop x) t (is-odd (1- x))))
                              (is-odd (x)
                                (if (zerop x) nil (is-even (1- x))))
                              (never-used (x) x))
                       (is-even n)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-local-functions-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1)
          "One unused function in LABELS should be flagged")
      (ok (search "Local function 'never-used' is unused"
                  (violation:violation-message (first violations)))
          "Should report 'never-used' as unused"))))

(deftest unknown-macro-car-position
  (testing "Variable in CAR position inside unknown macro should NOT report unused"
    (let* ((code "(let ((x 1))
                     (with-foo ()
                       (x 10)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Variable 'x' in CAR position in unknown context should not be flagged")))

  (testing "Variable in CAR position with known macro inside unknown macro"
    (let* ((code "(let ((lock *lock*))
                     (with-lock-held (lock)
                       (1+ x)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Variable 'lock' used in unknown macro args should not be flagged")))

  (testing "Variable shadowing function name in CAR - known context should report unused"
    (let* ((code "(let ((list (fetch-foo)))
                     (list 'a 'b))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1)
          "In known context, CAR 'list' is function, not variable - should report unused")
      (ok (search "Variable 'list' is unused"
                  (violation:violation-message (first violations)))
          "Should report 'list' variable as unused")))

  (testing "Known function inside unknown macro should be conservative"
    (let* ((code "(let ((list (fetch-foo)))
                     (with-bar ()
                       (list 'a)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Even though 'list' is known function, in unknown context be conservative")))

  (testing "Nested unknown macros with variable in CAR"
    (let* ((code "(let ((foo 1))
                     (with-bar ()
                       (with-baz ()
                         (foo 10))))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Variable 'foo' in CAR within nested unknown macros should not be flagged")))

  (testing "Variable used in unknown macro argument list"
    (let* ((code "(let ((x 1))
                     (unknown-macro (x 10) body))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Variable 'x' in CAR in unknown macro args should not be flagged")))

  (testing "Multiple variables in unknown macro"
    (let* ((code "(let ((x 1) (y 2))
                     (with-custom-syntax ()
                       (x y)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)
          "Both variables in unknown macro should not be flagged"))))

(deftest handler-case-variables-valid
  (testing "Valid: handler-case with used condition variable"
    (let* ((code "(handler-case
                       (some-function)
                     (error (e)
                       (format t \"Error: ~A\" e)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: handler-case with multiple handlers, all used"
    (let* ((code "(handler-case
                       (some-function)
                     (type-error (e)
                       (log-error e))
                     (parse-error (err)
                       (handle-parse-error err)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: handler-case with underscore variable"
    (let* ((code "(handler-case
                       (some-function)
                     (error (_e)
                       (format t \"An error occurred\")))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: handler-case with no-error clause"
    (let* ((code "(handler-case
                       (some-function)
                     (error (e)
                       (format t \"Error: ~A\" e))
                     (:no-error (result)
                       (process result)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)))))

(deftest handler-case-variables-invalid
  (testing "Invalid: handler-case with unused condition variable"
    (let* ((code "(handler-case
                       (some-function)
                     (error (e)
                       (format t \"An error occurred\")))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :unused-variables))
      (ok (search "Variable 'e' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Invalid: handler-case with multiple unused variables"
    (let* ((code "(handler-case
                       (some-function)
                     (type-error (e)
                       (log-error \"Type error\"))
                     (parse-error (err)
                       (handle-parse-error)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 2))
      (ok (every (lambda (v)
                   (eq (violation:violation-rule v) :unused-variables))
                 violations))))

  (testing "Invalid: handler-case with unused no-error variable"
    (let* ((code "(handler-case
                       (some-function)
                     (:no-error (result other)
                       (process result)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (search "Variable 'other' is unused"
                  (violation:violation-message (first violations)))))))

(deftest restart-case-variables-valid
  (testing "Valid: restart-case with used restart parameters"
    (let* ((code "(restart-case
                       (some-function)
                     (retry ()
                       (retry-operation))
                     (use-value (value)
                       (process value)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations))))

  (testing "Valid: restart-case with underscore parameter"
    (let* ((code "(restart-case
                       (some-function)
                     (use-value (_value)
                       (use-default)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (null violations)))))

(deftest restart-case-variables-invalid
  (testing "Invalid: restart-case with unused parameter"
    (let* ((code "(restart-case
                       (some-function)
                     (use-value (value)
                       (use-default)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations))
              :unused-variables))
      (ok (search "Variable 'value' is unused"
                  (violation:violation-message (first violations))))))

  (testing "Invalid: restart-case with multiple unused parameters"
    (let* ((code "(restart-case
                       (some-function)
                     (use-value (value)
                       (use-default))
                     (use-other (other)
                       (use-default-other)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #p"test.lisp")))
      (ok (= (length violations) 2))
      (ok (every (lambda (v)
                   (eq (violation:violation-rule v) :unused-variables))
                 violations)))))
