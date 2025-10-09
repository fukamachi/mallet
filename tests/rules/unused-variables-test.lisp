(defpackage #:malo/tests/rules/unused-variables
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:malo/rules)
   (#:parser #:malo/parser)
   (#:violation #:malo/violation)))
(in-package #:malo/tests/rules/unused-variables)

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
           (rule (make-instance 'rules:unused-variables-rule))
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
      (ok (null violations)))))

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
      (ok (null violations)))))

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
           (rule (make-instance 'rules:unused-variables-rule))
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
           (rule (make-instance 'rules:unused-variables-rule))
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
           (rule (make-instance 'rules:unused-variables-rule))
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
