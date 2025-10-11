(defpackage #:malo/tests/rules/with-macros
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:malo/rules)
   (#:parser #:malo/parser)
   (#:violation #:malo/violation)))
(in-package #:malo/tests/rules/with-macros)

;;; Tests for WITH-* and DO-SYMBOLS family macros

(deftest do-symbols-used
  (testing "DO-SYMBOLS with used variable should not report violations"
    (let* ((code "(do-symbols (sym *package*)
                    (print sym))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule)))
      (ok (null (rules:check-form rule (first forms) #P"/tmp/test.lisp"))
          "No violation for used DO-SYMBOLS variable"))))

(deftest do-symbols-unused
  (testing "DO-SYMBOLS with unused variable should report violation"
    (let* ((code "(do-symbols (sym *package*)
                    (print \"hello\"))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #P"/tmp/test.lisp")))
      (ok (= (length violations) 1)
          "One violation for unused DO-SYMBOLS variable")
      (ok (string= (violation:violation-message (first violations))
                   "Variable 'sym' is unused")
          "Correct violation message"))))

(deftest do-external-symbols-used
  (testing "DO-EXTERNAL-SYMBOLS with used variable should not report violations"
    (let* ((code "(do-external-symbols (sym *package*)
                    (print sym))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule)))
      (ok (null (rules:check-form rule (first forms) #P"/tmp/test.lisp"))
          "No violation for used DO-EXTERNAL-SYMBOLS variable"))))

(deftest do-external-symbols-unused
  (testing "DO-EXTERNAL-SYMBOLS with unused variable should report violation"
    (let* ((code "(do-external-symbols (sym *package*)
                    (print \"hello\"))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #P"/tmp/test.lisp")))
      (ok (= (length violations) 1)
          "One violation for unused DO-EXTERNAL-SYMBOLS variable"))))

(deftest do-all-symbols-used
  (testing "DO-ALL-SYMBOLS with used variable should not report violations"
    (let* ((code "(do-all-symbols (sym)
                    (print sym))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule)))
      (ok (null (rules:check-form rule (first forms) #P"/tmp/test.lisp"))
          "No violation for used DO-ALL-SYMBOLS variable"))))

(deftest do-all-symbols-unused
  (testing "DO-ALL-SYMBOLS with unused variable should report violation"
    (let* ((code "(do-all-symbols (sym)
                    (print \"hello\"))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #P"/tmp/test.lisp")))
      (ok (= (length violations) 1)
          "One violation for unused DO-ALL-SYMBOLS variable"))))

(deftest with-slots-used
  (testing "WITH-SLOTS with used slots should not report violations"
    (let* ((code "(with-slots (slot1 slot2) obj
                    (list slot1 slot2))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule)))
      (ok (null (rules:check-form rule (first forms) #P"/tmp/test.lisp"))
          "No violation for used WITH-SLOTS variables"))))

(deftest with-slots-unused
  (testing "WITH-SLOTS with unused slot should report violation"
    (let* ((code "(with-slots (slot1 slot2) obj
                    (print slot2))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #P"/tmp/test.lisp")))
      (ok (= (length violations) 1)
          "One violation for unused WITH-SLOTS slot")
      (ok (string= (violation:violation-message (first violations))
                   "Variable 'slot1' is unused")
          "Correct violation message"))))

(deftest with-slots-renamed
  (testing "WITH-SLOTS with (var slot-name) syntax should work"
    (let* ((code "(with-slots ((x slot-x) (y slot-y)) obj
                    (list x y))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule)))
      (ok (null (rules:check-form rule (first forms) #P"/tmp/test.lisp"))
          "No violation for used renamed slots"))))

(deftest with-slots-renamed-unused
  (testing "WITH-SLOTS with unused renamed slot should report violation"
    (let* ((code "(with-slots ((x slot-x) (y slot-y)) obj
                    (print y))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #P"/tmp/test.lisp")))
      (ok (= (length violations) 1)
          "One violation for unused renamed slot")
      (ok (string= (violation:violation-message (first violations))
                   "Variable 'x' is unused")
          "Correct violation message"))))

(deftest with-accessors-used
  (testing "WITH-ACCESSORS with used accessors should not report violations"
    (let* ((code "(with-accessors ((x get-x) (y get-y)) obj
                    (list x y))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule)))
      (ok (null (rules:check-form rule (first forms) #P"/tmp/test.lisp"))
          "No violation for used WITH-ACCESSORS variables"))))

(deftest with-accessors-unused
  (testing "WITH-ACCESSORS with unused accessor should report violation"
    (let* ((code "(with-accessors ((x get-x) (y get-y)) obj
                    (print y))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #P"/tmp/test.lisp")))
      (ok (= (length violations) 1)
          "One violation for unused WITH-ACCESSORS accessor")
      (ok (string= (violation:violation-message (first violations))
                   "Variable 'x' is unused")
          "Correct violation message"))))

(deftest with-input-from-string-used
  (testing "WITH-INPUT-FROM-STRING with used stream should not report violations"
    (let* ((code "(with-input-from-string (stream \"hello\")
                    (read stream))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule)))
      (ok (null (rules:check-form rule (first forms) #P"/tmp/test.lisp"))
          "No violation for used WITH-INPUT-FROM-STRING stream"))))

(deftest with-input-from-string-unused
  (testing "WITH-INPUT-FROM-STRING with unused stream should report violation"
    (let* ((code "(with-input-from-string (stream \"hello\")
                    (print \"world\"))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #P"/tmp/test.lisp")))
      (ok (= (length violations) 1)
          "One violation for unused WITH-INPUT-FROM-STRING stream")
      (ok (string= (violation:violation-message (first violations))
                   "Variable 'stream' is unused")
          "Correct violation message"))))

(deftest with-output-to-string-used
  (testing "WITH-OUTPUT-TO-STRING with used stream should not report violations"
    (let* ((code "(with-output-to-string (stream)
                    (print \"hello\" stream))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule)))
      (ok (null (rules:check-form rule (first forms) #P"/tmp/test.lisp"))
          "No violation for used WITH-OUTPUT-TO-STRING stream"))))

(deftest with-output-to-string-unused
  (testing "WITH-OUTPUT-TO-STRING with unused stream should report violation"
    (let* ((code "(with-output-to-string (stream)
                    (print \"hello\"))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #P"/tmp/test.lisp")))
      (ok (= (length violations) 1)
          "One violation for unused WITH-OUTPUT-TO-STRING stream")
      (ok (string= (violation:violation-message (first violations))
                   "Variable 'stream' is unused")
          "Correct violation message"))))

(deftest with-open-file-used
  (testing "WITH-OPEN-FILE with used stream should not report violations"
    (let* ((code "(with-open-file (stream \"file.txt\")
                    (read-line stream))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule)))
      (ok (null (rules:check-form rule (first forms) #P"/tmp/test.lisp"))
          "No violation for used WITH-OPEN-FILE stream"))))

(deftest with-open-file-unused
  (testing "WITH-OPEN-FILE with unused stream should report violation"
    (let* ((code "(with-open-file (stream \"file.txt\")
                    (print \"hello\"))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #P"/tmp/test.lisp")))
      (ok (= (length violations) 1)
          "One violation for unused WITH-OPEN-FILE stream")
      (ok (string= (violation:violation-message (first violations))
                   "Variable 'stream' is unused")
          "Correct violation message"))))

(deftest with-macros-check-body
  (testing "WITH-* macros should check bodies for unused variables"
    (let* ((code "(with-slots (slot1 slot2) obj
                    (let ((unused-var 1))
                      (print slot1)))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #P"/tmp/test.lisp")))
      (ok (= (length violations) 2)
          "Two violations: unused slot and unused LET variable")
      (ok (some (lambda (v)
                  (string= (violation:violation-message v)
                           "Variable 'slot2' is unused"))
                violations)
          "Violation for unused slot")
      (ok (some (lambda (v)
                  (string= (violation:violation-message v)
                           "Variable 'unused-var' is unused"))
                violations)
          "Violation for unused variable in body"))))

(deftest do-symbols-check-body
  (testing "DO-SYMBOLS should check body for unused variables"
    (let* ((code "(do-symbols (sym *package*)
                    (let ((unused-var 1))
                      (print sym)))")
           (forms (parser:parse-forms code #P"/tmp/test.lisp"))
           (rule (make-instance 'rules:unused-variables-rule))
           (violations (rules:check-form rule (first forms) #P"/tmp/test.lisp")))
      (ok (= (length violations) 1)
          "One violation for unused variable in body")
      (ok (string= (violation:violation-message (first violations))
                   "Variable 'unused-var' is unused")
          "Correct violation message"))))
