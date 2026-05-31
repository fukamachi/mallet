(defpackage #:mallet/tests/rules/with-macros
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/with-macros)

;;; Tests for WITH-* and DO-SYMBOLS family macros

(defun check-unused-variables (code)
  (let ((forms (parser:parse-forms code #P"/tmp/test.lisp"))
        (rule (make-instance 'rules:unused-variables-rule)))
    (rules:check-form rule (first forms) #P"/tmp/test.lisp")))

(defparameter *with-macro-binding-cases*
  '((do-symbols
        "(do-symbols (sym *package*)
        (print sym))"
      0
      nil)
    (do-symbols
        "(do-symbols (sym *package*)
        (print \"hello\"))"
      1
      ("Variable 'sym' is unused"))
    (do-external-symbols
        "(do-external-symbols (sym *package*)
        (print sym))"
      0
      nil)
    (do-external-symbols
        "(do-external-symbols (sym *package*)
        (print \"hello\"))"
      1
      ("Variable 'sym' is unused"))
    (do-all-symbols
        "(do-all-symbols (sym)
        (print sym))"
      0
      nil)
    (do-all-symbols
        "(do-all-symbols (sym)
        (print \"hello\"))"
      1
      ("Variable 'sym' is unused"))
    (with-slots
          "(with-slots (slot1 slot2) obj
        (list slot1 slot2))"
        0
      nil)
    (with-slots
          "(with-slots (slot1 slot2) obj
        (print slot2))"
        1
      ("Variable 'slot1' is unused"))
    (with-accessors
          "(with-accessors ((x get-x) (y get-y)) obj
        (list x y))"
        0
      nil)
    (with-accessors
          "(with-accessors ((x get-x) (y get-y)) obj
        (print y))"
        1
      ("Variable 'x' is unused"))
    (with-input-from-string
        "(with-input-from-string (stream \"hello\")
        (read stream))"
      0
      nil)
    (with-input-from-string
        "(with-input-from-string (stream \"hello\")
        (print \"world\"))"
      1
      ("Variable 'stream' is unused"))
    (with-output-to-string
        "(with-output-to-string (stream)
        (print \"hello\" stream))"
      0
      nil)
    (with-output-to-string
        "(with-output-to-string (stream)
        (print \"hello\"))"
      1
      ("Variable 'stream' is unused"))
    (with-open-file
        "(with-open-file (stream \"file.txt\")
        (read-line stream))"
      0
      nil)
    (with-open-file
        "(with-open-file (stream \"file.txt\")
        (print \"hello\"))"
      1
      ("Variable 'stream' is unused"))))

(deftest with-macro-binding-usage
  (dolist (case *with-macro-binding-cases*)
    (destructuring-bind (macro-name code expected-count expected-messages) case
      (testing (format nil "~A reports expected unused binding" macro-name)
        (let* ((violations (check-unused-variables code))
               (messages (mapcar #'violation:violation-message violations)))
          (ok (= (length violations) expected-count)
              "Expected number of unused binding violations")
          (ok (equal messages expected-messages)
              "Expected unused binding violation messages")
          (ok (every (lambda (violation)
                       (eq (violation:violation-rule violation) :unused-variables))
                     violations)
              "All reported violations are unused variable violations"))))))

(deftest with-slots-renamed-bindings
  (testing "WITH-SLOTS with (var slot-name) syntax accepts used renamed slots"
    (let ((violations (check-unused-variables
                       "(with-slots ((x slot-x) (y slot-y)) obj
                          (list x y))")))
      (ok (null violations)
          "No violation for used renamed slots")))

  (testing "WITH-SLOTS with (var slot-name) syntax reports unused renamed slots"
    (let ((violations (check-unused-variables
                       "(with-slots ((x slot-x) (y slot-y)) obj
                          (print y))")))
      (ok (= (length violations) 1)
          "One violation for unused renamed slot")
      (ok (string= (violation:violation-message (first violations))
                   "Variable 'x' is unused")
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
