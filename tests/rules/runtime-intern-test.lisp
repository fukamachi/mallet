(defpackage #:mallet/tests/rules/runtime-intern
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:runtime-intern #:mallet/rules/forms/runtime-intern)
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/runtime-intern)

;;; Registry tests

(defparameter *prohibited-intern-function-cases*
  '(("COMMON-LISP:intern" . "cl:intern")
    ("CL:intern" . "cl:intern")
    ("common-lisp:INTERN" . "cl:intern")
    ("COMMON-LISP:unintern" . "cl:unintern")
    ("UIOP:intern*" . "uiop:intern*")
    ("uiop:INTERN*" . "uiop:intern*")
    ("ALEXANDRIA:symbolicate" . "alexandria:symbolicate")
    ("ALEXANDRIA:format-symbol" . "alexandria:format-symbol")
    ("ALEXANDRIA:make-keyword" . "alexandria:make-keyword")
    ("intern" . "cl:intern")
    ("intern*" . "uiop:intern*")
    ("symbolicate" . "alexandria:symbolicate")
    ("MYPACKAGE:intern" . nil)
    ("COMMON-LISP:intern-something" . nil)
    ("COMMON-LISP:string" . nil)
    (nil . nil)
    (42 . nil)
    ("COMMON-LISP::intern" . "cl:intern")))

(deftest prohibited-intern-function-registry
  (testing "Function names resolve to their prohibited display names"
    (dolist (case *prohibited-intern-function-cases*)
      (destructuring-bind (function-name . expected) case
        (let ((actual (runtime-intern:prohibited-intern-function-p function-name)))
          (if expected
              (ok (string= actual expected))
              (ok (null actual)))))))

  (testing "INTERN and INTERN* remain distinct registry entries"
    (ok (string= (runtime-intern:prohibited-intern-function-p "COMMON-LISP:intern")
                 "cl:intern"))
    (ok (string= (runtime-intern:prohibited-intern-function-p "UIOP:intern*")
                 "uiop:intern*"))))

;;; Rule class tests

(defmacro with-test-file ((tmpfile-var code) &body body)
  "Helper: create a temporary file with CODE and clean up after."
  `(uiop:with-temporary-file (:stream stream :pathname ,tmpfile-var
                                      :type "lisp" :keep t)
     (write-string ,code stream)
     (finish-output stream)
     ,@body))

(defun check-intern (code)
  "Check CODE for runtime-intern violations using a fake file path (no context)."
  (let ((forms (parser:parse-forms code #p"test.lisp"))
        (rule (make-instance 'rules:runtime-intern-rule)))
    (mapcan (lambda (form)
              (rules:check-form rule form #p"test.lisp"))
            forms)))

(defun check-intern-file (tmpfile)
  "Check TMPFILE for runtime-intern violations using real file (with context)."
  (let* ((text (uiop:read-file-string tmpfile))
         (forms (parser:parse-forms text tmpfile))
         (rule (make-instance 'rules:runtime-intern-rule)))
    (mapcan (lambda (form)
              (rules:check-form rule form tmpfile))
            forms)))

;;; Valid cases (no violations)

(deftest intern-usage-valid
  (testing "No intern: plain function call"
    (ok (null (check-intern "(defun foo (x) (+ x 1))"))))

  (testing "No intern: unrelated string"
    (ok (null (check-intern "(defun foo () \"intern\")"))))

  (testing "No intern: quoted list (data, not code)"
    (ok (null (check-intern "'(intern \"foo\")"))))

  (testing "No intern: intern in a symbol name but not a call"
    (ok (null (check-intern "(defun internalize (x) x)"))))

  (testing "No intern: bare (intern ...) without import context — no false positive"
    ;; The parser produces CURRENT:intern, but without an import-from mapping
    ;; saying it comes from CL, we do NOT flag it (avoids false positives).
    (ok (null (check-intern "(defun foo () (intern \"FOO\"))"))))

  (testing "No intern: intern inside defmacro body is skipped"
    (ok (null (check-intern "(defmacro def-var (name)
                               `(defvar ,name (intern (symbol-name ',name))))"))))

  (testing "No intern: eval-when without :execute is skipped"
    (ok (null (check-intern "(eval-when (:compile-toplevel :load-toplevel)
                               (cl:intern \"FOO\"))"))))

  (testing "No intern: funcall with unrelated function"
    (ok (null (check-intern "(funcall #'string \"hello\")"))))

  (testing "No intern: apply with unrelated function"
    (ok (null (check-intern "(apply #'+ '(1 2 3))")))))

;;; Direct call violations

(deftest intern-usage-direct
  (testing "Qualified (cl:intern ...) is flagged"
    (let ((violations (check-intern "(cl:intern \"FOO\")")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :runtime-intern))
      (ok (search "cl:intern" (violation:violation-message (first violations))))))

  (testing "Qualified (cl:unintern ...) is flagged"
    (let ((violations (check-intern "(cl:unintern 'foo)")))
      (ok (= (length violations) 1))
      (ok (search "cl:unintern" (violation:violation-message (first violations))))))

  (testing "Qualified (uiop:intern* ...) is flagged"
    (let ((violations (check-intern "(uiop:intern* \"FOO\" :keyword)")))
      (ok (= (length violations) 1))
      (ok (search "uiop:intern*" (violation:violation-message (first violations))))))

  (testing "Qualified (alexandria:symbolicate ...) is flagged"
    (let ((violations (check-intern "(alexandria:symbolicate :foo :bar)")))
      (ok (= (length violations) 1))
      (ok (search "alexandria:symbolicate" (violation:violation-message (first violations))))))

  (testing "Qualified (alexandria:format-symbol ...) is flagged"
    (let ((violations (check-intern "(alexandria:format-symbol nil \"~A\" :foo)")))
      (ok (= (length violations) 1))
      (ok (search "alexandria:format-symbol" (violation:violation-message (first violations))))))

  (testing "Qualified (alexandria:make-keyword ...) is flagged"
    (let ((violations (check-intern "(alexandria:make-keyword \"FOO\")")))
      (ok (= (length violations) 1))
      (ok (search "alexandria:make-keyword" (violation:violation-message (first violations))))))

  (testing "Nested qualified cl:intern inside defun is flagged"
    (let ((violations (check-intern "(defun bad () (cl:intern \"FOO\"))")))
      (ok (= (length violations) 1))))

  (testing "Multiple qualified intern calls produce multiple violations"
    (let ((violations (check-intern "(progn (cl:intern \"A\") (cl:intern \"B\"))")))
      (ok (= (length violations) 2))))

  (testing "eval-when with :execute is flagged"
    (let ((violations (check-intern "(eval-when (:execute)
                                       (cl:intern \"FOO\"))")))
      (ok (= (length violations) 1))))

  (testing "eval-when with :load-toplevel :execute is flagged"
    (let ((violations (check-intern "(eval-when (:load-toplevel :execute)
                                       (cl:intern \"BAR\"))")))
      (ok (= (length violations) 1)))))

;;; Funcall / apply patterns

(deftest intern-usage-funcall-apply
  (testing "(funcall #'cl:intern ...) is flagged"
    (let ((violations (check-intern "(funcall #'cl:intern \"FOO\")")))
      (ok (= (length violations) 1))
      (ok (search "funcall" (violation:violation-message (first violations))))))

  (testing "(funcall 'cl:intern ...) is flagged"
    (let ((violations (check-intern "(funcall 'cl:intern \"FOO\")")))
      (ok (= (length violations) 1))
      (ok (search "funcall" (violation:violation-message (first violations))))))

  (testing "(apply #'cl:intern ...) is flagged"
    (let ((violations (check-intern "(apply #'cl:intern '(\"FOO\" :keyword))")))
      (ok (= (length violations) 1))
      (ok (search "apply" (violation:violation-message (first violations))))))

  (testing "(apply 'cl:unintern ...) is flagged"
    (let ((violations (check-intern "(apply 'cl:unintern '(foo))")))
      (ok (= (length violations) 1))
      (ok (search "apply" (violation:violation-message (first violations))))))

  (testing "(apply #'alexandria:symbolicate ...) is flagged"
    (let ((violations (check-intern "(apply #'alexandria:symbolicate '(:foo :bar))")))
      (ok (= (length violations) 1))
      (ok (search "apply" (violation:violation-message (first violations))))))

  (testing "(funcall #'uiop:intern* ...) is flagged"
    (let ((violations (check-intern "(funcall #'uiop:intern* \"FOO\" :keyword)")))
      (ok (= (length violations) 1))
      (ok (search "funcall" (violation:violation-message (first violations)))))))

;;; Context-dependent tests (with real files)

(deftest intern-usage-context-nickname
  (testing "Local nickname call is flagged through file context"
    (with-test-file (tmpfile
                     "(defpackage #:test (:use #:cl) (:local-nicknames (#:a #:alexandria)))
                      (in-package #:test)
                      (defun bad () (a:format-symbol nil \"~A\" :foo))")
      (let ((violations (check-intern-file tmpfile)))
        (ok (= (length violations) 1))
        (ok (search "alexandria:format-symbol" (violation:violation-message (first violations)))))))

  (testing "Import-from call is flagged through file context"
    (with-test-file (tmpfile
                     "(defpackage #:test (:use #:cl) (:import-from #:uiop #:intern*))
                      (in-package #:test)
                      (defun bad () (intern* \"FOO\" :keyword))")
      (let ((violations (check-intern-file tmpfile)))
        (ok (= (length violations) 1))
        (ok (search "uiop:intern*" (violation:violation-message (first violations)))))))

  (testing "Import from unknown package does not flag through file context"
    (with-test-file (tmpfile
                     "(defpackage #:test (:use #:cl) (:import-from #:my-package #:intern))
                      (in-package #:test)
                      (defun safe () (intern \"FOO\"))")
      (let ((violations (check-intern-file tmpfile)))
        (ok (null violations))))))

;;; U-1: resolve-runtime-intern nil-context guard

(deftest resolve-runtime-intern-nil-context
  (testing "resolve-runtime-intern with nil context returns nil without error"
    (ok (null (runtime-intern:resolve-runtime-intern "COMMON-LISP:intern" nil))))

  (testing "resolve-runtime-intern with nil context and unqualified name returns nil"
    (ok (null (runtime-intern:resolve-runtime-intern "intern" nil))))

  (testing "resolve-runtime-intern with nil context and nil string returns nil"
    (ok (null (runtime-intern:resolve-runtime-intern nil nil)))))

;;; U-3: direct-call branch handles already-interned CL symbol heads
;;;
;;; The parser normally produces string heads, but reader-macro processing can
;;; yield real interned CL symbol objects as form heads.  We test this by
;;; building expressions manually with interned symbol heads and checking via
;;; the rule.

(defun check-intern-with-symbol-head (head args)
  "Check a synthetic form (HEAD . ARGS) where HEAD is a Lisp symbol (not a string),
using an empty-context fake file.  Returns violations."
  (let* ((rule (make-instance 'rules:runtime-intern-rule))
         ;; Build a synthetic parser:form with an expr whose head is a real symbol
         (expr (cons head args))
         (form (make-instance 'parser:form
                              :expr expr
                              :line 1
                              :column 0
                              :end-line 1
                              :end-column 10
                              :source ""
                              :position-map (make-hash-table)
                              :file #p"test.lisp")))
    (rules:check-form rule form #p"test.lisp")))

(deftest runtime-intern-symbol-head
  (testing "Symbol head cl:intern is flagged"
    (let ((violations (check-intern-with-symbol-head 'cl:intern '("FOO"))))
      (ok (= (length violations) 1))
      (ok (search "cl:intern" (violation:violation-message (first violations))))))

  (testing "Symbol head cl:unintern is flagged"
    (let ((violations (check-intern-with-symbol-head 'cl:unintern '(foo))))
      (ok (= (length violations) 1))
      (ok (search "cl:unintern" (violation:violation-message (first violations))))))

  (testing "Symbol head cl:string is not flagged"
    (let ((violations (check-intern-with-symbol-head 'cl:string '(x))))
      (ok (null violations))))

  (testing "Symbol head from unrelated package is not flagged"
    (let ((violations (check-intern-with-symbol-head 'runtime-intern:resolve-runtime-intern '(x nil))))
      (ok (null violations)))))
