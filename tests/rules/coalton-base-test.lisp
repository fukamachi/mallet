(defpackage #:mallet/tests/rules/coalton-base
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:base #:mallet/rules/base)
   (#:cbase #:mallet/rules/coalton-base)
   (#:parser #:mallet/parser)))
(in-package #:mallet/tests/rules/coalton-base)

;;; Minimal rule subclasses for testing coalton-aware-p

(defclass plain-test-rule (base:rule)
  ()
  (:default-initargs :name :plain-test :description "plain" :severity :warning))

;;; Primary method for plain-test-rule.
;;; Returns (:triggered) to distinguish from nil (not called) vs nil (called, returned nil).
;;; This allows tests to verify the :around method did NOT call through to the primary.
(defmethod base:check-form ((rule plain-test-rule) form file)
  (declare (ignore file))
  (when (handler-case (parser:form-expr form) (error () nil))
    (list :triggered)))

(defclass aware-test-rule (base:rule)
  ()
  (:default-initargs :name :aware-test :description "aware" :severity :warning))

(defmethod base:coalton-aware-p ((rule aware-test-rule)) t)

;;; coalton-aware-p tests

(deftest coalton-aware-p-default-nil
  (testing "default coalton-aware-p returns nil for plain rule"
    (ok (null (base:coalton-aware-p (make-instance 'plain-test-rule)))))

  (testing "default coalton-aware-p returns nil for base rule class directly"
    (ok (null (base:coalton-aware-p
               (make-instance 'base:rule
                              :name :base-test :description "base" :severity :warning)))))

  (testing "coalton-aware-p returns t for rule that overrides it"
    (ok (base:coalton-aware-p (make-instance 'aware-test-rule)))))

(deftest coalton-aware-p-affects-around-method
  ;; The :around method on check-form should pass through when coalton-aware-p is t
  (testing "plain rule skips coalton-toplevel forms (existing behavior preserved)"
    (let* ((rule (make-instance 'plain-test-rule))
           (forms (parser:parse-forms "(coalton-toplevel (define x 1))" #p"test.lisp"))
           (results (mapcan (lambda (f) (base:check-form rule f #p"test.lisp")) forms)))
      ;; The :around must return nil without calling the primary method.
      ;; If the primary were called it would return (:triggered), so nil means :around short-circuited.
      (ok (null results))))

  (testing "coalton-aware rule is passed coalton-toplevel forms (not skipped)"
    ;; We verify this by checking that check-form is actually called on the form.
    ;; We do this by adding a primary method that signals a condition or returns a marker.
    (let ((called nil))
      (defmethod base:check-form ((rule aware-test-rule) form file)
        (setf called t)
        nil)
      (let ((rule (make-instance 'aware-test-rule))
            (forms (parser:parse-forms "(coalton-toplevel (define x 1))" #p"test.lisp")))
        (mapcan (lambda (f) (base:check-form rule f #p"test.lisp")) forms))
      (ok called)))

  (testing "coalton-aware rule also receives non-coalton forms (not filtered out)"
    (let ((called nil))
      (defmethod base:check-form ((rule aware-test-rule) form file)
        (setf called t)
        nil)
      (let ((rule (make-instance 'aware-test-rule))
            (forms (parser:parse-forms "(defun foo () nil)" #p"test.lisp")))
        (mapcan (lambda (f) (base:check-form rule f #p"test.lisp")) forms))
      (ok called "coalton-aware-p only affects coalton skip; non-coalton forms still pass through"))))

;;; coalton-define-p tests

(deftest coalton-define-p-function-define
  (testing "returns T for function define: (define (foo x) body)"
    (ok (cbase:coalton-define-p '("CURRENT:DEFINE" ("CURRENT:FOO" "CURRENT:X") "CURRENT:BODY"))))

  (testing "returns T for define with no args: (define (bar) body)"
    (ok (cbase:coalton-define-p '("CURRENT:DEFINE" ("CURRENT:BAR") "CURRENT:BODY"))))

  (testing "returns T for define with multiple body forms"
    (ok (cbase:coalton-define-p '("CURRENT:DEFINE" ("CURRENT:FOO" "CURRENT:X") "CURRENT:E1" "CURRENT:E2"))))

  (testing "returns T for define with multiple args"
    (ok (cbase:coalton-define-p '("CURRENT:DEFINE" ("CURRENT:ADD" "CURRENT:X" "CURRENT:Y") ("CURRENT:+" "CURRENT:X" "CURRENT:Y")))))

  (testing "returns T for package-qualified coalton:define"
    (ok (cbase:coalton-define-p '("COALTON:DEFINE" ("CURRENT:FOO" "CURRENT:X") "CURRENT:BODY"))))

  (testing "returns T for case-insensitive define"
    (ok (cbase:coalton-define-p '("CURRENT:define" ("CURRENT:foo" "CURRENT:x") "CURRENT:body")))))

(deftest coalton-define-p-value-define
  (testing "returns NIL for value define: (define x 42)"
    (ok (null (cbase:coalton-define-p '("CURRENT:DEFINE" "CURRENT:X" 42)))))

  (testing "returns NIL for value define: (define pi 3.14f0)"
    (ok (null (cbase:coalton-define-p '("CURRENT:DEFINE" "CURRENT:PI" 3.14f0)))))

  (testing "returns NIL for value define with symbol RHS"
    (ok (null (cbase:coalton-define-p '("CURRENT:DEFINE" "CURRENT:X" "CURRENT:SOME-VAL")))))

  (testing "returns NIL for value define with string RHS"
    (ok (null (cbase:coalton-define-p '("CURRENT:DEFINE" "CURRENT:GREETING" "hello")))))

  (testing "returns NIL for value define with fn body (lambda value)"
    (ok (null (cbase:coalton-define-p '("CURRENT:DEFINE" "CURRENT:FOO" ("CURRENT:FN" ("CURRENT:X") "CURRENT:X")))))))

(deftest coalton-define-p-non-define
  (testing "returns NIL for define-type"
    (ok (null (cbase:coalton-define-p '("CURRENT:DEFINE-TYPE" "CURRENT:Foo" ("CURRENT:Bar"))))))

  (testing "returns NIL for define-instance"
    (ok (null (cbase:coalton-define-p '("CURRENT:DEFINE-INSTANCE" ("CURRENT:Eq" "CURRENT:MyType")
                                        ("CURRENT:DEFINE" ("CURRENT:==" "CURRENT:A" "CURRENT:B")
                                         ("CURRENT:EQUAL" "CURRENT:A" "CURRENT:B")))))))

  (testing "returns NIL for define-struct"
    (ok (null (cbase:coalton-define-p '("CURRENT:DEFINE-STRUCT" "CURRENT:Point"
                                        ("CURRENT:X" "CURRENT:Integer")
                                        ("CURRENT:Y" "CURRENT:Integer"))))))

  (testing "returns NIL for non-define form"
    (ok (null (cbase:coalton-define-p '("CURRENT:DECLARE" "CURRENT:FOO" ("CURRENT:Integer" :-> "CURRENT:Integer"))))))

  (testing "returns NIL for non-list"
    (ok (null (cbase:coalton-define-p "CURRENT:DEFINE"))))

  (testing "returns NIL for nil"
    (ok (null (cbase:coalton-define-p nil))))

  (testing "returns NIL for empty list"
    (ok (null (cbase:coalton-define-p '()))))

  (testing "returns NIL for single-element list (just define, no args)"
    (ok (null (cbase:coalton-define-p '("CURRENT:DEFINE"))))))

;;; coalton-define-name tests

(deftest coalton-define-name-extracts-name
  (testing "extracts name from (define (foo x) ...)"
    (ok (string= "FOO" (cbase:coalton-define-name '("CURRENT:DEFINE" ("CURRENT:FOO" "CURRENT:X") "CURRENT:BODY")))))

  (testing "extracts name from (define (bar) ...)"
    (ok (string= "BAR" (cbase:coalton-define-name '("CURRENT:DEFINE" ("CURRENT:BAR") "CURRENT:BODY")))))

  (testing "is case-insensitive (normalises to uppercase)"
    (ok (string= "FOO" (cbase:coalton-define-name '("CURRENT:define" ("CURRENT:foo") "CURRENT:body")))))

  (testing "extracts name from multi-arg define (define (add x y) ...)"
    (ok (string= "ADD" (cbase:coalton-define-name '("CURRENT:DEFINE" ("CURRENT:ADD" "CURRENT:X" "CURRENT:Y") "CURRENT:BODY")))))

  (testing "extracts name from package-qualified coalton:define"
    (ok (string= "FOO" (cbase:coalton-define-name '("COALTON:DEFINE" ("CURRENT:FOO" "CURRENT:X") "CURRENT:BODY"))))))

(deftest coalton-define-name-returns-nil-for-non-function
  (testing "returns NIL for value define"
    (ok (null (cbase:coalton-define-name '("CURRENT:DEFINE" "CURRENT:X" 42)))))

  (testing "returns NIL for non-define form"
    (ok (null (cbase:coalton-define-name '("CURRENT:LET" (("CURRENT:X" 1)) "CURRENT:X")))))

  (testing "returns NIL for nil input"
    (ok (null (cbase:coalton-define-name nil))))

  (testing "returns NIL for define-type"
    (ok (null (cbase:coalton-define-name '("CURRENT:DEFINE-TYPE" "CURRENT:Foo" ("CURRENT:Bar")))))))

;;; coalton-define-body tests

(deftest coalton-define-body-extracts-body
  (testing "returns body forms from function define"
    (let ((body (cbase:coalton-define-body '("CURRENT:DEFINE" ("CURRENT:FOO" "CURRENT:X") "CURRENT:E1" "CURRENT:E2"))))
      (ok (equal body '("CURRENT:E1" "CURRENT:E2")))))

  (testing "returns single-element body"
    (let ((body (cbase:coalton-define-body '("CURRENT:DEFINE" ("CURRENT:FOO") "CURRENT:RESULT"))))
      (ok (equal body '("CURRENT:RESULT")))))

  (testing "returns nil for empty body"
    (ok (null (cbase:coalton-define-body '("CURRENT:DEFINE" ("CURRENT:FOO")))))))

(deftest coalton-define-body-returns-nil-for-non-function
  (testing "returns NIL for value define"
    (ok (null (cbase:coalton-define-body '("CURRENT:DEFINE" "CURRENT:X" 42)))))

  (testing "returns NIL for non-define"
    (ok (null (cbase:coalton-define-body '("CURRENT:LET" () "CURRENT:X")))))

  (testing "returns NIL for nil input"
    (ok (null (cbase:coalton-define-body nil))))

  (testing "returns NIL for define-type"
    (ok (null (cbase:coalton-define-body '("CURRENT:DEFINE-TYPE" "CURRENT:Foo" ("CURRENT:Bar")))))))

;;; coalton-match-p tests

(deftest coalton-match-p-match-forms
  (testing "returns T for (match expr clause...)"
    (ok (cbase:coalton-match-p '("CURRENT:MATCH" "CURRENT:X" ("CURRENT:True" "CURRENT:E1") ("CURRENT:False" "CURRENT:E2")))))

  (testing "returns T for match with no clauses"
    (ok (cbase:coalton-match-p '("CURRENT:MATCH" "CURRENT:X"))))

  (testing "is case-insensitive"
    (ok (cbase:coalton-match-p '("CURRENT:match" "CURRENT:x"))))

  (testing "returns T for package-qualified coalton:match"
    (ok (cbase:coalton-match-p '("COALTON:MATCH" "CURRENT:X" ("CURRENT:True" "CURRENT:E1"))))))

(deftest coalton-match-p-non-match-forms
  (testing "returns NIL for match-vector"
    (ok (null (cbase:coalton-match-p '("CURRENT:MATCH-VECTOR" "CURRENT:X")))))

  (testing "returns NIL for non-list"
    (ok (null (cbase:coalton-match-p "CURRENT:MATCH"))))

  (testing "returns NIL for nil"
    (ok (null (cbase:coalton-match-p nil))))

  (testing "returns NIL for other forms"
    (ok (null (cbase:coalton-match-p '("CURRENT:COND" ("CURRENT:X" "CURRENT:Y"))))))

  (testing "returns NIL for empty list"
    (ok (null (cbase:coalton-match-p '()))))

  (testing "returns NIL for single-element list (just match, no expr)"
    (ok (null (cbase:coalton-match-p '("CURRENT:MATCH"))))))

;;; coalton-match-clauses tests

(deftest coalton-match-clauses-counts-non-wildcard
  (testing "counts two non-wildcard clauses"
    (ok (= 2 (cbase:coalton-match-clauses
              '("CURRENT:MATCH" "CURRENT:X"
                ("CURRENT:True" "CURRENT:E1")
                ("CURRENT:False" "CURRENT:E2"))))))

  (testing "excludes wildcard _ clause"
    (ok (= 2 (cbase:coalton-match-clauses
              '("CURRENT:MATCH" "CURRENT:X"
                ("CURRENT:True" "CURRENT:E1")
                ("CURRENT:False" "CURRENT:E2")
                ("CURRENT:_" "CURRENT:DEFAULT"))))))

  (testing "returns 0 when all clauses are wildcards"
    (ok (= 0 (cbase:coalton-match-clauses
              '("CURRENT:MATCH" "CURRENT:X"
                ("CURRENT:_" "CURRENT:DEFAULT"))))))

  (testing "counts mixed patterns with Some, None, _"
    (ok (= 2 (cbase:coalton-match-clauses
              '("CURRENT:MATCH" "CURRENT:OPT"
                (("CURRENT:Some" "CURRENT:Y") "CURRENT:E1")
                ("CURRENT:None" "CURRENT:E2")
                ("CURRENT:_" "CURRENT:E3"))))))

  (testing "counts single non-wildcard clause"
    (ok (= 1 (cbase:coalton-match-clauses
              '("CURRENT:MATCH" "CURRENT:X"
                ("CURRENT:True" "CURRENT:E1"))))))

  (testing "returns 0 for empty match"
    (ok (= 0 (cbase:coalton-match-clauses '("CURRENT:MATCH" "CURRENT:X")))))

  (testing "returns 0 for non-match form"
    (ok (= 0 (cbase:coalton-match-clauses '("CURRENT:COND" ("CURRENT:T" "CURRENT:X"))))))

  (testing "returns 0 for nil input"
    (ok (= 0 (cbase:coalton-match-clauses nil))))

  (testing "handles multiple wildcard patterns correctly"
    (ok (= 1 (cbase:coalton-match-clauses
              '("CURRENT:MATCH" "CURRENT:X"
                ("CURRENT:True" "CURRENT:E1")
                ("CURRENT:_" "CURRENT:D1")
                ("CURRENT:_" "CURRENT:D2")))))))

;;; coalton-aware-p on coalton-rule subclass

(deftest coalton-aware-p-on-coalton-rule
  (testing "coalton-aware-p returns nil for coalton-rule subclass (uses separate :around)"
    (ok (null (base:coalton-aware-p
               (make-instance 'base:coalton-rule
                              :name :coal-test :description "coal" :severity :warning))))))

;;; Stub guard tests — ensure a no-op implementation would fail

(deftest coalton-define-p-stub-guard
  (testing "coalton-define-p must return T for a valid function define (not always NIL)"
    (ok (cbase:coalton-define-p '("CURRENT:DEFINE" ("CURRENT:FOO" "CURRENT:X") "CURRENT:BODY"))
        "A stub always returning NIL would fail this"))

  (testing "coalton-define-p must return NIL for a value define (not always T)"
    (ok (null (cbase:coalton-define-p '("CURRENT:DEFINE" "CURRENT:X" 42)))
        "A stub always returning T would fail this")))

(deftest coalton-define-name-stub-guard
  (testing "coalton-define-name must return a name for function define (not always NIL)"
    (ok (cbase:coalton-define-name '("CURRENT:DEFINE" ("CURRENT:FOO" "CURRENT:X") "CURRENT:BODY"))
        "A stub always returning NIL would fail this"))

  (testing "coalton-define-name must return NIL for value define (not always a string)"
    (ok (null (cbase:coalton-define-name '("CURRENT:DEFINE" "CURRENT:X" 42)))
        "A stub always returning a string would fail this")))

(deftest coalton-define-body-stub-guard
  (testing "coalton-define-body must return body for function define (not always NIL)"
    (ok (cbase:coalton-define-body '("CURRENT:DEFINE" ("CURRENT:FOO" "CURRENT:X") "CURRENT:BODY"))
        "A stub always returning NIL would fail this"))

  (testing "coalton-define-body must return NIL for value define (not always a list)"
    (ok (null (cbase:coalton-define-body '("CURRENT:DEFINE" "CURRENT:X" 42)))
        "A stub always returning a list would fail this")))

(deftest coalton-match-p-stub-guard
  (testing "coalton-match-p must return T for match form (not always NIL)"
    (ok (cbase:coalton-match-p '("CURRENT:MATCH" "CURRENT:X" ("CURRENT:True" "CURRENT:E1")))
        "A stub always returning NIL would fail this"))

  (testing "coalton-match-p must return NIL for non-match form (not always T)"
    (ok (null (cbase:coalton-match-p '("CURRENT:COND" ("CURRENT:X" "CURRENT:Y"))))
        "A stub always returning T would fail this")))

(deftest coalton-match-clauses-stub-guard
  (testing "coalton-match-clauses must return 2 for two-clause match (not always 0)"
    (ok (= 2 (cbase:coalton-match-clauses
              '("CURRENT:MATCH" "CURRENT:X"
                ("CURRENT:True" "CURRENT:E1")
                ("CURRENT:False" "CURRENT:E2"))))
        "A stub always returning 0 would fail this"))

  (testing "coalton-match-clauses must exclude wildcards (not just count all clauses)"
    (ok (= 1 (cbase:coalton-match-clauses
              '("CURRENT:MATCH" "CURRENT:X"
                ("CURRENT:True" "CURRENT:E1")
                ("CURRENT:_" "CURRENT:DEFAULT"))))
        "An implementation counting all clauses (returning 2) would fail this")))
