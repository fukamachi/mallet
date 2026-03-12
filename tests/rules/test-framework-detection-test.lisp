(defpackage #:mallet/tests/rules/test-framework-detection
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)))
(in-package #:mallet/tests/rules/test-framework-detection)

;;; Helper to parse defpackage clauses (cddr of the defpackage expr)
(defun parse-clauses (code)
  "Parse CODE as a defpackage form and return its clauses (cddr)."
  (let* ((forms (parser:parse-forms code #p"test.lisp"))
         (expr (parser:form-expr (first forms))))
    (cddr expr)))

;;; Tests for defpackage-uses-test-framework-p

(deftest defpackage-uses-test-framework-p-rove
  (testing "(:use #:cl #:rove) → T"
    (let ((clauses (parse-clauses "(defpackage #:foo (:use #:cl #:rove))")))
      (ok (base:defpackage-uses-test-framework-p clauses))))

  (testing "(:use #:cl) only → NIL"
    (let ((clauses (parse-clauses "(defpackage #:foo (:use #:cl))")))
      (ok (null (base:defpackage-uses-test-framework-p clauses)))))

  (testing "No :use clause → NIL"
    (let ((clauses (parse-clauses "(defpackage #:foo (:export #:bar))")))
      (ok (null (base:defpackage-uses-test-framework-p clauses))))))

(deftest defpackage-uses-test-framework-p-import-from
  (testing "(:import-from #:fiveam) → T"
    (let ((clauses (parse-clauses "(defpackage #:foo (:import-from #:fiveam #:is #:def-suite))")))
      (ok (base:defpackage-uses-test-framework-p clauses))))

  (testing "(:import-from #:alexandria) → NIL (not a test framework)"
    (let ((clauses (parse-clauses "(defpackage #:foo (:import-from #:alexandria #:when-let))")))
      (ok (null (base:defpackage-uses-test-framework-p clauses))))))

(deftest defpackage-uses-test-framework-p-lack-test
  (testing "(:use #:lack/test) → NIL (package name contains 'test' but is not a framework)"
    (let ((clauses (parse-clauses "(defpackage #:foo (:use #:lack/test))")))
      (ok (null (base:defpackage-uses-test-framework-p clauses))))))

(deftest defpackage-uses-test-framework-p-all-frameworks
  (testing "Each known framework is detected via :use"
    (dolist (fw '("rove" "fiveam" "5am" "fiasco" "parachute"
                  "coalton-testing" "lisp-unit2" "clunit" "try"))
      (let* ((code (format nil "(defpackage #:foo (:use #:~A))" fw))
             (clauses (parse-clauses code)))
        (ok (base:defpackage-uses-test-framework-p clauses)
            (format nil "~A detected as test framework" fw)))))

  (testing "Multiple clauses, one with framework → T"
    (let ((clauses (parse-clauses
                    "(defpackage #:foo (:export #:sym) (:use #:cl #:rove))")))
      (ok (base:defpackage-uses-test-framework-p clauses)))))

(deftest defpackage-uses-test-framework-p-non-use-clauses
  (testing "(:export #:rove) → NIL (framework in :export, not :use)"
    (let ((clauses (parse-clauses "(defpackage #:foo (:export #:rove))")))
      (ok (null (base:defpackage-uses-test-framework-p clauses)))))

  (testing "(:local-nicknames (#:r #:rove)) → NIL"
    (let ((clauses (parse-clauses "(defpackage #:foo (:local-nicknames (#:r #:rove)))")))
      (ok (null (base:defpackage-uses-test-framework-p clauses)))))

  (testing "(:import-from #:my-utils #:rove) → NIL (rove is an imported symbol name, not the package)"
    (let ((clauses (parse-clauses "(defpackage #:foo (:import-from #:my-utils #:rove))")))
      (ok (null (base:defpackage-uses-test-framework-p clauses))))))

(deftest defpackage-uses-test-framework-p-extra-frameworks
  (testing "extra-frameworks parameter detects additional names"
    (let ((clauses (parse-clauses "(defpackage #:foo (:use #:cffi))")))
      (ok (base:defpackage-uses-test-framework-p clauses '("CFFI")))))

  (testing "extra-frameworks is case-insensitive"
    (let ((clauses (parse-clauses "(defpackage #:foo (:use #:cffi))")))
      (ok (base:defpackage-uses-test-framework-p clauses '("cffi")))))

  (testing "extra-frameworks does not affect packages not in the list"
    (let ((clauses (parse-clauses "(defpackage #:foo (:use #:alexandria))")))
      (ok (null (base:defpackage-uses-test-framework-p clauses '("cffi")))))))

;;; Tests for tokens-use-test-framework-p

(defun tokenize-code (code)
  "Tokenize CODE string."
  (parser:tokenize code #p"test.lisp"))

(deftest tokens-use-test-framework-p-basic
  (testing "DEFPACKAGE with :USE :ROVE → T"
    (let ((tokens (tokenize-code "(defpackage #:my-pkg (:use #:cl #:rove))")))
      (ok (base:tokens-use-test-framework-p tokens))))

  (testing "DEFPACKAGE with :USE :CL only → NIL"
    (let ((tokens (tokenize-code "(defpackage #:my-pkg (:use #:cl))")))
      (ok (null (base:tokens-use-test-framework-p tokens)))))

  (testing "No DEFPACKAGE → NIL"
    (let ((tokens (tokenize-code "(defun foo () nil)")))
      (ok (null (base:tokens-use-test-framework-p tokens))))))

(deftest tokens-use-test-framework-p-import-from
  (testing "DEFPACKAGE with :IMPORT-FROM #:FIVEAM → T"
    (let ((tokens (tokenize-code "(defpackage #:my-pkg (:import-from #:fiveam #:def-suite))")))
      (ok (base:tokens-use-test-framework-p tokens))))

  (testing ":IMPORT-FROM non-framework → NIL"
    (let ((tokens (tokenize-code "(defpackage #:my-pkg (:import-from #:alexandria #:when-let))")))
      (ok (null (base:tokens-use-test-framework-p tokens))))))

(deftest tokens-use-test-framework-p-all-frameworks
  (testing "All known frameworks detected via :use tokens"
    (dolist (fw '("rove" "fiveam" "5am" "fiasco" "parachute"
                  "coalton-testing" "lisp-unit2" "clunit" "try"))
      (let* ((code (format nil "(defpackage #:my-pkg (:use #:~A))" fw))
             (tokens (tokenize-code code)))
        (ok (base:tokens-use-test-framework-p tokens)
            (format nil "~A detected via tokens" fw))))))

(deftest tokens-use-test-framework-p-lack-test
  (testing "lack/test not detected as test framework"
    (let ((tokens (tokenize-code "(defpackage #:foo (:use #:lack/test))")))
      (ok (null (base:tokens-use-test-framework-p tokens))))))

(deftest tokens-use-test-framework-p-non-use-clauses
  (testing "DEFPACKAGE with :EXPORT :ROVE → NIL (framework name only in :export)"
    (let ((tokens (tokenize-code "(defpackage #:foo (:export #:rove))")))
      (ok (null (base:tokens-use-test-framework-p tokens)))))

  (testing "DEFPACKAGE with :LOCAL-NICKNAMES #:ROVE → NIL"
    (let ((tokens (tokenize-code "(defpackage #:foo (:local-nicknames (#:r #:rove)))")))
      (ok (null (base:tokens-use-test-framework-p tokens))))))

(deftest tokens-use-test-framework-p-multiline
  (testing "Multiline defpackage with rove on separate line"
    (let ((tokens (tokenize-code
                   "(defpackage #:my-pkg
  (:use #:cl
        #:rove)
  (:export #:run-tests))")))
      (ok (base:tokens-use-test-framework-p tokens)))))

;;; U-1: define-package support

(deftest tokens-use-test-framework-p-define-package
  (testing "DEFINE-PACKAGE with :USE :ROVE is detected"
    (let ((tokens (tokenize-code
                   "(uiop:define-package #:my-tests (:use #:cl #:rove))")))
      (ok (base:tokens-use-test-framework-p tokens))))

  (testing "DEFINE-PACKAGE with :IMPORT-FROM #:FIVEAM is detected"
    (let ((tokens (tokenize-code
                   "(uiop:define-package #:my-tests (:import-from #:fiveam #:is))")))
      (ok (base:tokens-use-test-framework-p tokens))))

  (testing "DEFINE-PACKAGE without test framework is not detected"
    (let ((tokens (tokenize-code
                   "(uiop:define-package #:my-pkg (:use #:cl))")))
      (ok (null (base:tokens-use-test-framework-p tokens))))))

;;; U-2: large export lists (no 200-token limit)

(deftest tokens-use-test-framework-p-large-export
  (testing "Detection works when :use appears after 200+ export symbols"
    (let* ((exports (format nil "~{#:sym~A~^ ~}"
                            (loop for i from 1 to 250 collect i)))
           (code (format nil "(defpackage #:big-pkg~%  (:export ~A)~%  (:use #:cl #:rove))"
                         exports))
           (tokens (tokenize-code code)))
      (ok (base:tokens-use-test-framework-p tokens)
          "Should detect framework even after 250 exported symbols")))

  (testing "Non-framework defpackage with large export list is not detected"
    (let* ((exports (format nil "~{#:sym~A~^ ~}"
                            (loop for i from 1 to 250 collect i)))
           (code (format nil "(defpackage #:big-pkg~%  (:export ~A)~%  (:use #:cl))"
                         exports))
           (tokens (tokenize-code code)))
      (ok (null (base:tokens-use-test-framework-p tokens))
          "Should not detect framework in non-test package"))))
