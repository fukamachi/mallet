(defpackage #:mallet/tests/rules/interned-package-symbol
  (:use #:cl #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/interned-package-symbol)

(defmacro with-test-file ((tmpfile-var code) &body body)
  "Helper macro to create temporary file with CODE and clean up after."
  `(uiop:with-temporary-file (:stream stream :pathname ,tmpfile-var
                              :type "lisp" :keep t)
     (write-string ,code stream)
     (finish-output stream)
     ,@body))

(defun collect-violations (code)
  (with-test-file (tmpfile code)
    (multiple-value-bind (forms parse-errors)
        (parser:parse-forms (uiop:read-file-string tmpfile) tmpfile)
      (ok (null parse-errors) "Code should parse cleanly")
      (let ((rule (make-instance 'rules:interned-package-symbol-rule)))
        (loop for form in forms
              append (rules:check-form rule form tmpfile))))))

(deftest interned-package-symbol-valid
  (testing "Uninterned symbols and strings are allowed"
    (let ((violations
            (collect-violations "(defpackage #:clean-pkg
                                   (:use #:cl)
                                   (:local-nicknames
                                    (#:a #:alexandria))
                                   (:import-from #:alexandria
                                    #:if-let)
                                   (:export #:foo #:bar)
                                   (:nicknames \"CLEAN-PKG\"))
                                 (in-package #:clean-pkg)")))
      (ok (null violations))))

  (testing "String package designators are accepted"
    (let ((violations
            (collect-violations "(defpackage \"STR-PKG\"
                                   (:use \"CL\")
                                   (:export \"START\"))
                                 (in-package \"STR-PKG\")")))
      (ok (null violations)))))

(deftest interned-package-symbol-keyword-violations
  (testing "Keywords in defpackage and in-package are flagged"
    (let* ((violations
             (collect-violations "(defpackage :keyword-pkg
                                    (:use :cl)
                                    (:local-nicknames
                                     (:a :alexandria))
                                    (:export :foo :bar))
                                  (in-package :keyword-pkg)")))
      (ok (= (length violations) 7))
      (ok (every (lambda (v)
                   (eq (violation:violation-rule v) :interned-package-symbol))
                 violations))
      (ok (some (lambda (v)
                  (search "keyword" (violation:violation-message v)))
                violations))
      (ok (some (lambda (v)
                  (search "defpackage" (violation:violation-message v)))
                violations)))))

(deftest interned-package-symbol-bare-violations
  (testing "Bare symbols are flagged via source text"
    (let* ((violations
             (collect-violations "(defpackage bare-pkg
                                    (:use cl)
                                    (:export foo))
                                  (in-package bare-pkg)")))
      (ok (= (length violations) 4)))))

(deftest interned-package-symbol-qualified-violations
  (testing "Qualified symbols are flagged"
    (let* ((violations
             (collect-violations "(defpackage #:qualified-pkg
                                    (:use cl-user:helpers)
                                    (:import-from cl-user:helpers
                                     cl-user:do-it)
                                    (:export #:ok))
                                  (in-package #:qualified-pkg)")))
      (ok (= (length violations) 3))
      (ok (every (lambda (v)
                   (search "qualified" (violation:violation-message v)))
                 violations)))))

(deftest interned-package-symbol-uiop-clauses
  (testing "UIOP-specific clauses are checked"
    (let* ((violations
             (collect-violations "(uiop:define-package #:uiop-pkg
                                    (:mix :cl)
                                    (:reexport :cl)
                                    (:use-reexport :cl)
                                    (:unintern :old)
                                    (:recycle :cl))
                                  (in-package #:uiop-pkg)")))
      (ok (= (length violations) 5)))))

(deftest interned-package-symbol-severity
  (testing "Rule has :convention severity"
    (let ((rule (make-instance 'rules:interned-package-symbol-rule)))
      (ok (eq (rules:rule-severity rule) :convention)))))
