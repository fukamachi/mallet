(defpackage #:mallet/tests/rules/intern-usage
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:intern-usage #:mallet/rules/forms/intern-usage)))
(in-package #:mallet/tests/rules/intern-usage)

;;; Registry tests

(deftest prohibited-intern-function-registry
  (testing "cl:intern matched by full package name (as produced by parser)"
    (ok (string= (intern-usage:prohibited-intern-function-p "COMMON-LISP:intern")
                 "cl:intern")))

  (testing "cl:intern matched case-insensitively"
    (ok (string= (intern-usage:prohibited-intern-function-p "common-lisp:INTERN")
                 "cl:intern")))

  (testing "cl:unintern matched"
    (ok (string= (intern-usage:prohibited-intern-function-p "COMMON-LISP:unintern")
                 "cl:unintern")))

  (testing "uiop:intern* matched"
    (ok (string= (intern-usage:prohibited-intern-function-p "UIOP:intern*")
                 "uiop:intern*")))

  (testing "uiop:intern* matched case-insensitively"
    (ok (string= (intern-usage:prohibited-intern-function-p "uiop:INTERN*")
                 "uiop:intern*")))

  (testing "alexandria:symbolicate matched"
    (ok (string= (intern-usage:prohibited-intern-function-p "ALEXANDRIA:symbolicate")
                 "alexandria:symbolicate")))

  (testing "alexandria:format-symbol matched"
    (ok (string= (intern-usage:prohibited-intern-function-p "ALEXANDRIA:format-symbol")
                 "alexandria:format-symbol")))

  (testing "alexandria:make-keyword matched"
    (ok (string= (intern-usage:prohibited-intern-function-p "ALEXANDRIA:make-keyword")
                 "alexandria:make-keyword")))

  (testing "Unqualified intern matched by name (conservative)"
    (ok (string= (intern-usage:prohibited-intern-function-p "intern")
                 "cl:intern")))

  (testing "Unqualified intern* matched by name"
    (ok (string= (intern-usage:prohibited-intern-function-p "intern*")
                 "uiop:intern*")))

  (testing "Unqualified symbolicate matched by name"
    (ok (string= (intern-usage:prohibited-intern-function-p "symbolicate")
                 "alexandria:symbolicate")))

  (testing "intern from unknown package does not match"
    (ok (null (intern-usage:prohibited-intern-function-p "MYPACKAGE:intern"))))

  (testing "intern-something does not match (different name)"
    (ok (null (intern-usage:prohibited-intern-function-p "COMMON-LISP:intern-something"))))

  (testing "unrelated symbol does not match"
    (ok (null (intern-usage:prohibited-intern-function-p "COMMON-LISP:string"))))

  (testing "nil returns nil"
    (ok (null (intern-usage:prohibited-intern-function-p nil))))

  (testing "non-string returns nil"
    (ok (null (intern-usage:prohibited-intern-function-p 42))))

  (testing "INTERN and INTERN* are distinct (no false match of INTERN* as INTERN)"
    (ok (string= (intern-usage:prohibited-intern-function-p "UIOP:intern*")
                 "uiop:intern*"))
    (ok (string= (intern-usage:prohibited-intern-function-p "COMMON-LISP:intern")
                 "cl:intern")))

  (testing "double-colon internal access still matches"
    (ok (string= (intern-usage:prohibited-intern-function-p "COMMON-LISP::intern")
                 "cl:intern"))))
