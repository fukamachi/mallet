(defpackage #:mallet/tests/rules/intern-usage
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:intern-usage #:mallet/rules/forms/intern-usage)
   (#:parser #:mallet/parser)))
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

;;; Helper: make a minimal package-context with given nicknames and imports

(defun make-test-context (&key nicknames imports)
  "Create a package-context for testing.
NICKNAMES is an alist of (nick . pkg) e.g. ((\"A\" . \"ALEXANDRIA\"))
IMPORTS is an alist of (sym . pkg) e.g. ((\"SYMBOLICATE\" . \"ALEXANDRIA\"))"
  (let ((ctx (intern-usage:make-package-context)))
    (dolist (pair nicknames)
      (setf (gethash (car pair) (intern-usage:package-context-local-nicknames ctx))
            (cdr pair)))
    (dolist (pair imports)
      (setf (gethash (car pair) (intern-usage:package-context-imported-symbols ctx))
            (cdr pair)))
    ctx))

;;; Package context resolver tests

(deftest package-context-local-nicknames
  (testing "Local nickname for ALEXANDRIA resolves symbolicate"
    (let ((ctx (make-test-context :nicknames '(("A" . "ALEXANDRIA")))))
      (ok (string= (intern-usage:resolve-intern-usage "A:symbolicate" ctx)
                   "alexandria:symbolicate"))))

  (testing "Local nickname for ALEXANDRIA resolves format-symbol"
    (let ((ctx (make-test-context :nicknames '(("A" . "ALEXANDRIA")))))
      (ok (string= (intern-usage:resolve-intern-usage "A:format-symbol" ctx)
                   "alexandria:format-symbol"))))

  (testing "Local nickname for ALEXANDRIA resolves make-keyword"
    (let ((ctx (make-test-context :nicknames '(("A" . "ALEXANDRIA")))))
      (ok (string= (intern-usage:resolve-intern-usage "A:make-keyword" ctx)
                   "alexandria:make-keyword"))))

  (testing "Local nickname for UIOP resolves intern*"
    (let ((ctx (make-test-context :nicknames '(("U" . "UIOP")))))
      (ok (string= (intern-usage:resolve-intern-usage "U:intern*" ctx)
                   "uiop:intern*"))))

  (testing "Local nickname for CL resolves intern"
    (let ((ctx (make-test-context :nicknames '(("L" . "COMMON-LISP")))))
      (ok (string= (intern-usage:resolve-intern-usage "L:intern" ctx)
                   "cl:intern"))))

  (testing "Unknown prefix without nickname mapping does not match"
    (let ((ctx (make-test-context :nicknames '(("A" . "MYPACKAGE")))))
      (ok (null (intern-usage:resolve-intern-usage "A:symbolicate" ctx)))))

  (testing "Prefix for unrelated package does not match"
    (let ((ctx (make-test-context :nicknames '(("FOO" . "SOMETHING")))))
      (ok (null (intern-usage:resolve-intern-usage "FOO:intern" ctx))))))

(deftest package-context-imported-symbols
  (testing "Imported symbolicate from ALEXANDRIA matches"
    (let ((ctx (make-test-context :imports '(("SYMBOLICATE" . "ALEXANDRIA")))))
      (ok (string= (intern-usage:resolve-intern-usage "symbolicate" ctx)
                   "alexandria:symbolicate"))))

  (testing "Imported intern* from UIOP matches"
    (let ((ctx (make-test-context :imports '(("INTERN*" . "UIOP")))))
      (ok (string= (intern-usage:resolve-intern-usage "intern*" ctx)
                   "uiop:intern*"))))

  (testing "Imported intern from MY-PACKAGE does NOT match (wrong source)"
    (let ((ctx (make-test-context :imports '(("INTERN" . "MY-PACKAGE")))))
      (ok (null (intern-usage:resolve-intern-usage "intern" ctx)))))

  (testing "Unqualified symbol not in imports falls back to name-only match"
    (let ((ctx (make-test-context)))
      ;; intern is a CL symbol, name-only still matches
      (ok (string= (intern-usage:resolve-intern-usage "intern" ctx)
                   "cl:intern"))))

  (testing "Fully qualified symbol bypasses import lookup"
    (let ((ctx (make-test-context)))
      (ok (string= (intern-usage:resolve-intern-usage "COMMON-LISP:intern" ctx)
                   "cl:intern")))))

(deftest build-package-context-from-forms
  (testing "Extracts local nicknames from defpackage"
    (let* ((code "(defpackage #:test
                    (:use #:cl)
                    (:local-nicknames (#:a #:alexandria) (#:u #:uiop)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (ctx (intern-usage:build-package-context forms)))
      (ok (string= (gethash "A" (intern-usage:package-context-local-nicknames ctx))
                   "ALEXANDRIA"))
      (ok (string= (gethash "U" (intern-usage:package-context-local-nicknames ctx))
                   "UIOP"))))

  (testing "Extracts import-from mappings from defpackage"
    (let* ((code "(defpackage #:test
                    (:use #:cl)
                    (:import-from #:uiop #:intern*)
                    (:import-from #:alexandria #:symbolicate #:make-keyword))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (ctx (intern-usage:build-package-context forms)))
      (ok (string= (gethash "INTERN*" (intern-usage:package-context-imported-symbols ctx))
                   "UIOP"))
      (ok (string= (gethash "SYMBOLICATE" (intern-usage:package-context-imported-symbols ctx))
                   "ALEXANDRIA"))
      (ok (string= (gethash "MAKE-KEYWORD" (intern-usage:package-context-imported-symbols ctx))
                   "ALEXANDRIA"))))

  (testing "Empty forms produce empty context"
    (let* ((ctx (intern-usage:build-package-context '())))
      (ok (zerop (hash-table-count (intern-usage:package-context-local-nicknames ctx))))
      (ok (zerop (hash-table-count (intern-usage:package-context-imported-symbols ctx))))))

  (testing "Full resolve: nickname prefix detects violation"
    (let* ((code "(defpackage #:test
                    (:use #:cl)
                    (:local-nicknames (#:a #:alexandria)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (ctx (intern-usage:build-package-context forms)))
      (ok (string= (intern-usage:resolve-intern-usage "A:symbolicate" ctx)
                   "alexandria:symbolicate"))))

  (testing "Full resolve: import-from detects violation"
    (let* ((code "(defpackage #:test
                    (:use #:cl)
                    (:import-from #:uiop #:intern*))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (ctx (intern-usage:build-package-context forms)))
      (ok (string= (intern-usage:resolve-intern-usage "intern*" ctx)
                   "uiop:intern*"))))

  (testing "Full resolve: import from wrong package avoids false positive"
    (let* ((code "(defpackage #:test
                    (:use #:cl)
                    (:import-from #:my-package #:intern))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (ctx (intern-usage:build-package-context forms)))
      ;; intern imported from MY-PACKAGE → not prohibited
      (ok (null (intern-usage:resolve-intern-usage "intern" ctx))))))
