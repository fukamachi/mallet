(defpackage #:mallet/tests/rules/intern-usage
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:intern-usage #:mallet/rules/forms/intern-usage)
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
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

;;; Rule class tests

(defmacro with-test-file ((tmpfile-var code) &body body)
  "Helper: create a temporary file with CODE and clean up after."
  `(uiop:with-temporary-file (:stream stream :pathname ,tmpfile-var
                              :type "lisp" :keep t)
     (write-string ,code stream)
     (finish-output stream)
     ,@body))

(defun check-intern (code)
  "Check CODE for intern-usage violations using a fake file path (no context)."
  (let* ((forms (parser:parse-forms code #p"test.lisp"))
         (rule (make-instance 'rules:intern-usage-rule)))
    (mapcan (lambda (form)
              (rules:check-form rule form #p"test.lisp"))
            forms)))

(defun check-intern-file (tmpfile)
  "Check TMPFILE for intern-usage violations using real file (with context)."
  (let* ((text (uiop:read-file-string tmpfile))
         (forms (parser:parse-forms text tmpfile))
         (rule (make-instance 'rules:intern-usage-rule)))
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

  (testing "No intern: intern inside defmacro body is skipped"
    (ok (null (check-intern "(defmacro def-var (name)
                               `(defvar ,name (intern (symbol-name ',name))))"))))

  (testing "No intern: eval-when without :execute is skipped"
    (ok (null (check-intern "(eval-when (:compile-toplevel :load-toplevel)
                               (intern \"FOO\"))"))))

  (testing "No intern: funcall with unrelated function"
    (ok (null (check-intern "(funcall #'string \"hello\")"))))

  (testing "No intern: apply with unrelated function"
    (ok (null (check-intern "(apply #'+ '(1 2 3))")))))

;;; Direct call violations

(deftest intern-usage-direct
  (testing "Direct (intern ...) call is flagged"
    (let ((violations (check-intern "(intern \"FOO\")")))
      (ok (= (length violations) 1))
      (ok (eq (violation:violation-rule (first violations)) :intern-usage))
      (ok (search "cl:intern" (violation:violation-message (first violations))))))

  (testing "Direct (unintern ...) call is flagged"
    (let ((violations (check-intern "(unintern 'foo)")))
      (ok (= (length violations) 1))
      (ok (search "cl:unintern" (violation:violation-message (first violations))))))

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

  (testing "Qualified (uiop:intern* ...) is flagged"
    (let ((violations (check-intern "(uiop:intern* \"FOO\" :keyword)")))
      (ok (= (length violations) 1))
      (ok (search "uiop:intern*" (violation:violation-message (first violations))))))

  (testing "Nested intern inside defun is flagged"
    (let ((violations (check-intern "(defun bad () (intern \"FOO\"))")))
      (ok (= (length violations) 1))))

  (testing "Multiple intern calls produce multiple violations"
    (let ((violations (check-intern "(progn (intern \"A\") (intern \"B\"))")))
      (ok (= (length violations) 2))))

  (testing "eval-when with :execute is flagged"
    (let ((violations (check-intern "(eval-when (:execute)
                                       (intern \"FOO\"))")))
      (ok (= (length violations) 1))))

  (testing "eval-when with :load-toplevel :execute is flagged"
    (let ((violations (check-intern "(eval-when (:load-toplevel :execute)
                                       (intern \"BAR\"))")))
      (ok (= (length violations) 1)))))

;;; Funcall / apply patterns

(deftest intern-usage-funcall-apply
  (testing "(funcall #'intern ...) is flagged"
    (let ((violations (check-intern "(funcall #'intern \"FOO\")")))
      (ok (= (length violations) 1))
      (ok (search "funcall" (violation:violation-message (first violations))))))

  (testing "(funcall 'intern ...) is flagged"
    (let ((violations (check-intern "(funcall 'intern \"FOO\")")))
      (ok (= (length violations) 1))
      (ok (search "funcall" (violation:violation-message (first violations))))))

  (testing "(apply #'intern ...) is flagged"
    (let ((violations (check-intern "(apply #'intern '(\"FOO\" :keyword))")))
      (ok (= (length violations) 1))
      (ok (search "apply" (violation:violation-message (first violations))))))

  (testing "(apply 'unintern ...) is flagged"
    (let ((violations (check-intern "(apply 'unintern '(foo))")))
      (ok (= (length violations) 1))
      (ok (search "apply" (violation:violation-message (first violations))))))

  (testing "(funcall #'symbolicate ...) is flagged (name-only match)"
    (let ((violations (check-intern "(funcall #'symbolicate :foo :bar)")))
      (ok (= (length violations) 1))))

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
  (testing "Local nickname a:symbolicate is flagged"
    (with-test-file (tmpfile
                     "(defpackage #:test (:use #:cl) (:local-nicknames (#:a #:alexandria)))
                      (in-package #:test)
                      (defun bad () (a:symbolicate :foo :bar))")
      (let ((violations (check-intern-file tmpfile)))
        (ok (= (length violations) 1))
        (ok (search "alexandria:symbolicate" (violation:violation-message (first violations)))))))

  (testing "Local nickname a:make-keyword is flagged"
    (with-test-file (tmpfile
                     "(defpackage #:test (:use #:cl) (:local-nicknames (#:a #:alexandria)))
                      (in-package #:test)
                      (defun bad () (a:make-keyword \"FOO\"))")
      (let ((violations (check-intern-file tmpfile)))
        (ok (= (length violations) 1))
        (ok (search "alexandria:make-keyword" (violation:violation-message (first violations)))))))

  (testing "Import-from intern* is flagged"
    (with-test-file (tmpfile
                     "(defpackage #:test (:use #:cl) (:import-from #:uiop #:intern*))
                      (in-package #:test)
                      (defun bad () (intern* \"FOO\" :keyword))")
      (let ((violations (check-intern-file tmpfile)))
        (ok (= (length violations) 1))
        (ok (search "uiop:intern*" (violation:violation-message (first violations)))))))

  (testing "Import from unknown package does not flag"
    (with-test-file (tmpfile
                     "(defpackage #:test (:use #:cl) (:import-from #:my-package #:intern))
                      (in-package #:test)
                      (defun safe () (intern \"FOO\"))")
      (let ((violations (check-intern-file tmpfile)))
        (ok (null violations))))))
