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

(deftest prohibited-intern-function-registry
  (testing "cl:intern matched by full package name (as produced by parser)"
    (ok (string= (runtime-intern:prohibited-intern-function-p "COMMON-LISP:intern")
                 "cl:intern")))

  (testing "cl:intern matched case-insensitively"
    (ok (string= (runtime-intern:prohibited-intern-function-p "common-lisp:INTERN")
                 "cl:intern")))

  (testing "cl:unintern matched"
    (ok (string= (runtime-intern:prohibited-intern-function-p "COMMON-LISP:unintern")
                 "cl:unintern")))

  (testing "uiop:intern* matched"
    (ok (string= (runtime-intern:prohibited-intern-function-p "UIOP:intern*")
                 "uiop:intern*")))

  (testing "uiop:intern* matched case-insensitively"
    (ok (string= (runtime-intern:prohibited-intern-function-p "uiop:INTERN*")
                 "uiop:intern*")))

  (testing "alexandria:symbolicate matched"
    (ok (string= (runtime-intern:prohibited-intern-function-p "ALEXANDRIA:symbolicate")
                 "alexandria:symbolicate")))

  (testing "alexandria:format-symbol matched"
    (ok (string= (runtime-intern:prohibited-intern-function-p "ALEXANDRIA:format-symbol")
                 "alexandria:format-symbol")))

  (testing "alexandria:make-keyword matched"
    (ok (string= (runtime-intern:prohibited-intern-function-p "ALEXANDRIA:make-keyword")
                 "alexandria:make-keyword")))

  (testing "Unqualified intern matched by name (conservative)"
    (ok (string= (runtime-intern:prohibited-intern-function-p "intern")
                 "cl:intern")))

  (testing "Unqualified intern* matched by name"
    (ok (string= (runtime-intern:prohibited-intern-function-p "intern*")
                 "uiop:intern*")))

  (testing "Unqualified symbolicate matched by name"
    (ok (string= (runtime-intern:prohibited-intern-function-p "symbolicate")
                 "alexandria:symbolicate")))

  (testing "intern from unknown package does not match"
    (ok (null (runtime-intern:prohibited-intern-function-p "MYPACKAGE:intern"))))

  (testing "intern-something does not match (different name)"
    (ok (null (runtime-intern:prohibited-intern-function-p "COMMON-LISP:intern-something"))))

  (testing "unrelated symbol does not match"
    (ok (null (runtime-intern:prohibited-intern-function-p "COMMON-LISP:string"))))

  (testing "nil returns nil"
    (ok (null (runtime-intern:prohibited-intern-function-p nil))))

  (testing "non-string returns nil"
    (ok (null (runtime-intern:prohibited-intern-function-p 42))))

  (testing "INTERN and INTERN* are distinct (no false match of INTERN* as INTERN)"
    (ok (string= (runtime-intern:prohibited-intern-function-p "UIOP:intern*")
                 "uiop:intern*"))
    (ok (string= (runtime-intern:prohibited-intern-function-p "COMMON-LISP:intern")
                 "cl:intern")))

  (testing "double-colon internal access still matches"
    (ok (string= (runtime-intern:prohibited-intern-function-p "COMMON-LISP::intern")
                 "cl:intern"))))

;;; Helper: make a minimal package-context with given nicknames and imports

(defun make-test-context (&key nicknames imports)
  "Create a package-context for testing.
NICKNAMES is an alist of (nick . pkg) e.g. ((\"A\" . \"ALEXANDRIA\"))
IMPORTS is an alist of (sym . pkg) e.g. ((\"SYMBOLICATE\" . \"ALEXANDRIA\"))"
  (let ((ctx (runtime-intern:make-package-context)))
    (dolist (pair nicknames)
      (setf (gethash (car pair) (runtime-intern:package-context-local-nicknames ctx))
            (cdr pair)))
    (dolist (pair imports)
      (setf (gethash (car pair) (runtime-intern:package-context-imported-symbols ctx))
            (cdr pair)))
    ctx))

;;; Package context resolver tests

(deftest package-context-local-nicknames
  (testing "Local nickname for ALEXANDRIA resolves symbolicate"
    (let ((ctx (make-test-context :nicknames '(("A" . "ALEXANDRIA")))))
      (ok (string= (runtime-intern:resolve-runtime-intern "A:symbolicate" ctx)
                   "alexandria:symbolicate"))))

  (testing "Local nickname for ALEXANDRIA resolves format-symbol"
    (let ((ctx (make-test-context :nicknames '(("A" . "ALEXANDRIA")))))
      (ok (string= (runtime-intern:resolve-runtime-intern "A:format-symbol" ctx)
                   "alexandria:format-symbol"))))

  (testing "Local nickname for ALEXANDRIA resolves make-keyword"
    (let ((ctx (make-test-context :nicknames '(("A" . "ALEXANDRIA")))))
      (ok (string= (runtime-intern:resolve-runtime-intern "A:make-keyword" ctx)
                   "alexandria:make-keyword"))))

  (testing "Local nickname for UIOP resolves intern*"
    (let ((ctx (make-test-context :nicknames '(("U" . "UIOP")))))
      (ok (string= (runtime-intern:resolve-runtime-intern "U:intern*" ctx)
                   "uiop:intern*"))))

  (testing "Local nickname for CL resolves intern"
    (let ((ctx (make-test-context :nicknames '(("L" . "COMMON-LISP")))))
      (ok (string= (runtime-intern:resolve-runtime-intern "L:intern" ctx)
                   "cl:intern"))))

  (testing "Unknown prefix without nickname mapping does not match"
    (let ((ctx (make-test-context :nicknames '(("A" . "MYPACKAGE")))))
      (ok (null (runtime-intern:resolve-runtime-intern "A:symbolicate" ctx)))))

  (testing "Prefix for unrelated package does not match"
    (let ((ctx (make-test-context :nicknames '(("FOO" . "SOMETHING")))))
      (ok (null (runtime-intern:resolve-runtime-intern "FOO:intern" ctx))))))

(deftest package-context-imported-symbols
  (testing "Imported symbolicate from ALEXANDRIA matches"
    (let ((ctx (make-test-context :imports '(("SYMBOLICATE" . "ALEXANDRIA")))))
      (ok (string= (runtime-intern:resolve-runtime-intern "symbolicate" ctx)
                   "alexandria:symbolicate"))))

  (testing "Imported intern* from UIOP matches"
    (let ((ctx (make-test-context :imports '(("INTERN*" . "UIOP")))))
      (ok (string= (runtime-intern:resolve-runtime-intern "intern*" ctx)
                   "uiop:intern*"))))

  (testing "Imported intern from MY-PACKAGE does NOT match (wrong source)"
    (let ((ctx (make-test-context :imports '(("INTERN" . "MY-PACKAGE")))))
      (ok (null (runtime-intern:resolve-runtime-intern "intern" ctx)))))

  (testing "Unqualified symbol not in imports returns NIL (no false positives)"
    (let ((ctx (make-test-context)))
      ;; Without an explicit import, unqualified symbols are not flagged
      (ok (null (runtime-intern:resolve-runtime-intern "intern" ctx)))))

  (testing "Fully qualified symbol bypasses import lookup"
    (let ((ctx (make-test-context)))
      (ok (string= (runtime-intern:resolve-runtime-intern "COMMON-LISP:intern" ctx)
                   "cl:intern"))))

  (testing "CURRENT-prefixed symbol not in imports returns NIL"
    (let ((ctx (make-test-context)))
      (ok (null (runtime-intern:resolve-runtime-intern "CURRENT:intern" ctx)))))

  (testing "CURRENT-prefixed symbol in imports matches"
    (let ((ctx (make-test-context :imports '(("INTERN" . "COMMON-LISP")))))
      (ok (string= (runtime-intern:resolve-runtime-intern "CURRENT:intern" ctx)
                   "cl:intern")))))

(deftest build-package-context-from-forms
  (testing "Extracts local nicknames from defpackage"
    (let* ((code "(defpackage #:test
                    (:use #:cl)
                    (:local-nicknames (#:a #:alexandria) (#:u #:uiop)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (ctx (runtime-intern:build-package-context forms)))
      (ok (string= (gethash "A" (runtime-intern:package-context-local-nicknames ctx))
                   "ALEXANDRIA"))
      (ok (string= (gethash "U" (runtime-intern:package-context-local-nicknames ctx))
                   "UIOP"))))

  (testing "Extracts import-from mappings from defpackage"
    (let* ((code "(defpackage #:test
                    (:use #:cl)
                    (:import-from #:uiop #:intern*)
                    (:import-from #:alexandria #:symbolicate #:make-keyword))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (ctx (runtime-intern:build-package-context forms)))
      (ok (string= (gethash "INTERN*" (runtime-intern:package-context-imported-symbols ctx))
                   "UIOP"))
      (ok (string= (gethash "SYMBOLICATE" (runtime-intern:package-context-imported-symbols ctx))
                   "ALEXANDRIA"))
      (ok (string= (gethash "MAKE-KEYWORD" (runtime-intern:package-context-imported-symbols ctx))
                   "ALEXANDRIA"))))

  (testing "Empty forms produce empty context"
    (let* ((ctx (runtime-intern:build-package-context '())))
      (ok (zerop (hash-table-count (runtime-intern:package-context-local-nicknames ctx))))
      (ok (zerop (hash-table-count (runtime-intern:package-context-imported-symbols ctx))))))

  (testing "Full resolve: nickname prefix detects violation"
    (let* ((code "(defpackage #:test
                    (:use #:cl)
                    (:local-nicknames (#:a #:alexandria)))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (ctx (runtime-intern:build-package-context forms)))
      (ok (string= (runtime-intern:resolve-runtime-intern "A:symbolicate" ctx)
                   "alexandria:symbolicate"))))

  (testing "Full resolve: import-from detects violation"
    (let* ((code "(defpackage #:test
                    (:use #:cl)
                    (:import-from #:uiop #:intern*))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (ctx (runtime-intern:build-package-context forms)))
      (ok (string= (runtime-intern:resolve-runtime-intern "intern*" ctx)
                   "uiop:intern*"))))

  (testing "Full resolve: import from wrong package avoids false positive"
    (let* ((code "(defpackage #:test
                    (:use #:cl)
                    (:import-from #:my-package #:intern))")
           (forms (parser:parse-forms code #p"test.lisp"))
           (ctx (runtime-intern:build-package-context forms)))
      ;; intern imported from MY-PACKAGE → not prohibited
      (ok (null (runtime-intern:resolve-runtime-intern "intern" ctx))))))

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
  (let* ((forms (parser:parse-forms code #p"test.lisp"))
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

  (testing "Local nickname a:format-symbol is flagged"
    (with-test-file (tmpfile
                     "(defpackage #:test (:use #:cl) (:local-nicknames (#:a #:alexandria)))
                      (in-package #:test)
                      (defun bad () (a:format-symbol nil \"~A\" :foo))")
      (let ((violations (check-intern-file tmpfile)))
        (ok (= (length violations) 1))
        (ok (search "alexandria:format-symbol" (violation:violation-message (first violations)))))))

  (testing "Local nickname u:intern* is flagged"
    (with-test-file (tmpfile
                     "(defpackage #:test (:use #:cl) (:local-nicknames (#:u #:uiop)))
                      (in-package #:test)
                      (defun bad () (u:intern* \"FOO\" :keyword))")
      (let ((violations (check-intern-file tmpfile)))
        (ok (= (length violations) 1))
        (ok (search "uiop:intern*" (violation:violation-message (first violations)))))))

  (testing "Import-from uiop:intern* is flagged"
    (with-test-file (tmpfile
                     "(defpackage #:test (:use #:cl) (:import-from #:uiop #:intern*))
                      (in-package #:test)
                      (defun bad () (intern* \"FOO\" :keyword))")
      (let ((violations (check-intern-file tmpfile)))
        (ok (= (length violations) 1))
        (ok (search "uiop:intern*" (violation:violation-message (first violations)))))))

  (testing "Import-from cl:intern is flagged"
    (with-test-file (tmpfile
                     "(defpackage #:test (:use #:cl) (:import-from #:cl #:intern))
                      (in-package #:test)
                      (defun bad () (intern \"FOO\"))")
      (let ((violations (check-intern-file tmpfile)))
        (ok (= (length violations) 1))
        (ok (search "cl:intern" (violation:violation-message (first violations)))))))

  (testing "Import-from alexandria:symbolicate is flagged"
    (with-test-file (tmpfile
                     "(defpackage #:test (:use #:cl) (:import-from #:alexandria #:symbolicate))
                      (in-package #:test)
                      (defun bad () (symbolicate :foo :bar))")
      (let ((violations (check-intern-file tmpfile)))
        (ok (= (length violations) 1))
        (ok (search "alexandria:symbolicate" (violation:violation-message (first violations)))))))

  (testing "Import from unknown package does not flag"
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
