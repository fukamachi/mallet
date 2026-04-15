(defpackage #:mallet/tests/parser/unknown-reader-macros
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:parser #:mallet/parser)))
(in-package #:mallet/tests/parser/unknown-reader-macros)

(defun %form-contains-name-p (form name)
  "Recursively check FORM for a string element whose unprefixed symbol name
matches NAME case-insensitively."
  (labels ((match-one (s)
             (and (stringp s)
                  (let ((c (position #\: s :from-end t)))
                    (string-equal (if c (subseq s (1+ c)) s) name)))))
    (cond ((match-one form) t)
          ((consp form)
           (or (%form-contains-name-p (car form) name)
               (%form-contains-name-p (cdr form) name)))
          (t nil))))

(deftest parse-with-unknown-reader-macro
  (testing "Parse forms with unknown reader macro (#?\"...\" from cl-interpol)"
    (let* ((text "(defun before ()
  (let ((x 1))
    (print x)))

(progn
  #?\"test string\"
  (let ((unused 1))
    (print 'ok)))

(defun after ()
  (let ((y 1))
    (print y)))")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))

      ;; Should parse 3 forms (before, progn, after)
      (ok (= 3 (length forms)))

      ;; First form should be (defun before ...)
      (let ((first-form (parser:form-expr (first forms))))
        (ok (consp first-form))
        (ok (search "defun" (string-downcase (first first-form)))))

      ;; Second form should be (progn :unknown-reader-macro ...) — the cl-interpol
      ;; string has no interpolation, so it becomes :unknown-reader-macro.
      (let ((second-form (parser:form-expr (second forms))))
        (ok (consp second-form))
        (ok (search "progn" (string-downcase (first second-form))))
        (ok (eq :unknown-reader-macro (second second-form))))

      ;; Third form should be (defun after ...)
      (let ((third-form (parser:form-expr (third forms))))
        (ok (consp third-form))
        (ok (search "defun" (string-downcase (first third-form)))))))

  (testing "Parse form with unknown dispatch reader macro at top level"
    (let* ((text "#?\"ignored\"
(defun foo ()
  (let ((x 1))
    (print x)))")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))

      (ok (<= 1 (length forms)))

      (let ((has-defun (some (lambda (f)
                               (let ((expr (parser:form-expr f)))
                                 (and (consp expr)
                                      (stringp (first expr))
                                      (search "defun" (string-downcase (first expr))))))
                             forms)))
        (ok has-defun))))

  (testing "Multiple unknown reader macros in sequence"
    (let* ((text "(progn
  #?\"first\"
  #?\"second\"
  (let ((x 1)) x))")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))

      (ok (= 1 (length forms)))

      (let ((form (parser:form-expr (first forms))))
        (ok (consp form))
        (ok (search "progn" (string-downcase (first form)))))))

  (testing "Unknown reader macro with quote syntax (#?'...)"
    (let* ((text "(with-buffer (#?' \"test\" 'symbol)
  (do-something))")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))

      (ok (= 1 (length forms)))

      (let ((form (parser:form-expr (first forms))))
        (ok (consp form))
        (ok (search "with-buffer" (string-downcase (first form))))))))

(deftest parse-cl-interpol-with-interpolation
  (testing "#?\"Hello ${name}\" produces a synthetic PROGN containing NAME reference"
    (let* ((text "#?\"Hello ${name}\"")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))

      ;; Should produce one top-level form: the synthetic PROGN
      (ok (<= 1 (length forms)))
      (ok (%form-contains-name-p (parser:form-expr (first forms)) "NAME"))))

  (testing "#?\"${x} and ${y}\" produces a form containing both X and Y"
    (let* ((text "#?\"${x} and ${y}\"")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))

      (ok (<= 1 (length forms)))

      (let ((f (parser:form-expr (first forms))))
        (ok (%form-contains-name-p f "X"))
        (ok (%form-contains-name-p f "Y")))))

  (testing "#?\"plain string\" (no interpolation) returns :unknown-reader-macro placeholder"
    (let* ((text "(progn #?\"plain string\" (values))")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))

      (ok (= 1 (length forms)))

      (let ((progn-form (parser:form-expr (first forms))))
        (ok (consp progn-form))
        (ok (eq :unknown-reader-macro (second progn-form))))))

  (testing "Non-? unknown dispatch macro still returns :unknown-reader-macro"
    (let* ((text "(progn #~\"something\" (values))")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))

      (ok (= 1 (length forms)))

      (let ((progn-form (parser:form-expr (first forms))))
        (ok (consp progn-form))
        (ok (eq :unknown-reader-macro (second progn-form))))))

  (testing "#?\"${name}\" inside let exposes NAME as a reference"
    (let* ((text "(let ((name \"Alice\"))
  #?\"Hello ${name}!\")")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))

      (ok (= 1 (length forms)))
      (ok (%form-contains-name-p (parser:form-expr (first forms)) "NAME"))))

  ;; --- Issue #48 regression: unescaped " inside ${...} ---

  (testing "Issue #48: unescaped \"\" inside ${(or x \"\")} exposes the reference"
    (let* ((text "(let ((ctx \"hi\"))
  #?\"${(or ctx \"\")}\")")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))
      (ok (= 1 (length forms)))
      (ok (%form-contains-name-p (parser:form-expr (first forms)) "CTX"))))

  ;; --- Other inner delimiters that cl-interpol supports ---

  (testing "Alternative inner delimiter $(expr) extracts the reference"
    (let* ((text "(let ((x 1)) #?\"=$(x)=\")")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))
      (ok (= 1 (length forms)))
      (ok (%form-contains-name-p (parser:form-expr (first forms)) "X"))))

  ;; --- @{} list interpolation ---

  (testing "@{list} produces a reference to the list expression"
    (let* ((text "(let ((items '(1 2 3))) #?\"items: @{items}\")")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))
      (ok (= 1 (length forms)))
      (ok (%form-contains-name-p (parser:form-expr (first forms)) "ITEMS")))))

(deftest parse-cl-interpol-edge-cases
  ;; Migrated from the deleted interpolation-extractor-test.lisp:
  ;; these now exercise the end-to-end parse path through cl-interpol's reader.

  (testing "Escaped \\${name}: cl-interpol does not treat as interpolation"
    (let* ((text "#?\"\\${name}\"")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))
      ;; Either parses as constant string (→ :unknown-reader-macro) or extracts name.
      ;; Either way, must not crash the parser.
      (ok (<= 1 (length forms)))))

  (testing "Empty ${} does not crash the parser"
    (let* ((text "#?\"before${}after\"")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))
      (ok (<= 1 (length forms)))))

  (testing "Literal $10 (no interpolation) does not crash"
    (let* ((text "#?\"cost is $10\"")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))
      (ok (<= 1 (length forms)))))

  (testing "Literal email@example.com (no interpolation) does not crash"
    (let* ((text "#?\"email@example.com\"")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))
      (ok (<= 1 (length forms)))))

  (testing "*read-eval* defense: #.(error \"nope\") inside ${...} must not execute"
    ;; If *read-eval* leaked through, this would error during parse with
    ;; "should-not-eval". With *read-eval* bound to nil, cl-interpol's reader
    ;; errors out cleanly. The parser may then drop the form entirely or
    ;; recover to :unknown-reader-macro — either is acceptable; what matters
    ;; is that the lint pass does not crash and no code executes.
    (let ((text "(progn #?\"${#.(error \\\"should-not-eval\\\")}\" (values))"))
      ;; If the test runs to completion without signaling "should-not-eval",
      ;; the *read-eval* defense worked.
      (handler-case
          (progn
            (parser:parse-forms text #p"/tmp/test.lisp")
            (ok t))
        (error (e)
          ;; If error message contains "should-not-eval", code executed → fail.
          ;; Other errors (e.g. read errors) indicate cl-interpol failed to
          ;; parse, which is fine.
          (ok (not (search "should-not-eval" (princ-to-string e)))))))))
