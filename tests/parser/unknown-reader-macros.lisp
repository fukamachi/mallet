(defpackage #:mallet/tests/parser/unknown-reader-macros
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:parser #:mallet/parser)))
(in-package #:mallet/tests/parser/unknown-reader-macros)

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

      ;; Second form should be (progn :unknown-reader-macro ...)
      (let ((second-form (parser:form-expr (second forms))))
        (ok (consp second-form))
        (ok (search "progn" (string-downcase (first second-form))))
        ;; The unknown reader macro should be replaced with placeholder
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

      ;; Should parse at least the defun (unknown reader macro might be skipped or included)
      (ok (<= 1 (length forms)))

      ;; Should have a defun somewhere in the forms
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

      ;; Should parse the progn form
      (ok (= 1 (length forms)))

      (let ((form (parser:form-expr (first forms))))
        (ok (consp form))
        (ok (search "progn" (string-downcase (first form)))))))

  (testing "Unknown reader macro with quote syntax (#?'...)"
    (let* ((text "(with-buffer (#?' \"test\" 'symbol)
  (do-something))")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))

      ;; Should parse the with-buffer form
      (ok (= 1 (length forms)))

      (let ((form (parser:form-expr (first forms))))
        (ok (consp form))
        (ok (search "with-buffer" (string-downcase (first form))))))))

(deftest parse-cl-interpol-with-interpolation
  (testing "#?\"Hello ${name}\" produces PROGN with NAME reference"
    (let* ((text "#?\"Hello ${name}\"")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))

      ;; Should produce one top-level form: the synthetic PROGN
      (ok (<= 1 (length forms)))

      ;; Find the PROGN form
      (let ((progn-form (some (lambda (f)
                                (let ((expr (parser:form-expr f)))
                                  (and (consp expr)
                                       (stringp (first expr))
                                       (search "progn" (string-downcase (first expr)))
                                       expr)))
                              forms)))
        (ok progn-form)
        ;; PROGN should contain the NAME reference
        (let ((refs (rest progn-form)))
          (ok (some (lambda (r)
                      (and (stringp r) (search "NAME" (string-upcase r))))
                    refs))))))

  (testing "#?\"${x} and ${y}\" produces PROGN with both X and Y"
    (let* ((text "#?\"${x} and ${y}\"")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))

      (ok (<= 1 (length forms)))

      (let ((progn-form (some (lambda (f)
                                (let ((expr (parser:form-expr f)))
                                  (and (consp expr)
                                       (stringp (first expr))
                                       (search "progn" (string-downcase (first expr)))
                                       expr)))
                              forms)))
        (ok progn-form)
        (let ((refs (rest progn-form)))
          (ok (some (lambda (r) (and (stringp r) (search "X" (string-upcase r)))) refs))
          (ok (some (lambda (r) (and (stringp r) (search "Y" (string-upcase r)))) refs))))))

  (testing "#?\"plain string\" (no interpolation) returns :unknown-reader-macro placeholder"
    (let* ((text "(progn #?\"plain string\" (values))")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))

      (ok (= 1 (length forms)))

      (let ((progn-form (parser:form-expr (first forms))))
        (ok (consp progn-form))
        ;; The #?"plain string" slot should be :unknown-reader-macro
        (ok (eq :unknown-reader-macro (second progn-form))))))

  (testing "Non-? unknown dispatch macro still returns :unknown-reader-macro"
    (let* ((text "(progn #~\"something\" (values))")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))

      (ok (= 1 (length forms)))

      (let ((progn-form (parser:form-expr (first forms))))
        (ok (consp progn-form))
        (ok (eq :unknown-reader-macro (second progn-form))))))

  (testing "#?\"${name}\" inside let makes NAME visible as a reference"
    (let* ((text "(let ((name \"Alice\"))
  #?\"Hello ${name}!\")")
           (forms (parser:parse-forms text #p"/tmp/test.lisp")))

      (ok (= 1 (length forms)))

      ;; The let body should contain a PROGN-like form with NAME ref
      (let* ((let-form (parser:form-expr (first forms)))
             ;; let body is after the bindings: (LET bindings body...)
             (body (cddr let-form))
             (interp-form (first body)))
        (ok (consp interp-form))
        (ok (search "progn" (string-downcase (first interp-form))))
        (let ((refs (rest interp-form)))
          (ok (some (lambda (r)
                      (and (stringp r) (search "NAME" (string-upcase r))))
                    refs)))))))
