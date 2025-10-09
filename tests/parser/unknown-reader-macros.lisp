(defpackage #:malo/tests/parser/unknown-reader-macros
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:parser #:malo/parser)))
(in-package #:malo/tests/parser/unknown-reader-macros)

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
