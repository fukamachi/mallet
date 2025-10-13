(defpackage #:mallet/tests/rules/asdf
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/asdf)

;;; ASDF component strings tests

(deftest asdf-component-strings-valid
  (testing "Valid ASDF system definitions with strings"
    (let ((rule (make-instance 'rules:asdf-component-strings-rule))
          (file (uiop:parse-native-namestring "test.asd")))

      (testing "System name as string is valid"
        (let* ((text "(defsystem \"my-system\" :depends-on (\"alexandria\"))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (null violations))))

      (testing "Dependencies as strings are valid"
        (let* ((text "(defsystem \"my-system\" :depends-on (\"alexandria\" \"cl-ppcre\"))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (null violations))))

      (testing "Components as strings are valid"
        (let* ((text "(defsystem \"my-system\" :components ((:file \"main\") (:file \"utils\")))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (null violations))))))

  (testing "Non-asd files are ignored"
    (let ((rule (make-instance 'rules:asdf-component-strings-rule))
          (file (uiop:parse-native-namestring "test.lisp")))

      (testing "defsystem in .lisp file is ignored"
        (let* ((text "(defsystem #:my-system :depends-on (#:alexandria))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (null violations)))))))

(deftest asdf-component-strings-invalid
  (testing "Invalid ASDF system definitions with symbols"
    (let ((rule (make-instance 'rules:asdf-component-strings-rule))
          (file (uiop:parse-native-namestring "test.asd")))

      (testing "System name as symbol violates"
        (let* ((text "(defsystem #:my-system :depends-on (\"alexandria\"))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1))
          (ok (eq (violation:violation-rule (first violations)) :asdf-component-strings))
          (ok (search "instead of symbol #:my-system"
                      (violation:violation-message (first violations))))))

      (testing "Dependency as symbol violates"
        (let* ((text "(defsystem \"my-system\" :depends-on (#:alexandria))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1))
          (ok (eq (violation:violation-rule (first violations)) :asdf-component-strings))))

      (testing "Multiple symbol dependencies violate"
        (let* ((text "(defsystem \"my-system\" :depends-on (#:alexandria #:cl-ppcre))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 2))))

      (testing "Component name as symbol violates"
        (let* ((text "(defsystem \"my-system\" :components ((:file #:main)))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1))
          (ok (eq (violation:violation-rule (first violations)) :asdf-component-strings))))

      (testing "Mixed symbols and strings show only symbol violations"
        (let* ((text "(defsystem \"my-system\" :depends-on (\"alexandria\" #:cl-ppcre))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1)))))))
