(defpackage #:mallet/tests/rules/asdf-reader-conditional
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/asdf-reader-conditional)

;;; Helper

(defun check (text &optional (filename "test.asd"))
  "Run asdf-reader-conditional-rule on TEXT with FILE."
  (let ((rule (make-instance 'rules:asdf-reader-conditional-rule))
        (file (uiop:parse-native-namestring filename)))
    (rules:check-text rule text file)))

;;; Valid cases — zero violations

(deftest asdf-reader-conditional-valid
  (testing "No reader conditionals in defsystem"
    (let ((violations (check "(defsystem \"foo\"
  :depends-on (\"alexandria\")
  :components ((:file \"main\")))")))
      (ok (null violations))))

  (testing "#+sbcl in a comment line is excluded"
    (let ((violations (check "(defsystem \"foo\"
  ; #+sbcl some comment
  :components ((:file \"main\")))")))
      (ok (null violations))))

  (testing "#+ inside string literal is excluded"
    (let ((violations (check (format nil "(defsystem \"foo\"~%  :description \"#+sbcl only\")") )))
      (ok (null violations))))

  (testing "#+sbcl inside :perform body is excluded"
    (let ((violations (check "(defsystem \"foo\"
  :components ((:file \"main\"))
  :perform (test-op (o c)
             #+sbcl (load-file \"sbcl.lisp\")))")))
      (ok (null violations))))

  (testing "#+ outside any defsystem form is excluded"
    (let ((violations (check (format nil "#+sbcl (load \"sbcl-extra.lisp\")~%(defsystem \"foo\")"))))
      (ok (null violations))))

  (testing "#\\+ character literal is not a reader conditional"
    (let* ((char-literal (format nil "#\\+"))
           (text (format nil "(defsystem \"foo\"~%  :description ~S)" char-literal))
           (violations (check text)))
      (ok (null violations))))

  (testing "Non-.asd file is ignored"
    (let ((violations (check "(defsystem \"foo\"
  :depends-on (#+sbcl \"sbcl-extra\"))"
                             "test.lisp")))
      (ok (null violations))))

  (testing "#-ccl inside :perform body is excluded"
    (let ((violations (check "(defsystem \"foo\"
  :perform (test-op (o c)
             #-ccl (do-something)))")))
      (ok (null violations))))

  (testing "#+sbcl in :perform body in nested :components is excluded"
    (let ((violations (check "(defsystem \"foo\"
  :components
  ((:module \"tests\"
    :perform (test-op (o c)
               #+sbcl (load \"x\")))))")))
      (ok (null violations))))

  (testing "#+sbcl inside :around-compile body is excluded"
    (let ((violations (check "(defsystem \"foo\"
  :around-compile (lambda (next)
                    #+sbcl (sbcl-setup)
                    (funcall next)))")))
      (ok (null violations))))

  (testing "#-ccl inside :around-compile body is excluded"
    (let ((violations (check "(defsystem \"foo\"
  :around-compile (lambda (next)
                    #-ccl (non-ccl-setup)
                    (funcall next)))")))
      (ok (null violations))))

  (testing "#+sbcl in :around-compile in nested :components is excluded"
    (let ((violations (check "(defsystem \"foo\"
  :components
  ((:module \"src\"
    :around-compile (lambda (next)
                      #+sbcl (do-thing)
                      (funcall next)))))")))
      (ok (null violations))))

  (testing "#+asdf3 guarding a defsystem option keyword is excluded"
    (let ((violations (check "(defsystem \"foo\"
  :description \"bar\"
  #+asdf3 :mailto
  #+asdf3 \"dev@example.com\"
  :components ((:file \"main\")))")))
      (ok (null violations))))

  (testing "#-asdf3 guarding a defsystem option value is excluded"
    (let ((violations (check "(defsystem \"foo\"
  #-asdf3 :defsystem-depends-on
  #-asdf3 (\"my-defsystem-extension\")
  :components ((:file \"main\")))")))
      (ok (null violations))))

  (testing "#+sbcl option-pair excluded but #+sbcl in :depends-on sublist still flagged"
    (let ((violations (check "(defsystem \"foo\"
  #+sbcl :entry-point
  #+sbcl \"foo:main\"
  :depends-on (#+sbcl \"sbcl-dep\")
  :components ((:file \"main\")))")))
      ;; #+sbcl at option-plist level (2 occurrences) are excluded
      ;; #+sbcl inside :depends-on (...) is flagged
      (ok (= 1 (length violations)))
      (ok (eq :asdf-reader-conditional
              (violation:violation-rule (first violations)))))))

;;; Invalid cases — violations expected

(deftest asdf-reader-conditional-invalid
  (testing "#+sbcl in :components list produces 1 violation"
    (let ((violations (check "(defsystem \"foo\"
  :components (#+sbcl (:file \"sbcl-impl\")))")))
      (ok (= 1 (length violations)))
      (ok (eq :asdf-reader-conditional
              (violation:violation-rule (first violations))))
      (ok (search "if-feature" (violation:violation-message (first violations))
                  :test #'char-equal))))

  (testing "#-ccl in :depends-on produces 1 violation"
    (let ((violations (check "(defsystem \"foo\"
  :depends-on (#-ccl \"non-ccl-dep\"))")))
      (ok (= 1 (length violations)))
      (ok (eq :asdf-reader-conditional
              (violation:violation-rule (first violations))))))

  (testing "Multiple #+/#- on different lines produces correct count"
    (let ((violations (check "(defsystem \"foo\"
  :depends-on (#+sbcl \"sbcl-dep\"
               #-ccl \"non-ccl-dep\")
  :components (#+sbcl (:file \"sbcl-impl\")))")))
      (ok (= 3 (length violations)))))

  (testing "Violation reports correct line number"
    (let ((violations (check "(defsystem \"foo\"
  :depends-on (#+sbcl \"sbcl-dep\"))")))
      (ok (= 2 (violation:violation-line (first violations))))))

  (testing "Violation reports correct column for #+"
    (let ((violations (check "(defsystem \"foo\"
  :depends-on (#+sbcl \"x\"))")))
      ;; Column 15 is where #+ appears on the second line: "  :depends-on (#+sbcl"
      ;;                                                    0123456789012345
      (ok (= 15 (violation:violation-column (first violations))))))

  (testing "#+sbcl before :perform and #-ccl after: only the first is flagged"
    (let ((violations (check "(defsystem \"foo\"
  :depends-on (#+sbcl \"x\")
  :perform (test-op (o c) #-ccl (foo)))")))
      (ok (= 1 (length violations)))
      (ok (= 2 (violation:violation-line (first violations))))))

  (testing "#+ is flagged but #+sbcl inside string in same form is not"
    (let ((violations (check (format nil "(defsystem \"foo\"~%  :depends-on (#+sbcl \"#+sbcl-dep\"))"))))
      ;; The first #+sbcl is in source, the second is inside a string — only 1 violation
      (ok (= 1 (length violations)))))

  (testing "#+sbcl in second defsystem is flagged"
    (let ((violations (check "(defsystem \"foo\")
(defsystem \"bar\"
  :depends-on (#+sbcl \"x\"))")))
      (ok (= 1 (length violations)))
      (ok (= 3 (violation:violation-line (first violations))))))

  (testing "#+sbcl before :around-compile and #-ccl inside: only the first is flagged"
    (let ((violations (check "(defsystem \"foo\"
  :depends-on (#+sbcl \"x\")
  :around-compile (lambda (next) #-ccl (foo) (funcall next)))")))
      (ok (= 1 (length violations)))
      (ok (= 2 (violation:violation-line (first violations)))))))
