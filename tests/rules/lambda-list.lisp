(defpackage #:mallet/tests/rules/lambda-list
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:mallet/rules)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/lambda-list)

;;; Mixed &optional and &key tests

(deftest mixed-optional-and-key-valid
  (testing "Valid lambda lists without mixing"
    (let ((rule (make-instance 'rules:mixed-optional-and-key-rule))
          (file (uiop:parse-native-namestring "test.lisp")))

      (testing "Only &optional is valid"
        (let* ((text "(defun foo (a &optional b) (list a b))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (null violations))))

      (testing "Only &key is valid"
        (let* ((text "(defun foo (a &key b) (list a b))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (null violations))))

      (testing "Required params only is valid"
        (let* ((text "(defun foo (a b) (list a b))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (null violations))))

      (testing "Lambda with only &key is valid"
        (let* ((text "(lambda (a &key b) (list a b))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (null violations))))

      (testing "Defmethod with only &key is valid"
        (let* ((text "(defmethod foo ((obj string) &key bar) (list obj bar))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (null violations)))))))

(deftest mixed-optional-and-key-invalid
  (testing "Invalid lambda lists with mixed &optional and &key"
    (let ((rule (make-instance 'rules:mixed-optional-and-key-rule))
          (file (uiop:parse-native-namestring "test.lisp")))

      (testing "defun with mixed &optional and &key violates"
        (let* ((text "(defun foo (a &optional b &key c) (list a b c))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1))
          (ok (eq (violation:violation-rule (first violations)) :mixed-optional-and-key))
          (ok (search "mixes &optional and &key"
                      (violation:violation-message (first violations))))))

      (testing "lambda with mixed &optional and &key violates"
        (let* ((text "(lambda (a &optional b &key c) (list a b c))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1))
          (ok (eq (violation:violation-rule (first violations)) :mixed-optional-and-key))))

      (testing "defmethod with mixed &optional and &key violates"
        (let* ((text "(defmethod foo ((obj string) &optional b &key c) (list obj b c))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1))
          (ok (eq (violation:violation-rule (first violations)) :mixed-optional-and-key))))

      (testing "defmacro with mixed &optional and &key violates"
        (let* ((text "(defmacro foo (a &optional b &key c) `(list ,a ,b ,c))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1))
          (ok (eq (violation:violation-rule (first violations)) :mixed-optional-and-key))))

      (testing "flet with mixed &optional and &key violates"
        (let* ((text "(flet ((helper (a &optional b &key c) (list a b c))) (helper 1))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1))
          (ok (eq (violation:violation-rule (first violations)) :mixed-optional-and-key)))))))
