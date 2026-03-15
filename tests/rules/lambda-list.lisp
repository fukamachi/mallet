;; Tests for lambda-list rules: :mixed-optional-and-key and :no-allow-other-keys.
;; The :no-allow-other-keys rule was formerly known as :allow-other-keys.
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

;;; Allow &allow-other-keys tests

(deftest allow-other-keys-valid
  (testing "Valid lambda lists without &allow-other-keys"
    (let ((rule (make-instance 'rules:allow-other-keys-rule))
          (file (uiop:parse-native-namestring "test.lisp")))

      (testing "Only &key without allow-other-keys is valid"
        (let* ((text "(defun foo (a &key b) (list a b))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (null violations))))

      (testing "Required params only is valid"
        (let* ((text "(defun foo (a b) (list a b))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (null violations))))

      (testing "Lambda with &rest is valid"
        (let* ((text "(lambda (a &rest args) args)")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (null violations))))

      (testing "Defmethod with &optional is valid"
        (let* ((text "(defmethod foo ((obj string) &optional bar) obj)")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (null violations)))))))

(deftest allow-other-keys-invalid
  (testing "Invalid lambda lists with &allow-other-keys"
    (let ((rule (make-instance 'rules:allow-other-keys-rule))
          (file (uiop:parse-native-namestring "test.lisp")))

      (testing "defun with &allow-other-keys violates"
        (let* ((text "(defun foo (a &key b &allow-other-keys) (list a b))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1))
          (ok (eq (violation:violation-rule (first violations)) :no-allow-other-keys))
          (ok (search "allow-other-keys" (violation:violation-message (first violations))))))

      (testing "lambda with &allow-other-keys violates"
        (let* ((text "(lambda (a &key b &allow-other-keys) (list a b))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1))
          (ok (eq (violation:violation-rule (first violations)) :no-allow-other-keys))))

      (testing "defmethod with &allow-other-keys violates"
        (let* ((text "(defmethod foo ((obj string) &key bar &allow-other-keys) obj)")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1))
          (ok (eq (violation:violation-rule (first violations)) :no-allow-other-keys))))

      (testing "defmacro with &allow-other-keys violates"
        (let* ((text "(defmacro with-opts (name &key opt &allow-other-keys) `(list ,name ,opt))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1))
          (ok (eq (violation:violation-rule (first violations)) :no-allow-other-keys))))

      (testing "flet with &allow-other-keys violates"
        (let* ((text "(flet ((helper (a &key b &allow-other-keys) (list a b))) (helper 1))")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1))
          (ok (eq (violation:violation-rule (first violations)) :no-allow-other-keys))))

      (testing "violation severity is :warning"
        (let* ((text "(defun foo (a &key b &allow-other-keys) b)")
               (forms (parser:parse-forms text file))
               (violations (rules:check-form rule (first forms) file)))
          (ok (= (length violations) 1))
          (ok (eq (violation:violation-severity (first violations)) :warning)))))))

;;; Registration tests

(deftest allow-other-keys-registration
  (testing ":no-allow-other-keys is NOT in default config"
    (let* ((cfg (mallet/config:get-built-in-config :default))
           (rule-names (mapcar #'rules:rule-name (mallet/config:config-rules cfg))))
      (ok (not (member :no-allow-other-keys rule-names)))))

  (testing ":no-allow-other-keys is in :all config"
    (let* ((cfg (mallet/config:get-built-in-config :all))
           (rule-names (mapcar #'rules:rule-name (mallet/config:config-rules cfg))))
      (ok (member :no-allow-other-keys rule-names))))

  (testing ":no-allow-other-keys rule has :practice category"
    (let ((rule (rules:make-rule :no-allow-other-keys)))
      (ok (eq :practice (rules:rule-category rule))))))
