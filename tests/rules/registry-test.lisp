(defpackage #:malvolio/tests/rules/registry
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:rules #:malvolio/rules)))
(in-package #:malvolio/tests/rules/registry)

(deftest register-and-find-rules
  (testing "Register a rule"
    (let ((registry (rules:make-registry)))
      (rules:register-rule registry :test-rule
                          :description "Test rule"
                          :severity :warning
                          :enabled t)
      (ok (rules:find-rule registry :test-rule))))

  (testing "Find non-existent rule"
    (let ((registry (rules:make-registry)))
      (ok (null (rules:find-rule registry :non-existent)))))

  (testing "List all rules"
    (let ((registry (rules:make-registry)))
      (rules:register-rule registry :rule1
                          :description "Rule 1"
                          :severity :error)
      (rules:register-rule registry :rule2
                          :description "Rule 2"
                          :severity :warning)
      (let ((all-rules (rules:list-rules registry)))
        (ok (= 2 (length all-rules)))
        (ok (member :rule1 all-rules :key #'rules:rule-name))
        (ok (member :rule2 all-rules :key #'rules:rule-name))))))

(deftest rule-properties
  (testing "Rule has correct properties"
    (let ((registry (rules:make-registry)))
      (rules:register-rule registry :test-rule
                          :description "A test rule"
                          :severity :error
                          :enabled nil)
      (let ((rule (rules:find-rule registry :test-rule)))
        (ok (eq :test-rule (rules:rule-name rule)))
        (ok (string= "A test rule" (rules:rule-description rule)))
        (ok (eq :error (rules:rule-severity rule)))
        (ok (not (rules:rule-enabled-p rule)))))))

(deftest enable-disable-rules
  (testing "Enable and disable a rule"
    (let ((registry (rules:make-registry)))
      (rules:register-rule registry :test-rule
                          :description "Test"
                          :severity :warning
                          :enabled t)
      (let ((rule (rules:find-rule registry :test-rule)))
        (ok (rules:rule-enabled-p rule))
        (rules:disable-rule rule)
        (ok (not (rules:rule-enabled-p rule)))
        (rules:enable-rule rule)
        (ok (rules:rule-enabled-p rule))))))

(deftest rule-types
  (testing "Text rule type"
    (let ((registry (rules:make-registry)))
      (rules:register-rule registry :line-length
                          :type :text
                          :description "Line length check"
                          :severity :warning)
      (let ((rule (rules:find-rule registry :line-length)))
        (ok (eq :text (rules:rule-type rule))))))

  (testing "Form rule type"
    (let ((registry (rules:make-registry)))
      (rules:register-rule registry :if-without-else
                          :type :form
                          :description "If without else"
                          :severity :warning)
      (let ((rule (rules:find-rule registry :if-without-else)))
        (ok (eq :form (rules:rule-type rule)))))))
