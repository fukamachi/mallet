(defpackage #:mallet/tests/rules/rule-type-system
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:base #:mallet/rules/base)
   (#:violation #:mallet/violation)))
(in-package #:mallet/tests/rules/rule-type-system)

;;; Tests for the updated type system: 3-value severity enum and category slot

(deftest rule-severity-type
  (testing "Rule accepts :error severity"
    (let ((rule (make-instance 'base:rule
                               :name :test-rule
                               :description "test"
                               :severity :error
                               :category :correctness)))
      (ok (eq :error (base:rule-severity rule)))))

  (testing "Rule accepts :warning severity"
    (let ((rule (make-instance 'base:rule
                               :name :test-rule
                               :description "test"
                               :severity :warning
                               :category :correctness)))
      (ok (eq :warning (base:rule-severity rule)))))

  (testing "Rule accepts :info severity"
    (let ((rule (make-instance 'base:rule
                               :name :test-rule
                               :description "test"
                               :severity :info
                               :category :correctness)))
      (ok (eq :info (base:rule-severity rule))))))

(deftest rule-category-slot
  (testing "Rule has category slot with reader rule-category"
    (let ((rule (make-instance 'base:rule
                               :name :test-rule
                               :description "test"
                               :severity :warning
                               :category :correctness)))
      (ok (eq :correctness (base:rule-category rule)))))

  (testing "Rule accepts :suspicious category"
    (let ((rule (make-instance 'base:rule
                               :name :test-rule
                               :description "test"
                               :severity :warning
                               :category :suspicious)))
      (ok (eq :suspicious (base:rule-category rule)))))

  (testing "Rule accepts :cleanliness category"
    (let ((rule (make-instance 'base:rule
                               :name :test-rule
                               :description "test"
                               :severity :info
                               :category :cleanliness)))
      (ok (eq :cleanliness (base:rule-category rule)))))

  (testing "Rule accepts :style category"
    (let ((rule (make-instance 'base:rule
                               :name :test-rule
                               :description "test"
                               :severity :info
                               :category :style)))
      (ok (eq :style (base:rule-category rule)))))

  (testing "Rule accepts :format category"
    (let ((rule (make-instance 'base:rule
                               :name :test-rule
                               :description "test"
                               :severity :info
                               :category :format)))
      (ok (eq :format (base:rule-category rule)))))

  (testing "Rule accepts :metrics category"
    (let ((rule (make-instance 'base:rule
                               :name :test-rule
                               :description "test"
                               :severity :info
                               :category :metrics)))
      (ok (eq :metrics (base:rule-category rule))))))

(deftest rule-category-exported-from-mallet
  (testing "rule-category is exported from the mallet package"
    (ok (find-symbol "RULE-CATEGORY" "MALLET")))

  (testing "rule-category reader works via mallet package"
    (let ((rule (make-instance 'base:rule
                               :name :test-rule
                               :description "test"
                               :severity :warning
                               :category :correctness)))
      (ok (eq :correctness (mallet:rule-category rule))))))

(deftest violation-severity-type
  (testing "Violation accepts :error severity"
    (let ((v (make-instance 'violation:violation
                            :rule :test-rule
                            :file #p"test.lisp"
                            :line 1
                            :column 0
                            :severity :error
                            :message "test")))
      (ok (eq :error (violation:violation-severity v)))))

  (testing "Violation accepts :warning severity"
    (let ((v (make-instance 'violation:violation
                            :rule :test-rule
                            :file #p"test.lisp"
                            :line 1
                            :column 0
                            :severity :warning
                            :message "test")))
      (ok (eq :warning (violation:violation-severity v)))))

  (testing "Violation accepts :info severity"
    (let ((v (make-instance 'violation:violation
                            :rule :test-rule
                            :file #p"test.lisp"
                            :line 1
                            :column 0
                            :severity :info
                            :message "test")))
      (ok (eq :info (violation:violation-severity v))))))
