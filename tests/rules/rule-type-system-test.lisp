(defpackage #:mallet/tests/rules/rule-type-system
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:base #:mallet/rules/base)
   (#:rules #:mallet/rules)
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

  (testing "Rule accepts :practice category"
    (let ((rule (make-instance 'base:rule
                               :name :test-rule
                               :description "test"
                               :severity :warning
                               :category :practice)))
      (ok (eq :practice (base:rule-category rule)))))

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

;;; Tests for violation-category accessor

(deftest violation-category-slot
  (testing "Violation defaults to nil category"
    (let ((v (make-instance 'violation:violation
                            :rule :test-rule
                            :file #p"test.lisp"
                            :line 1
                            :column 0
                            :severity :warning
                            :message "test")))
      (ok (null (violation:violation-category v)))))

  (testing "Violation accepts :category initarg"
    (let ((v (make-instance 'violation:violation
                            :rule :test-rule
                            :file #p"test.lisp"
                            :line 1
                            :column 0
                            :severity :warning
                            :category :cleanliness
                            :message "test")))
      (ok (eq :cleanliness (violation:violation-category v)))))

  (testing "violation-category is settable (accessor)"
    (let ((v (make-instance 'violation:violation
                            :rule :test-rule
                            :file #p"test.lisp"
                            :line 1
                            :column 0
                            :severity :info
                            :message "test")))
      (setf (violation:violation-category v) :style)
      (ok (eq :style (violation:violation-category v)))))

  (testing "violation-category is exported from mallet package"
    (ok (find-symbol "VIOLATION-CATEGORY" "MALLET"))))

;;; Tests for actual rule categories

(deftest actual-rule-categories
  (testing ":unused-variables rule has :cleanliness category"
    (let ((rule (rules:make-rule :unused-variables)))
      (ok (eq :cleanliness (base:rule-category rule)))))

  (testing ":wrong-otherwise rule has :correctness category"
    (let ((rule (rules:make-rule :wrong-otherwise)))
      (ok (eq :correctness (base:rule-category rule)))))

  (testing ":no-eval rule has :suspicious category"
    (let ((rule (rules:make-rule :no-eval)))
      (ok (eq :suspicious (base:rule-category rule)))))

  (testing ":trailing-whitespace rule has :format category"
    (let ((rule (rules:make-rule :trailing-whitespace)))
      (ok (eq :format (base:rule-category rule)))))

  (testing ":missing-else rule has :style category"
    (let ((rule (rules:make-rule :missing-else)))
      (ok (eq :style (base:rule-category rule)))))

  (testing ":no-package-use rule has :practice category"
    (let ((rule (rules:make-rule :no-package-use)))
      (ok (eq :practice (base:rule-category rule)))))

  (testing ":no-allow-other-keys rule has :practice category"
    (let ((rule (rules:make-rule :no-allow-other-keys)))
      (ok (eq :practice (base:rule-category rule)))))

  (testing ":cyclomatic-complexity rule has :metrics category"
    (let ((rule (rules:make-rule :cyclomatic-complexity)))
      (ok (eq :metrics (base:rule-category rule)))))

  (testing ":double-colon-access rule has :practice category"
    (let ((rule (rules:make-rule :double-colon-access)))
      (ok (eq :practice (base:rule-category rule)))))

  (testing ":error-without-custom-condition rule has :practice category"
    (let ((rule (rules:make-rule :error-without-custom-condition)))
      (ok (eq :practice (base:rule-category rule))))))
