(defpackage #:mallet/tests/rules/rule-type-system
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:base #:mallet/rules/base)
   (#:rules #:mallet/rules)))
(in-package #:mallet/tests/rules/rule-type-system)

;;; Tests for exported category access and real rule category metadata.

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

(deftest violation-category-exported-and-populated
  (testing "violation-category is exported from the mallet package"
    (multiple-value-bind (symbol status)
        (find-symbol "VIOLATION-CATEGORY" "MALLET")
      (ok (eq :external status))
      (ok (eq symbol 'mallet:violation-category))))

  (testing "violation-category is populated from the real rule category"
    (uiop:with-temporary-file (:pathname path
                               :type "lisp"
                               :prefix "mallet-rule-type-system"
                               :keep t)
      (unwind-protect
           (progn
             (with-open-file (out path
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
               (write-string "(defun test () (let ((x 1)) nil))
" out))
             (let* ((config (mallet/config:make-config
                             :rules (list (rules:make-rule :unused-variables))))
                    (violations (mallet/engine:lint-file path :config config)))
               (ok (= 1 (length violations)))
               (ok (eq :cleanliness
                       (mallet:violation-category (first violations))))))
        (when (probe-file path)
          (delete-file path))))))

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
