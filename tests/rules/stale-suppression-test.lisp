(defpackage #:mallet/tests/rules/stale-suppression
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:base #:mallet/rules/base)
   (#:rules #:mallet/rules)
   (#:ss #:mallet/rules/stale-suppression)
   (#:parser #:mallet/parser)
   (#:config #:mallet/config)))
(in-package #:mallet/tests/rules/stale-suppression)

;;; Rule metadata tests

(deftest stale-suppression-rule-metadata
  (testing "make-rule :stale-suppression returns a stale-suppression-rule"
    (let ((rule (rules:make-rule :stale-suppression)))
      (ok (typep rule 'ss:stale-suppression-rule))))

  (testing "Rule name is :stale-suppression"
    (let ((rule (rules:make-rule :stale-suppression)))
      (ok (eq :stale-suppression (base:rule-name rule)))))

  (testing "Rule category is :cleanliness"
    (let ((rule (rules:make-rule :stale-suppression)))
      (ok (eq :cleanliness (base:rule-category rule)))))

  (testing "Rule severity is :warning"
    (let ((rule (rules:make-rule :stale-suppression)))
      (ok (eq :warning (base:rule-severity rule)))))

  (testing "Rule type is :form"
    (let ((rule (rules:make-rule :stale-suppression)))
      (ok (eq :form (base:rule-type rule))))))

;;; check-form is a no-op (engine creates violations directly)

(deftest stale-suppression-check-form-noop
  (testing "check-form returns empty list (engine creates violations directly)"
    (let* ((rule (rules:make-rule :stale-suppression))
           (forms (parser:parse-forms "(defun foo () nil)" #p"test.lisp"))
           (violations (mapcan (lambda (form)
                                 (base:check-form rule form #p"test.lisp"))
                               forms)))
      (ok (null violations)))))

;;; Violation message format

(deftest stale-suppression-message-format
  (testing "Message without reason"
    (let ((msg (ss:make-stale-suppression-message :unused-variables)))
      (ok (string= "suppression for :UNUSED-VARIABLES has no effect (no matching violation found)"
                   msg))))

  (testing "Message with reason"
    (let ((msg (ss:make-stale-suppression-message :missing-else "not needed here")))
      (ok (string= "suppression for :MISSING-ELSE has no effect (no matching violation found) (reason: \"not needed here\")"
                   msg)))))

;;; Config integration tests

(deftest stale-suppression-in-default-config
  (testing "default config includes :stale-suppression"
    (let* ((cfg (config:get-built-in-config :default))
           (rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
      (ok (member :stale-suppression rule-names)))))

(deftest stale-suppression-in-all-config
  (testing "all config includes :stale-suppression"
    (let* ((cfg (config:get-built-in-config :all))
           (rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
      (ok (member :stale-suppression rule-names)))))
