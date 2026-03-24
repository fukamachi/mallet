(defpackage #:mallet/tests/utils
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:utils #:mallet/utils)))
(in-package #:mallet/tests/utils)

;;; resolve-rule-alias tests

(deftest resolve-rule-alias-known-aliases
  (testing "eval-usage resolves to no-eval"
    (let ((result (handler-bind ((warning #'muffle-warning))
                    (utils:resolve-rule-alias :eval-usage))))
      (ok (eq :no-eval result))))

  (testing "ignore-errors-usage resolves to no-ignore-errors"
    (let ((result (handler-bind ((warning #'muffle-warning))
                    (utils:resolve-rule-alias :ignore-errors-usage))))
      (ok (eq :no-ignore-errors result))))

  (testing "allow-other-keys resolves to no-allow-other-keys"
    (let ((result (handler-bind ((warning #'muffle-warning))
                    (utils:resolve-rule-alias :allow-other-keys))))
      (ok (eq :no-allow-other-keys result))))

  (testing "if-without-else resolves to missing-else"
    (let ((result (handler-bind ((warning #'muffle-warning))
                    (utils:resolve-rule-alias :if-without-else))))
      (ok (eq :missing-else result))))

  (testing "final-newline resolves to missing-final-newline"
    (let ((result (handler-bind ((warning #'muffle-warning))
                    (utils:resolve-rule-alias :final-newline))))
      (ok (eq :missing-final-newline result))))

  (testing "interned-package-symbol resolves to defpackage-interned-symbol"
    (let ((result (handler-bind ((warning #'muffle-warning))
                    (utils:resolve-rule-alias :interned-package-symbol))))
      (ok (eq :defpackage-interned-symbol result))))

  (testing "error-with-string-only resolves to error-without-custom-condition"
    (let ((result (handler-bind ((warning #'muffle-warning))
                    (utils:resolve-rule-alias :error-with-string-only))))
      (ok (eq :error-without-custom-condition result)))))

(deftest resolve-rule-alias-unknown-names
  (testing "unknown name is returned unchanged"
    (ok (eq :no-eval (utils:resolve-rule-alias :no-eval))))

  (testing "another unknown name is returned unchanged"
    (ok (eq :unused-variables (utils:resolve-rule-alias :unused-variables))))

  (testing "canonical new names are returned unchanged"
    (ok (eq :missing-else (utils:resolve-rule-alias :missing-else)))
    (ok (eq :missing-final-newline (utils:resolve-rule-alias :missing-final-newline)))
    (ok (eq :defpackage-interned-symbol (utils:resolve-rule-alias :defpackage-interned-symbol)))))

(deftest resolve-rule-alias-deprecation-warning
  (testing "using an old name signals a deprecation warning"
    (let ((warned nil))
      (handler-bind ((warning (lambda (w)
                                (setf warned (format nil "~A" w))
                                (muffle-warning w))))
        (utils:resolve-rule-alias :eval-usage))
      (ok (not (null warned)))
      (ok (search "deprecated" warned))))

  (testing "deprecation warning names the old and new rule"
    (let ((warning-text nil))
      (handler-bind ((warning (lambda (w)
                                (setf warning-text (format nil "~A" w))
                                (muffle-warning w))))
        (utils:resolve-rule-alias :if-without-else))
      (ok (search "IF-WITHOUT-ELSE" warning-text))
      (ok (search "MISSING-ELSE" warning-text))))

  (testing "no warning is signalled for canonical names"
    (let ((warned nil))
      (handler-bind ((warning (lambda (w)
                                (setf warned t)
                                (muffle-warning w))))
        (utils:resolve-rule-alias :no-eval))
      (ok (null warned)))))
