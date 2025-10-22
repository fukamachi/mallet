(defpackage #:mallet/tests/suppression
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:suppression #:mallet/suppression)))
(in-package #:mallet/tests/suppression)

(deftest suppression-state-creation
  (testing "Create a new suppression state"
    (let ((state (suppression:make-suppression-state)))
      (ok state)
      (ok (null (suppression:region-disabled-rules state)))
      (ok (null (suppression:scope-stack state)))
      (ok (hash-table-p (suppression:function-suppressions state))))))

(deftest suppression-state-region-disable-enable
  (testing "Region-based disable/enable operations"
    (let ((state (suppression:make-suppression-state)))
      ;; Initially no rules suppressed
      (ng (suppression:rule-suppressed-p state :line-length))

      ;; Disable some rules
      (suppression:set-region-disabled state '(:line-length :if-without-else))
      (ok (suppression:rule-suppressed-p state :line-length))
      (ok (suppression:rule-suppressed-p state :if-without-else))
      (ng (suppression:rule-suppressed-p state :unused-variables))

      ;; Enable one rule
      (suppression:enable-region-rules state '(:line-length))
      (ng (suppression:rule-suppressed-p state :line-length))
      (ok (suppression:rule-suppressed-p state :if-without-else))

      ;; Enable all
      (suppression:enable-region-rules state '(:if-without-else))
      (ng (suppression:rule-suppressed-p state :if-without-else)))))

(deftest suppression-state-region-all
  (testing "Region-based :all suppression"
    (let ((state (suppression:make-suppression-state)))
      (suppression:set-region-disabled state '(:all))
      ;; All rules should be suppressed
      (ok (suppression:rule-suppressed-p state :line-length))
      (ok (suppression:rule-suppressed-p state :any-rule))
      (ok (suppression:rule-suppressed-p state :everything)))))

(deftest suppression-state-scope-stack
  (testing "Scope-based suppressions with stack"
    (let ((state (suppression:make-suppression-state)))
      ;; Initially no suppression
      (ng (suppression:rule-suppressed-p state :line-length))

      ;; Push first scope
      (suppression:push-scope-suppression state '(:line-length))
      (ok (suppression:rule-suppressed-p state :line-length))
      (ng (suppression:rule-suppressed-p state :unused-variables))

      ;; Push nested scope
      (suppression:push-scope-suppression state '(:unused-variables))
      (ok (suppression:rule-suppressed-p state :line-length))      ; Parent scope
      (ok (suppression:rule-suppressed-p state :unused-variables)) ; Current scope

      ;; Pop nested scope
      (suppression:pop-scope-suppression state)
      (ok (suppression:rule-suppressed-p state :line-length))
      (ng (suppression:rule-suppressed-p state :unused-variables))

      ;; Pop parent scope
      (suppression:pop-scope-suppression state)
      (ng (suppression:rule-suppressed-p state :line-length)))))

(deftest suppression-state-scope-all
  (testing "Scope-based :all suppression"
    (let ((state (suppression:make-suppression-state)))
      (suppression:push-scope-suppression state '(:all))
      ;; All rules should be suppressed in this scope
      (ok (suppression:rule-suppressed-p state :line-length))
      (ok (suppression:rule-suppressed-p state :any-rule)))))

(deftest suppression-state-function-specific
  (testing "Function-specific suppressions"
    (let ((state (suppression:make-suppression-state)))
      ;; Add suppression for specific function
      (suppression:add-function-suppression state 'factorial '(:line-length))

      ;; Should be suppressed for that function
      (ok (suppression:rule-suppressed-p state :line-length
                                         :form-type :function-body
                                         :function-name 'factorial))

      ;; Should NOT be suppressed for other functions
      (ng (suppression:rule-suppressed-p state :line-length
                                         :form-type :function-body
                                         :function-name 'other-function))

      ;; Should NOT be suppressed at top-level
      (ng (suppression:rule-suppressed-p state :line-length
                                         :form-type :top-level)))))

(deftest suppression-state-function-multiple-rules
  (testing "Multiple rules for same function"
    (let ((state (suppression:make-suppression-state)))
      ;; Add first rule
      (suppression:add-function-suppression state 'factorial '(:line-length))
      ;; Add second rule for same function
      (suppression:add-function-suppression state 'factorial '(:if-without-else))

      ;; Both rules should be suppressed
      (ok (suppression:rule-suppressed-p state :line-length
                                         :form-type :function-body
                                         :function-name 'factorial))
      (ok (suppression:rule-suppressed-p state :if-without-else
                                         :form-type :function-body
                                         :function-name 'factorial)))))

(deftest suppression-state-function-all
  (testing "Function-specific :all suppression"
    (let ((state (suppression:make-suppression-state)))
      (suppression:add-function-suppression state 'legacy-code '(:all))

      ;; All rules should be suppressed for that function
      (ok (suppression:rule-suppressed-p state :line-length
                                         :form-type :function-body
                                         :function-name 'legacy-code))
      (ok (suppression:rule-suppressed-p state :any-rule
                                         :form-type :function-body
                                         :function-name 'legacy-code)))))

(deftest suppression-state-combined
  (testing "Combined suppressions (region + scope + function)"
    (let ((state (suppression:make-suppression-state)))
      ;; Region disables line-length
      (suppression:set-region-disabled state '(:line-length))

      ;; Scope adds unused-variables
      (suppression:push-scope-suppression state '(:unused-variables))

      ;; Function adds if-without-else for specific function
      (suppression:add-function-suppression state 'foo '(:if-without-else))

      ;; Check combinations
      ;; 1. In function foo: all three rules suppressed
      (ok (suppression:rule-suppressed-p state :line-length
                                         :form-type :function-body
                                         :function-name 'foo))
      (ok (suppression:rule-suppressed-p state :unused-variables
                                         :form-type :function-body
                                         :function-name 'foo))
      (ok (suppression:rule-suppressed-p state :if-without-else
                                         :form-type :function-body
                                         :function-name 'foo))

      ;; 2. In other function: only region and scope
      (ok (suppression:rule-suppressed-p state :line-length
                                         :form-type :function-body
                                         :function-name 'bar))
      (ok (suppression:rule-suppressed-p state :unused-variables
                                         :form-type :function-body
                                         :function-name 'bar))
      (ng (suppression:rule-suppressed-p state :if-without-else
                                         :form-type :function-body
                                         :function-name 'bar))

      ;; 3. Pop scope - only region remains
      (suppression:pop-scope-suppression state)
      (ok (suppression:rule-suppressed-p state :line-length
                                         :form-type :function-body
                                         :function-name 'bar))
      (ng (suppression:rule-suppressed-p state :unused-variables
                                         :form-type :function-body
                                         :function-name 'bar)))))

(deftest suppression-next-form
  (testing "Next-form suppression"
    (let ((state (suppression:make-suppression-state)))
      ;; Set next-form suppression
      (suppression:set-next-form-suppression state '(:line-length :if-without-else))

      ;; Should be in pending state (not active yet)
      (ok (equal '(:line-length :if-without-else)
                 (suppression:next-form-suppressions state)))

      ;; Consume it (simulates processing next form)
      (let ((rules (suppression:consume-next-form-suppression state)))
        (ok (equal '(:line-length :if-without-else) rules))
        ;; Should be cleared after consumption
        (ok (null (suppression:next-form-suppressions state)))))))

(deftest feature-flag-reading
  (testing "#+mallet feature flag works with *features*"
    ;; Create test file with #+mallet declarations
    (let ((file "/tmp/test-mallet-feature.lisp")
          (content "#+mallet
(declaim (mallet:suppress-next line-length))

(defun foo () \"test\")"))
      ;; Write test file
      (with-open-file (out file :direction :output :if-exists :supersede)
        (write-string content out))

      ;; Test 1: Without :mallet in *features*
      (with-open-file (in file)
        (let ((forms (loop for form = (read in nil :eof)
                           until (eq form :eof)
                           collect form)))
          ;; Should only see the defun, not the declaim
          (ok (= (length forms) 1))
          (ok (eq (first (first forms)) 'defun))))

      ;; Test 2: With :mallet in *features* and stub package
      (suppression:ensure-mallet-package-exists)
      (let ((*features* (cons :mallet *features*)))
        (with-open-file (in file)
          (let ((forms (loop for form = (read in nil :eof)
                             until (eq form :eof)
                             collect form)))
            ;; Should see both declaim and defun
            (ok (= (length forms) 2))
            (ok (eq (first (first forms)) 'declaim))
            (ok (eq (first (second forms)) 'defun))))))))
