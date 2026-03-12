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

(deftest suppression-state-region-nesting
  (testing "Region-based disable/enable with nesting depth"
    (let ((state (suppression:make-suppression-state)))
      ;; Two nested disables for the same rule
      (suppression:set-region-disabled state '(:line-length))
      (suppression:set-region-disabled state '(:line-length))
      (ok (suppression:rule-suppressed-p state :line-length))

      ;; First enable does not re-enable — depth goes from 2 to 1
      (suppression:enable-region-rules state '(:line-length))
      (ok (suppression:rule-suppressed-p state :line-length))

      ;; Second enable brings depth to 0 — rule is re-enabled
      (suppression:enable-region-rules state '(:line-length))
      (ng (suppression:rule-suppressed-p state :line-length)))))

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

;;; Suppression Registration and Stale Detection Tests

(deftest register-suppression-basic
  (testing "register-suppression returns unique integer IDs"
    (let ((state (suppression:make-suppression-state)))
      (let ((id1 (suppression:register-suppression state 10 '(:line-length) nil :inline-comment))
            (id2 (suppression:register-suppression state 20 '(:if-without-else) "reason" :inline-comment)))
        (ok (integerp id1))
        (ok (integerp id2))
        (ng (= id1 id2))))))

(deftest register-suppression-stores-data
  (testing "each registered suppression appears in collect-stale-suppressions"
    (let ((state (suppression:make-suppression-state)))
      (ok (null (suppression:collect-stale-suppressions state)))
      (suppression:register-suppression state 5 '(:line-length) "too long" :inline-comment)
      (ok (= 1 (length (suppression:collect-stale-suppressions state))))
      (suppression:register-suppression state 10 '(:if-without-else) nil :inline-comment)
      (ok (= 2 (length (suppression:collect-stale-suppressions state)))))))

(deftest mark-suppression-used-basic
  (testing "mark-suppression-used marks an ID as used"
    (let ((state (suppression:make-suppression-state)))
      (let ((id (suppression:register-suppression state 10 '(:line-length) nil :inline-comment)))
        ;; Before marking: stale
        (ok (= 1 (length (suppression:collect-stale-suppressions state))))
        ;; After marking: not stale
        (suppression:mark-suppression-used state id)
        (ok (null (suppression:collect-stale-suppressions state)))))))

(deftest collect-stale-suppressions-all-stale
  (testing "collect-stale-suppressions returns all unused entries"
    (let ((state (suppression:make-suppression-state)))
      (suppression:register-suppression state 5 '(:line-length) nil :inline-comment)
      (suppression:register-suppression state 15 '(:if-without-else) nil :inline-comment)
      (let ((stale (suppression:collect-stale-suppressions state)))
        (ok (= 2 (length stale)))))))

(deftest collect-stale-suppressions-partial
  (testing "collect-stale-suppressions returns only unused entries"
    (let ((state (suppression:make-suppression-state)))
      (let ((id1 (suppression:register-suppression state 5 '(:line-length) nil :inline-comment))
            (id2 (suppression:register-suppression state 15 '(:if-without-else) nil :inline-comment)))
        ;; Mark only id1 as used
        (suppression:mark-suppression-used state id1)
        (let ((stale (suppression:collect-stale-suppressions state)))
          (ok (= 1 (length stale)))
          ;; The stale entry should be the one for id2
          (ok (= id2 (car (first stale)))))))))

(deftest collect-stale-suppressions-entry-structure
  (testing "stale entry is a cons (id . plist) with :line :rules :reason :type"
    (let ((state (suppression:make-suppression-state)))
      (let ((id (suppression:register-suppression state 42 '(:line-length) "my reason" :inline-comment)))
        (let ((stale (suppression:collect-stale-suppressions state)))
          (ok (= 1 (length stale)))
          (let ((entry (first stale)))
            (ok (= id (car entry)))
            (ok (= 42 (getf (cdr entry) :line)))
            (ok (equal '(:line-length) (getf (cdr entry) :rules)))
            (ok (equal "my reason" (getf (cdr entry) :reason)))
            (ok (eq :inline-comment (getf (cdr entry) :type)))))))))

(deftest rule-suppressed-p-returns-id-for-registered
  (testing "rule-suppressed-p returns the suppression ID for registered suppressions"
    (let ((state (suppression:make-suppression-state)))
      (let ((id (suppression:register-suppression state 10 '(:line-length) nil :inline-comment)))
        (let ((result (suppression:rule-suppressed-p state :line-length)))
          (ok (integerp result))
          (ok (= id result)))))))

(deftest rule-suppressed-p-marks-registered-as-used
  (testing "rule-suppressed-p marks matched registered suppression as used"
    (let ((state (suppression:make-suppression-state)))
      (suppression:register-suppression state 10 '(:line-length) nil :inline-comment)
      ;; Before checking: stale
      (ok (= 1 (length (suppression:collect-stale-suppressions state))))
      ;; Check suppression (should match and mark as used)
      (suppression:rule-suppressed-p state :line-length)
      ;; After checking: no longer stale
      (ok (null (suppression:collect-stale-suppressions state))))))

(deftest rule-suppressed-p-no-match-in-registered
  (testing "rule-suppressed-p returns NIL when no registered suppression matches"
    (let ((state (suppression:make-suppression-state)))
      (suppression:register-suppression state 10 '(:line-length) nil :inline-comment)
      ;; Different rule — no match
      (ng (suppression:rule-suppressed-p state :if-without-else)))))

(deftest rule-suppressed-p-registered-all-keyword
  (testing "rule-suppressed-p matches :all keyword in registered suppressions"
    (let ((state (suppression:make-suppression-state)))
      (let ((id (suppression:register-suppression state 10 '(:all) nil :inline-comment)))
        (let ((result (suppression:rule-suppressed-p state :any-random-rule)))
          (ok (integerp result))
          (ok (= id result))
          ;; Side effect: matched suppression should now be marked as used
          (ok (null (suppression:collect-stale-suppressions state))))))))

(deftest rule-suppressed-p-first-match-wins
  (testing "when multiple registered suppressions match same rule, most recently registered is consumed"
    (let ((state (suppression:make-suppression-state)))
      (let ((id1 (suppression:register-suppression state 5 '(:line-length) nil :inline-comment))
            (id2 (suppression:register-suppression state 10 '(:line-length) nil :inline-comment)))
        ;; Both match :line-length; call rule-suppressed-p once
        ;; id2 was registered last, so it is stored first (newest-first) and consumed first
        (let ((result (suppression:rule-suppressed-p state :line-length)))
          (ok (= id2 result))
          ;; Only id2 was consumed; id1 remains stale
          (ok (= 1 (length (suppression:collect-stale-suppressions state))))
          (ok (= id1 (car (first (suppression:collect-stale-suppressions state))))))))))

(deftest rule-suppressed-p-existing-region-still-truthy
  (testing "rule-suppressed-p returns truthy for region suppressions (backward compat)"
    (let ((state (suppression:make-suppression-state)))
      (suppression:set-region-disabled state '(:line-length))
      (ok (suppression:rule-suppressed-p state :line-length))
      (ng (suppression:rule-suppressed-p state :if-without-else)))))

(deftest feature-flag-reading
  (testing "#+mallet feature flag works with *features*"
    ;; Create test file with #+mallet declarations
    (let ((file (merge-pathnames "test-mallet-feature.lisp" (uiop:temporary-directory)))
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
