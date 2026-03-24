(defpackage #:mallet/tests/errors
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:errors #:mallet/errors)))
(in-package #:mallet/tests/errors)

(deftest error-condition-hierarchy
  (testing "CLI errors are of type cli-error"
    (ok (typep (make-condition 'errors:unknown-option :option "--foo")
               'errors:cli-error))
    (ok (typep (make-condition 'errors:missing-option-value :option "--foo")
               'errors:cli-error))
    (ok (typep (make-condition 'errors:invalid-option-value
                               :option "--foo"
                               :value "bar"
                               :expected "baz")
               'errors:cli-error))
    (ok (typep (make-condition 'errors:file-not-found :path "foo.lisp")
               'errors:cli-error))
    (ok (typep (make-condition 'errors:no-files-specified)
               'errors:cli-error)))

  (testing "CLI errors are of type mallet-error"
    (ok (typep (make-condition 'errors:unknown-option :option "--foo")
               'errors:mallet-error)))

  (testing "All CLI errors are of type error"
    (ok (typep (make-condition 'errors:unknown-option :option "--foo")
               'error))))

(deftest error-reporting
  (testing "unknown-option has helpful message"
    (let ((err (make-condition 'errors:unknown-option :option "--foo")))
      (ok (search "Unknown option: --foo" (format nil "~A" err)))
      (ok (search "mallet --help" (format nil "~A" err)))))

  (testing "missing-option-value has clear message"
    (let ((err (make-condition 'errors:missing-option-value :option "--format")))
      (ok (search "Missing value for option: --format" (format nil "~A" err)))))

  (testing "invalid-format has specific message"
    (let ((err (make-condition 'errors:invalid-format
                               :option "--format"
                               :value "xml"
                               :expected "text or json")))
      (ok (search "Invalid format: xml" (format nil "~A" err)))
      (ok (search "text or json" (format nil "~A" err)))))

  (testing "invalid-preset has specific message"
    (let ((err (make-condition 'errors:invalid-preset
                               :option "--preset"
                               :value "bad"
                               :expected "default, strict, all, or none")))
      (ok (search "bad" (format nil "~A" err)))
      (ok (search "default, strict, all, or none" (format nil "~A" err)))))

  (testing "file-not-found has clear message"
    (let ((err (make-condition 'errors:file-not-found :path "missing.lisp")))
      (ok (search "File not found: missing.lisp" (format nil "~A" err)))))

  (testing "no-files-specified has helpful message"
    (let ((err (make-condition 'errors:no-files-specified)))
      (ok (search "No files specified" (format nil "~A" err)))
      (ok (search "mallet --help" (format nil "~A" err))))))

(deftest error-accessors
  (testing "unknown-option has option accessor"
    (let ((err (make-condition 'errors:unknown-option :option "--test")))
      (ok (equal (errors:unknown-option-option err) "--test"))))

  (testing "missing-option-value has option accessor"
    (let ((err (make-condition 'errors:missing-option-value :option "--config")))
      (ok (equal (errors:missing-option-value-option err) "--config"))))

  (testing "invalid-option-value has accessors"
    (let ((err (make-condition 'errors:invalid-option-value
                               :option "--format"
                               :value "bad"
                               :expected "good")))
      (ok (equal (errors:invalid-option-value-option err) "--format"))
      (ok (equal (errors:invalid-option-value-value err) "bad"))
      (ok (equal (errors:invalid-option-value-expected err) "good"))))

  (testing "file-not-found has path accessor"
    (let ((err (make-condition 'errors:file-not-found :path "test.lisp")))
      (ok (equal (errors:file-not-found-path err) "test.lisp"))))

  (testing "config-not-found has path accessor"
    (let ((err (make-condition 'errors:config-not-found :path ".mallet.lisp")))
      (ok (equal (errors:config-not-found-path err) ".mallet.lisp"))))

  (testing "config-parse-failed has accessors"
    (let ((err (make-condition 'errors:config-parse-failed
                               :path ".mallet.lisp"
                               :cause "Invalid syntax")))
      (ok (equal (errors:config-parse-failed-path err) ".mallet.lisp"))
      (ok (equal (errors:config-parse-failed-cause err) "Invalid syntax"))))

  (testing "circular-preset-reference has chain accessor"
    (let ((err (make-condition 'errors:circular-preset-reference
                               :chain '(:ci :strict :ci))))
      (ok (equal (errors:circular-preset-reference-chain err) '(:ci :strict :ci)))))

  (testing "unknown-preset has name and available-names accessors"
    (let ((err (make-condition 'errors:unknown-preset
                               :name :missing-preset
                               :available-names '(:default :strict))))
      (ok (eq (errors:unknown-preset-name err) :missing-preset))
      (ok (equal (errors:unknown-preset-available-names err) '(:default :strict)))))

  (testing "duplicate-preset-name has name accessor"
    (let ((err (make-condition 'errors:duplicate-preset-name :name :my-preset)))
      (ok (eq (errors:duplicate-preset-name-name err) :my-preset))))

  (testing "unknown-config-form has form accessor"
    (let ((err (make-condition 'errors:unknown-config-form :form :bad-form)))
      (ok (eq (errors:unknown-config-form-form err) :bad-form)))))

(deftest preset-error-exports
  (testing "circular-preset-reference is exported"
    (ok (find-symbol "CIRCULAR-PRESET-REFERENCE" :mallet/errors)))
  (testing "circular-preset-reference-chain is exported"
    (ok (find-symbol "CIRCULAR-PRESET-REFERENCE-CHAIN" :mallet/errors)))
  (testing "unknown-preset is exported"
    (ok (find-symbol "UNKNOWN-PRESET" :mallet/errors)))
  (testing "unknown-preset-name is exported"
    (ok (find-symbol "UNKNOWN-PRESET-NAME" :mallet/errors)))
  (testing "unknown-preset-available-names is exported"
    (ok (find-symbol "UNKNOWN-PRESET-AVAILABLE-NAMES" :mallet/errors)))
  (testing "duplicate-preset-name is exported"
    (ok (find-symbol "DUPLICATE-PRESET-NAME" :mallet/errors)))
  (testing "duplicate-preset-name-name is exported"
    (ok (find-symbol "DUPLICATE-PRESET-NAME-NAME" :mallet/errors)))
  (testing "multiple-config-forms is exported"
    (ok (find-symbol "MULTIPLE-CONFIG-FORMS" :mallet/errors)))
  (testing "unknown-config-form is exported"
    (ok (find-symbol "UNKNOWN-CONFIG-FORM" :mallet/errors)))
  (testing "unknown-config-form-form is exported"
    (ok (find-symbol "UNKNOWN-CONFIG-FORM-FORM" :mallet/errors)))
  (testing "exported symbols are :external"
    (ok (eq :external (nth-value 1 (find-symbol "CIRCULAR-PRESET-REFERENCE" :mallet/errors))))
    (ok (eq :external (nth-value 1 (find-symbol "UNKNOWN-PRESET" :mallet/errors))))
    (ok (eq :external (nth-value 1 (find-symbol "DUPLICATE-PRESET-NAME" :mallet/errors))))
    (ok (eq :external (nth-value 1 (find-symbol "MULTIPLE-CONFIG-FORMS" :mallet/errors))))
    (ok (eq :external (nth-value 1 (find-symbol "UNKNOWN-CONFIG-FORM" :mallet/errors))))))

(deftest preset-error-hierarchy
  (testing "circular-preset-reference is a cli-error"
    (ok (typep (make-condition 'errors:circular-preset-reference :chain '(:x))
               'errors:cli-error))
    (ok (typep (make-condition 'errors:circular-preset-reference :chain '(:x))
               'errors:mallet-error)))

  (testing "unknown-preset is a cli-error"
    (ok (typep (make-condition 'errors:unknown-preset :name :x :available-names '())
               'errors:cli-error))
    (ok (typep (make-condition 'errors:unknown-preset :name :x :available-names '())
               'errors:mallet-error)))

  (testing "duplicate-preset-name is a cli-error"
    (ok (typep (make-condition 'errors:duplicate-preset-name :name :x)
               'errors:cli-error))
    (ok (typep (make-condition 'errors:duplicate-preset-name :name :x)
               'errors:mallet-error)))

  (testing "multiple-config-forms is a cli-error"
    (ok (typep (make-condition 'errors:multiple-config-forms)
               'errors:cli-error))
    (ok (typep (make-condition 'errors:multiple-config-forms)
               'errors:mallet-error)))

  (testing "unknown-config-form is a cli-error"
    (ok (typep (make-condition 'errors:unknown-config-form :form :bad)
               'errors:cli-error))
    (ok (typep (make-condition 'errors:unknown-config-form :form :bad)
               'errors:mallet-error)))

  (testing "preset conditions are distinct types"
    (let ((circular (make-condition 'errors:circular-preset-reference :chain '(:a)))
          (unknown (make-condition 'errors:unknown-preset :name :a :available-names nil))
          (dup (make-condition 'errors:duplicate-preset-name :name :a))
          (multi (make-condition 'errors:multiple-config-forms))
          (unk-form (make-condition 'errors:unknown-config-form :form :a)))
      (ok (not (typep circular 'errors:unknown-preset)))
      (ok (not (typep circular 'errors:duplicate-preset-name)))
      (ok (not (typep unknown 'errors:circular-preset-reference)))
      (ok (not (typep dup 'errors:unknown-preset)))
      (ok (not (typep multi 'errors:unknown-config-form)))
      (ok (not (typep unk-form 'errors:multiple-config-forms))))))

(deftest preset-error-reporting
  (testing "circular-preset-reference reports full chain"
    (let* ((chain '(:ci :strict :ci))
           (err (make-condition 'errors:circular-preset-reference :chain chain))
           (msg (format nil "~A" err)))
      (ok (search "circular" (string-downcase msg)))
      ;; Must show the full cycle, not just one element
      (ok (search "ci" (string-downcase msg)))
      (ok (search "strict" (string-downcase msg)))))

  (testing "circular-preset-reference with longer chain shows all elements"
    (let* ((chain '(:a :b :c :d :a))
           (err (make-condition 'errors:circular-preset-reference :chain chain))
           (msg (string-downcase (format nil "~A" err))))
      (ok (search "a" msg))
      (ok (search "b" msg))
      (ok (search "c" msg))
      (ok (search "d" msg))))

  (testing "unknown-preset reports requested name and available names"
    (let* ((err (make-condition 'errors:unknown-preset
                                :name :no-such-preset
                                :available-names '(:default :strict)))
           (msg (format nil "~A" err)))
      (ok (search "no-such-preset" (string-downcase msg)))
      (ok (search "default" (string-downcase msg)))
      (ok (search "strict" (string-downcase msg)))))

  (testing "unknown-preset with empty available-names still reports name"
    (let* ((err (make-condition 'errors:unknown-preset
                                :name :ghost
                                :available-names nil))
           (msg (format nil "~A" err)))
      (ok (search "ghost" (string-downcase msg)))))

  (testing "duplicate-preset-name reports preset name"
    (let ((err (make-condition 'errors:duplicate-preset-name :name :dupe)))
      (ok (search "dupe" (string-downcase (format nil "~A" err))))))

  (testing "multiple-config-forms has informative message"
    (let ((err (make-condition 'errors:multiple-config-forms)))
      (ok (search "form" (string-downcase (format nil "~A" err))))))

  (testing "unknown-config-form reports form"
    (let ((err (make-condition 'errors:unknown-config-form :form :bad-key)))
      (ok (search "bad-key" (string-downcase (format nil "~A" err)))))))

(deftest preset-error-slot-edge-cases
  (testing "circular-preset-reference chain preserves exact order"
    (let* ((chain '(:z :y :x :z))
           (err (make-condition 'errors:circular-preset-reference :chain chain)))
      (ok (equal (errors:circular-preset-reference-chain err) '(:z :y :x :z)))))

  (testing "circular-preset-reference with two-element cycle"
    (let ((err (make-condition 'errors:circular-preset-reference :chain '(:a :a))))
      (ok (equal (errors:circular-preset-reference-chain err) '(:a :a)))))

  (testing "unknown-preset with nil available-names"
    (let ((err (make-condition 'errors:unknown-preset
                               :name :missing
                               :available-names nil)))
      (ok (eq (errors:unknown-preset-name err) :missing))
      (ok (null (errors:unknown-preset-available-names err)))))

  (testing "unknown-preset with many available names preserves all"
    (let* ((names '(:default :strict :ci :security :performance))
           (err (make-condition 'errors:unknown-preset
                                :name :typo
                                :available-names names)))
      (ok (equal (errors:unknown-preset-available-names err) names))))

  (testing "unknown-config-form preserves non-keyword form"
    (let ((err (make-condition 'errors:unknown-config-form :form "some-string")))
      (ok (equal (errors:unknown-config-form-form err) "some-string")))))

(deftest preset-error-signaling
  (testing "circular-preset-reference can be caught as cli-error"
    (ok (handler-case
            (error 'errors:circular-preset-reference :chain '(:a :b :a))
          (errors:cli-error () t))))

  (testing "unknown-preset can be caught as cli-error"
    (ok (handler-case
            (error 'errors:unknown-preset :name :bad :available-names nil)
          (errors:cli-error () t))))

  (testing "duplicate-preset-name can be caught as cli-error"
    (ok (handler-case
            (error 'errors:duplicate-preset-name :name :dup)
          (errors:cli-error () t))))

  (testing "multiple-config-forms can be caught as cli-error"
    (ok (handler-case
            (error 'errors:multiple-config-forms)
          (errors:cli-error () t))))

  (testing "unknown-config-form can be caught as cli-error"
    (ok (handler-case
            (error 'errors:unknown-config-form :form :bad)
          (errors:cli-error () t))))

  (testing "circular-preset-reference caught as cli-error exposes chain"
    (let ((caught-chain
            (handler-case
                (error 'errors:circular-preset-reference :chain '(:x :y :x))
              (errors:circular-preset-reference (c)
                (errors:circular-preset-reference-chain c)))))
      (ok (equal caught-chain '(:x :y :x)))))

  (testing "unknown-preset caught as cli-error exposes name and available-names"
    (multiple-value-bind (name avail)
        (handler-case
            (error 'errors:unknown-preset :name :missing :available-names '(:a :b))
          (errors:unknown-preset (c)
            (values (errors:unknown-preset-name c)
                    (errors:unknown-preset-available-names c))))
      (ok (eq name :missing))
      (ok (equal avail '(:a :b))))))
