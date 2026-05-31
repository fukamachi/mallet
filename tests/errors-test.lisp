(defpackage #:mallet/tests/errors
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:errors #:mallet/errors)))
(in-package #:mallet/tests/errors)

(defun condition-text (condition)
  (format nil "~A" condition))

(defun message-contains-p (condition token)
  (search token (string-downcase (condition-text condition))))

(defun message= (condition expected)
  (string= expected (condition-text condition)))

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

(deftest unknown-rule-error-reporting
  (testing "unknown-rule message identifies the bad rule name"
    (let ((err (make-condition 'errors:unknown-rule :value "no-such-rule")))
      (ok (typep err 'errors:unknown-rule))
      (ok (equal (errors:unknown-rule-value err) "no-such-rule"))
      (ok (message-contains-p err "no-such-rule")
          "error message must include the unrecognized rule name")))

  (testing "unknown-rule recovery hint names the --list-rules command"
    (let ((err (make-condition 'errors:unknown-rule :value "no-such-rule")))
      (ok (message= err
                    (format nil "Unknown rule: no-such-rule~%Run 'mallet --list-rules' to see available rules.")))
      (ok (message-contains-p err "--list-rules")
          "recovery hint must reference --list-rules so the hint actually works"))))

(deftest error-reporting
  (testing "unknown-option has helpful message"
    (let ((err (make-condition 'errors:unknown-option :option "--foo")))
      (ok (typep err 'errors:unknown-option))
      (ok (equal (errors:unknown-option-option err) "--foo"))
      (ok (message= err
                    (format nil "Unknown option: --foo~%Run 'mallet --help' to see available options.")))
      (ok (message-contains-p err "--foo"))
      (ok (message-contains-p err "mallet --help"))))

  (testing "missing-option-value has clear message"
    (let ((err (make-condition 'errors:missing-option-value :option "--format")))
      (ok (typep err 'errors:missing-option-value))
      (ok (equal (errors:missing-option-value-option err) "--format"))
      (ok (message= err "Missing value for option: --format"))
      (ok (message-contains-p err "--format"))))

  (testing "invalid-format has specific message"
    (let ((err (make-condition 'errors:invalid-format
                               :option "--format"
                               :value "xml"
                               :expected "text, line, or json")))
      (ok (typep err 'errors:invalid-format))
      (ok (equal (errors:invalid-option-value-option err) "--format"))
      (ok (equal (errors:invalid-option-value-value err) "xml"))
      (ok (equal (errors:invalid-option-value-expected err) "text, line, or json"))
      (ok (message= err
                    (format nil "Invalid format: xml~%Expected: text, line, or json")))
      (ok (message-contains-p err "xml"))
      (ok (message-contains-p err "text, line, or json")
          "accepted formats must be listed as structured expected values")))

  (testing "invalid-preset has specific message"
    (let ((err (make-condition 'errors:invalid-preset
                               :option "--preset"
                               :value "bad"
                               :expected "default, strict, all, or none")))
      (ok (typep err 'errors:invalid-preset))
      (ok (equal (errors:invalid-option-value-option err) "--preset"))
      (ok (equal (errors:invalid-option-value-value err) "bad"))
      (ok (equal (errors:invalid-option-value-expected err) "default, strict, all, or none"))
      (ok (message= err
                    (format nil "Invalid value 'bad' for option --preset~%Expected: default, strict, all, or none")))
      (ok (message-contains-p err "bad"))
      (ok (message-contains-p err "default, strict, all, or none"))))

  (testing "file-not-found has clear message"
    (let ((err (make-condition 'errors:file-not-found :path "missing.lisp")))
      (ok (typep err 'errors:file-not-found))
      (ok (equal (errors:file-not-found-path err) "missing.lisp"))
      (ok (message= err "File not found: missing.lisp"))
      (ok (message-contains-p err "missing.lisp"))))

  (testing "file-already-exists has clear message"
    (let ((err (make-condition 'errors:file-already-exists :path "existing.lisp")))
      (ok (typep err 'errors:file-already-exists))
      (ok (equal (errors:file-already-exists-path err) "existing.lisp"))
      (ok (message= err
                    (format nil "File already exists: existing.lisp~%Use --force to overwrite.")))
      (ok (message-contains-p err "existing.lisp"))))

  (testing "no-files-specified has helpful message"
    (let ((err (make-condition 'errors:no-files-specified)))
      (ok (typep err 'errors:no-files-specified))
      (ok (message= err
                    (format nil "No files specified~%Run 'mallet --help' for usage information.")))
      (ok (message-contains-p err "mallet --help"))))

  (testing "config-not-found reports path"
    (let ((err (make-condition 'errors:config-not-found :path ".mallet.lisp")))
      (ok (typep err 'errors:config-not-found))
      (ok (equal (errors:config-not-found-path err) ".mallet.lisp"))
      (ok (message= err "Config file not found: .mallet.lisp"))
      (ok (message-contains-p err ".mallet.lisp"))))

  (testing "config-parse-failed reports path and cause"
    (let ((err (make-condition 'errors:config-parse-failed
                               :path ".mallet.lisp"
                               :cause "Invalid syntax")))
      (ok (typep err 'errors:config-parse-failed))
      (ok (equal (errors:config-parse-failed-path err) ".mallet.lisp"))
      (ok (equal (errors:config-parse-failed-cause err) "Invalid syntax"))
      (ok (message= err
                    (format nil "Failed to parse config file: .mallet.lisp~%Cause: Invalid syntax")))
      (ok (message-contains-p err ".mallet.lisp"))
      (ok (message-contains-p err "invalid syntax"))))

  (testing "invalid-rule-option reports offending value"
    (let ((err (make-condition 'errors:invalid-rule-option :value "max")))
      (ok (typep err 'errors:invalid-rule-option))
      (ok (equal (errors:invalid-rule-option-value err) "max"))
      (ok (message= err
                    (format nil "Invalid rule option syntax: max~%Expected format: key=value (e.g., max=15)")))
      (ok (message-contains-p err "max")))))

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
    (ok (typep (make-condition 'errors:circular-preset-reference :chain '(:ci))
               'errors:cli-error))
    (ok (typep (make-condition 'errors:circular-preset-reference :chain '(:ci))
               'errors:mallet-error)))

  (testing "unknown-preset is a cli-error"
    (ok (typep (make-condition 'errors:unknown-preset :name :missing-preset :available-names '())
               'errors:cli-error))
    (ok (typep (make-condition 'errors:unknown-preset :name :missing-preset :available-names '())
               'errors:mallet-error)))

  (testing "duplicate-preset-name is a cli-error"
    (ok (typep (make-condition 'errors:duplicate-preset-name :name :duplicate-preset)
               'errors:cli-error))
    (ok (typep (make-condition 'errors:duplicate-preset-name :name :duplicate-preset)
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
    (let ((circular (make-condition 'errors:circular-preset-reference :chain '(:ci)))
          (unknown (make-condition 'errors:unknown-preset :name :missing-preset :available-names nil))
          (dup (make-condition 'errors:duplicate-preset-name :name :duplicate-preset))
          (multi (make-condition 'errors:multiple-config-forms))
          (unk-form (make-condition 'errors:unknown-config-form :form :bad-form)))
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
      (ok (typep err 'errors:circular-preset-reference))
      (ok (equal (errors:circular-preset-reference-chain err) chain))
      (ok (message= err "Circular preset reference detected: ci -> strict -> ci"))
      (ok (search "circular" (string-downcase msg)))
      ;; Must show the full cycle, not just one element
      (ok (search "ci" (string-downcase msg)))
      (ok (search "strict" (string-downcase msg)))))

  (testing "circular-preset-reference with longer chain shows all elements"
    (let* ((chain '(:ci :strict :relaxed :default :ci))
           (err (make-condition 'errors:circular-preset-reference :chain chain))
           (msg (string-downcase (format nil "~A" err))))
      (ok (typep err 'errors:circular-preset-reference))
      (ok (equal (errors:circular-preset-reference-chain err) chain))
      (ok (search "ci" msg))
      (ok (search "strict" msg))
      (ok (search "relaxed" msg))
      (ok (search "default" msg))))

  (testing "unknown-preset reports requested name and available names"
    (let* ((err (make-condition 'errors:unknown-preset
                                :name :no-such-preset
                                :available-names '(:default :strict)))
           (msg (format nil "~A" err)))
      (ok (typep err 'errors:unknown-preset))
      (ok (eq (errors:unknown-preset-name err) :no-such-preset))
      (ok (equal (errors:unknown-preset-available-names err) '(:default :strict)))
      (ok (message= err
                    (format nil "Unknown preset: no-such-preset~%Available presets: default, strict")))
      (ok (search "no-such-preset" (string-downcase msg)))
      (ok (search "default" (string-downcase msg)))
      (ok (search "strict" (string-downcase msg)))))

  (testing "unknown-preset with empty available-names still reports name"
    (let* ((err (make-condition 'errors:unknown-preset
                                :name :ghost
                                :available-names nil))
           (msg (format nil "~A" err)))
      (ok (typep err 'errors:unknown-preset))
      (ok (eq (errors:unknown-preset-name err) :ghost))
      (ok (null (errors:unknown-preset-available-names err)))
      (ok (message= err "Unknown preset: ghost"))
      (ok (search "ghost" (string-downcase msg)))))

  (testing "duplicate-preset-name reports preset name"
    (let ((err (make-condition 'errors:duplicate-preset-name :name :dupe)))
      (ok (typep err 'errors:duplicate-preset-name))
      (ok (eq (errors:duplicate-preset-name-name err) :dupe))
      (ok (message= err "Duplicate preset name: dupe"))
      (ok (message-contains-p err "dupe"))))

  (testing "multiple-config-forms has informative message"
    (let ((err (make-condition 'errors:multiple-config-forms)))
      (ok (typep err 'errors:multiple-config-forms))
      (ok (message= err "Multiple :mallet-config forms found; at most one is allowed."))
      (ok (message-contains-p err ":mallet-config"))))

  (testing "unknown-config-form reports form"
    (let ((err (make-condition 'errors:unknown-config-form :form :bad-key)))
      (ok (typep err 'errors:unknown-config-form))
      (ok (eq (errors:unknown-config-form-form err) :bad-key))
      (ok (message= err "Unknown config form: :BAD-KEY"))
      (ok (message-contains-p err "bad-key")))))

(deftest preset-error-slot-edge-cases
  (testing "circular-preset-reference chain preserves exact order"
    (let* ((chain '(:ci :security :performance :ci))
           (err (make-condition 'errors:circular-preset-reference :chain chain)))
      (ok (equal (errors:circular-preset-reference-chain err) chain))))

  (testing "circular-preset-reference with two-element cycle"
    (let ((err (make-condition 'errors:circular-preset-reference :chain '(:ci :ci))))
      (ok (equal (errors:circular-preset-reference-chain err) '(:ci :ci)))))

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
            (error 'errors:circular-preset-reference :chain '(:ci :strict :ci))
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
                (error 'errors:circular-preset-reference :chain '(:ci :strict :ci))
              (errors:circular-preset-reference (c)
                (errors:circular-preset-reference-chain c)))))
      (ok (equal caught-chain '(:ci :strict :ci)))))

  (testing "unknown-preset caught as cli-error exposes name and available-names"
    (multiple-value-bind (name avail)
        (handler-case
            (error 'errors:unknown-preset :name :missing :available-names '(:default :strict))
          (errors:unknown-preset (c)
            (values (errors:unknown-preset-name c)
                    (errors:unknown-preset-available-names c))))
      (ok (eq name :missing))
      (ok (equal avail '(:default :strict))))))
