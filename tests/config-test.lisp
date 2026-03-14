(defpackage #:mallet/tests/config
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:config #:mallet/config)
   (#:rules #:mallet/rules)))
(in-package #:mallet/tests/config)

;;; Config data structure tests

(deftest config-creation
  (testing "Create empty config"
    (let ((cfg (config:make-config)))
      (ok (not (null cfg)))
      (ok (typep cfg 'config:config))))

  (testing "Create config with rules"
    (let ((cfg (config:make-config
                :rules (list (rules:make-rule :line-length :max 100)
                             (rules:make-rule :if-without-else))
                :disabled-rules '(:if-without-else))))
      (ok (not (null cfg)))
      (ok (= 2 (length (config:config-rules cfg))))
      (ok (member :if-without-else (config:config-disabled-rules cfg))))))

;;; Config parsing tests

(deftest parse-config
  (testing "Parse simple config with :enable"
    (let* ((sexp '(:mallet-config
                   (:enable :line-length :max 120)))
           (cfg (config:parse-config sexp)))
      (ok (= 1 (length (config:config-rules cfg))))
      (let ((rule (first (config:config-rules cfg))))
        (ok (eq :line-length (rules:rule-name rule)))
        (ok (= 120 (rules:line-length-rule-max-length rule))))))

  (testing "Parse config with :disable"
    (let* ((sexp '(:mallet-config
                   (:disable :if-without-else)))
           (cfg (config:parse-config sexp)))
      (ok (member :if-without-else (config:config-disabled-rules cfg)))))

  (testing "Parse config with severity override"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables :severity :error)))
           (cfg (config:parse-config sexp)))
      (let ((rule (first (config:config-rules cfg))))
        (ok (eq :error (rules:rule-severity rule)))))))

;;; Config file loading tests

(defmacro with-temporary-config ((content pathname) &body body)
  `(uiop:with-temporary-file (:stream out
                              :pathname ,pathname
                              :direction :output
                              :prefix "mallet-config"
                              :type "lisp")
     (write-string ,content out)
     (force-output out)
     ,@body))

(deftest load-config-file
  (testing "Load config from file"
    (let ((config-content "(:mallet-config
                              (:enable :line-length :max 100))"))
      (with-temporary-config (config-content tmpfile)
        (let ((cfg (config:load-config tmpfile)))
          (ok (= 1 (length (config:config-rules cfg))))
          (let ((rule (first (config:config-rules cfg))))
            (ok (eq :line-length (rules:rule-name rule)))
            (ok (= 100 (rules:line-length-rule-max-length rule)))))))))

;;; Built-in config tests

(deftest built-in-configs
  (testing "Load default config"
    (let ((cfg (config:get-built-in-config :default)))
      (ok (not (null cfg)))
      ;; Check that some rules are in the enabled list
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :trailing-whitespace rule-names))
        (ok (member :no-tabs rule-names))
        (ok (member :unused-variables rule-names))
        (ok (member :if-without-else rule-names))
        ;; missing-exported-docstring is opt-in; must not be in default
        (ok (not (member :missing-exported-docstring rule-names)))
        ;; missing-docstring is opt-in only; must not be in default
        (ok (not (member :missing-docstring rule-names)))
        ;; ASDF best-practices rules in the default preset
        (ok (not (member :asdf-redundant-package-prefix rule-names)))
        (ok (member :asdf-operate-in-perform rule-names))
        (ok (member :asdf-secondary-system-name rule-names))
        (ok (member :asdf-if-feature-keyword rule-names))
        ;; asdf-reader-conditional is disabled in default
        (ok (not (member :asdf-reader-conditional rule-names))))
      ;; Check that some rules are disabled
      (let ((disabled (config:config-disabled-rules cfg)))
        (ok (member :line-length disabled))
        (ok (member :constant-naming disabled))
        (ok (member :special-variable-naming disabled)))))

  (testing "Load all config"
    (let ((cfg (config:get-built-in-config :all)))
      (ok (not (null cfg)))
      ;; All rules should be enabled (none in disabled list)
      (ok (null (config:config-disabled-rules cfg)))
      ;; Check that various rules are present
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :line-length rule-names))
        (ok (member :trailing-whitespace rule-names))
        (ok (member :if-without-else rule-names))
        ;; Both docstring rules are in the all preset
        (ok (member :missing-exported-docstring rule-names))
        (ok (member :missing-docstring rule-names))
        ;; All 5 ASDF best-practices rules are in the all preset
        (ok (member :asdf-redundant-package-prefix rule-names))
        (ok (member :asdf-operate-in-perform rule-names))
        (ok (member :asdf-secondary-system-name rule-names))
        (ok (member :asdf-if-feature-keyword rule-names))
        (ok (member :asdf-reader-conditional rule-names)))))

  (testing "Load none config"
    (let ((cfg (config:get-built-in-config :none)))
      (ok (not (null cfg)))
      ;; No rules should be enabled
      (ok (null (config:config-rules cfg)))
      ;; No rules should be disabled
      (ok (null (config:config-disabled-rules cfg))))))

;;; Config extends tests

(deftest config-extends
  (testing "Parse config with extends"
    (let* ((sexp '(:mallet-config
                   (:extends :all)
                   (:enable :line-length :max 100)))
           (cfg (config:parse-config sexp)))
      ;; Should inherit all rules from :all
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :line-length rule-names))
        (ok (member :trailing-whitespace rule-names)))
      ;; Check that line-length has the overridden max-length
      (let ((line-length-rule (find :line-length (config:config-rules cfg)
                                    :key #'rules:rule-name)))
        (ok (= 100 (rules:line-length-rule-max-length line-length-rule))))))

  (testing "Parse config with extends :none"
    (let* ((sexp '(:mallet-config
                   (:extends :none)
                   (:enable :unused-variables)
                   (:enable :trailing-whitespace)))
           (cfg (config:parse-config sexp)))
      ;; Should only have the explicitly enabled rules
      (ok (= 2 (length (config:config-rules cfg))))
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :unused-variables rule-names))
        (ok (member :trailing-whitespace rule-names))
        (ok (not (member :line-length rule-names)))))))

(deftest parse-new-syntax-enable
  (testing "Parse config with :enable syntax"
    (let* ((sexp '(:mallet-config
                   (:enable :line-length :max 120)))
           (cfg (config:parse-config sexp)))
      (ok (= 1 (length (config:config-rules cfg))))
      (let ((rule (first (config:config-rules cfg))))
        (ok (eq :line-length (rules:rule-name rule)))
        (ok (= 120 (rules:line-length-rule-max-length rule))))))

  (testing "Parse config with :enable without options"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)))
           (cfg (config:parse-config sexp)))
      (ok (= 1 (length (config:config-rules cfg))))
      (let ((rule (first (config:config-rules cfg))))
        (ok (eq :unused-variables (rules:rule-name rule))))))

  (testing "Parse config with multiple :enable forms"
    (let* ((sexp '(:mallet-config
                   (:enable :line-length :max 100)
                   (:enable :unused-variables :severity :error)
                   (:enable :special-variable-naming)))
           (cfg (config:parse-config sexp)))
      (ok (= 3 (length (config:config-rules cfg))))
      (let ((rule-map (make-hash-table)))
        (dolist (rule (config:config-rules cfg))
          (setf (gethash (rules:rule-name rule) rule-map) rule))
        ;; Check line-length
        (let ((rule (gethash :line-length rule-map)))
          (ok (not (null rule)))
          (ok (= 100 (rules:line-length-rule-max-length rule))))
        ;; Check unused-variables
        (let ((rule (gethash :unused-variables rule-map)))
          (ok (not (null rule)))
          (ok (eq :error (rules:rule-severity rule))))
        ;; Check special-variable-naming
        (ok (not (null (gethash :special-variable-naming rule-map))))))))

(deftest parse-new-syntax-disable
  (testing "Parse config with :disable syntax"
    (let* ((sexp '(:mallet-config
                   (:extends :default)
                   (:disable :constant-naming)))
           (cfg (config:parse-config sexp)))
      (ok (member :constant-naming (config:config-disabled-rules cfg)))))

  (testing "Parse config with multiple :disable forms"
    (let* ((sexp '(:mallet-config
                   (:extends :default)
                   (:disable :line-length)
                   (:disable :consecutive-blank-lines)))
           (cfg (config:parse-config sexp)))
      (ok (member :line-length (config:config-disabled-rules cfg)))
      (ok (member :consecutive-blank-lines (config:config-disabled-rules cfg))))))

;;; Path override tests

(deftest parse-for-paths
  (testing "Parse config with :for-paths"
    (let* ((sexp '(:mallet-config
                   (:extends :default)
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :line-length :max 120)
                    (:disable :unused-variables))))
           (cfg (config:parse-config sexp)))
      ;; Base config should have default rules
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :unused-variables rule-names)))
      ;; Check that path-rules are stored
      (ok (= 1 (length (config:config-path-rules cfg))))))

  (testing "Parse config with directory name patterns"
    (let* ((sexp '(:mallet-config
                   (:for-paths ("tests/**/*.lisp" "scripts/**/*.lisp")
                    (:disable :line-length))))
           (cfg (config:parse-config sexp)))
      ;; Check that path-rules are stored
      (ok (= 1 (length (config:config-path-rules cfg))))
      ;; Patterns should be stored as-is
      (let* ((path-rule (first (config:config-path-rules cfg)))
             (patterns (config:path-override-patterns path-rule)))
        (ok (= 2 (length patterns)))))))

;;; get-rules-for-file tests

(deftest get-rules-for-file
  (testing "Get rules for file without path-specific overrides"
    (let ((cfg (config:get-built-in-config :default)))
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        ;; Should get base rules, minus disabled ones
        (ok (member :trailing-whitespace rule-names))
        (ok (not (member :line-length rule-names))))))  ; line-length is disabled

  (testing "Get rules for file with path-specific overrides"
    (let* ((sexp '(:mallet-config
                   (:enable :line-length :max 80)
                   (:enable :unused-variables)
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :line-length :max 120)
                    (:disable :unused-variables))))
           (cfg (config:parse-config sexp)))
      ;; For a normal file, should get base rules
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (member :line-length rule-names))
        (ok (member :unused-variables rule-names)))
      ;; For a test file, should get overridden rules
      (let* ((rules (config:get-rules-for-file cfg #P"/tests/foo-test.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (member :line-length rule-names))
        (ok (not (member :unused-variables rule-names)))  ; disabled for tests
        ;; Check that line-length has the overridden max-length
        (let ((line-length-rule (find :line-length rules :key #'rules:rule-name)))
          (ok (= 120 (rules:line-length-rule-max-length line-length-rule)))))))

  (testing "Get rules for specific file path"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)
                   (:enable :unused-local-nicknames)
                   (:for-paths ("/src/parser.lisp")
                    (:disable :unused-local-nicknames))))
           (cfg (config:parse-config sexp)))
      ;; For src/parser.lisp, unused-local-nicknames should be disabled
      (let* ((rules (config:get-rules-for-file cfg #P"/src/parser.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (member :unused-variables rule-names))
        (ok (not (member :unused-local-nicknames rule-names))))  ; disabled for this file
      ;; For other files, both rules should be enabled
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (member :unused-variables rule-names))
        (ok (member :unused-local-nicknames rule-names))))))

;;; Comprehensive :ignore pattern tests

(deftest ignore-patterns
  (testing "Ignore exact file path with leading /"
    (let* ((sexp '(:mallet-config
                   (:ignore "/tests/fixtures/example.lisp")))
           (cfg (config:parse-config sexp)))
      (ok (config:file-ignored-p cfg #P"/tests/fixtures/example.lisp"))
      (ok (not (config:file-ignored-p cfg #P"/tests/other.lisp")))))

  (testing "Ignore directory pattern with trailing /"
    (let* ((sexp '(:mallet-config
                   (:ignore "/tests/fixtures/")))
           (cfg (config:parse-config sexp)))
      (ok (config:file-ignored-p cfg #P"/tests/fixtures/example.lisp"))
      (ok (config:file-ignored-p cfg #P"/tests/fixtures/nested/deep.lisp"))
      (ok (not (config:file-ignored-p cfg #P"/tests/main-test.lisp")))))

  (testing "Ignore wildcard patterns"
    (let* ((sexp '(:mallet-config
                   (:ignore "**/*-generated.lisp")))
           (cfg (config:parse-config sexp)))
      (ok (config:file-ignored-p cfg #P"/src/auto-generated.lisp"))
      (ok (config:file-ignored-p cfg #P"/tests/fixtures/foo-generated.lisp"))
      (ok (not (config:file-ignored-p cfg #P"/src/main.lisp")))))

  (testing "Multiple ignore patterns"
    (let* ((sexp '(:mallet-config
                   (:ignore "/tests/fixtures/"
                            "**/*-generated.lisp"
                            "/build/")))
           (cfg (config:parse-config sexp)))
      (ok (config:file-ignored-p cfg #P"/tests/fixtures/example.lisp"))
      (ok (config:file-ignored-p cfg #P"/src/auto-generated.lisp"))
      (ok (config:file-ignored-p cfg #P"/build/output.lisp"))
      (ok (not (config:file-ignored-p cfg #P"/src/main.lisp")))))

  (testing "Ignore patterns with specific extensions"
    (let* ((sexp '(:mallet-config
                   (:ignore "**/*.asd")))
           (cfg (config:parse-config sexp)))
      (ok (config:file-ignored-p cfg #P"/mallet.asd"))
      (ok (config:file-ignored-p cfg #P"/tests/test-system.asd"))
      (ok (not (config:file-ignored-p cfg #P"/src/main.lisp")))))

  (testing "Ignore nested directories"
    (let* ((sexp '(:mallet-config
                   (:ignore "/.qlot/")))
           (cfg (config:parse-config sexp)))
      (ok (config:file-ignored-p cfg #P"/.qlot/local-projects/foo/src/main.lisp"))
      (ok (config:file-ignored-p cfg #P"/.qlot/dists/quicklisp/software/alexandria.lisp"))
      (ok (not (config:file-ignored-p cfg #P"/src/main.lisp"))))))

;;; Comprehensive :for-paths pattern tests

(deftest for-paths-patterns
  (testing "For-paths with exact file path"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)
                   (:enable :line-length :max 80)
                   (:for-paths ("/src/parser.lisp")
                    (:disable :unused-variables))))
           (cfg (config:parse-config sexp)))
      ;; For src/parser.lisp, unused-variables should be disabled
      (let* ((rules (config:get-rules-for-file cfg #P"/src/parser.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (not (member :unused-variables rule-names)))
        (ok (member :line-length rule-names)))
      ;; For other files, both rules should be enabled
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (member :unused-variables rule-names))
        (ok (member :line-length rule-names)))))

  (testing "For-paths with directory pattern"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)
                   (:enable :line-length :max 80)
                   (:for-paths ("/tests/")
                    (:enable :line-length :max 120)
                    (:disable :unused-variables))))
           (cfg (config:parse-config sexp)))
      ;; For test files, line-length should be 120 and unused-variables disabled
      (let* ((rules (config:get-rules-for-file cfg #P"/tests/main-test.lisp"))
             (rule-names (mapcar #'rules:rule-name rules))
             (line-length-rule (find :line-length rules :key #'rules:rule-name)))
        (ok (not (member :unused-variables rule-names)))
        (ok (= 120 (rules:line-length-rule-max-length line-length-rule))))
      ;; For src files, should use base config
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (rule-names (mapcar #'rules:rule-name rules))
             (line-length-rule (find :line-length rules :key #'rules:rule-name)))
        (ok (member :unused-variables rule-names))
        (ok (= 80 (rules:line-length-rule-max-length line-length-rule))))))

  (testing "For-paths with multiple file paths"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)
                   (:for-paths ("/src/parser.lisp" "/src/tokenizer.lisp")
                    (:disable :unused-variables))))
           (cfg (config:parse-config sexp)))
      ;; Both specified files should have unused-variables disabled
      (ok (not (member :unused-variables
                       (mapcar #'rules:rule-name
                               (config:get-rules-for-file cfg #P"/src/parser.lisp")))))
      (ok (not (member :unused-variables
                       (mapcar #'rules:rule-name
                               (config:get-rules-for-file cfg #P"/src/tokenizer.lisp")))))
      ;; Other files should have it enabled
      (ok (member :unused-variables
                  (mapcar #'rules:rule-name
                          (config:get-rules-for-file cfg #P"/src/main.lisp"))))))

  (testing "For-paths with wildcard patterns"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)
                   (:for-paths ("**/*-test.lisp")
                    (:disable :unused-variables))))
           (cfg (config:parse-config sexp)))
      ;; Test files should have unused-variables disabled
      (ok (not (member :unused-variables
                       (mapcar #'rules:rule-name
                               (config:get-rules-for-file cfg #P"/tests/main-test.lisp")))))
      (ok (not (member :unused-variables
                       (mapcar #'rules:rule-name
                               (config:get-rules-for-file cfg #P"/src/parser-test.lisp")))))
      ;; Non-test files should have it enabled
      (ok (member :unused-variables
                  (mapcar #'rules:rule-name
                          (config:get-rules-for-file cfg #P"/src/main.lisp"))))))

  (testing "For-paths with multiple rule overrides"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)
                   (:enable :line-length :max 80)
                   (:enable :trailing-whitespace)
                   (:for-paths ("/scripts/")
                    (:enable :line-length :max 100)
                    (:disable :unused-variables)
                    (:disable :trailing-whitespace))))
           (cfg (config:parse-config sexp)))
      ;; For scripts, multiple rules should be overridden
      (let* ((rules (config:get-rules-for-file cfg #P"/scripts/build.lisp"))
             (rule-names (mapcar #'rules:rule-name rules))
             (line-length-rule (find :line-length rules :key #'rules:rule-name)))
        (ok (not (member :unused-variables rule-names)))
        (ok (not (member :trailing-whitespace rule-names)))
        (ok (= 100 (rules:line-length-rule-max-length line-length-rule))))
      ;; For other files, base config applies
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (member :unused-variables rule-names))
        (ok (member :trailing-whitespace rule-names)))))

  (testing "For-paths with nested directory patterns"
    (let* ((sexp '(:mallet-config
                   (:enable :line-length :max 80)
                   (:for-paths ("/tests/fixtures/")
                    (:enable :line-length :max 200))))
           (cfg (config:parse-config sexp)))
      ;; Files in nested fixtures should use overridden config
      (let* ((rules (config:get-rules-for-file cfg #P"/tests/fixtures/nested/deep/example.lisp"))
             (line-length-rule (find :line-length rules :key #'rules:rule-name)))
        (ok (= 200 (rules:line-length-rule-max-length line-length-rule))))
      ;; Files outside should use base config
      (let* ((rules (config:get-rules-for-file cfg #P"/tests/main-test.lisp"))
             (line-length-rule (find :line-length rules :key #'rules:rule-name)))
        (ok (= 80 (rules:line-length-rule-max-length line-length-rule))))))

  (testing "For-paths with .asd files"
    (let* ((sexp '(:mallet-config
                   (:enable :line-length :max 80)
                   (:for-paths ("*.asd")
                    (:enable :line-length :max 100))))
           (cfg (config:parse-config sexp)))
      ;; .asd files should use overridden config
      (let* ((rules (config:get-rules-for-file cfg #P"/mallet.asd"))
             (line-length-rule (find :line-length rules :key #'rules:rule-name)))
        (ok (= 100 (rules:line-length-rule-max-length line-length-rule))))
      ;; .lisp files should use base config
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (line-length-rule (find :line-length rules :key #'rules:rule-name)))
        (ok (= 80 (rules:line-length-rule-max-length line-length-rule)))))))

;;; Preset override tests

(deftest preset-override
  (testing "Override :extends with preset-override parameter"
    (let* ((sexp '(:mallet-config
                   (:extends :default)
                   (:enable :line-length :max 100)
                   (:disable :trailing-whitespace)))
           ;; Override with :all preset
           (cfg (config:parse-config sexp :preset-override :all)))
      ;; Should inherit rules from :all (not :default)
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        ;; :all includes cyclomatic-complexity, :default doesn't
        (ok (member :cyclomatic-complexity rule-names))
        ;; Should have line-length with overridden max-length from config
        (let ((line-length-rule (find :line-length (config:config-rules cfg)
                                      :key #'rules:rule-name)))
          (ok (= 100 (rules:line-length-rule-max-length line-length-rule)))))
      ;; Should respect :disable from config file
      (ok (member :trailing-whitespace (config:config-disabled-rules cfg)))))

  (testing "Preset override when no :extends in config"
    (let* ((sexp '(:mallet-config
                   (:enable :line-length :max 120)
                   (:disable :no-tabs)))
           ;; Provide preset-override even though there's no :extends
           (cfg (config:parse-config sexp :preset-override :default)))
      ;; Should use :default as base
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        ;; :default includes trailing-whitespace
        (ok (member :trailing-whitespace rule-names))
        ;; Should have line-length with custom max-length
        (let ((line-length-rule (find :line-length (config:config-rules cfg)
                                      :key #'rules:rule-name)))
          (ok (= 120 (rules:line-length-rule-max-length line-length-rule)))))
      ;; Should respect :disable from config file
      (ok (member :no-tabs (config:config-disabled-rules cfg)))))

  (testing "Load config file with preset override"
    (let ((config-content "(:mallet-config
                              (:extends :default)
                              (:enable :line-length :max 100)
                              (:disable :unused-variables))"))
      (with-temporary-config (config-content tmpfile)
        (let ((cfg (config:load-config tmpfile :preset-override :all)))
          ;; Should use :all as base (not :default from file)
          (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
            ;; :all includes cyclomatic-complexity
            (ok (member :cyclomatic-complexity rule-names))
            ;; Should have custom line-length
            (let ((line-length-rule (find :line-length (config:config-rules cfg)
                                          :key #'rules:rule-name)))
              (ok (= 100 (rules:line-length-rule-max-length line-length-rule)))))
          ;; Should respect :disable from config file
          (ok (member :unused-variables (config:config-disabled-rules cfg)))))))

  (testing "Preset override respects :for-paths from config"
    (let* ((sexp '(:mallet-config
                   (:extends :default)
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :line-length :max 120)
                    (:disable :unused-variables))))
           (cfg (config:parse-config sexp :preset-override :all)))
      ;; Should have path overrides
      (ok (= 1 (length (config:config-path-rules cfg))))
      ;; For test files, should get path-specific rules
      (let* ((rules (config:get-rules-for-file cfg #P"/tests/foo-test.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (member :line-length rule-names))
        (ok (not (member :unused-variables rule-names))))
      ;; For non-test files, should get base rules from :all
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        ;; :all includes cyclomatic-complexity
        (ok (member :cyclomatic-complexity rule-names))))))

;;; :for-paths inheritance tests

(deftest for-paths-inherits-project-wide-config
  (testing ":for-paths inherits project-wide :disable settings"
    (let* ((sexp '(:mallet-config
                   (:extends :default)
                   (:disable :if-without-else)  ; Project-wide disable
                   (:for-paths ("src/main.lisp")
                    (:enable :line-length :max 100))))
           (cfg (config:parse-config sexp)))
      ;; For src/main.lisp, :if-without-else should be disabled (inherited from project-wide)
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (not (member :if-without-else rule-names)) "Project-wide :disable should apply to :for-paths")
        (ok (member :line-length rule-names) ":for-paths :enable should work"))
      ;; For other files, :if-without-else should also be disabled
      (let* ((rules (config:get-rules-for-file cfg #P"/src/other.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (not (member :if-without-else rule-names)) "Project-wide :disable should apply to all files"))))

  (testing ":for-paths inherits project-wide :enable settings"
    (let* ((sexp '(:mallet-config
                   (:extends :default)
                   (:enable :line-length :max 80)  ; Project-wide enable
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :line-length :max 120))))  ; Override max-length for tests
           (cfg (config:parse-config sexp)))
      ;; For test files, should have line-length with test-specific max
      (let* ((rules (config:get-rules-for-file cfg #P"/tests/foo-test.lisp"))
             (line-length-rule (find :line-length rules :key #'rules:rule-name)))
        (ok (not (null line-length-rule)) "Should inherit :line-length from project-wide")
        (ok (= 120 (rules:line-length-rule-max-length line-length-rule))
            ":for-paths override should apply"))
      ;; For other files, should have line-length with project-wide max
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (line-length-rule (find :line-length rules :key #'rules:rule-name)))
        (ok (not (null line-length-rule)))
        (ok (= 80 (rules:line-length-rule-max-length line-length-rule))
            "Project-wide settings should apply"))))

  (testing ":for-paths can override project-wide :disable"
    (let* ((sexp '(:mallet-config
                   (:extends :default)
                   (:disable :unused-variables)  ; Project-wide disable
                   (:for-paths ("src/critical.lisp")
                    (:enable :unused-variables))))  ; Re-enable for critical file
           (cfg (config:parse-config sexp)))
      ;; For src/critical.lisp, :unused-variables should be enabled
      (let* ((rules (config:get-rules-for-file cfg #P"/src/critical.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (member :unused-variables rule-names)
            ":for-paths :enable should override project-wide :disable"))
      ;; For other files, :unused-variables should be disabled
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (not (member :unused-variables rule-names))
            "Project-wide :disable should apply"))))

  (testing "Priority: extends < project-wide < :for-paths < CLI"
    (let* ((sexp '(:mallet-config
                   (:extends :default)              ; :default has :unused-variables enabled
                   (:disable :unused-variables)     ; Project-wide disables it
                   (:enable :line-length :max 80)   ; Project-wide enables line-length
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :unused-variables)     ; Re-enable for tests
                    (:disable :line-length))))      ; Disable line-length for tests
           (cfg (config:parse-config sexp)))
      ;; For test files
      (let* ((rules (config:get-rules-for-file cfg #P"/tests/foo-test.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (member :unused-variables rule-names)
            ":for-paths should override project-wide :disable")
        (ok (not (member :line-length rule-names))
            ":for-paths should override project-wide :enable"))
      ;; For other files
      (let* ((rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (rule-names (mapcar #'rules:rule-name rules)))
        (ok (not (member :unused-variables rule-names))
            "Project-wide :disable should override :extends")
        (ok (member :line-length rule-names)
            "Project-wide :enable should work")))))

;;; Config discovery tests

(deftest find-config-file
  (testing "Find config in current directory"
    (let* ((tmpdir-str (format nil "/tmp/mallet-test-dir-~A" (get-universal-time)))
           (tmpdir (uiop:ensure-directory-pathname tmpdir-str))
           (config-file (merge-pathnames ".mallet.lisp" tmpdir)))
      (unwind-protect
           (progn
             (ensure-directories-exist tmpdir)
             (with-open-file (out config-file :direction :output :if-exists :supersede)
               (write-string "(:mallet-config)" out))
             (let ((found (config:find-config-file tmpdir)))
               (ok (not (null found)))
               ;; Compare truenames to handle pathname variations
               (ok (equal (truename found) (truename config-file)))))
        ;; Cleanup
        (when (probe-file config-file)
          (delete-file config-file))
        (when (probe-file tmpdir)
          (uiop:delete-directory-tree tmpdir :validate t)))))

  (testing "Find config in parent directory"
    (let* ((tmpdir-str (format nil "/tmp/mallet-test-parent-~A" (get-universal-time)))
           (tmpdir (uiop:ensure-directory-pathname tmpdir-str))
           (subdir (merge-pathnames "sub/" tmpdir))
           (config-file (merge-pathnames ".mallet.lisp" tmpdir)))
      (unwind-protect
           (progn
             (ensure-directories-exist subdir)
             (with-open-file (out config-file :direction :output :if-exists :supersede)
               (write-string "(:mallet-config)" out))
             ;; Search from subdir should find config in parent
             (let ((found (config:find-config-file subdir)))
               (ok (not (null found)))
               ;; Compare truenames to handle pathname variations
               (ok (equal (truename found) (truename config-file)))))
        ;; Cleanup
        (when (probe-file config-file)
          (delete-file config-file))
        (when (probe-file tmpdir)
          (uiop:delete-directory-tree tmpdir :validate t :if-does-not-exist :ignore))))))

;;; CLI override tests

;;; :set-severity directive tests

(deftest set-severity-directive
  (testing "Set severity for all rules in a category"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)  ; :cleanliness category, default :warning
                   (:set-severity :cleanliness :error)))
           (cfg (config:parse-config sexp)))
      (let ((rule (find :unused-variables (config:config-rules cfg)
                        :key #'rules:rule-name)))
        (ok (not (null rule)))
        (ok (eq :error (rules:rule-severity rule))))))

  (testing "Set severity does not affect rules in other categories"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)    ; :cleanliness
                   (:enable :wrong-otherwise)     ; :correctness, default :error
                   (:set-severity :cleanliness :info)))
           (cfg (config:parse-config sexp)))
      (let ((unused (find :unused-variables (config:config-rules cfg) :key #'rules:rule-name))
            (wrong (find :wrong-otherwise (config:config-rules cfg) :key #'rules:rule-name)))
        (ok (eq :info (rules:rule-severity unused)))
        (ok (eq :error (rules:rule-severity wrong))))))

  (testing "Duplicate :set-severity for same category — last one wins"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)     ; :cleanliness
                   (:set-severity :cleanliness :error)
                   (:set-severity :cleanliness :info)))  ; last wins
           (cfg (config:parse-config sexp)))
      (let ((unused (find :unused-variables (config:config-rules cfg) :key #'rules:rule-name)))
        (ok (eq :info (rules:rule-severity unused))
            "last :set-severity for a category should win"))))

  (testing "Multiple :set-severity directives for different categories"
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)     ; :cleanliness
                   (:enable :trailing-whitespace)  ; :format
                   (:set-severity :cleanliness :error)
                   (:set-severity :format :info)))
           (cfg (config:parse-config sexp)))
      (let ((unused (find :unused-variables (config:config-rules cfg) :key #'rules:rule-name))
            (whitespace (find :trailing-whitespace (config:config-rules cfg) :key #'rules:rule-name)))
        (ok (eq :error (rules:rule-severity unused)))
        (ok (eq :info (rules:rule-severity whitespace))))))

  (testing ":set-severity with :extends applies to inherited rules"
    (let* ((sexp '(:mallet-config
                   (:extends :default)
                   (:set-severity :format :error)))
           (cfg (config:parse-config sexp)))
      ;; Trailing whitespace is :format category and should be :error now
      (let ((rule (find :trailing-whitespace (config:config-rules cfg)
                        :key #'rules:rule-name)))
        (ok (not (null rule)))
        (ok (eq :error (rules:rule-severity rule)))))))

(deftest set-severity-validates-severity
  (testing "Invalid severity signals a descriptive error"
    (let ((sexp '(:mallet-config
                  (:enable :unused-variables)
                  (:set-severity :cleanliness :foo))))
      (ok (signals (config:parse-config sexp) 'simple-error)
          ":set-severity with invalid severity should signal an error")))

  (testing "Valid severities are accepted without error"
    (dolist (sev '(:error :warning :info))
      (let* ((sexp `(:mallet-config
                     (:enable :unused-variables)
                     (:set-severity :cleanliness ,sev)))
             (cfg (config:parse-config sexp)))
        (ok (not (null cfg))
            (format nil ":set-severity ~S should be accepted" sev))))))

(deftest set-severity-applies-to-for-paths
  (testing ":set-severity propagates into :for-paths rules"
    (let* ((sexp '(:mallet-config
                   (:enable :trailing-whitespace)    ; :format, default :info
                   (:set-severity :format :error)
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :unused-variables))))   ; :cleanliness, unaffected category
           (cfg (config:parse-config sexp)))
      ;; For test files: trailing-whitespace should have :error severity from :set-severity
      (let* ((path-rules (config:get-rules-for-file cfg #P"/tests/foo-test.lisp"))
             (whitespace-rule (find :trailing-whitespace path-rules :key #'rules:rule-name)))
        (ok (not (null whitespace-rule))
            ":trailing-whitespace should be present in :for-paths rules")
        (ok (eq :error (rules:rule-severity whitespace-rule))
            ":set-severity :format :error should apply to :for-paths rules"))))

  (testing ":set-severity applies to rules enabled inside :for-paths"
    (let* ((sexp '(:mallet-config
                   (:set-severity :format :error)
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :trailing-whitespace))))  ; :format category
           (cfg (config:parse-config sexp)))
      (let* ((path-rules (config:get-rules-for-file cfg #P"/tests/foo-test.lisp"))
             (whitespace-rule (find :trailing-whitespace path-rules :key #'rules:rule-name)))
        (ok (not (null whitespace-rule))
            ":trailing-whitespace should be in :for-paths rules")
        (ok (eq :error (rules:rule-severity whitespace-rule))
            ":set-severity should apply to rules enabled inside :for-paths"))))

  (testing ":set-severity and :for-paths: base rules also get correct severity"
    (let* ((sexp '(:mallet-config
                   (:enable :trailing-whitespace)     ; :format
                   (:set-severity :format :error)
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :unused-variables))))
           (cfg (config:parse-config sexp)))
      ;; For non-test files: trailing-whitespace severity should be :error
      (let* ((base-rules (config:get-rules-for-file cfg #P"/src/main.lisp"))
             (whitespace-rule (find :trailing-whitespace base-rules :key #'rules:rule-name)))
        (ok (not (null whitespace-rule)))
        (ok (eq :error (rules:rule-severity whitespace-rule))
            ":set-severity should apply to base rules too")))))

(deftest apply-cli-overrides-enable-rule
  (testing "Enable a rule that's not in base config"
    (let* ((base-config (config:get-built-in-config :default))
           (cli-rules '(:enable-rules ((:line-length :max 120))
                        :disable-rules ()))
           (merged (config:apply-cli-overrides base-config cli-rules)))
      (ok (find :line-length (config:config-rules merged)
                :key #'rules:rule-name))
      ;; Check that the option was applied
      (let ((rule (find :line-length (config:config-rules merged)
                        :key #'rules:rule-name)))
        (ok (not (null rule)))))))

(deftest apply-cli-overrides-disable-rule
  (testing "Disable a rule from base config"
    (let* ((base-config (config:get-built-in-config :default))
           (cli-rules '(:enable-rules ()
                        :disable-rules (:trailing-whitespace)))
           (merged (config:apply-cli-overrides base-config cli-rules)))
      (ok (not (find :trailing-whitespace (config:config-rules merged)
                     :key #'rules:rule-name)))
      (ok (member :trailing-whitespace (config:config-disabled-rules merged))))))

(deftest apply-cli-overrides-empty
  (testing "Empty CLI rules doesn't change config"
    (let* ((base-config (config:get-built-in-config :default))
           (cli-rules '(:enable-rules ()
                        :disable-rules ()))
           (merged (config:apply-cli-overrides base-config cli-rules)))
      ;; Should have same number of rules
      (ok (= (length (config:config-rules base-config))
             (length (config:config-rules merged)))))))

(deftest apply-cli-overrides-combined
  (testing "Complex combination of CLI overrides"
    (let* ((base-config (config:get-built-in-config :default))
           (cli-rules '(:enable-rules ((:line-length :max 120))
                        :disable-rules (:if-without-else)))
           (merged (config:apply-cli-overrides base-config cli-rules)))
      ;; line-length enabled with custom option
      (ok (find :line-length (config:config-rules merged)
                :key #'rules:rule-name))
      ;; if-without-else disabled
      (ok (member :if-without-else (config:config-disabled-rules merged))))))

;;; Severity precedence contract tests:
;;; per-rule :severity > :set-severity > rule default

(deftest per-rule-severity-wins-over-set-severity
  (testing "explicit per-rule :severity wins over :set-severity for the same category"
    ;; (:enable :unused-variables :severity :warning) wins over
    ;; (:set-severity :cleanliness :info) — per-rule is more specific.
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables :severity :warning)
                   (:set-severity :cleanliness :info)))
           (cfg (config:parse-config sexp)))
      (let ((rule (find :unused-variables (config:config-rules cfg)
                        :key #'rules:rule-name)))
        (ok (not (null rule)))
        (ok (eq :warning (rules:rule-severity rule))
            "per-rule :severity :warning wins over :set-severity :info"))))

  (testing ":set-severity applies to rules without explicit per-rule :severity"
    ;; A rule enabled without :severity gets the :set-severity override.
    (let* ((sexp '(:mallet-config
                   (:enable :unused-variables)
                   (:set-severity :cleanliness :error)))
           (cfg (config:parse-config sexp)))
      (let ((rule (find :unused-variables (config:config-rules cfg)
                        :key #'rules:rule-name)))
        (ok (not (null rule)))
        (ok (eq :error (rules:rule-severity rule))
            ":set-severity applies when no per-rule :severity is given"))))

  (testing "per-rule :severity wins over :set-severity inside :for-paths"
    (let* ((sexp '(:mallet-config
                   (:set-severity :cleanliness :error)
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :unused-variables :severity :warning))))
           (cfg (config:parse-config sexp)))
      (let* ((path-rules (config:get-rules-for-file cfg #P"/tests/foo-test.lisp"))
             (rule (find :unused-variables path-rules :key #'rules:rule-name)))
        (ok (not (null rule)))
        (ok (eq :warning (rules:rule-severity rule))
            "per-rule :severity :warning wins over :set-severity :error inside :for-paths")))))

;;; CLI --enable and :set-severity interaction tests

(deftest apply-cli-overrides-set-severity-interaction
  (testing "CLI-enabled rule not in base config gets :set-severity override applied"
    ;; When a rule is absent from the base config and enabled on the CLI without
    ;; an explicit :severity, the config's :set-severity override is applied.
    (let* ((sexp '(:mallet-config
                   (:extends :none)
                   (:set-severity :format :error)))
           (base-config (config:parse-config sexp))
           (cli-rules '(:enable-rules ((:trailing-whitespace))
                        :disable-rules ()))
           (merged (config:apply-cli-overrides base-config cli-rules)))
      (let ((rule (find :trailing-whitespace (config:config-rules merged)
                        :key #'rules:rule-name)))
        (ok (not (null rule)) ":trailing-whitespace should be in merged rules")
        (ok (eq :error (rules:rule-severity rule))
            "CLI-enabled rule gets :set-severity from config when no explicit :severity given"))))

  (testing "CLI-enabled rule that IS in base config preserves :set-severity from config"
    ;; When the rule already exists in the base config, Step 1 of
    ;; apply-cli-overrides copies the config-applied severity unless the
    ;; CLI provides an explicit :severity override.
    (let* ((sexp '(:mallet-config
                   (:enable :trailing-whitespace)   ; :format category
                   (:set-severity :format :error)))
           (base-config (config:parse-config sexp))
           (cli-rules '(:enable-rules ((:trailing-whitespace))
                        :disable-rules ()))
           (merged (config:apply-cli-overrides base-config cli-rules)))
      (let ((rule (find :trailing-whitespace (config:config-rules merged)
                        :key #'rules:rule-name)))
        (ok (not (null rule)))
        (ok (eq :error (rules:rule-severity rule))
            "Existing rule re-enabled by CLI preserves config-applied severity"))))

  (testing "Explicit CLI :severity wins over :set-severity (per-rule > set-severity)"
    (let* ((sexp '(:mallet-config
                   (:extends :none)
                   (:set-severity :format :error)))
           (base-config (config:parse-config sexp))
           (cli-rules '(:enable-rules ((:trailing-whitespace :severity :info))
                        :disable-rules ()))
           (merged (config:apply-cli-overrides base-config cli-rules)))
      (let ((rule (find :trailing-whitespace (config:config-rules merged)
                        :key #'rules:rule-name)))
        (ok (not (null rule)))
        (ok (eq :info (rules:rule-severity rule))
            "Explicit CLI :severity :info wins over :set-severity :error")))))
