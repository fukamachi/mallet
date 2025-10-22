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
                :rules (list (rules:make-rule :line-length :max-length 100)
                             (rules:make-rule :if-without-else))
                :disabled-rules '(:if-without-else))))
      (ok (not (null cfg)))
      (ok (= 2 (length (config:config-rules cfg))))
      (ok (member :if-without-else (config:config-disabled-rules cfg))))))

;;; Config parsing tests

(deftest parse-config
  (testing "Parse simple config with :enable"
    (let* ((sexp '(:mallet-config
                   (:enable :line-length :max-length 120)))
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
                              (:enable :line-length :max-length 100))"))
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
        (ok (member :if-without-else rule-names)))
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
        (ok (member :if-without-else rule-names))))))

;;; Config extends tests

(deftest config-extends
  (testing "Parse config with extends"
    (let* ((sexp '(:mallet-config
                   (:extends :all)
                   (:enable :line-length :max-length 100)))
           (cfg (config:parse-config sexp)))
      ;; Should inherit all rules from :all
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :line-length rule-names))
        (ok (member :trailing-whitespace rule-names)))
      ;; Check that line-length has the overridden max-length
      (let ((line-length-rule (find :line-length (config:config-rules cfg)
                                    :key #'rules:rule-name)))
        (ok (= 100 (rules:line-length-rule-max-length line-length-rule)))))))

(deftest parse-new-syntax-enable
  (testing "Parse config with :enable syntax"
    (let* ((sexp '(:mallet-config
                   (:enable :line-length :max-length 120)))
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
                   (:enable :line-length :max-length 100)
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
                    (:enable :line-length :max-length 120)
                    (:disable :unused-variables))))
           (cfg (config:parse-config sexp)))
      ;; Base config should have default rules
      (let ((rule-names (mapcar #'rules:rule-name (config:config-rules cfg))))
        (ok (member :unused-variables rule-names)))
      ;; Check that path-rules are stored
      (ok (= 1 (length (config:config-path-rules cfg))))))

  (testing "Parse config with directory name (shorthand)"
    (let* ((sexp '(:mallet-config
                   (:for-paths ("tests" "scripts")
                    (:disable :line-length))))
           (cfg (config:parse-config sexp)))
      ;; Check that path-rules are stored
      (ok (= 1 (length (config:config-path-rules cfg))))
      ;; Patterns should be expanded
      (let* ((path-rule (first (config:config-path-rules cfg)))
             (patterns (config:path-override-patterns path-rule)))
        (ok (= 2 (length patterns)))
        (ok (search "tests/**/*.{lisp,asd}" (first patterns)))
        (ok (search "scripts/**/*.{lisp,asd}" (second patterns)))))))

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
                   (:enable :line-length :max-length 80)
                   (:enable :unused-variables)
                   (:for-paths ("tests/**/*.lisp")
                    (:enable :line-length :max-length 120)
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
          (ok (= 120 (rules:line-length-rule-max-length line-length-rule))))))))

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
