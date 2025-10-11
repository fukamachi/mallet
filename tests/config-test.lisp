(defpackage #:malo/tests/config
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:config #:malo/config)))
(in-package #:malo/tests/config)

;;; Config data structure tests

(deftest config-creation
  (testing "Create empty config"
    (let ((cfg (config:make-config)))
      (ok (not (null cfg)))
      (ok (typep cfg 'config:config))))

  (testing "Create config with options (internal representation)"
    (let ((cfg (config:make-config
                :rules '((:line-length :enabled t :max-length 100)
                         (:if-without-else :enabled nil)))))
      (ok (not (null cfg)))
      (ok (config:rule-enabled-p cfg :line-length))
      (ok (not (config:rule-enabled-p cfg :if-without-else)))
      (ok (= 100 (config:get-rule-option cfg :line-length :max-length))))))

;;; Config parsing tests

(deftest parse-config
  (testing "Parse simple config with :enable"
    (let* ((sexp '(:malo-config
                   (:enable :line-length :max-length 120)))
           (cfg (config:parse-config sexp)))
      (ok (config:rule-enabled-p cfg :line-length))
      (ok (= 120 (config:get-rule-option cfg :line-length :max-length)))))

  (testing "Parse config with :disable"
    (let* ((sexp '(:malo-config
                   (:disable :if-without-else)))
           (cfg (config:parse-config sexp)))
      (ok (not (config:rule-enabled-p cfg :if-without-else)))))

  (testing "Parse config with severity override"
    (let* ((sexp '(:malo-config
                   (:enable :unused-variables :severity :error)))
           (cfg (config:parse-config sexp)))
      (ok (eq :error (config:get-rule-option cfg :unused-variables :severity))))))

;;; Config file loading tests

(defmacro with-temporary-config ((content pathname) &body body)
  `(uiop:with-temporary-file (:stream out
                              :pathname ,pathname
                              :direction :output
                              :prefix "malo-config"
                              :type "lisp")
     (write-string ,content out)
     (force-output out)
     ,@body))

(deftest load-config-file
  (testing "Load config from file"
    (let ((config-content "(:malo-config
                              (:enable :line-length :max-length 100))"))
      (with-temporary-config (config-content tmpfile)
        (let ((cfg (config:load-config tmpfile)))
          (ok (config:rule-enabled-p cfg :line-length))
          (ok (= 100 (config:get-rule-option cfg :line-length :max-length))))))))

;;; Config merging tests

(deftest merge-configs
  (testing "Merge two configs"
    (let ((base (config:make-config
                 :rules '((:line-length :enabled t :max-length 80)
                          (:if-without-else :enabled t))))
          (override (config:make-config
                    :rules '((:line-length :max-length 120)
                             (:unused-variables :enabled t)))))
      (let ((merged (config:merge-configs base override)))
        ;; line-length should have overridden max-length
        (ok (= 120 (config:get-rule-option merged :line-length :max-length)))
        ;; if-without-else should still be enabled from base
        (ok (config:rule-enabled-p merged :if-without-else))
        ;; unused-variables should be added from override
        (ok (config:rule-enabled-p merged :unused-variables))))))

;;; Built-in config tests

(deftest built-in-configs
  (testing "Load default config"
    (let ((cfg (config:get-built-in-config :default)))
      (ok (not (null cfg)))
      ;; Universally-accepted rules should be enabled
      (ok (config:rule-enabled-p cfg :trailing-whitespace))
      (ok (config:rule-enabled-p cfg :no-tabs))
      (ok (config:rule-enabled-p cfg :unused-variables))
      (ok (config:rule-enabled-p cfg :if-without-else))
      (ok (config:rule-enabled-p cfg :special-variable-naming))
      ;; Style preferences should be disabled
      (ok (not (config:rule-enabled-p cfg :line-length)))
      (ok (not (config:rule-enabled-p cfg :constant-naming)))))

  (testing "Load all config"
    (let ((cfg (config:get-built-in-config :all)))
      (ok (not (null cfg)))
      ;; All rules should be enabled
      (ok (config:rule-enabled-p cfg :line-length))
      (ok (config:rule-enabled-p cfg :trailing-whitespace))
      (ok (config:rule-enabled-p cfg :if-without-else)))))

;;; Config extends tests

(deftest config-extends
  (testing "Parse config with extends"
    (let* ((sexp '(:malo-config
                   (:extends :all)
                   (:enable :line-length :max-length 100)))
           (cfg (config:parse-config sexp)))
      ;; Should inherit all rules from :all
      (ok (config:rule-enabled-p cfg :line-length))
      (ok (config:rule-enabled-p cfg :trailing-whitespace))
      ;; But override specific options
      (ok (= 100 (config:get-rule-option cfg :line-length :max-length))))))

(deftest parse-new-syntax-enable
  (testing "Parse config with :enable syntax"
    (let* ((sexp '(:malo-config
                   (:enable :line-length :max-length 120)))
           (cfg (config:parse-config sexp)))
      (ok (config:rule-enabled-p cfg :line-length))
      (ok (= 120 (config:get-rule-option cfg :line-length :max-length)))))

  (testing "Parse config with :enable without options"
    (let* ((sexp '(:malo-config
                   (:enable :unused-variables)))
           (cfg (config:parse-config sexp)))
      (ok (config:rule-enabled-p cfg :unused-variables))))

  (testing "Parse config with multiple :enable forms"
    (let* ((sexp '(:malo-config
                   (:enable :line-length :max-length 100)
                   (:enable :unused-variables :severity :error)
                   (:enable :special-variable-naming)))
           (cfg (config:parse-config sexp)))
      (ok (config:rule-enabled-p cfg :line-length))
      (ok (= 100 (config:get-rule-option cfg :line-length :max-length)))
      (ok (config:rule-enabled-p cfg :unused-variables))
      (ok (eq :error (config:get-rule-option cfg :unused-variables :severity)))
      (ok (config:rule-enabled-p cfg :special-variable-naming)))))

(deftest parse-new-syntax-disable
  (testing "Parse config with :disable syntax"
    (let* ((sexp '(:malo-config
                   (:extends :default)
                   (:disable :constant-naming)))
           (cfg (config:parse-config sexp)))
      (ok (not (config:rule-enabled-p cfg :constant-naming)))))

  (testing "Parse config with multiple :disable forms"
    (let* ((sexp '(:malo-config
                   (:extends :default)
                   (:disable :line-length)
                   (:disable :consecutive-blank-lines)))
           (cfg (config:parse-config sexp)))
      (ok (not (config:rule-enabled-p cfg :line-length)))
      (ok (not (config:rule-enabled-p cfg :consecutive-blank-lines))))))

;;; Path override tests

(deftest parse-for-paths
  (testing "Parse config with :for-paths"
    (let* ((sexp '(:malo-config
                   (:extends :default)
                   (:for-paths ("tests/**/*.lisp")
                     (:enable :line-length :max-length 120)
                     (:disable :unused-variables))))
           (cfg (config:parse-config sexp)))
      ;; Base config should have default rules
      (ok (config:rule-enabled-p cfg :unused-variables))
      ;; Check that overrides are stored
      (ok (= 1 (length (config:config-overrides cfg))))))

  (testing "Parse config with directory name (shorthand)"
    (let* ((sexp '(:malo-config
                   (:for-paths ("tests" "scripts")
                     (:disable :line-length))))
           (cfg (config:parse-config sexp)))
      ;; Check that overrides are stored
      (ok (= 1 (length (config:config-overrides cfg))))
      ;; Patterns should be expanded
      (let* ((override (first (config:config-overrides cfg)))
             (patterns (car override)))
        (ok (= 2 (length patterns)))
        (ok (search "tests/**/*.{lisp,asd}" (first patterns)))
        (ok (search "scripts/**/*.{lisp,asd}" (second patterns)))))))

;;; Config discovery tests

(deftest find-config-file
  (testing "Find config in current directory"
    (let* ((tmpdir-str (format nil "/tmp/malo-test-dir-~A" (get-universal-time)))
           (tmpdir (uiop:ensure-directory-pathname tmpdir-str))
           (config-file (merge-pathnames ".malo.lisp" tmpdir)))
      (unwind-protect
          (progn
            (ensure-directories-exist tmpdir)
            (with-open-file (out config-file :direction :output :if-exists :supersede)
              (write-string "(:malo-config)" out))
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
    (let* ((tmpdir-str (format nil "/tmp/malo-test-parent-~A" (get-universal-time)))
           (tmpdir (uiop:ensure-directory-pathname tmpdir-str))
           (subdir (merge-pathnames "sub/" tmpdir))
           (config-file (merge-pathnames ".malo.lisp" tmpdir)))
      (unwind-protect
          (progn
            (ensure-directories-exist subdir)
            (with-open-file (out config-file :direction :output :if-exists :supersede)
              (write-string "(:malo-config)" out))
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
