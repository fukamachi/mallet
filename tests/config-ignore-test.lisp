(defpackage #:malo/tests/config-ignore
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:config #:malo/config)))
(in-package #:malo/tests/config-ignore)

(deftest parse-ignore-directive
  (testing "Parse single ignore pattern"
    (let ((cfg (config:parse-config
                '(:malo-config
                  (:extends :default)
                  (:ignore "vendor/")))))
      (ok (equal (config:config-ignore cfg) '("vendor/")))))

  (testing "Parse multiple ignore patterns"
    (let ((cfg (config:parse-config
                '(:malo-config
                  (:extends :default)
                  (:ignore "vendor/" "build/" "**/*-generated.lisp")))))
      (ok (equal (config:config-ignore cfg)
                 '("vendor/" "build/" "**/*-generated.lisp")))))

  (testing "Empty ignore list by default"
    (let ((cfg (config:parse-config '(:malo-config (:extends :default)))))
      (ok (null (config:config-ignore cfg))))))

(deftest file-ignored-p-basic
  (testing "Match wildcard pattern"
    (let ((cfg (config:parse-config
                '(:malo-config
                  (:extends :default)
                  (:ignore "**/*-generated.lisp")))))
      (ok (config:file-ignored-p cfg #P"/project/src/foo-generated.lisp"))
      (ok (config:file-ignored-p cfg #P"/project/build/bar-generated.lisp"))
      (ok (not (config:file-ignored-p cfg #P"/project/src/main.lisp")))))

  (testing "No ignore patterns - nothing ignored"
    (let ((cfg (config:parse-config '(:malo-config (:extends :default)))))
      (ok (not (config:file-ignored-p cfg #P"/project/anything.lisp"))))))

(deftest merge-configs-with-ignore
  (testing "Merge ignore patterns from base and override"
    (let* ((base (config:make-config :ignore '("vendor/")))
           (override (config:make-config :ignore '("build/")))
           (merged (config:merge-configs base override)))
      (ok (equal (config:config-ignore merged) '("vendor/" "build/")))))

  (testing "Override with empty ignore list"
    (let* ((base (config:make-config :ignore '("vendor/")))
           (override (config:make-config))
           (merged (config:merge-configs base override)))
      (ok (equal (config:config-ignore merged) '("vendor/")))))

  (testing "Base with empty ignore list"
    (let* ((base (config:make-config))
           (override (config:make-config :ignore '("build/")))
           (merged (config:merge-configs base override)))
      (ok (equal (config:config-ignore merged) '("build/"))))))
