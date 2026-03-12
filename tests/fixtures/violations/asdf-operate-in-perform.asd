;; Test fixture for asdf-operate-in-perform rule
;; asdf:load-system and friends must not be called inside :perform bodies

(defsystem "asdf-operate-in-perform"
  :depends-on ("alexandria")
  :components ((:file "main"))
  :perform (test-op (o c)
    (asdf:load-system "asdf-operate-in-perform/tests")
    (asdf:test-system "asdf-operate-in-perform/tests")))
