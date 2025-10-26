(:mallet-config
 (:extends :default)
 (:ignore "/tests/fixtures/")
 (:enable :cyclomatic-complexity)
 (:enable :consecutive-blank-lines :max 2)
 (:for-paths ("/src/parser.lisp")
  (:disable :unused-local-nicknames))
 (:for-paths ("src/main.lisp")
  (:enable :cyclomatic-complexity :max 25)))
