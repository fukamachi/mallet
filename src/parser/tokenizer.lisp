(defpackage #:malvolio/parser/tokenizer
  (:use #:cl)
  (:local-nicknames
   (#:parser #:malvolio/parser))
  (:export #:tokenize))
(in-package #:malvolio/parser/tokenizer)

(defun tokenize (text file)
  "Tokenize TEXT from FILE, preserving comments and positions.
Returns a list of TOKEN objects."
  (declare (ignore text file))
  ;; Stub implementation - return empty list for now
  '())
