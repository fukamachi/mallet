(defpackage #:mallet/rules/tokens/double-colon-access
  (:use #:cl)
  (:local-nicknames
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation))
  (:export #:double-colon-access-rule))
(in-package #:mallet/rules/tokens/double-colon-access)

(defclass double-colon-access-rule (base:rule)
  ((include-tests
    :initarg :include-tests
    :initform nil
    :reader include-tests-p
    :type boolean
    :documentation "When NIL (default), skip files whose defpackage uses a test framework.
Set to T to check test files as well."))
  (:default-initargs
   :name :double-colon-access
   :description "Avoid accessing internal symbols via double-colon (::) package qualifier"
   :severity :warning
   :category :practice
   :type :token)
  (:documentation "Rule to detect package::internal-symbol access which bypasses encapsulation.
Using :: to access internal symbols couples code to package internals, making it
fragile and violating encapsulation. Prefer using only exported symbols (single colon).

Test files are exempt by default because they legitimately need to access internals
for white-box testing. Set :include-tests T to check even in test files."))

(defun double-colon-symbol-p (raw)
  "Check if RAW is a symbol with double-colon package qualifier.
Returns T if RAW contains '::' which indicates internal symbol access.

Examples:
  'foo::bar'        => T (double-colon access)
  'foo:bar'         => NIL (public single-colon access)
  '#:foo'           => NIL (uninterned symbol)
  ':keyword'        => NIL (keyword)
  'foo::bar\\'      => NIL (tokenizer artifact from escaped quotes in strings)"
  (and (stringp raw)
       ;; Exclude tokenizer artifacts: symbols ending with backslash are produced
       ;; when the tokenizer encounters escaped quotes inside string literals
       ;; (e.g., \"PKG::NAME\" inside a docstring).  These are not real symbols.
       (not (and (plusp (length raw))
                 (char= (char raw (1- (length raw))) #\\)))
       (let ((pos (search "::" raw)))
         (and pos
              ;; The :: must have something before it (the package name)
              (> pos 0)
              ;; Ensure the char before :: is not # (to exclude #:: forms)
              (not (char= (char raw (1- pos)) #\#))))))

(defmethod base:check-tokens ((rule double-colon-access-rule) tokens file)
  "Check for double-colon internal symbol access in TOKENS.
Skips files that use a known test framework unless include-tests is enabled."
  (unless (and (not (include-tests-p rule))
               (base:tokens-use-test-framework-p tokens))
    (loop for token in tokens
          when (and (eq (parser:token-type token) :symbol)
                    (double-colon-symbol-p (parser:token-raw token)))
            collect (make-instance 'violation:violation
                                   :rule :double-colon-access
                                   :file file
                                   :line (parser:token-line token)
                                   :column (parser:token-column token)
                                   :severity (base:rule-severity rule)
                                   :message (format nil "Avoid internal symbol access via '::': ~A"
                                                    (parser:token-raw token))
                                   :fix nil))))
