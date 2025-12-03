(defpackage #:mallet/rules/tokens/bare-float-literal
  (:use #:cl)
  (:local-nicknames
   (#:base #:mallet/rules/base)
   (#:parser #:mallet/parser)
   (#:violation #:mallet/violation))
  (:export #:bare-float-literal-rule))
(in-package #:mallet/rules/tokens/bare-float-literal)

(defclass bare-float-literal-rule (base:rule)
  ()
  (:default-initargs
   :name :bare-float-literal
   :description "Float literals should have explicit type markers (f, d, s, l)"
   :severity :info
   :type :token)
  (:documentation "Rule to detect float literals without explicit precision."))

(defun valid-float-p (raw)
  "Check if RAW is a valid float literal using the Lisp reader.
Returns T if RAW parses as a float, NIL otherwise."
  (handler-case
      (floatp (read-from-string raw))
    (error () nil)))

(defun has-explicit-type-marker-p (raw)
  "Check if RAW has an explicit type marker (d, f, s, l) followed by exponent.
Returns T if the float has explicit precision."
  (loop for i from 0 below (length raw)
        for c = (char raw i)
        when (member c '(#\d #\D #\f #\F #\s #\S #\l #\L))
          do (let ((rest (subseq raw (1+ i))))
               (when (and (plusp (length rest))
                          (or (digit-char-p (char rest 0))
                              (and (member (char rest 0) '(#\+ #\-))
                                   (> (length rest) 1)
                                   (digit-char-p (char rest 1)))))
                 (return t)))
        finally (return nil)))

(defun bare-float-p (raw)
  "Check if RAW is a bare float literal (type depends on *read-default-float-format*).
Returns T if it's a float without explicit type marker."
  (and (stringp raw)
       (valid-float-p raw)
       (not (has-explicit-type-marker-p raw))))

(defmethod base:check-tokens ((rule bare-float-literal-rule) tokens file)
  "Check for bare float literals in TOKENS."
  (loop for token in tokens
        when (and (eq (parser:token-type token) :number)
                  (bare-float-p (parser:token-raw token)))
          collect (make-instance 'violation:violation
                                 :rule :bare-float-literal
                                 :file file
                                 :line (parser:token-line token)
                                 :column (parser:token-column token)
                                 :severity (base:rule-severity rule)
                                 :message "Use explicit float precision (e.g., 1.0f0 or 1.0d0)"
                                 :fix nil)))
