(defpackage #:mallet/utils
  (:use #:cl)
  (:export
   #:symbol-name-from-string))
(in-package #:mallet/utils)

(defun symbol-name-from-string (str)
  "Extract the symbol name from a string representation.
Handles qualified symbols like \"PACKAGE:NAME\" → \"NAME\"
and unqualified symbols like \"NAME\" → \"NAME\"."
  (if (stringp str)
      (let ((colon-pos (position #\: str :from-end t)))
        (if colon-pos
            (subseq str (1+ colon-pos))
            str))
      str))
