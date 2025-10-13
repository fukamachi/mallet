(defpackage #:mallet/utils
  (:use #:cl)
  (:export
   #:symbol-name-from-string
   #:keyword-string-p
   #:lambda-list-keyword-p))
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

(defun keyword-string-p (str)
  "Check if STR is a keyword string (starts with colon)."
  (and (stringp str)
       (> (length str) 0)
       (char= (char str 0) #\:)))

(defun lambda-list-keyword-p (elem)
  "Check if ELEM is a lambda-list keyword (&optional, &key, etc.)."
  (and (stringp elem)
       (let ((name (symbol-name-from-string elem)))
         (and (> (length name) 0)
              (char= (char name 0) #\&)))))
