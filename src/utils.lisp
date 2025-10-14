(defpackage #:mallet/utils
  (:use #:cl)
  (:export
   #:symbol-name-from-string
   #:keyword-string-p
   #:lambda-list-keyword-p
   #:form-head-matches-p
   #:proper-list-length
   #:proper-list-of-min-length-p
   #:proper-list-of-exact-length-p
   #:proper-list-of-length-range-p))
(in-package #:mallet/utils)

(defun symbol-name-from-string (str)
  "Extract the symbol name from a string representation.
Handles qualified symbols like \"PACKAGE:NAME\" → \"NAME\"
and unqualified symbols like \"NAME\" → \"NAME\".
Also handles Eclector reader objects gracefully."
  (cond
    ;; Normal string - extract symbol name
    ((stringp str)
     (let ((colon-pos (position #\: str :from-end t :test #'char=)))
       (if colon-pos
           (subseq str (1+ colon-pos))
           str)))
    ;; Eclector UNQUOTE object: (ECLECTOR.READER:UNQUOTE "string")
    ((and (consp str)
          (symbolp (first str))
          (or (eq (first str) 'eclector.reader:unquote)
              (and (eq (symbol-package (first str))
                      (find-package "ECLECTOR.READER"))
                   (string-equal (symbol-name (first str)) "UNQUOTE"))))
     ;; Recursively process the unquoted value
     (when (rest str)
       (symbol-name-from-string (second str))))
    ;; Other Eclector objects or unknown types - try to get a string representation
    ((consp str)
     (format nil "~A" (first str)))
    ;; Last resort - return as-is
    (t str)))

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

(defun form-head-matches-p (head name)
  "Check if HEAD is a non-keyword string matching NAME (case-insensitive).
Useful for identifying special forms while excluding keyword arguments."
  (and (stringp head)
       (not (keyword-string-p head))
       (string-equal (symbol-name-from-string head) name)))

(defun proper-list-length (list)
  "Return the length of LIST if it is a proper list, or NIL otherwise.
This checks for proper list structure and calculates length in a single traversal,
unlike calling both PROPER-LIST-P and LENGTH separately."
  (when (listp list)
    (let ((length 0))
      (loop for tail = list then (cdr tail)
            while (consp tail)
            do (incf length)
            finally (return (if (null tail) length nil))))))

(defun proper-list-of-min-length-p (list min-length)
  "Check if LIST is a proper list with at least MIN-LENGTH elements.
Returns T if LIST is proper and has length >= MIN-LENGTH, NIL otherwise.
This combines proper-list-p and length checking in a single traversal for efficiency."
  (let ((len (proper-list-length list)))
    (and len (>= len min-length))))

(defun proper-list-of-exact-length-p (list length)
  "Check if LIST is a proper list with exactly LENGTH elements.
Returns T if LIST is proper and has length = LENGTH, NIL otherwise.
This combines proper-list-p and length checking in a single traversal for efficiency."
  (let ((len (proper-list-length list)))
    (and len (= len length))))

(defun proper-list-of-length-range-p (list min-length max-length)
  "Check if LIST is a proper list with length between MIN-LENGTH and MAX-LENGTH (inclusive).
Returns T if LIST is proper and has MIN-LENGTH <= length <= MAX-LENGTH, NIL otherwise.
This combines proper-list-p and length checking in a single traversal for efficiency."
  (let ((len (proper-list-length list)))
    (and len (>= len min-length) (<= len max-length))))
