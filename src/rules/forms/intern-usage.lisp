(defpackage #:mallet/rules/forms/intern-usage
  (:use #:cl)
  (:local-nicknames
   (#:utils #:mallet/utils))
  (:export #:*prohibited-intern-functions*
           #:prohibited-intern-function-p))
(in-package #:mallet/rules/forms/intern-usage)

;;; Target Function Registry
;;;
;;; The six prohibited symbol-interning functions, identified by their canonical
;;; package-qualified names. Package names are stored as full package names (not
;;; nicknames) to match how the Eclector parser represents them.

(defparameter *prohibited-intern-functions*
  '((:name "INTERN"          :package "COMMON-LISP" :display "cl:intern")
    (:name "UNINTERN"        :package "COMMON-LISP" :display "cl:unintern")
    (:name "INTERN*"         :package "UIOP"        :display "uiop:intern*")
    (:name "SYMBOLICATE"     :package "ALEXANDRIA"  :display "alexandria:symbolicate")
    (:name "FORMAT-SYMBOL"   :package "ALEXANDRIA"  :display "alexandria:format-symbol")
    (:name "MAKE-KEYWORD"    :package "ALEXANDRIA"  :display "alexandria:make-keyword"))
  "List of prohibited symbol-interning functions with their canonical packages.
Each entry is a plist with :name (symbol name, uppercase), :package (full package name),
and :display (human-readable canonical form).")

(defun extract-package-from-symbol-string (str)
  "Extract the package prefix from a symbol string like \"PKG:NAME\" or \"PKG::NAME\".
Returns NIL if the string has no colon (unqualified symbol).
Uses the first colon position, so double-colon access \"PKG::NAME\" returns \"PKG\"."
  (let ((colon-pos (position #\: str)))
    (when colon-pos
      (subseq str 0 colon-pos))))

(defun prohibited-intern-function-p (symbol-string)
  "Return the display name if SYMBOL-STRING refers to a prohibited intern function, NIL otherwise.

SYMBOL-STRING is a string representing a symbol as produced by the mallet parser:
- Qualified:   \"PACKAGE:name\" (package name is the full CL package name)
- Unqualified: \"name\"         (no colon prefix)

Matching rules:
- Qualified with a known package: matches by package AND name (avoids false positives).
- Unqualified (no colon):         matches by name only (conservative — flags likely usage).
- Qualified with an unknown package: no match."
  (unless (stringp symbol-string)
    (return-from prohibited-intern-function-p nil))
  (let ((name (utils:symbol-name-from-string symbol-string))
        (package (extract-package-from-symbol-string symbol-string)))
    (dolist (entry *prohibited-intern-functions*)
      (when (string-equal name (getf entry :name))
        (when (or (null package)
                  (string-equal package (getf entry :package)))
          (return-from prohibited-intern-function-p (getf entry :display)))))
    nil))
