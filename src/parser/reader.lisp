(in-package #:mallet/parser)

;;; Eclector parse-result based parser for precise source tracking
;;;
;;; This parser uses Eclector's parse-result protocol to track source positions
;;; for ALL expressions (including nested ones), not just top-level forms.
;;; This enables accurate violation reporting.

;;; Custom client for string-based parsing with source tracking

(defclass string-parse-result-client (eclector.parse-result:parse-result-client)
  ()
  (:documentation "Parse-result client that returns string representations of symbols."))

(defmethod eclector.reader:interpret-symbol
    ((client string-parse-result-client) input-stream package-indicator symbol-name internp)
  "Return a string representation of a symbol without interning.

Symbols are represented as strings:
- Keywords: \":NAME\"
- Qualified: \"PACKAGE:NAME\"
- Unqualified: \"NAME\"

This avoids package resolution issues when linting code with unknown packages."
  (declare (ignore input-stream internp))
  (cond
    ;; Keyword
    ((eq package-indicator :keyword)
     (concatenate 'string ":" symbol-name))
    ;; Unqualified symbol (no package indicator or in current package)
    ((or (null package-indicator)
         (eq package-indicator *package*)
         ;; Check if it's the CURRENT package used during parsing
         (and (packagep package-indicator)
              (or (string-equal (package-name package-indicator) "CURRENT")
                  (string-equal (package-name package-indicator) "COMMON-LISP-USER"))))
     symbol-name)
    ;; Qualified symbol (e.g., config:load-config)
    (t
     (concatenate 'string
                  (if (packagep package-indicator)
                      (package-name package-indicator)
                      (string package-indicator))
                  ":"
                  symbol-name))))

(defmethod eclector.reader:check-feature-expression ((client string-parse-result-client) feature-expression)
  "Check validity of feature expressions.
Delegates to default implementation."
  (call-next-method))

(defmethod eclector.reader:evaluate-feature-expression ((client string-parse-result-client) feature-expression)
  "Evaluate feature expressions for reader conditionals.
Always returns T to include all conditional code during linting."
  (declare (ignore feature-expression))
  ;; Always return T to include all conditional code
  ;; This ensures we lint all code regardless of features
  t)

(defmethod eclector.reader:evaluate-expression ((client string-parse-result-client) expression)
  "Handle read-time evaluation (#. reader macro).
Returns a placeholder list since we cannot evaluate expressions with string-based symbols.
Returns a list to ensure it can be safely processed by recursive form-checking code."
  (declare (ignore expression))
  ;; Return a placeholder list for read-time evaluated expressions
  ;; We can't actually evaluate since symbols are strings
  ;; Using a list ensures it won't cause type errors when embedded in parent forms
  '(:read-time-eval-placeholder))

(defmethod eclector.reader:call-reader-macro :around ((client string-parse-result-client) input-stream char readtable)
  "Wrap reader macro calls to handle unknown reader macros gracefully."
  (handler-case
      (call-next-method)
    (eclector.readtable:unknown-macro-sub-character ()
      ;; Unknown dispatch reader macro (e.g., #?"..." from cl-interpol)
      ;; The stream is after the dispatch char (#) but we need to consume the sub-char
      ;; Then consume what follows using standard read
      (handler-case
          (cl:read input-stream)
        ;; If that also fails, just continue
        (error ()
          nil))
      ;; Return a placeholder
      ':unknown-reader-macro)))

(defmethod eclector.reader:find-character ((client string-parse-result-client) (designator string))
  "Find character by name, supporting SBCL character name extensions.

SBCL supports these character name extensions:
- ASCII control characters (codes 0-31):
  * Nul, Null (code 0) - both variants accepted
  * Soh, Stx, Etx, Eot, Enq, Ack, Bel (codes 1-7)
  * Vt, Ff, Cr, So, Si (codes 11-15)
  * Dle, Dc1-Dc4, Nak, Syn, Etb, Can, Em, Sub (codes 16-26)
  * Esc, Escape (code 27) - both variants accepted
  * Fs, Gs, Rs, Us (codes 28-31)
- Standard character names:
  * Backspace (code 8), Tab (code 9), Newline (code 10)
  * Page (code 12), Return (code 13), Space (code 32), Rubout (code 127)
- Unicode hex escapes (variable-length):
  * #\\uXX, #\\uXXX, #\\uXXXX - e.g., #\\u2602 = ☂
  * #\\UXXXXXXXX - e.g., #\\U0001F600 = 😀
- Unicode character names (via name-char):
  * #\\HIRAGANA_LETTER_A, #\\GREEK_SMALL_LETTER_ALPHA, etc.

The DESIGNATOR parameter contains the exact character name as written in source,
allowing future linting rules to distinguish between variants like #\\Esc vs #\\Escape."
  ;; First try standard character names
  (or (call-next-method)
      ;; If not found, try SBCL extensions
      (cond
        ;; Unicode character: #\uXX, #\uXXX, #\uXXXX, #\UXXXXXXXX
        ;; Must check that ALL remaining chars are hex digits to avoid matching "Us"
        ((and (> (length designator) 1)
              (or (char= (char designator 0) #\u)
                  (char= (char designator 0) #\U))
              ;; Check if all remaining characters are valid hex digits
              (let ((hex-string (subseq designator 1)))
                (and (> (length hex-string) 0)
                     (every (lambda (c)
                              (or (char<= #\0 c #\9)
                                  (char<= #\a c #\f)
                                  (char<= #\A c #\F)))
                            hex-string))))
         (let* ((hex-string (subseq designator 1))
                (code-point (parse-integer hex-string :radix 16)))
           (code-char code-point)))
        ;; Use name-char which looks up characters by their names
        (t (name-char designator)))))

(defmethod eclector.parse-result:make-expression-result
    ((client string-parse-result-client) expression children source)
  "Create a parse result wrapping EXPRESSION with source location.

The result is a plist with :expr, :children, and :source keys.
SOURCE is a cons cell (start . end) of character positions."
  (list :expr expression :children children :source source))

;;; Helper functions for position conversion

(defun build-line-starts (text)
  "Build a vector of line start positions in TEXT.
Returns a vector where element N is the character position of line N (0-based)."
  (let ((len (length text)))
    (coerce
      (cons 0
            (loop for i from 0 below len
                  when (char= (char text i) #\Newline)
                  collect (1+ i)))
      'vector)))

(defun char-pos-to-line-column (char-pos line-starts)
  "Convert character position CHAR-POS to (line . column).
LINE-STARTS is a vector from BUILD-LINE-STARTS.
Returns 1-based line and 0-based column."
  (let ((line-count (length line-starts)))
    (loop for line-num from 1 below line-count
          for line-start = (aref line-starts line-num)
          when (< char-pos line-start)
          do (let ((prev-line-start (aref line-starts (1- line-num))))
               (return (values line-num (- char-pos prev-line-start))))
          finally
          (return (values line-count
                          (- char-pos (aref line-starts (1- line-count))))))))

(defun extract-source (text source)
  "Extract source text from TEXT using SOURCE range (start . end)."
  (when (and source (consp source))
    (let ((start (car source))
          (end (cdr source)))
      (when (and (numberp start) (numberp end)
                 (<= 0 start end (length text)))
        (subseq text start end)))))

;;; Position map - stores source positions for expressions

(defvar *position-map* nil
  "Hash table mapping expressions to source positions.")

(defun make-position-map ()
  "Create a new position map.
Uses EQ test for object identity - each symbol from interpret-symbol
is a unique string object, so we can store positions for all occurrences."
  (make-hash-table :test 'eq))

(defun build-position-map (parse-result line-starts)
  "Build a position map from PARSE-RESULT.
Returns a hash table mapping expressions to (line . column) positions."
  (let ((position-map (make-position-map)))
    (labels ((process-result (result)
               (when (and result (listp result))
                 (let ((expr (getf result :expr))
                       (source (getf result :source))
                       (children (getf result :children)))
                   ;; Map this expression to its position
                   ;; Since we use EQ test, each unique object (including duplicate variable names
                   ;; in different scopes) gets its own position entry. Still check if already
                   ;; present to avoid reprocessing the same object if seen multiple times.
                   (when (and source (not (gethash expr position-map)))
                     (multiple-value-bind (line column)
                         (char-pos-to-line-column (car source) line-starts)
                       (setf (gethash expr position-map) (cons line column))))
                   ;; Process children
                   (when children
                     (dolist (child children)
                       (process-result child)))))))
      (process-result parse-result))
    position-map))

(defun find-position (expr position-map fallback-line fallback-column)
  "Find position for EXPR in POSITION-MAP.
Returns (values line column). If not found, returns fallback values."
  (let ((pos (gethash expr position-map)))
    (if pos
        (values (car pos) (cdr pos))
        (values fallback-line fallback-column))))

;;; Helper function to find unmatched opening paren

(defun find-unmatched-opener (text start-pos)
  "Find the position of an unmatched opening paren in TEXT starting from START-POS.
Returns the character position of the unmatched opener, or NIL if balanced."
  (let ((depth 0)
        (in-string nil)
        (in-comment nil)
        (escape-next nil)
        (unmatched-pos nil))
    (loop for i from start-pos below (length text)
          for char = (char text i)
          do (cond
               ;; Handle escape in strings
               (escape-next
                (setf escape-next nil))
               ;; Handle strings
               ((and (char= char #\\) in-string)
                (setf escape-next t))
               ((and (char= char #\") (not in-comment))
                (setf in-string (not in-string)))
               ;; Handle comments
               ((and (char= char #\;) (not in-string))
                (setf in-comment t))
               ((and (char= char #\Newline) in-comment)
                (setf in-comment nil))
               ;; Handle parens (only when not in string or comment)
               ((and (not in-string) (not in-comment))
                (cond
                  ((char= char #\()
                   (when (zerop depth)
                     (setf unmatched-pos i))
                   (incf depth))
                  ((char= char #\))
                   (decf depth))))))
    ;; If depth > 0, we have unmatched openers
    (when (plusp depth)
      unmatched-pos)))

;;; Main parsing function

(defun parse-forms (text file)
  "Parse forms from TEXT in FILE using Eclector parse-result.
Returns (values forms parse-errors) where:
- forms is a list of FORM objects with source location information
- parse-errors is a list of PARSE-ERROR-INFO objects for any parse errors encountered

Uses eclector.parse-result to track positions for all nested expressions,
enabling accurate violation reporting."
  (check-type text string)
  (check-type file pathname)

  (let ((forms '())
        (parse-errors '())
        (client (make-instance 'string-parse-result-client))
        (stream (make-string-input-stream text))
        (line-starts (build-line-starts text))
        (last-end-pos 0))  ; Track end position of last successful form

    ;; Set readtable case to :preserve to maintain original case
    (setf (eclector.readtable:readtable-case eclector.readtable:*readtable*) :preserve)

    ;; Read all forms
    (loop
      (handler-case
          (let ((result (eclector.parse-result:read client stream nil :eof)))
            (when (eq result :eof)
              (return))

            (let* ((expr (getf result :expr))
                   (source (getf result :source))
                   (start-pos (car source))
                   (end-pos (cdr source)))

              (multiple-value-bind (start-line start-column)
                  (char-pos-to-line-column start-pos line-starts)
                (multiple-value-bind (end-line end-column)
                    (char-pos-to-line-column end-pos line-starts)

                  ;; Build position map for this form
                  (let ((position-map (build-position-map result line-starts)))

                    ;; Track end position for EOF error reporting
                    (setf last-end-pos end-pos)

                    ;; Create form object
                    (push (make-instance 'form
                                         :expr expr
                                         :file file
                                         :line start-line
                                         :column start-column
                                         :end-line end-line
                                         :end-column end-column
                                         :source (extract-source text source)
                                         :position-map position-map)
                          forms))))))

        (end-of-file ()
          ;; EOF error - find position of unmatched opening parenthesis
          (let ((unmatched-pos (find-unmatched-opener text last-end-pos)))
            (if unmatched-pos
                (multiple-value-bind (line column)
                    (char-pos-to-line-column unmatched-pos line-starts)
                  (push (make-instance 'parse-error-info
                                       :message "Unmatched opening parenthesis"
                                       :file file
                                       :line line
                                       :column column)
                        parse-errors))
                ;; Fallback if we can't find the unmatched opener
                (let ((pos (file-position stream)))
                  (multiple-value-bind (line column)
                      (char-pos-to-line-column pos line-starts)
                    (push (make-instance 'parse-error-info
                                         :message "Unexpected end of file"
                                         :file file
                                         :line line
                                         :column column)
                          parse-errors)))))
          (return))
        ;; Unknown reader macros (e.g., #?"..." from cl-interpol)
        ;; We can't parse the current top-level form, so skip it entirely
        (eclector.readtable:unknown-macro-sub-character ()
          ;; The error occurred while reading a top-level form
          ;; Try to skip to the next complete top-level form by reading with standard reader
          ;; which may have the reader macro defined (or will error in a different way)
          (handler-case
              (cl:read stream nil :eof)
            (end-of-file ()
              (return))
            ;; If standard reader also fails, continue anyway
            (error ()
              nil)))
        ;; Quote without following object (from malformed reader macro)
        (eclector.reader:object-must-follow-quote ()
          ;; Skip and continue
          nil)
        ;; Unmatched closing paren
        (eclector.reader:invalid-context-for-right-parenthesis ()
          (let ((pos (file-position stream)))
            (multiple-value-bind (line column)
                (char-pos-to-line-column pos line-starts)
              (push (make-instance 'parse-error-info
                                   :message "Unmatched closing parenthesis"
                                   :file file
                                   :line line
                                   :column column)
                    parse-errors)))
          ;; Continue parsing to find more errors
          nil)
        ;; Other reader errors we can't handle - re-signal
        (eclector.base:stream-position-reader-error (e)
          (let* ((pos (file-position stream))
                 ;; Extract a snippet of text around the error position
                 (snippet-start (max 0 (- pos 10)))
                 (snippet-end (min (length text) (+ pos 20)))
                 (snippet (subseq text snippet-start snippet-end))
                 ;; Find where the error position is in the snippet
                 (marker-pos (- pos snippet-start)))
            (multiple-value-bind (line column)
                (char-pos-to-line-column pos line-starts)
              (format *error-output* "~%Warning: Skipping form at ~A:~D:~D (unknown reader macro)~%"
                      file line column)
              (format *error-output* "  Near: ~S~%" snippet)
              (format *error-output* "        ~v@T^--- here~%"  marker-pos)))
          ;; Try to skip the entire top-level form using standard reader
          ;; This preserves context (like backquote) better than skipping to whitespace
          (handler-case
              (cl:read stream nil :eof)
            (end-of-file ()
              (return))
            ;; If standard reader also fails, skip to next whitespace as fallback
            (error ()
              (loop for char = (read-char stream nil :eof)
                    until (or (eq char :eof)
                              (member char '(#\Space #\Tab #\Newline #\Return)))
                    finally (when (not (eq char :eof))
                              (unread-char char stream))))))
        (error (e)
          ;; Other parse errors (e.g., reader errors, syntax errors)
          (let ((pos (file-position stream)))
            (multiple-value-bind (line column)
                (char-pos-to-line-column pos line-starts)
              (let ((message (if (and (find-symbol "*DEBUG-MODE*" "MALO")
                                      (symbol-value (find-symbol "*DEBUG-MODE*" "MALO")))
                                 (format nil "Parse error: ~A" e)
                                 "Parse error (use --debug for details)")))
                (push (make-instance 'parse-error-info
                                     :message message
                                     :file file
                                     :line line
                                     :column column)
                      parse-errors)))
            (return)))))

    (values (nreverse forms) (nreverse parse-errors))))
