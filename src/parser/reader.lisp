; mallet:suppress package-per-file implementation file shares mallet/parser package defined in parser.lisp
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

   For most features, returns T to include all conditional code during linting.
   However, #+mallet is special - it should only be included when :mallet is in *features*,
   so suppression declarations don't affect normal code loading."
  (if (eq feature-expression :mallet)
      (member :mallet *features*)
      t))

(defmethod eclector.reader:evaluate-expression ((client string-parse-result-client) expression)
  "Handle read-time evaluation (#. reader macro).
Returns a placeholder list since we cannot evaluate expressions with string-based symbols.
Returns a list to ensure it can be safely processed by recursive form-checking code."
  (declare (ignore expression))
  '(:read-time-eval-placeholder))

(defmethod eclector.reader:call-reader-macro :around ((client string-parse-result-client) input-stream char readtable)
  "Wrap reader macro calls to handle unknown reader macros gracefully.

For #? (cl-interpol), reads the following string and extracts interpolation
references, returning a synthetic (PROGN ref1 ref2 ...) form so downstream
rules can see variable references. For plain strings with no interpolation,
returns :unknown-reader-macro. All other unknown dispatch characters also
return :unknown-reader-macro after consuming the following form."
  (handler-case
      (call-next-method)
    (eclector.readtable:unknown-macro-sub-character (condition)
      ;; Extract sub-char from eclector-internal slot.  This accesses
      ;; an implementation detail (%SUB-CHAR) that may change across
      ;; eclector versions.  If the slot lookup fails, fall back to
      ;; consuming the next form and returning :unknown-reader-macro.
      (let ((sub (handler-case
                     (slot-value condition
                                (load-time-value
                                 (find-symbol "%SUB-CHAR" "ECLECTOR.READTABLE")))
                   (error () nil))))
        (cond
          ;; cl-interpol #?"..." — extract interpolated references
          ((and sub (char= sub #\?))
           (let ((str (handler-case (cl:read input-stream)
                        (error () nil))))
             (if (stringp str)
                 (let ((forms (extract-interpolation-forms str)))
                   (if forms
                       (list* "PROGN" forms)
                       ':unknown-reader-macro))
                 ':unknown-reader-macro)))
          ;; Other unknown dispatch macros (or failed sub-char extraction)
          ;; — consume and return placeholder
          (t
           (handler-case (cl:read input-stream) (error () nil))
           ':unknown-reader-macro))))))

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


(defun handle-eof-error (file stream last-end-pos text line-starts parse-errors)
  "Handle end-of-file error by finding unmatched opener."
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
        (let ((pos (file-position stream)))
          (multiple-value-bind (line column)
              (char-pos-to-line-column pos line-starts)
            (push (make-instance 'parse-error-info
                                 :message "Unexpected end of file"
                                 :file file
                                 :line line
                                 :column column)
                  parse-errors))))))

(defun handle-unmatched-closer (file stream line-starts parse-errors)
  "Handle unmatched closing parenthesis error."
  (let ((pos (file-position stream)))
    (multiple-value-bind (line column)
        (char-pos-to-line-column pos line-starts)
      (push (make-instance 'parse-error-info
                           :message "Unmatched closing parenthesis"
                           :file file
                           :line line
                           :column column)
            parse-errors))))

;;; cl-interpol interpolation extractor

(defun find-matching-brace (text start)
  "Find the position of the matching closing brace in TEXT starting after START.
START should be the position right after the opening '{'.
Handles nested braces and string literals (braces inside strings are ignored).
Returns the position of the matching '}', or NIL if not found."
  (let ((depth 1)
        (i start)
        (len (length text))
        (in-string nil)
        (escape-next nil))
    (loop while (and (< i len) (plusp depth))
          do (let ((ch (char text i)))
               (cond
                 (escape-next
                  (setf escape-next nil))
                 ((and (char= ch #\\) in-string)
                  (setf escape-next t))
                 ((char= ch #\")
                  (setf in-string (not in-string)))
                 ((not in-string)
                  (cond
                    ((char= ch #\{) (incf depth))
                    ((char= ch #\}) (decf depth))))))
          do (incf i))
    (when (zerop depth)
      (1- i))))

(defun parse-one-form-from-string (content)
  "Parse one Lisp form from CONTENT string using eclector.
Returns the parsed expression (just the :expr part), or NIL on failure."
  (when (and content (> (length (string-trim '(#\Space #\Tab #\Newline) content)) 0))
    (handler-case
        (let* ((client (make-instance 'string-parse-result-client))
               (stream (make-string-input-stream content))
               (result (eclector.parse-result:read client stream nil :eof)))
          (when (and result (not (eq result :eof)))
            (getf result :expr)))
      (error () nil))))

(defun parse-all-forms-from-string (content)
  "Parse all Lisp forms from CONTENT string using eclector.
Returns a list of parsed expressions (just :expr parts), skipping errors."
  (when (and content (> (length (string-trim '(#\Space #\Tab #\Newline) content)) 0))
    (let ((client (make-instance 'string-parse-result-client))
          (stream (make-string-input-stream content))
          (forms '()))
      (loop
        (handler-case
            (let ((result (eclector.parse-result:read client stream nil :eof)))
              (if (eq result :eof)
                  (return)
                  (let ((expr (getf result :expr)))
                    (when expr
                      (push expr forms)))))
          (error () (return))))
      (nreverse forms))))

(defun extract-interpolation-forms (string-body)
  "Scan STRING-BODY (contents of a cl-interpol #?\"...\" string) for ${...} and @{...}
patterns and parse each into Lisp forms.

Handles:
- ${expr}: parse one form
- @{func args...}: parse all forms
- Nested braces inside interpolation blocks
- Empty interpolation or parse errors: skip

Note: cl:read has already consumed one level of backslash escaping before
this function sees the string contents, so we do NOT apply any manual
escape handling for \\$ or \\@ here.

Returns a flat list of all parsed expressions."
  (let ((forms '())
        (i 0)
        (len (length string-body)))
    (loop while (< i len)
          do (let ((ch (char string-body i)))
               (cond
                 ;; ${...} expression interpolation
                 ((and (char= ch #\$)
                       (< (1+ i) len)
                       (char= (char string-body (1+ i)) #\{))
                  (let* ((content-start (+ i 2))
                         (close-pos (find-matching-brace string-body content-start)))
                    (if close-pos
                        (let* ((content (subseq string-body content-start close-pos))
                               (expr (parse-one-form-from-string content)))
                          (when expr
                            (push expr forms))
                          (setf i (1+ close-pos)))
                        ;; No matching brace: skip the $ and continue
                        (incf i))))

                 ;; @{...} function-call interpolation
                 ((and (char= ch #\@)
                       (< (1+ i) len)
                       (char= (char string-body (1+ i)) #\{))
                  (let* ((content-start (+ i 2))
                         (close-pos (find-matching-brace string-body content-start)))
                    (if close-pos
                        (let* ((content (subseq string-body content-start close-pos))
                               (exprs (parse-all-forms-from-string content)))
                          (dolist (expr exprs)
                            (push expr forms))
                          (setf i (1+ close-pos)))
                        ;; No matching brace: skip the @ and continue
                        (incf i))))

                 (t (incf i)))))
    (nreverse forms)))

;;; Main parsing function


(defun try-skip-unknown-macro (stream)
  "Try to skip unknown macro using standard reader."
  (handler-case
      (cl:read stream nil :eof)
    (end-of-file ()
      :stop-parsing)
    (error ()
      nil)))

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
          (handle-eof-error file stream last-end-pos text line-starts parse-errors)
          (return))
        ;; Unknown reader macros (e.g., #?"..." from cl-interpol)
        ;; We can't parse the current top-level form, so skip it entirely
        (eclector.readtable:unknown-macro-sub-character ()
          (when (eq (try-skip-unknown-macro stream) :stop-parsing)
            (return)))
        ;; Quote without following object (from malformed reader macro)
        (eclector.reader:object-must-follow-quote ()
          ;; Skip and continue
          nil)
        ;; Unmatched closing paren
        (eclector.reader:invalid-context-for-right-parenthesis ()
          (handle-unmatched-closer file stream line-starts parse-errors)
          nil)
        ;; Other reader errors we can't handle - re-signal
        (eclector.base:stream-position-reader-error ()
          (let* ((pos (file-position stream))
                 ;; Extract a snippet of text around the error position
                 (snippet-start (max 0 (- pos 10)))
                 (snippet-end (min (length text) (+ pos 20)))
                 (snippet (subseq text snippet-start snippet-end))
                 ;; Find where the error position is in the snippet
                 (marker-pos (- pos snippet-start)))
            (multiple-value-bind (line column)
                (char-pos-to-line-column pos line-starts)
              ;; Only show warning in debug mode
              (when (utils:debug-mode-p)
                (format *error-output* "~%Warning: Skipping form at ~A:~D:~D (unknown reader macro)~%"
                        file line column)
                (format *error-output* "  Near: ~S~%" snippet)
                (format *error-output* "        ~v@T^--- here~%"  marker-pos))))
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
              (let ((message (if (utils:debug-mode-p)
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
