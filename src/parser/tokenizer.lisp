(in-package #:mallet/parser)

;; Regex pattern for Common Lisp numeric tokens
;; Based on CLHS 2.3.1 syntax for integers, ratios, and floats
;; Note: Does not handle #x, #o, #b radix prefixes (not needed for float detection)
(defparameter *number-pattern*
  (ppcre:create-scanner
   "^[+-]?(?:[0-9]+/[0-9]+|[0-9]+\\.[0-9]*(?:[eEdDfFsSlL][+-]?[0-9]+)?|[0-9]*\\.[0-9]+(?:[eEdDfFsSlL][+-]?[0-9]+)?|[0-9]+[eEdDfFsSlL][+-]?[0-9]+|[0-9]+\\.?)$")
  "Compiled regex for detecting numeric tokens.
Matches:
  - Integers: 123, +45, -67, 123. (trailing dot = decimal integer)
  - Ratios: 1/2, +3/4, -5/6
  - Floats: 1.0, .5, 1e10, 1.0e10, 1.0d0, .5f0")

(defun make-token (type value file line column raw)
  "Create a token object."
  (make-instance 'token
                 :type type
                 :value value
                 :file file
                 :line line
                 :column column
                 :raw raw))

(defun count-leading-semicolons (text pos)
  "Count consecutive semicolons starting at POS."
  (loop for i from pos below (length text)
        while (char= (char text i) #\;)
        count 1))

(defun read-until-newline (text pos)
  "Read from POS until newline or end, returning the substring."
  (let ((end (or (position #\Newline text :start pos)
                 (length text))))
    (subseq text pos end)))

(defun whitespace-char-p (char)
  "Check if CHAR is whitespace."
  (member char '(#\Space #\Tab #\Newline #\Return #\Linefeed)))

(defun constituent-char-p (char)
  "Check if CHAR can be part of a symbol or number."
  (and (not (whitespace-char-p char))
       (not (member char '(#\( #\) #\; #\" #\' #\` #\,)))))

(defun tokenize (text file)
  "Tokenize TEXT from FILE, preserving comments and positions.
Returns a list of TOKEN objects."
  (check-type text string)
  (check-type file pathname)

  (let ((tokens '())
        (pos 0)
        (line 1)
        (column 0)
        (len (length text)))

    (loop while (< pos len) do
      (let ((ch (char text pos)))
        (cond
          ;; Comments
          ((char= ch #\;)
           (let* ((start-pos pos)
                  (start-column column)
                  (semicolon-count
                    (count-leading-semicolons text start-pos))
                  (comment-type
                    (case semicolon-count
                      (1 :comment-inline)
                      (2 :comment-line)
                      (3 :comment-section)
                      (otherwise :comment-file)))
                  (comment-start (+ start-pos semicolon-count))
                  (comment-text (read-until-newline text comment-start))
                  (comment-value (string-trim '(#\Space #\Tab) comment-text))
                  (raw (concatenate 'string
                                    (make-string semicolon-count
                                                 :initial-element #\;)
                                    comment-text)))
             (push (make-token comment-type
                               comment-value
                               file
                               line
                               start-column
                               raw)
                   tokens)
             (incf pos (length raw))
             (incf column (length raw))))

          ;; Open paren
          ((char= ch #\()
           (push (make-token :open-paren "(" file line column "(") tokens)
           (incf pos)
           (incf column))

          ;; Close paren
          ((char= ch #\))
           (push (make-token :close-paren ")" file line column ")") tokens)
           (incf pos)
           (incf column))

          ;; Newline
          ((char= ch #\Newline)
           (incf pos)
           (incf line)
           (setf column 0))

          ;; Whitespace (space, tab, etc.)
          ((whitespace-char-p ch)
           (incf pos)
           (incf column))

          ;; Strings
          ((char= ch #\")
           (let* ((start-pos pos)
                  (start-column column)
                  (end-pos (position #\" text :start (1+ pos)))
                  (raw
                    (if end-pos
                        (subseq text start-pos (1+ end-pos))
                        (subseq text start-pos)))
                  (value (subseq raw 1 (1- (length raw)))))
             (push (make-token :string value file line start-column raw) tokens)
             (incf pos (length raw))
             (incf column (length raw))))

          ;; Symbols and numbers
          ((constituent-char-p ch)
           (let* ((start-pos pos)
                  (start-column column))
             (loop while (and (< pos len)
                              (constituent-char-p (char text pos)))
                   do (incf pos))
             (let* ((raw (subseq text start-pos pos))
                    (type (if (ppcre:scan *number-pattern* raw)
                              :number
                              :symbol)))
               (push (make-token type raw file line start-column raw) tokens)
               (incf column (length raw)))))

          ;; Other characters (quote, backquote, comma, etc.)
          (t
           (let ((raw (string ch)))
             (push (make-token :other raw file line column raw) tokens)
             (incf pos)
             (incf column))))))

    (nreverse tokens)))
