(defpackage #:trivial-glob/compiler
  (:use #:cl)
  (:export
   #:*match-dotfiles*
   ;; Main compilation function - returns a matcher function
   #:compile-pattern
   #:compile-exclusion-pattern))
(in-package #:trivial-glob/compiler)

(defvar *match-dotfiles* nil
  "When NIL, wildcards do not match leading dots in filenames.
When T, wildcards match leading dots like any other character.")

;;; Pattern AST Node Types

(defstruct pattern-node
  "Base type for all pattern nodes.")

(defstruct (literal-node (:include pattern-node))
  "Matches literal text exactly."
  (text "" :type string))

(defstruct (wildcard-node (:include pattern-node))
  "Matches exactly one character (?).
In pathname mode, does not match '/'.")

(defstruct (star-node (:include pattern-node))
  "Matches zero or more characters (*).
In pathname mode, does not match '/'.")

(defstruct (doublestar-node (:include pattern-node))
  "Matches zero or more directory levels (**).
Only valid in pathname mode. Matches across '/' boundaries.")

(defstruct (char-class-node (:include pattern-node))
  "Matches one character from a character class [...]."
  (content "" :type string)     ; Original bracket content
  (negated nil :type boolean)   ; T if [!...] or [^...]
  (ranges nil :type list)       ; List of (start . end) character ranges
  (classes nil :type list))     ; List of POSIX class names

(defstruct (sequence-node (:include pattern-node))
  "A sequence of pattern nodes that must match in order."
  (elements nil :type list))

(defstruct (alternatives-node (:include pattern-node))
  "Alternatives from brace expansion {a,b,c}.
Matches if any of the options match."
  (options nil :type list))

;;; POSIX Character Class Predicates (resolved at compile time)

(defun resolve-posix-class-predicate (class-name)
  "Resolve a POSIX class name to a predicate function at compile time.
Returns a function (char) -> boolean."
  (cond
    ((string= class-name "alnum") #'alphanumericp)
    ((string= class-name "alpha") #'alpha-char-p)
    ((string= class-name "digit") #'digit-char-p)
    ((string= class-name "lower") #'lower-case-p)
    ((string= class-name "upper") #'upper-case-p)
    ((string= class-name "space")
     (lambda (char)
       (member char '(#\Space #\Tab #\Newline #\Return #\Linefeed #\Page))))
    ((string= class-name "blank")
     (lambda (char) (or (char= char #\Space) (char= char #\Tab))))
    ((string= class-name "punct")
     (lambda (char)
       (and (graphic-char-p char)
            (not (alphanumericp char))
            (not (char= char #\Space)))))
    ((string= class-name "print")
     (lambda (char) (or (graphic-char-p char) (char= char #\Space))))
    ((string= class-name "graph") #'graphic-char-p)
    ((string= class-name "cntrl")
     (lambda (char)
       (or (< (char-code char) 32)
           (= (char-code char) 127))))
    ((string= class-name "xdigit")
     (lambda (char)
       (or (digit-char-p char)
           (and (char<= #\a char) (char<= char #\f))
           (and (char<= #\A char) (char<= char #\F)))))
    (t (lambda (char) (declare (ignore char)) nil))))

(defun compile-char-class-predicate (ranges classes negated casefold)
  "Generate an optimized predicate closure for a character class.
All POSIX class name resolution happens at compile time.
Returns a function (char) -> boolean."
  ;; Resolve POSIX class predicates at compile time
  (let ((class-preds (mapcar #'resolve-posix-class-predicate classes)))
    ;; Return optimized closure
    (lambda (char)
      (let ((matched nil))
        ;; Check ranges (compile-time list, runtime iteration)
        (loop for (start . end) in ranges
              do (when (if casefold
                          (and (char<= start (char-upcase char))
                               (char<= (char-upcase char) end))
                          (and (char<= start char)
                               (char<= char end)))
                   (setf matched t)
                   (return)))
        ;; Check POSIX class predicates (compile-time resolved)
        (unless matched
          (loop for pred in class-preds
                do (when (funcall pred char)
                     (setf matched t)
                     (return))))
        ;; Apply negation
        (if negated (not matched) matched)))))

;;; Code Generation - Convert AST to Matcher Functions

(defun generate-matcher (node pathname period casefold)
  "Generate a matcher function from a pattern node.
Returns a function (string start end) -> (or null position) that attempts to match
from START position and returns the ending position if successful, or NIL."
  (etypecase node
    (literal-node
     (let ((text (literal-node-text node))
           (text-len (length (literal-node-text node))))
       (if casefold
           (lambda (string start end)
             (when (and (<= (+ start text-len) end)
                        (string-equal text string :start2 start :end2 (+ start text-len)))
               (+ start text-len)))
           (lambda (string start end)
             (when (and (<= (+ start text-len) end)
                        (string= text string :start2 start :end2 (+ start text-len)))
               (+ start text-len))))))

    (wildcard-node
     ;; ? matches exactly one character (not / in pathname mode)
     (if pathname
         (lambda (string start end)
           (when (and (< start end)
                      (char/= (char string start) #\/))
             (1+ start)))
         (lambda (string start end)
           (when (< start end)
             (1+ start)))))

    (star-node
     ;; * matches zero or more characters (not / in pathname mode)
     (if pathname
         (lambda (string start end)
           ;; Find the next / or end
           (let ((pos start))
             (loop while (and (< pos end) (char/= (char string pos) #\/))
                   do (incf pos))
             pos))
         (lambda (string start end)
           ;; Match to end
           end)))

    (doublestar-node
     ;; ** matches zero or more directory levels
     ;; This is a no-op matcher that returns start position
     ;; The actual logic is handled in sequence-node backtracking
     (lambda (string start end)
       (declare (ignore string end))
       start))

    (char-class-node
     ;; Generate optimized character class predicate
     (let ((pred (compile-char-class-predicate
                  (char-class-node-ranges node)
                  (char-class-node-classes node)
                  (char-class-node-negated node)
                  casefold)))
       (lambda (string start end)
         (when (and (< start end)
                    (funcall pred (char string start)))
           (1+ start)))))

    (sequence-node
     ;; Generate sequence matcher with backtracking for star/doublestar nodes
     ;; Check if sequence contains nodes that need backtracking
     (let ((elements (sequence-node-elements node)))
       (if (some (lambda (elem) (or (star-node-p elem) (doublestar-node-p elem))) elements)
           ;; Contains stars/doublestars - need backtracking support
           (lambda (string start end)
             (labels ((try-match (remaining-elems pos)
                        (cond
                          ((null remaining-elems) pos)  ; Success
                          ((star-node-p (first remaining-elems))
                           ;; Star node - try matching 0 to (end - pos) characters
                           (let ((rest-pattern (rest remaining-elems))
                                 (can-match-slash (not pathname)))
                             ;; Try from longest match to shortest (greedy)
                             (some (lambda (star-end)
                                     (when (or can-match-slash
                                              (not (find #\/ string :start pos :end star-end)))
                                       (try-match rest-pattern star-end)))
                                   (loop for i from end downto pos collect i))))
                          ((doublestar-node-p (first remaining-elems))
                           ;; Doublestar node - matches zero or more path segments
                           (let ((rest-pattern (rest remaining-elems)))
                             (if (null rest-pattern)
                                 ;; ** at end of pattern - matches everything remaining
                                 end
                                 ;; ** with more pattern after it - try matching rest at various positions
                                 ;; Try from longest match to shortest (greedy, like star-node)
                                 (some (lambda (try-pos)
                                         (try-match rest-pattern try-pos))
                                       ;; Collect positions after each '/' in reverse, then try zero-match last
                                       (append (reverse (loop for i from pos below end
                                                              when (char= (char string i) #\/)
                                                              collect (1+ i)))
                                               (list pos))))))
                          (t
                           ;; Non-star/doublestar node
                           (let* ((matcher (generate-matcher (first remaining-elems) pathname period casefold))
                                  (result (funcall matcher string pos end)))
                             (when result
                               (try-match (rest remaining-elems) result)))))))
               (try-match elements start)))
           ;; No stars/doublestars - simple sequential matching
           (let ((element-matchers
                  (mapcar (lambda (elem) (generate-matcher elem pathname period casefold))
                          elements)))
             (lambda (string start end)
               (labels ((match-sequence (matchers pos)
                          (if (null matchers)
                              pos
                              (let ((result (funcall (first matchers) string pos end)))
                                (when result
                                  (match-sequence (rest matchers) result))))))
                 (match-sequence element-matchers start)))))))

    (alternatives-node
     ;; Try each alternative in order
     (let ((alt-matchers
            (mapcar (lambda (alt) (generate-matcher alt pathname period casefold))
                    (alternatives-node-options node))))
       (lambda (string start end)
         (loop for matcher in alt-matchers
               for result = (funcall matcher string start end)
               when result return result))))))

(defun generate-pattern-matcher (node pathname period casefold)
  "Generate a complete pattern matcher function.
Returns a function (string) -> boolean."
  (let ((node-matcher (generate-matcher node pathname period casefold)))
    (lambda (string)
      ;; Handle period flag: if true and string starts with '.',
      ;; pattern must also start with '.'
      (if (and period
               (> (length string) 0)
               (char= (char string 0) #\.)
               (not (and (literal-node-p node)
                         (> (length (literal-node-text node)) 0)
                         (char= (char (literal-node-text node) 0) #\.)))
               (not (and (sequence-node-p node)
                         (let ((first-elem (first (sequence-node-elements node))))
                           (and (literal-node-p first-elem)
                                (> (length (literal-node-text first-elem)) 0)
                                (char= (char (literal-node-text first-elem) 0) #\.))))))
          nil  ; Period check failed
          ;; Match from start to end
          (let ((result (funcall node-matcher string 0 (length string))))
            (and result (= result (length string))))))))

;;; Pattern Compilation

(defun parse-char-class-ranges (content)
  "Parse character ranges from bracket expression CONTENT.
Returns (values ranges classes) where ranges is a list of (start . end) cons cells
and classes is a list of POSIX class names."
  (let ((ranges nil)
        (classes nil)
        (i 0)
        (len (length content)))
    (loop while (< i len)
          do (cond
               ;; POSIX character class [:name:]
               ((and (< (+ i 4) len)
                     (char= (char content i) #\[)
                     (char= (char content (1+ i)) #\:))
                (let ((close-pos (search ":]" content :start2 (+ i 2))))
                  (if close-pos
                      (let ((class-name (subseq content (+ i 2) close-pos)))
                        (push class-name classes)
                        (setf i (+ close-pos 2)))
                      ;; No closing :], treat as regular character
                      (progn
                        (push (cons (char content i) (char content i)) ranges)
                        (incf i)))))

               ;; Range like a-z
               ((and (< (+ i 2) len)
                     (char= (char content (1+ i)) #\-))
                (push (cons (char content i) (char content (+ i 2))) ranges)
                (incf i 3))

               ;; Individual character
               (t
                (push (cons (char content i) (char content i)) ranges)
                (incf i))))
    (values (nreverse ranges) (nreverse classes))))

(defun find-bracket-close-in-pattern (pattern start end)
  "Find the closing ] for a bracket expression, handling character classes properly.
START should point to the character after the opening [.
Returns the position of the closing ] or NIL if not found."
  (loop with i = start
        while (< i end)
        do (cond
             ;; Character class [:name:] - skip past it
             ((and (< (+ i 3) end)
                   (char= (char pattern i) #\[)
                   (char= (char pattern (1+ i)) #\:))
              (let ((class-end (search ":]" pattern :start2 (+ i 2) :end2 end)))
                (if class-end
                    (setf i (+ class-end 2))
                    (incf i))))
             ;; Found closing bracket
             ((char= (char pattern i) #\])
              (return i))
             ;; Regular character, keep going
             (t
              (incf i)))))

(defun parse-bracket-expression (pattern start end)
  "Parse a bracket expression starting at START in PATTERN.
Returns (values char-class-node next-position) or NIL if invalid."
  (let ((close-pos (find-bracket-close-in-pattern pattern (1+ start) end)))
    (when close-pos
      (let* ((content (subseq pattern (1+ start) close-pos))
             (negated nil))
        ;; Check for negation
        (when (and (> (length content) 0)
                   (or (char= (char content 0) #\!)
                       (char= (char content 0) #\^)))
          (setf negated t)
          (setf content (subseq content 1)))

        ;; Parse ranges and classes
        (multiple-value-bind (ranges classes)
            (parse-char-class-ranges content)
          (values (make-char-class-node :content content
                                        :negated negated
                                        :ranges ranges
                                        :classes classes)
                  (1+ close-pos)))))))

(defun parse-pattern-sequence (pattern start end)
  "Parse pattern from START to END, returning a list of pattern nodes."
  (let ((nodes nil)
        (literal-acc nil)
        (i start))

    (labels ((flush-literal ()
               "Flush accumulated literal characters as a literal-node."
               (when literal-acc
                 (push (make-literal-node :text (coerce (nreverse literal-acc) 'string))
                       nodes)
                 (setf literal-acc nil))))

      (loop while (< i end)
            do (let ((ch (char pattern i)))
                 (case ch
                   (#\*
                    (flush-literal)
                    ;; Check for **/ (doublestar with slash)
                    (cond
                      ;; **/ - consume the slash too
                      ((and (< (+ i 2) end)
                            (char= (char pattern (1+ i)) #\*)
                            (char= (char pattern (+ i 2)) #\/))
                       (push (make-doublestar-node) nodes)
                       (incf i 3))
                      ;; ** at end or followed by non-slash
                      ((and (< (1+ i) end)
                            (char= (char pattern (1+ i)) #\*))
                       (push (make-doublestar-node) nodes)
                       (incf i 2))
                      ;; Just a single *
                      (t
                       (push (make-star-node) nodes)
                       (incf i))))

                   (#\?
                    (flush-literal)
                    (push (make-wildcard-node) nodes)
                    (incf i))

                   (#\[
                    (flush-literal)
                    (multiple-value-bind (class-node next-pos)
                        (parse-bracket-expression pattern i end)
                      (if class-node
                          (progn
                            (push class-node nodes)
                            (setf i next-pos))
                          ;; Invalid bracket, treat [ as literal
                          (progn
                            (push ch literal-acc)
                            (incf i)))))

                   (#\\
                    ;; Escape sequence - next character is literal
                    (when (< (1+ i) end)
                      (push (char pattern (1+ i)) literal-acc)
                      (incf i 2)))

                   (otherwise
                    (push ch literal-acc)
                    (incf i)))))

      (flush-literal)
      (nreverse nodes))))

(defun find-brace-group (pattern start end)
  "Find the next brace group {a,b,c} starting from START.
Returns (values options-list next-position) or NIL if no valid brace group found."
  (let ((open-pos (position #\{ pattern :start start :end end)))
    (when open-pos
      (let ((close-pos (position #\} pattern :start (1+ open-pos) :end end)))
        (when close-pos
          (let* ((content (subseq pattern (1+ open-pos) close-pos))
                 (options (uiop:split-string content :separator ",")))
            (values options (1+ close-pos))))))))

(defun expand-brace-patterns (pattern)
  "Expand brace groups in PATTERN into a list of pattern strings.
Example: 'file{1,2}.{txt,log}' => ('file1.txt' 'file1.log' 'file2.txt' 'file2.log')"
  (multiple-value-bind (options end-pos)
      (find-brace-group pattern 0 (length pattern))
    (if options
        ;; Found a brace group, expand it
        (let ((prefix (subseq pattern 0 (position #\{ pattern)))
              (suffix (subseq pattern end-pos))
              (results nil))
          (dolist (option options)
            (let ((expanded (concatenate 'string prefix option suffix)))
              ;; Recursively expand any remaining brace groups
              (setf results (append results (expand-brace-patterns expanded)))))
          results)
        ;; No brace groups found
        (list pattern))))

(defun compile-pattern (pattern &key pathname period casefold)
  "Compile a glob pattern string into a matcher function.

PATTERN - A glob pattern string (may contain brace expansions).
PATHNAME - If true, '/' characters are not matched by wildcards.
PERIOD - If true, leading '.' must be matched explicitly.
CASEFOLD - If true, perform case-insensitive matching.

Returns a function (string) -> boolean that tests if a string matches the pattern."
  (check-type pattern string)

  ;; First, expand brace groups
  (let ((expanded-patterns (expand-brace-patterns pattern)))
    (let ((root-node
            (if (= (length expanded-patterns) 1)
                ;; No brace expansion (or single result)
                (let* ((pat (first expanded-patterns))
                       (nodes (parse-pattern-sequence pat 0 (length pat))))
                  (if (= (length nodes) 1)
                      (first nodes)
                      (make-sequence-node :elements nodes)))
                ;; Multiple alternatives from brace expansion
                (let ((alternative-nodes
                        (loop for pat in expanded-patterns
                              collect (let ((nodes (parse-pattern-sequence pat 0 (length pat))))
                                        (if (= (length nodes) 1)
                                            (first nodes)
                                            (make-sequence-node :elements nodes))))))
                  (make-alternatives-node :options alternative-nodes)))))
      ;; Generate and return matcher function
      (generate-pattern-matcher root-node pathname period casefold))))

;;; Exclusion Pattern Helpers

(defun normalize-exclusion-pattern (pattern)
  "Normalize an exclusion pattern for consistent matching.

Returns the normalized pattern string with the following transformations:
- Trailing '/' is converted to '/**' for directory exclusion
- Relative paths (with '/' but not starting with '/' or '**') are prefixed with '**/'
- Leading '*/' is converted to '**/'

Examples:
  \"build/\"       => \"build/**\"
  \"*/core/*.lisp\" => \"**/core/*.lisp\"
  \"build/*.log\"   => \"**/build/*.log\"
  \"/tmp/out.txt\"  => \"/tmp/out.txt\" (absolute, unchanged)
  \"*.log\"         => \"*.log\" (filename-only, unchanged)"
  (check-type pattern string)
  (let ((normalized pattern))
    ;; Step 1: Handle trailing / → /**
    (when (and (> (length normalized) 0)
               (char= (char normalized (1- (length normalized))) #\/))
      (setf normalized (concatenate 'string
                                    (subseq normalized 0 (1- (length normalized)))
                                    "/**")))

    ;; Step 2: Handle relative paths (only if contains /)
    (cond
      ;; Already starts with **/ - no change
      ((and (>= (length normalized) 3)
            (string= "**/" normalized :end2 3))
       normalized)
      ;; Starts with */ - convert to **/
      ((and (>= (length normalized) 2)
            (char= (char normalized 0) #\*)
            (char= (char normalized 1) #\/))
       (setf normalized (concatenate 'string "**" (subseq normalized 1))))
      ;; Starts with / - absolute path, no change
      ((char= (char normalized 0) #\/)
       normalized)
      ;; Relative path - prepend **/
      (t
       (setf normalized (concatenate 'string "**/" normalized))))

    normalized))

(defun ensure-namestring (path)
  (etypecase path
    (string path)
    (pathname (namestring path))))

(defun compile-exclusion-pattern (pattern)
  "Compile an exclusion pattern for filtering results.

PATTERN - A glob pattern string used for exclusion.

Returns a function (pathname) -> boolean that returns T if the pathname
should be excluded.

Exclusion patterns have special semantics:
- If pattern contains '/', it matches against the full pathname string
- If pattern has no '/', it matches against just the filename
- Patterns with '/' that don't start with '/' are treated as relative and
  automatically prefixed with '**/' to match at any depth
- Patterns starting with '/' match absolute paths literally
- Patterns ending with '/' or '/**' are treated as directory exclusions and
  match all files recursively within that directory

Examples:
  (compile-exclusion-pattern \"*.log\")         ; Matches any .log file
  (compile-exclusion-pattern \"test*.txt\")     ; Matches test*.txt anywhere
  (compile-exclusion-pattern \"build/*.log\")   ; Matches build/*.log at any depth
  (compile-exclusion-pattern \"*/core/*.lisp\") ; Matches core/*.lisp at any depth
  (compile-exclusion-pattern \"/tmp/out.txt\")  ; Matches absolute path only
  (compile-exclusion-pattern \"build/\")        ; Matches all files in build/ recursively
  (compile-exclusion-pattern \"build/**\")      ; Same as above (recursive)
  (compile-exclusion-pattern \"**/build/**\")   ; Same as above (explicit)"
  (check-type pattern string)
  (cond
    ((find #\/ pattern :test 'char=)
     (let* ((normalized (normalize-exclusion-pattern pattern))
            (compiled-pattern (compile-pattern normalized)))
       (lambda (path)
         (funcall compiled-pattern (ensure-namestring path)))))
    (t
     ;; When there's no slash, it matches all files and directories that have the pattern as their name.
     (let ((as-file (compile-pattern (format nil "**/~A" pattern)))
           (as-dir (compile-pattern (format nil "**/~A/**" pattern))))
       (lambda (path)
         (let ((path-str (ensure-namestring path)))
           (or (funcall as-file path-str)
               (funcall as-dir path-str))))))))
