(uiop:define-package #:trivial-glob
  (:use #:cl)
  (:local-nicknames
   (#:compiler #:trivial-glob/compiler)
   (#:fs #:trivial-glob/filesystem))
  (:use-reexport #:trivial-glob/compiler)
  (:export
   #:glob
   #:glob-match
   #:glob-path-match))
(in-package #:trivial-glob)

(defun find-brace-group (pattern start)
  "Find the next brace group starting from START.
Returns (values options-list end-position) or NIL if no valid brace group found."
  (let ((open-pos (position #\{ pattern :start start)))
    (when open-pos
      (let ((close-pos (position #\} pattern :start (1+ open-pos))))
        (when close-pos
          ;; Extract the content between braces
          (let* ((content (subseq pattern (1+ open-pos) close-pos))
                 (options (uiop:split-string content :separator ",")))
            (values options (1+ close-pos))))))))

(defun expand-braces (pattern)
  "Expand brace groups in PATTERN into a list of patterns.
Example: 'file{1,2}.{txt,log}' => ('file1.txt' 'file1.log' 'file2.txt' 'file2.log')"
  (multiple-value-bind (options end-pos)
      (find-brace-group pattern 0)
    (if options
        ;; Found a brace group, expand it
        (let ((prefix (subseq pattern 0 (position #\{ pattern)))
              (suffix (subseq pattern end-pos))
              (results nil))
          ;; For each option in this brace group
          (dolist (option options)
            (let ((expanded (concatenate 'string prefix option suffix)))
              ;; Recursively expand any remaining brace groups
              (let ((further-expanded (expand-braces expanded)))
                (setf results (append results further-expanded)))))
          results)
        ;; No brace groups found, return pattern as-is
        (list pattern))))

(defun glob (pathname-or-pattern &key follow-symlinks exclude)
  "Return a list of pathnames matching the glob pattern.

PATHNAME-OR-PATTERN - A pathname designator or glob pattern string.
FOLLOW-SYMLINKS - If true, follow symbolic links during traversal.
EXCLUDE - A pattern string or list of pattern strings to exclude from results.
          Excluded patterns are matched against the full pathname.

Examples:
  (glob \"*.txt\")
  (glob \"src/**/*.lisp\")
  (glob \"{foo,bar}/*.c\")
  (glob \"*.txt\" :exclude \"test*.txt\")
  (glob \"**/*.lisp\" :exclude '(\"*/test/*.lisp\" \"*/vendor/*.lisp\"))

Returns a list of pathnames matching the pattern."
  ;; Convert to namestring to check for brace expansion
  (let ((pattern-string (etypecase pathname-or-pattern
                          (string pathname-or-pattern)
                          (pathname (namestring pathname-or-pattern)))))
    (let ((expanded-patterns (expand-braces pattern-string)))
      (if (and (= (length expanded-patterns) 1)
               (string= (first expanded-patterns) pattern-string))
          ;; No braces found, use original pattern
          (fs:glob-filesystem pathname-or-pattern
                              :follow-symlinks follow-symlinks
                              :exclude exclude)
          ;; Had brace expansion (even if just one result)
          (let ((all-results nil))
            (dolist (pattern expanded-patterns)
              (let ((results (fs:glob-filesystem pattern
                                                 :follow-symlinks follow-symlinks
                                                 :exclude exclude)))
                (setf all-results (append all-results results))))
            ;; Remove duplicates
            (remove-duplicates all-results :test #'equal))))))

(defun glob-match (pattern string &key match-slash period casefold)
  "Test whether STRING matches the glob PATTERN.

PATTERN - A glob pattern string.
STRING - The string to test.
MATCH-SLASH - If true, '/' characters can be matched by wildcards. Default is NIL (shell-like behavior).
PERIOD - If true, leading '.' must be matched explicitly.
CASEFOLD - If true, perform case-insensitive matching.

Returns T if the string matches, NIL otherwise.

Examples:
  (glob-match \"*.txt\" \"file.txt\") => T
  (glob-match \"test?.c\" \"test1.c\") => T
  (glob-match \"[a-z]*\" \"hello\") => T"
  ;; Use compiled pattern for optimized matching
  (let ((matcher (compiler:compile-pattern pattern
                                           :match-slash match-slash
                                           :period period
                                           :casefold casefold)))
    (funcall matcher string)))

(defun glob-path-match (pattern pathname)
  "Test whether PATHNAME matches the PATTERN with path-aware semantics.

PATTERN - A pattern string with special path semantics:
          - Patterns with '/' match against the full pathname
          - Patterns without '/' match against just the filename
          - Relative paths are auto-prefixed with '**/' to match at any depth
          - Trailing '/' or '/**' matches directories recursively
PATHNAME - A pathname designator to test against the pattern.

Returns T if the pathname matches the pattern, NIL otherwise.

Examples:
  (glob-path-match \"*.log\" #P\"/tmp/foo.log\") => T
  (glob-path-match \"build/\" #P\"/tmp/build/out.txt\") => T
  (glob-path-match \"**/test/*.lisp\" #P\"/src/test/foo.lisp\") => T
  (glob-path-match \"*.txt\" #P\"/tmp/foo.log\") => NIL"
  (when (funcall (compiler:compile-path-pattern pattern)
                 pathname)
    t))
