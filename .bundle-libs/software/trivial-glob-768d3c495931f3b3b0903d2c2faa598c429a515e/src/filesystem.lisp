(defpackage #:trivial-glob/filesystem
  (:use #:cl)
  (:local-nicknames
   (#:compiler #:trivial-glob/compiler))
  (:export
   #:glob-filesystem))
(in-package #:trivial-glob/filesystem)

(defun has-wildcards-p (string)
  "Check if STRING contains glob wildcard characters."
  (some (lambda (char) (find char "*?[")) string))

(defun split-pattern-string (pattern-string)
  "Split pattern string into directory components and filename pattern.
Returns (values dir-components filename-pattern) where:
- dir-components: list of directory component strings
- filename-pattern: filename portion (string)

Examples:
  \"*.txt\" => (NIL \"*.txt\")
  \"dir/*.txt\" => ((\"dir\") \"*.txt\")
  \"dir/sub/**/*.lisp\" => ((\"dir\" \"sub\" \"**\") \"*.lisp\")
  \"/abs/path/*.c\" => ((\"\" \"abs\" \"path\") \"*.c\")  ; empty string = absolute"
  (let ((components (uiop:split-string pattern-string :separator "/")))
    (if (null components)
        (values nil "")
        (let ((filename (car (last components)))
              (dirs (butlast components)))
          (values dirs filename)))))

(defun split-filename-pattern (filename-pattern)
  "Split filename pattern into name and type (extension) parts.
Returns (values name-pattern type-pattern) where both are strings or NIL.

Examples:
  \"*.lisp\" => (\"*\" \"lisp\")
  \"*-test.lisp\" => (\"*-test\" \"lisp\")
  \"test*\" => (\"test*\" NIL)
  \"test.tar.gz\" => (\"test.tar\" \"gz\")  ; split on last dot only"
  (let ((parts (uiop:split-string filename-pattern :separator '(#\.) :max 2)))
    (cond
      ((null parts) (values nil nil))
      ((= (length parts) 1) (values (first parts) nil))
      (t (values (first parts) (second parts))))))

(defun symlink-p (pathname)
  "Check if PATHNAME is a symbolic link.
Note: This uses a heuristic that works on most Unix systems but may not be 100% accurate
on all platforms. It resolves relative paths to absolute before checking."
  (handler-case
      (let* ((absolute-path (uiop:ensure-absolute-pathname pathname *default-pathname-defaults*))
             (truename-result (truename absolute-path))
             (namestr-abs (namestring absolute-path))
             (namestr-true (namestring truename-result)))
        ;; Compare absolute path with its truename
        ;; On Unix, symlinks will have different paths
        (not (equal namestr-abs namestr-true)))
    (error () nil)))

(defun filter-excluded (pathnames exclude-matchers)
  "Filter out pathnames that match any exclude pattern.
PATHNAMES - List of pathnames to filter.
EXCLUDE-MATCHERS - List of compiled exclusion matcher functions, or NIL.
Returns filtered list of pathnames."
  (if (null exclude-matchers)
      pathnames
      (remove-if (lambda (pathname)
                   (some (lambda (matcher)
                           (funcall matcher pathname))
                         exclude-matchers))
                 pathnames)))

(defun glob-filesystem (pathname-or-pattern &key follow-symlinks exclude)
  "Return a list of pathnames matching the glob pattern.

PATHNAME-OR-PATTERN - A pathname designator or glob pattern string.
FOLLOW-SYMLINKS - If true, follow symbolic links during traversal.
EXCLUDE - A pattern string or list of pattern strings to exclude from results.

Returns a list of pathnames matching the pattern."
  ;; Compile exclude patterns once
  (let ((exclude-matchers
          (when exclude
            (let ((patterns (if (listp exclude) exclude (list exclude))))
              (mapcar #'compiler:compile-exclusion-pattern patterns))))
        (pattern-string (etypecase pathname-or-pattern
                          (pathname (namestring pathname-or-pattern))
                          (string
                           ;; Expand tilde if present
                           (cond
                             ((and (> (length pathname-or-pattern) 0)
                                   (char= (char pathname-or-pattern 0) #\~))
                              ;; Replace ~ or ~/ with home directory
                              (let ((home-dir (namestring (user-homedir-pathname))))
                                (cond
                                  ((= (length pathname-or-pattern) 1)
                                   ;; Just "~"
                                   home-dir)
                                  ((char= (char pathname-or-pattern 1) #\/)
                                   ;; "~/..."
                                   (concatenate 'string home-dir (subseq pathname-or-pattern 2)))
                                  (t
                                   ;; "~user/..." - not supported, use as-is
                                   pathname-or-pattern))))
                             (t pathname-or-pattern))))))

    ;; Parse pattern string ourselves to avoid CL pathname corruption
    (multiple-value-bind (dir-components filename-pattern)
        (split-pattern-string pattern-string)

      ;; Split filename into name and type parts
      (multiple-value-bind (name-pattern type-pattern)
          (split-filename-pattern filename-pattern)

        ;; Check for wildcards
        (let* ((has-name-wildcard-p (and name-pattern (has-wildcards-p name-pattern)))
               (has-type-wildcard-p (and type-pattern (has-wildcards-p type-pattern)))
               (has-dir-wildcard-p (some (lambda (comp)
                                           (or (string= comp "**")
                                               (has-wildcards-p comp)))
                                         dir-components)))

          ;; If no wildcards, just check if file exists
          (when (and (not has-dir-wildcard-p)
                     (not has-name-wildcard-p)
                     (not has-type-wildcard-p))
            (let ((path (pathname pattern-string)))
              (return-from glob-filesystem
                (when (uiop:file-exists-p path)
                  (filter-excluded (list (truename path)) exclude-matchers)))))

          ;; Compile filename patterns once
          (let ((name-matcher (when has-name-wildcard-p
                                (compiler:compile-pattern name-pattern
                                                          :period (not compiler:*match-dotfiles*))))
                (type-matcher (when has-type-wildcard-p
                                (compiler:compile-pattern type-pattern))))

            ;; If only name/type have wildcards (no directory wildcards)
            (when (and (or has-name-wildcard-p has-type-wildcard-p)
                       (not has-dir-wildcard-p))
              ;; Build directory path
              (let* ((dir-path (if dir-components
                                  (make-pathname :directory (cons (if (string= (first dir-components) "")
                                                                      :absolute
                                                                      :relative)
                                                                  (if (string= (first dir-components) "")
                                                                      (rest dir-components)
                                                                      dir-components))
                                                :name nil
                                                :type nil)
                                  (uiop:getcwd))))
                (unless (uiop:directory-exists-p dir-path)
                  (error 'file-error
                         :pathname dir-path
                         :format-control "Directory does not exist: ~A"
                         :format-arguments (list dir-path)))
                (when (and (not follow-symlinks) (symlink-p dir-path))
                  (return-from glob-filesystem nil))
                (return-from glob-filesystem
                  (filter-excluded
                   (match-files-in-directory dir-path
                                             name-pattern name-matcher
                                             type-pattern type-matcher)
                   exclude-matchers))))

            ;; Has directory wildcards - perform custom pattern matching
            (filter-excluded
             (glob-match-filesystem dir-components
                                    name-pattern name-matcher
                                    type-pattern type-matcher
                                    follow-symlinks)
             exclude-matchers)))))))

(defun match-files-in-directory (directory name-pattern name-matcher type-pattern type-matcher)
  "Match files in DIRECTORY using compiled matchers.
NAME-PATTERN and TYPE-PATTERN are strings (or NIL for no pattern).
NAME-MATCHER and TYPE-MATCHER are compiled functions (or NIL for no wildcard)."
  (let ((files (uiop:directory-files directory))
        (results nil))
    (dolist (file files)
      (let ((file-name (pathname-name file))
            (file-type (pathname-type file)))
        (when (and (or (null name-pattern)
                       (if name-matcher
                           (funcall name-matcher (if file-name (string file-name) ""))
                           (string= name-pattern (if file-name (string file-name) ""))))
                   (or (null type-pattern)
                       (if type-matcher
                           (funcall type-matcher (if file-type (string file-type) ""))
                           (string= type-pattern (if file-type (string file-type) "")))))
          (push file results))))
    (nreverse results)))

(defun glob-match-filesystem (dir-components name-pattern name-matcher
                               type-pattern type-matcher follow-symlinks)
  "Match pattern against filesystem using compiled matchers."
  ;; Split directory components into base (no wildcards) and remaining (with wildcards)
  (let* ((split-pos (position-if (lambda (comp)
                                   (or (string= comp "**")
                                       (has-wildcards-p comp)))
                                 dir-components))
         (base-components (if split-pos
                              (subseq dir-components 0 split-pos)
                              dir-components))
         (remaining-components (when split-pos
                                 (subseq dir-components split-pos))))

    ;; Build base directory pathname
    (let ((base-dir (if base-components
                        (make-pathname :directory (cons (if (string= (first base-components) "")
                                                            :absolute
                                                            :relative)
                                                        (if (string= (first base-components) "")
                                                            (rest base-components)
                                                            base-components))
                                       :name nil
                                       :type nil)
                        (uiop:getcwd))))

      ;; Check if base directory exists
      (unless (uiop:directory-exists-p base-dir)
        (error 'file-error
               :pathname base-dir
               :format-control "Directory does not exist: ~A"
               :format-arguments (list base-dir)))

      ;; If no remaining dir components, match files in base directory
      (if (null remaining-components)
          (match-files-in-directory base-dir name-pattern name-matcher
                                    type-pattern type-matcher)
          ;; Otherwise, need to match through subdirectories
          (let ((visited (make-hash-table :test 'equal)))
            (match-through-directories base-dir remaining-components
                                       name-pattern name-matcher
                                       type-pattern type-matcher
                                       follow-symlinks visited))))))

(defun match-through-directories (base-dir remaining-dir-components
                                  name-pattern name-matcher
                                  type-pattern type-matcher
                                  follow-symlinks visited)
  "Walk through REMAINING-DIR-COMPONENTS using compiled matchers."
  (if (null remaining-dir-components)
      (match-files-in-directory base-dir name-pattern name-matcher
                                type-pattern type-matcher)
      (let ((current-component (first remaining-dir-components))
            (rest-components (rest remaining-dir-components))
            (results nil))

        ;; Get subdirectories, filtering symlinks if needed
        (let ((all-subdirs (uiop:subdirectories base-dir)))
          (let ((subdirs (if follow-symlinks
                             all-subdirs
                             (remove-if #'symlink-p all-subdirs))))

            ;; Handle ** pattern specially
            (cond
              ((string= current-component "**")
               ;; ** matches zero or more directory levels
               ;; First, try matching at current level (zero directories)
               (let ((zero-match (match-through-directories base-dir
                                                            rest-components
                                                            name-pattern name-matcher
                                                            type-pattern type-matcher
                                                            follow-symlinks
                                                            visited)))
                 (setf results (nconc results zero-match)))

               ;; Then recursively search all subdirectories
               (dolist (subdir subdirs)
                 (let ((truename-str (namestring (truename subdir))))
                   (unless (gethash truename-str visited)
                     (setf (gethash truename-str visited) t)
                     ;; Continue the ** search in this subdirectory
                     (let ((sub-results (match-through-directories
                                         subdir
                                         remaining-dir-components ; Keep the ** component
                                         name-pattern name-matcher
                                         type-pattern type-matcher
                                         follow-symlinks
                                         visited)))
                       (setf results (nconc results sub-results)))
                     (remhash truename-str visited)))))

              (t
               ;; Regular directory component (not **)
               (let ((dir-matcher (when (has-wildcards-p current-component)
                                    (compiler:compile-pattern current-component
                                                              :period (not compiler:*match-dotfiles*)))))
                 (dolist (subdir subdirs)
                   (let ((subdir-name (first (last (pathname-directory subdir)))))
                     (when (and subdir-name
                                (if dir-matcher
                                    (funcall dir-matcher subdir-name)
                                    (string= current-component subdir-name)))
                       (let ((truename-str (namestring (truename subdir))))
                         (unless (gethash truename-str visited)
                           (setf (gethash truename-str visited) t)
                           ;; Recursively match in this subdirectory
                           (let ((sub-results (match-through-directories
                                               subdir
                                               rest-components
                                               name-pattern name-matcher
                                               type-pattern type-matcher
                                               follow-symlinks
                                               visited)))
                             (setf results (nconc results sub-results)))
                           (remhash truename-str visited)))))))))))

        results)))
