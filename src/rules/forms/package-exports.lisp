(defpackage #:mallet/rules/forms/package-exports
  (:use #:cl)
  (:local-nicknames
   (#:utils #:mallet/utils)
   (#:parser #:mallet/parser)
   (#:base #:mallet/rules/base))
  (:export #:exported-symbol-p
           #:clear-package-export-cache
           #:test-package-p
           #:find-project-root-for-file))
(in-package #:mallet/rules/forms/package-exports)

;;; Package form detection

(defun defpackage-or-define-package-p (expr)
  "Return T if EXPR is a defpackage or uiop:define-package form."
  (and (consp expr)
       (stringp (first expr))
       (or (utils:form-head-matches-p (first expr) "DEFPACKAGE")
           (utils:form-head-matches-p (first expr) "DEFINE-PACKAGE"))))

;;; Symbol name normalization

(defun normalize-name (str)
  "Normalize a symbol-like string to uppercase bare name.
Strips package prefix and #: or : sigils, returns UPPERCASE string.
Returns NIL if STR is not a string."
  (when (stringp str)
    (string-upcase (utils:symbol-name-from-string str))))

;;; Extract package exports from a single defpackage expression

(defun extract-package-name (defpackage-expr)
  "Extract the normalized (uppercase) package name from DEFPACKAGE-EXPR.
Returns NIL if the name cannot be determined."
  (when (consp defpackage-expr)
    (normalize-name (second defpackage-expr))))

(defun extract-export-symbol-names (defpackage-expr)
  "Extract list of exported symbol names (uppercase) from DEFPACKAGE-EXPR.
Collects all symbols from all :export clauses."
  (let ((exports '()))
    (dolist (clause (cddr defpackage-expr))
      (when (and (consp clause)
                 (stringp (first clause))
                 (string-equal (first clause) ":export"))
        (dolist (sym (rest clause))
          (let ((name (normalize-name sym)))
            (when name
              (push name exports))))))
    exports))

;;; File scanning

(defparameter *excluded-dirs*
  '(".qlot" ".bundle-libs" ".git" ".svn" ".hg" "node_modules" "_build" ".cache" ".claude"
    ".zig-cache")
  "Directory names to skip when scanning for Lisp files.
NOTE: A parallel list exists in the EXCLUDED-DIRS local binding inside EXPAND-FILE-ARGS
in src/main.lisp. Keep both lists in sync whenever adding or removing entries.")

(defun collect-lisp-files (project-root)
  "Return list of .lisp file pathnames under PROJECT-ROOT, excluding build/VCS directories.
Performs a recursive traversal that stops at excluded directory names, avoiding
the cost of descending into large build artifact trees."
  (let ((result '())
        (dir (uiop:ensure-directory-pathname project-root)))
    (labels ((recurse (d)
               (handler-case
                   (progn
                     (dolist (f (uiop:directory-files d "*.lisp"))
                       (push f result))
                     (dolist (subdir (uiop:subdirectories d))
                       (let ((name (car (last (pathname-directory subdir)))))
                         (unless (member name *excluded-dirs* :test #'string=)
                           (recurse subdir)))))
                 (error () nil))))
      (recurse dir))
    result))

;;; Build the exports index for a project

(defun build-exports-index (project-root)
  "Scan all .lisp files under PROJECT-ROOT and return a cons
  (exports-index . test-packages) where each is a hash-table keyed by
  UPPERCASE package-name strings."
  (let ((index (make-hash-table :test 'equal))
        (test-packages (make-hash-table :test 'equal))
        (files (collect-lisp-files project-root)))
    (dolist (file files)
      (let ((text (handler-case (uiop:read-file-string file)
                    (error () nil))))
        (when text
          (let ((forms (handler-case (parser:parse-forms text file)
                         (error () nil))))
            (when forms
              (dolist (form forms)
                (let ((expr (parser:form-expr form)))
                  (when (defpackage-or-define-package-p expr)
                    (let ((pkg-name (extract-package-name expr)))
                      (when pkg-name
                        (let ((symbols (extract-export-symbol-names expr))
                              (sym-set (or (gethash pkg-name index)
                                           (setf (gethash pkg-name index)
                                                 (make-hash-table :test 'equal)))))
                          (dolist (sym symbols)
                            (setf (gethash sym sym-set) t)))
                        (when (base:defpackage-uses-test-framework-p (cddr expr))
                          (setf (gethash pkg-name test-packages) t))))))))))))
    (cons index test-packages)))

;;; Cache

(defvar *export-cache* (make-hash-table :test 'equal)
  "Cache mapping project-root namestring to a cons (exports-index . test-packages).")

(defun get-project-data (project-root)
  "Return the cached cons (exports-index . test-packages) for PROJECT-ROOT,
building and caching it if necessary."
  (let ((key (namestring (uiop:ensure-directory-pathname project-root))))
    (or (gethash key *export-cache*)
        (setf (gethash key *export-cache*)
              (build-exports-index project-root)))))

(defun get-exports-index (project-root)
  "Return the export index hash-table for PROJECT-ROOT."
  (car (get-project-data project-root)))

(defun clear-package-export-cache ()
  "Clear the project export cache. Call this between test cases or after file changes."
  (clrhash *export-cache*))

;;; Public API

(defun test-package-p (project-root package-name)
  "Return T if PACKAGE-NAME is a test package in the project at PROJECT-ROOT.
A package is considered a test package if its defpackage uses a known test framework.
Comparison is case-insensitive. Results are cached per project-root."
  (let* ((data (get-project-data project-root))
         (test-packages (cdr data))
         (pkg-key (string-upcase (utils:symbol-name-from-string
                                   (if (stringp package-name)
                                       package-name
                                       (string package-name))))))
    (and (gethash pkg-key test-packages) t)))

(defun exported-symbol-p (project-root package-name symbol-name)
  "Return T if SYMBOL-NAME is exported from PACKAGE-NAME in the project at PROJECT-ROOT.
All comparisons are case-insensitive. PROJECT-ROOT is a pathname or namestring.
Results are cached per project-root; call CLEAR-PACKAGE-EXPORT-CACHE to invalidate."
  (let* ((index (get-exports-index project-root))
         (pkg-key (string-upcase (utils:symbol-name-from-string
                                   (if (stringp package-name)
                                       package-name
                                       (string package-name)))))
         (sym-key (string-upcase (utils:symbol-name-from-string
                                   (if (stringp symbol-name)
                                       symbol-name
                                       (string symbol-name)))))
         (sym-set (gethash pkg-key index)))
    (and sym-set (gethash sym-key sym-set) t)))

;;; Project root detection

(defun find-project-root-for-file (file)
  "Walk up from FILE's directory to find the project root directory.
Recognizes roots by presence of .git/, .hg/, .qlot/ directories or qlfile.
Returns the project root pathname, or FILE's own directory if none found."
  (let ((start-dir (uiop:pathname-directory-pathname file)))
    (labels ((root-marker-p (dir)
               (or (some (lambda (d)
                           (uiop:directory-exists-p (merge-pathnames d dir)))
                         '(".git/" ".hg/" ".qlot/"))
                   (uiop:file-exists-p (merge-pathnames "qlfile" dir))))
             (walk (dir)
               (cond
                 ((root-marker-p dir) dir)
                 (t
                  (let ((parent (uiop:pathname-parent-directory-pathname dir)))
                    (if (or (null parent) (equal parent dir))
                        start-dir
                        (walk parent)))))))
      (walk start-dir))))
