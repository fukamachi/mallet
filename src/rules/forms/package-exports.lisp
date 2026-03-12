(defpackage #:mallet/rules/forms/package-exports
  (:use #:cl)
  (:local-nicknames
   (#:utils #:mallet/utils)
   (#:parser #:mallet/parser))
  (:export #:exported-symbol-p
           #:clear-package-export-cache))
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
  '(".qlot" ".bundle-libs" ".git" ".svn" ".hg" "node_modules" "_build" ".cache" ".claude")
  "Directory names to skip when scanning for Lisp files.")

(defun should-exclude-path-p (path)
  "Return T if PATH is inside an excluded directory."
  (let ((path-string (namestring path)))
    (some (lambda (excluded)
            (search (concatenate 'string "/" excluded "/") path-string))
          *excluded-dirs*)))

(defun collect-lisp-files (project-root)
  "Return list of .lisp file pathnames under PROJECT-ROOT, excluding build/VCS directories."
  (let* ((dir (uiop:ensure-directory-pathname project-root))
         (all-files (handler-case (uiop:directory-files dir "**/*.lisp")
                      (error () nil))))
    (remove-if #'should-exclude-path-p (or all-files '()))))

;;; Build the exports index for a project

(defun build-exports-index (project-root)
  "Scan all .lisp files under PROJECT-ROOT and return a hash-table:
  package-name (UPPERCASE string) → hash-table of exported symbol names (UPPERCASE strings)."
  (let ((index (make-hash-table :test 'equal))
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
                            (setf (gethash sym sym-set) t)))))))))))))
    index))

;;; Cache

(defvar *export-cache* (make-hash-table :test 'equal)
  "Cache mapping project-root namestring to its export index hash-table.")

(defun get-exports-index (project-root)
  "Return the export index for PROJECT-ROOT, building and caching it if necessary."
  (let ((key (namestring (uiop:ensure-directory-pathname project-root))))
    (or (gethash key *export-cache*)
        (setf (gethash key *export-cache*)
              (build-exports-index project-root)))))

(defun clear-package-export-cache ()
  "Clear the project export cache. Call this between test cases or after file changes."
  (clrhash *export-cache*))

;;; Public API

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
