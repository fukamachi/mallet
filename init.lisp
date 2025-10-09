(require 'asdf)

(defvar *project-dir*
  (make-pathname :name nil
                 :type nil
                 :version nil
                 :defaults *load-pathname*))

(asdf:initialize-source-registry
  '(:source-registry
    :ignore-inherited-configuration
    (:also-exclude ".qlot")
    (:also-exclude ".bundle-libs")
    (:directory :here)))

(defvar *setup-file*
  (merge-pathnames #P".bundle-libs/setup.lisp" *project-dir*))

(unless (probe-file *setup-file*)
  (format *error-output*
          "~&Dependencies are not bundled~%")
  (uiop:quit -1))

(load *setup-file*)
