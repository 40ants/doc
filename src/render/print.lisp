(defpackage #:40ants-doc/render/print
  (:use #:cl)
  (:import-from #:40ants-doc/markdown/transform)
  (:import-from #:40ants-doc/utils))
(in-package 40ants-doc/render/print)

;;; Print (DOCUMENTATION OBJECT DOC-TYPE) to STREAM in FORMAT. Clean
;;; up docstring indentation, then indent it by four spaces.
;;; Automarkup symbols.
(defun maybe-print-docstring (object doc-type stream)
  (let* ((package (40ants-doc/utils::object-package object))
         (*package* package
           ;; We need to set the package to the object's package
           ;; to because in package inferred systems documentation
           ;; section can referer objects from other sub-packages.
           ;; If we don't do this, then argument reference will not
           ;; work and uppercased words will not become `CODE`:
           ;; (40ants-doc/utils::object-package object)
           ))
    (let ((docstring (filter-documentation object doc-type)))
      (when docstring
        (format stream "~%~A~%"
                (40ants-doc/markdown/transform::massage-docstring docstring))))))


(defun get-docstring (object doc-type)
  (let* ((package (40ants-doc/utils::object-package object))
         (*package* package
                    ;; We need to set the package to the object's package
                    ;; to because in package inferred systems documentation
                    ;; section can referer objects from other sub-packages.
                    ;; If we don't do this, then argument reference will not
                    ;; work and uppercased words will not become `CODE`:
                    ;; (40ants-doc/utils::object-package object)
                    ))
    (let ((docstring (filter-documentation object doc-type)))
      (when docstring
        (40ants-doc/markdown/transform::massage-docstring2 docstring)))))


(defun filter-documentation (symbol doc-type)
  (let ((docstring (documentation symbol doc-type)))
    #+sbcl
    (if (member docstring
                '("Return whether debug-block represents elsewhere code."
                  "automatically generated accessor method"
                  "automatically generated reader method"
                  "automatically generated writer method")
                :test #'equal)
        ;; Discard the garbage docstring.
        nil
        docstring)
    #-sbcl
    docstring))

