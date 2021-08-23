(defpackage #:40ants-doc/render/print
  (:use #:cl)
  (:import-from #:40ants-doc/markdown/transform)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/object-package))
(in-package 40ants-doc/render/print)


;; TODO: move to 40ants-doc/docstring package
(defun get-docstring (object doc-type)
  (let* ((package (40ants-doc/object-package:object-package object))
         (*package* package
                    ;; We need to set the package to the object's package
                    ;; to because in package inferred systems documentation
                    ;; section can referer objects from other sub-packages.
                    ;; If we don't do this, then argument reference will not
                    ;; work and uppercased words will not become `CODE`:
                    ;; (40ants-doc/object-package:object-package object)
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

