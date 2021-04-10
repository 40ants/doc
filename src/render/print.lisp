(defpackage #:40ants-doc/render/print
  (:use #:cl)
  (:import-from #:40ants-doc/markdown/transform))
(in-package 40ants-doc/render/print)

;;; Print (DOCUMENTATION OBJECT DOC-TYPE) to STREAM in FORMAT. Clean
;;; up docstring indentation, then indent it by four spaces.
;;; Automarkup symbols.
(defun maybe-print-docstring (object doc-type stream &key break)
  (let ((*package* (typecase object
                     (symbol
                      (symbol-package object))
                     (t
                      *package*))))
    (let ((docstring (filter-documentation object doc-type)))
      (when break
        (break))
      (when docstring
        (format stream "~%~A~%"
                (40ants-doc/markdown/transform::massage-docstring docstring))))))


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

