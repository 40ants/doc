(defpackage #:40ants-doc/markdown/transform
  (:use #:cl)
  (:import-from #:40ants-doc/builder/vars)
  (:import-from #:40ants-doc/utils))
(in-package 40ants-doc/markdown/transform)


(defun massage-docstring (docstring &key (indentation "    "))
  (if 40ants-doc/builder/vars::*table-of-contents-stream*
      ;; The output is going to /dev/null and this is a costly
      ;; operation, skip it.
      ""
      (let ((docstring (40ants-doc/utils::strip-docstring-indentation docstring)))
        (40ants-doc/utils::prefix-lines indentation
                                        docstring))))

(defun massage-docstring2 (docstring)
  (40ants-doc/utils::strip-docstring-indentation docstring))
