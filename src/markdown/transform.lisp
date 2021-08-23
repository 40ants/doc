(defpackage #:40ants-doc/markdown/transform
  (:use #:cl)
  (:import-from #:40ants-doc/builder/vars)
  (:import-from #:40ants-doc/utils))
(in-package 40ants-doc/markdown/transform)


;; TODO: move to 40ants-doc/docstring package
(defun massage-docstring2 (docstring)
  (40ants-doc/utils::strip-docstring-indentation docstring))
