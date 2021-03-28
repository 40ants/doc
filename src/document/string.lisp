(defpackage #:40ants-doc/document/string
  (:use #:cl)
  (:import-from #:40ants-doc/markdown/transform))
(in-package 40ants-doc/document/string)


(defmethod 40ants-doc/document::document-object ((string string) stream)
  "Print STRING verbatim to STREAM after cleaning up indentation.

  Docstrings in sources are indented in various ways which can easily
  mess up markdown. To handle the most common cases leave the first
  line alone, but from the rest of the lines strip the longest run of
  leading spaces that is common to all non-blank lines."
  (format stream "~a~%"
          (40ants-doc/markdown/transform::massage-docstring string :indentation "")))
