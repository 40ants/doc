(defpackage #:40ants-doc/document
  (:use #:cl))
(in-package 40ants-doc/document)


(defgeneric document-object (object stream)
  (:method (object stream)
    (format stream "Object ~S has no 40ants-doc/document:document-object method."
            object)))


(defgeneric document (object &key stream pages format))
