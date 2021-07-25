(defpackage #:40ants-doc/commondoc/piece
  (:use #:cl)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/reference)
  (:export
   #:doc-reference
   #:documentation-piece))
(in-package 40ants-doc/commondoc/piece)


(defclass documentation-piece ()
  ((doc-reference :initarg :doc-reference
                  :type 40ants-doc/reference::reference
                  :reader doc-reference))
  (:documentation "This class is a mixin to be added to any common doc node, which can be linked to a 40ANTS-DOC/REFERENCE::REFERENCE"))


(defmethod 40ants-doc/utils::object-package ((obj documentation-piece))
  (let* ((reference (doc-reference obj))
         (ref-object (40ants-doc/reference::reference-object reference))
         (package (40ants-doc/utils::object-package ref-object)))
    package))
