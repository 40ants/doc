(defpackage #:40ants-doc/commondoc/piece
  (:use #:cl)
  (:export
   #:doc-reference
   #:documentation-piece))
(in-package 40ants-doc/commondoc/piece)


(defclass documentation-piece ()
  ((doc-reference :initarg :doc-reference
                  :type 40ants-doc/reference::reference
                  :reader doc-reference))
  (:documentation "This class is a mixin to be added to any common doc node, which can be linked to a 40ANTS-DOC/REFERENCE::REFERENCE"))
