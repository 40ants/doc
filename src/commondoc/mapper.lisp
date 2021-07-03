(defpackage #:40ants-doc/commondoc/mapper
  (:use #:cl)
  (:export
   #:map-nodes))
(in-package 40ants-doc/commondoc/mapper)


(defgeneric map-nodes (node func)
  (:documentation "Recursively replaces or modifies a CommonDoc NODE with results of the FUNC call.")
  
  (:method (node func)
    (funcall func node))
  
  (:method ((node common-doc:content-node) func)
    (setf (common-doc:children node)
          (mapcar func (common-doc:children node)))
    (funcall func node)))
