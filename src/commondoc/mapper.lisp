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
    (let ((children (common-doc:children node)))
      (setf (common-doc:children node)
            (etypecase children
              (list (loop for child in (common-doc:children node)
                          collect (map-nodes child func)))
              ;; Sometimes (children) contains an object like
              ;; COMMON-DOC:UNORDERED-LIST
              (common-doc:document-node
               (map-nodes children func)))))
    (funcall func node)))
