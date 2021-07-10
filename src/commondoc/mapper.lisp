(defpackage #:40ants-doc/commondoc/mapper
  (:use #:cl)
  (:export
   #:map-nodes))
(in-package 40ants-doc/commondoc/mapper)


(defun do-nothing (node)
  (declare (ignore node))
  (values))


(defun process-node-with-children (node func &key
                                             (on-going-down #'do-nothing)
                                             (on-going-up #'do-nothing))
  (let* ((result (funcall func node))
         (children (common-doc:children result)))
      
    (funcall on-going-down result)
    (setf (common-doc:children result)
          (etypecase children
            (list (loop for child in (common-doc:children result)
                        collect (map-nodes child func
                                           :on-going-up on-going-up
                                           :on-going-down on-going-down)))
            ;; Sometimes (children) contains an object like
            ;; COMMON-DOC:UNORDERED-LIST
            (common-doc:document-node
             (map-nodes children func
                        :on-going-up on-going-up
                        :on-going-down on-going-down))))
      
    (funcall on-going-up result)
    (values result)))


(defgeneric map-nodes (node func &key)
  (:documentation "Recursively replaces or modifies a CommonDoc NODE with results of the FUNC call.")
  
  (:method (node func &key &allow-other-keys)
    (funcall func node))
  
  (:method ((node common-doc:base-list) func &key
                                             (on-going-down #'do-nothing)
                                             (on-going-up #'do-nothing))
    (process-node-with-children node func
                                :on-going-down on-going-down
                                :on-going-up on-going-up))
  
  (:method ((node common-doc:document) func &key
                                            (on-going-down #'do-nothing)
                                            (on-going-up #'do-nothing))
    (process-node-with-children node func
                                :on-going-down on-going-down
                                :on-going-up on-going-up))
  
  (:method ((node common-doc:content-node) func &key
                                                (on-going-down #'do-nothing)
                                                (on-going-up #'do-nothing))
    (process-node-with-children node func
                                :on-going-down on-going-down
                                :on-going-up on-going-up)))
