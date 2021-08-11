(defpackage #:40ants-doc/commondoc/mapper
  (:use #:cl)
  (:export
   #:map-nodes
   #:node-supports-children
   #:with-node-package))
(in-package 40ants-doc/commondoc/mapper)


(defvar *on-going-down* nil
  "A list of callbacks to be called inside MAP-NODES.")

(defvar *on-going-up* nil
  "A list of callbacks to be called inside MAP-NODES.")


(defun do-nothing (node)
  (declare (ignore node))
  (values))


(defun call-with-node-package (func)
  (let ((packages-stack nil))
    (flet ((set-package (node)
             (let ((package (or (40ants-doc/object-package:object-package node)
                                *package*)))
               (push *package* packages-stack)
               (setf *package* package)))
           (reset-package (node)
             (declare (ignore node))
             (setf *package*
                   (pop packages-stack))))
      
      (let ((*on-going-down* (list* #'set-package *on-going-down*))
            (*on-going-up* (list* #'reset-package *on-going-up*))
            (*package* *package*))
        (funcall func)))))


(defmacro with-node-package (&body body)
  `(call-with-node-package (lambda () ,@body)))


(defun process-node-with-children (node func &key
                                             (on-going-down #'do-nothing)
                                             (on-going-up #'do-nothing))
  (let* ((result (funcall func node))
         (children (when (node-supports-children result)
                     (common-doc:children result))))
      
    (when (node-supports-children result)
      (loop for callback in *on-going-down*
            do (funcall callback result))
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
      (loop for callback in *on-going-up*
            do (funcall callback result)))
    
    (values result)))


(defgeneric node-supports-children (node)
  (:documentation "We have to use this function because some common-doc node types
                   supporting COMMON-DOC:CHILDREN do not share a common type.")
  
  (:method (node)
    nil)
  
  (:method ((node common-doc:base-list))
    t)
  
  (:method ((node common-doc:document))
    t)
  
  (:method ((node common-doc:content-node))
    t))


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
