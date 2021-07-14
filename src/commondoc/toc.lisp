(uiop:define-package #:40ants-doc/commondoc/toc
  (:use #:cl)
  (:import-from #:40ants-doc/commondoc/section
                #:documentation-section-uri-fragment)
  (:export
   #:*main-toc*
   #:make-toc))
(in-package 40ants-doc/commondoc/toc)


(defvar *main-toc*)


(defun make-toc (document)
  "Collects toplevel objects of 40ANTS-DOC/COMMONDOC/SECTION:DOCUMENTATION-SECTION class
   and returns a table of content CommonDoc node."
  (let* ((current-sublist (list (common-doc:make-unordered-list (list))))
         (last-list-item nil)
         (current-page nil))
    (flet ((collector (node)
             (when (typep node '40ants-doc/commondoc/section:documentation-section)
               (let* ((html-fragment (documentation-section-uri-fragment node))
                      (page-uri
                        (when current-page
                          (format nil "~A"
                                  (40ants-doc/commondoc/page::html-filename current-page))))
                      
                      (text (common-doc:make-document-link page-uri
                                                           html-fragment
                                                           (common-doc:title node)))
                      (p (common-doc:make-paragraph text))
                      (li (common-doc:make-list-item p)))
                 (setf (common-doc:children (car current-sublist))
                       (append (common-doc:children (car current-sublist))
                               (list li)))
                 (setf last-list-item
                       li)))
             node)
           (on-down (node)
             
             (typecase node
               (40ants-doc/commondoc/page:page
                (setf current-page node))
               
               (40ants-doc/commondoc/section:documentation-section
                (push (common-doc:make-unordered-list (list))
                      current-sublist)
                (setf (common-doc:children last-list-item)
                      (append (common-doc:children last-list-item)
                              (list (car current-sublist)))))))
           (on-up (node)
             (when (typep node '40ants-doc/commondoc/section:documentation-section)
               (pop current-sublist))))
      (40ants-doc/commondoc/mapper:map-nodes document #'collector
                                             :on-going-down #'on-down
                                             :on-going-up #'on-up))
    
    (assert (= (length current-sublist) 1))
    (flet ((remove-empty-sublists (node)
             (when (typep node 'common-doc:list-item)
               (setf (common-doc:children node)
                     (loop for child in (common-doc:children node)
                           unless (null (common-doc:children child))
                           collect child)))
             node))
      (40ants-doc/commondoc/mapper:map-nodes (car current-sublist)
                                             #'remove-empty-sublists))))


(defmethod 40ants-doc/commondoc/page:make-page-toc ((obj 40ants-doc/commondoc/page:page))
  (or *main-toc*
      (make-toc obj)))
