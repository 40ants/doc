(uiop:define-package #:40ants-doc/commondoc/toc
  (:use #:cl)
  (:import-from #:40ants-doc/commondoc/section
                #:documentation-section-uri-fragment))
(in-package 40ants-doc/commondoc/toc)


(defun make-toc (document)
  "Collects toplevel objects of DOCUMENT-OBJECT class and returns a table of content CommonDoc node."
  (let* ((current-sublist (list (common-doc:make-unordered-list (list))))
         (last-list-item nil))
    (flet ((collector (node)
             (when (typep node '40ants-doc/commondoc/section:documentation-section)
               (let* ((uri-fragment (documentation-section-uri-fragment node))
                      (text (common-doc:make-web-link (format nil "#~A"
                                                              uri-fragment)
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
             (when (typep node '40ants-doc/commondoc/section:documentation-section)
               
               (push (common-doc:make-unordered-list (list))
                     current-sublist)

               (setf (common-doc:children last-list-item)
                     (append (common-doc:children last-list-item)
                             (list (car current-sublist))))))
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


;; (defun make-toc (document)
;;   "Collects toplevel objects of DOCUMENT-OBJECT class and returns a table of content CommonDoc node."
;;   (let* (
;;          ;; Alist with a tree
;;          (results nil)
;;          (prev-subtries nil)
;;          (current-subtree results))
;;     (flet ((collector (node)
;;              (when (typep node '40ants-doc/commondoc/section:documentation-section)
;;                (push (cons node nil)
;;                      current-subtree))
;;              node)
;;            (on-down (node)
;;              (when (typep node '40ants-doc/commondoc/section:documentation-section)
;;                (let ((new-subtree (list)))
;;                  (push (cons node new-subtree)
;;                        current-subtree)
;;                  (push current-subtree prev-subtries)
;;                  (format t "PUSH prev-subtries = ~A~%"
;;                          prev-subtries)
;;                  (setf current-subtree new-subtree))))
;;            (on-up (node)
;;              (when (typep node '40ants-doc/commondoc/section:documentation-section)
;;                (setf current-subtree
;;                      (pop prev-subtries))
;;                (format t "POP prev-subtries = ~A~%"
;;                        prev-subtries)
;;                )))
;;       (40ants-doc/commondoc/mapper:map-nodes document #'collector
;;                                              :on-going-down #'on-down
;;                                              :on-going-up #'on-up))
;;     current-subtree))

;; (defun make-toc (document)
;;   "Collects toplevel objects of DOCUMENT-OBJECT class and returns a table of content CommonDoc node."
;;   (let* (
;;          ;; Alist with a tree
;;          (level 0)
;;          (results nil))
;;     (flet ((collector (node)
;;              (when (typep node '40ants-doc/commondoc/section:documentation-section)
;;                (pushnew (cons node level)
;;                         results
;;                         :key #'car
;;                         :test #'eql))
;;              node)
;;            (on-down (node)
;;              (when (typep node '40ants-doc/commondoc/section:documentation-section)
;;                (incf level)))
;;            (on-up (node)
;;              (when (typep node '40ants-doc/commondoc/section:documentation-section)
;;                (decf level))))
;;       (40ants-doc/commondoc/mapper:map-nodes document #'collector
;;                                              :on-going-down #'on-down
;;                                              :on-going-up #'on-up))
;;     (loop with current-level = 0
;;           with sections = nil
;;           for (section . level) in results
;;           when (= level current-level)
;;           do (push (cons section level)
;;                    sections)
;;           unless (= level current-level)
;;           do append (nreverse sections)
;;           )))
