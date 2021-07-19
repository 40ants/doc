(uiop:define-package #:40ants-doc/commondoc/markdown
  (:use #:cl)
  (:import-from #:commondoc-markdown)
  (:import-from #:common-doc)
  (:import-from #:40ants-doc/commondoc/xref)
  (:export
   #:parse-markdown))
(in-package 40ants-doc/commondoc/markdown)


;; TODO: Remove if common-doc.ops:collect-all-text will be OK:
;; 
;; (defgeneric node-to-text (node)
;;   (:method ((node common-doc:bold))
;;     (node-to-text
;;      (common-doc:children node)))
;;   (:method ((node common-doc:italic))
;;     (node-to-text
;;      (common-doc:children node)))
;;   (:method ((node common-doc:text-node))
;;     (common-doc:text node))
;;   (:method ((node list))
;;     (apply #'concatenate 'string
;;            (mapcar #'node-to-text
;;                    node))))


(defun replace-markdown-links (node)
  "Replaces unresolved markdown nodes with XREF objects."
  (flet ((replacer (node)
           (cond
             ((typep node 'commondoc-markdown::markdown-link)
              (let* ((children (common-doc:children node)))
                
                (let* ((text (common-doc.ops:collect-all-text children))
                       (symbol (40ants-doc/swank::read-locative-from-string text))
                       (locative-name (commondoc-markdown:markdown-link-definition node))
                       (locative (when locative-name
                                   (40ants-doc/swank::read-locative-from-string locative-name
                                                                                :package (find-package "40ANTS-DOC/LOCATIVES")))))
                  (40ants-doc/commondoc/xref:make-xref text
                                                       :symbol symbol
                                                       :locative locative))))
             (t
              node))))
    (40ants-doc/commondoc/mapper:map-nodes node #'replacer)))


(defun parse-markdown (text)
  (replace-markdown-links
   (common-doc.format:parse-document (make-instance 'commondoc-markdown:markdown)
                                     text)))
