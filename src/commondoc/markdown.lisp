(uiop:define-package #:40ants-doc/commondoc/markdown
  (:use #:cl)
  (:import-from #:commondoc-markdown)
  (:import-from #:common-doc)
  (:export
   #:parse-markdown))
(in-package 40ants-doc/commondoc/markdown)


(defun replace-markdown-links (node)
  "Replaces unresolved markdown nodes with XREF objects."
  (flet ((replacer (node)
           (cond
             ((typep node 'commondoc-markdown::markdown-link)
              (let* ((children (common-doc:children node))
                     (first-child (first children)))
                (assert (typep first-child 'common-doc:text-node))
                 
                (let* ((text (common-doc:text first-child))
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
