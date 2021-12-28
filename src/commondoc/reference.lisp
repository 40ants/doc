(defpackage #:40ants-doc/commondoc/reference
  (:use #:cl)
  (:import-from #:40ants-doc/commondoc/section
                #:section-definition
                #:documentation-section)
  (:import-from #:40ants-doc
                #:section-external-docs)
  (:import-from #:40ants-doc/external-index
                #:read-references-index)
  (:import-from #:40ants-doc/commondoc/piece
                #:doc-reference
                #:documentation-piece)
  (:import-from #:alexandria
                #:when-let)
  (:import-from #:40ants-doc/commondoc/page
                #:page)
  (:import-from #:40ants-doc/reference
                #:reference=)
  (:import-from #:40ants-doc/commondoc/mapper
                #:map-nodes))
(in-package 40ants-doc/commondoc/reference)


(defun append-no-page-to (references)
  (loop for ref in references
        collect (cons ref :no-page)))


(defun collect-references (node &aux current-page results)
  "Returns a list of pairs where the CAR is 40ANTS-DOC/REFERENCE:REFERENCE object
   and CDR is 40ANTS-DOC/COMMONDOC/PAGE:PAGE."
  
  (flet ((track-page (node)
           (typecase node
             (page
              (setf current-page
                    node))))
         (collector (node)
           (when (typep node 'documentation-section)
             (loop with section = (section-definition node)
                   for url in (section-external-docs section)
                   for references = (read-references-index url)
                   do (setf results
                            (nunion results
                                   (append-no-page-to references)
                                   :key #'car
                                   :test #'reference=))))
           (when (typep node 'documentation-piece) 
             (when-let ((reference (doc-reference node)))
               (pushnew (cons reference
                              (or current-page
                                  :no-page))
                        results
                        :key #'car
                        :test #'reference=)))
           node))
    (map-nodes node #'collector
               :on-going-down #'track-page))

  results)
