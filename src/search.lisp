(defpackage #:40ants-doc/search
  (:use #:cl)
  (:import-from #:40ants-doc/commondoc/mapper
                #:map-nodes)
  (:import-from #:40ants-doc/commondoc/section))
(in-package 40ants-doc/search)


(defun objects-to-alist (hash)
  (list* :obj
         (loop for prefix being the hash-key of hash
               using (hash-value subhash)
               collect (cons prefix
                             (list* :obj
                                    (loop for name being the hash-key of subhash
                                          using (hash-value values)
                                          collect (cons name values)))))))


(defun generate-search-index (document)
  (40ants-doc/commondoc/format:with-format (:html)
    (let ((docnames nil)
          (filenames nil)
          (objects (make-hash-table :test 'equal))
          (objnames '(("lisp" "symbol" "Lisp Symbol"))) ;; names of types from objtypes
          (objtypes '("lisp:symbol"))
          (terms nil)                   ; map of terms to indices in filenames
          (titles nil)
          (titleterms nil)
          (current-page nil)
          (document-idx -1))
      (labels ((first-section (page)
                 (loop for child in (common-doc:children page)
                       when (typep child '40ants-doc/commondoc/section:documentation-section)
                       do (return child)))
               (process (node)
                 (typecase node
                   (40ants-doc/commondoc/bullet::bullet
                    (when current-page
                      (let* ((reference (40ants-doc/commondoc/piece:doc-reference node))
                             (symbol (40ants-doc/reference:reference-object reference))
                             (package (symbol-package symbol))
                             (prefix (if package
                                         (package-name
                                          package)
                                         ""))
                             
                             (name (symbol-name symbol))
                             (html-fragment (common-doc:reference node))
                             (object (list document-idx 0 2 html-fragment))
                             (prefixed (or (gethash prefix objects)
                                           (setf (gethash prefix objects)
                                                 (make-hash-table :test 'equal)))))
                        (setf (gethash name prefixed)
                              object)))))
                 node)
               (go-down (node)
                 (typecase node
                   (40ANTS-DOC/COMMONDOC/PAGE:PAGE
                    (when (member (40ants-doc/page:page-format node)
                                  (list 'common-html:html nil))
                      (setf current-page node)
                      (incf document-idx)
                      (push (common-doc.ops:collect-all-text
                             (common-doc:title
                              (first-section node)))
                            titles)
                      (push (40ants-doc/page:base-filename node)
                            docnames)
                      (push (40ants-doc/commondoc/page:full-filename node)
                            filenames))))
                 node)
               (go-up (node)
                 (typecase node
                   (40ANTS-DOC/COMMONDOC/PAGE:PAGE
                    (when (member (40ants-doc/page:page-format node)
                                  (list 'common-html:html nil))
                      (setf current-page nil))))
                 node))
        (map-nodes document #'process
                   :on-going-down #'go-down
                   :on-going-up #'go-up))
      (let ((index (list :obj
                         (cons "docnames" (nreverse docnames))
                         (cons "filenames" (nreverse filenames))
                         (cons "objects" (objects-to-alist objects))
                         (cons "objnames" objnames)
                         (cons "objtypes" objtypes)
                         (cons "terms" terms)
                         (cons "titles" (nreverse titles))
                         (cons "titleterms" titleterms))))
        (with-output-to-string (s)
          (write-string "Search.setIndex(" s)
          (write-string (jonathan:to-json index :from :jsown) s)
          (write-string ")" s))))))
