(uiop:define-package #:40ants-doc/commondoc/page
  (:use #:cl)
  (:import-from #:common-doc)
  (:import-from #:common-html.emitter)
  (:import-from #:40ants-doc/commondoc/html
                #:with-html)
  (:import-from #:common-html.emitter
                #:define-emitter)
  (:import-from #:40ants-doc/commondoc/bullet)
  (:import-from #:40ants-doc/commondoc/section)
  (:import-from #:40ants-doc/reference-api)
  (:import-from #:40ants-doc/commondoc/mapper)
  (:import-from #:40ants-doc/ignored-words
                #:ignored-words
                #:supports-ignored-words-p)
  (:import-from #:40ants-doc/commondoc/piece
                #:doc-reference
                #:documentation-piece)
  (:export
   ;; #:ensure-page
   #:make-page
   #:page
   #:make-page-toc))
(in-package 40ants-doc/commondoc/page)


(defclass page (common-doc:content-node)
  ((html-filename :type pathname
                  :reader html-filename
                  :initarg :html-filename)))


(defmethod html-filename ((obj (eql :no-page)))
  "If page is unknown, then we'll return an empty name for a file. We need this for unit-tests only."
  "")


(defun make-page (sections html-filename)
  (make-instance 'page
                 :children (uiop:ensure-list sections)
                 :html-filename html-filename))


(defgeneric make-page-toc (page)
  (:method ((page t))
    nil))


(define-emitter (obj page)
  "Emit an piece of documentation."
  (let ((toc (make-page-toc obj)))
    (with-html
      (:html
       (:head
        (:meta :name "viewport"
               :content "width=device-width, initial-scale=1")
        (:title "Example page")
        (:link :rel "stylesheet"
               :type "text/css"
               :href "theme.css"))
       (:body
        (:div :id "content-container"
              (:div :id "toc"
                    (:div :id "page-toc"
                          (common-html.emitter::emit toc))
                    (:div :id "toc-footer"
                          (:a :href "https://40ants.com/doc"
                              "[generated by 40ANTS-DOC]")))
              (:div :id "content"
                    (mapc #'common-html.emitter::emit
                          (common-doc::children obj)))))))))


(defun collect-references (node &aux current-page results)
  "Returns a list of pairs where the CAR is 40ANTS-DOC/REFERENCE:REFERENCE object
   and CDR is 40ANTS-DOC/COMMONDOC/PAGE:PAGE."
  
  (flet ((track-page (node)
           (typecase node
             (page
              (setf current-page
                    node))))
         (collector (node)
           (let ((node
                   (when (typep node 'documentation-piece)
                     (doc-reference node))))
             (when node
               (push (cons node
                           (or current-page
                               :no-page))
                     results)))
           node))
    (40ants-doc/commondoc/mapper:map-nodes node #'collector
                                           :on-going-down #'track-page))

  results)


(defun replace-xrefs (node known-references &aux ignored-words
                                                 sections
                                                 (common-lisp-package (find-package :common-lisp)))
  "Replaces XREF with COMMON-DOC:WEB-LINK.

   Returns links which were not replaced because there wasn't
   a corresponding reference in the KNOWN-REFERENCES argument.

   KNOWING-REFERENCE argument should be a list of pairs
   of a COMMON-DOC:REFERENCE and a 40ANTS-DOC/COMMON-DOC/PAGE:PAGE objects.

   IGNORED-WORDS will be a list of list of strings where each sublist
   contains words, specified as IGNORE-WORDS argument of the 40ANTS-DOC:DEFSECTION macro.
  "
  
  (labels ((collect-ignored-words (node)
             (when (supports-ignored-words-p node)
               (let ((words (ignored-words node)))
                 (push words
                       ignored-words))))
           (pop-ignored-words (node)
             (when (supports-ignored-words-p node)
               (pop ignored-words)))
           (collect-section (node)
             (when (typep node '40ants-doc/commondoc/section:documentation-section)
               (push node sections)))
           (pop-section (node)
             (when (typep node '40ants-doc/commondoc/section:documentation-section)
               (pop sections)))
           (go-down (node)
             (collect-ignored-words node)
             (collect-section node))
           (go-up (node)
             (pop-ignored-words node)
             (pop-section node))
           (should-be-ignored-p (text symbol)
             (or (loop for sublist in ignored-words
                       thereis (member text sublist
                                       :test #'string=))
                 ;; This is a special case
                 ;; because we can't distinguish between absent SYMBOL
                 ;; and NIL.
                 (string= text
                          "NIL")
                 (and symbol
                      (eql (symbol-package symbol)
                           common-lisp-package))))
           (replacer (node)
             (typecase node
               (40ants-doc/commondoc/xref:xref
                (let* ((text (40ants-doc/commondoc/xref:xref-name node))
                       (symbol (40ants-doc/commondoc/xref:xref-symbol node))
                       (locative (40ants-doc/commondoc/xref:xref-locative node))
                       (should-be-ignored
                         (should-be-ignored-p text symbol))
                       (found-references
                         (unless should-be-ignored
                           (loop for (reference . page) in known-references
                                 ;; This can be a symbol or a string.
                                 ;; For example, for SYSTEM locative, object
                                 ;; is a string name of a system.
                                 ;; 
                                 ;; TODO: Think about a GENERIC to compare
                                 ;;       XREF with references of different locative types.
                                 for reference-object = (40ants-doc/reference::reference-object reference)
                                 when (and (etypecase reference-object
                                             (symbol
                                              (eql reference-object
                                                   symbol))
                                             (string
                                              ;; Here we intentionally use case insensitive
                                              ;; comparison, because a canonical reference
                                              ;; to ASDF system contains it's name in a lowercase,
                                              ;; but some other locatives like a PACKAGE, might
                                              ;; keep a name in the uppercase.
                                              (string-equal reference-object
                                                            text)))
                                           (or (null locative)
                                               (eql (40ants-doc/reference::reference-locative-type reference)
                                                    locative)))
                                 collect (cons reference
                                               page)))))

                  (cond
                    (should-be-ignored
                     (common-doc:make-code
                      (common-doc:make-text text)))
                    (found-references
                     (labels ((make-link (reference page text)
                                (let ((page-uri
                                        (when page
                                          (format nil "~A"
                                                  (40ants-doc/commondoc/page::html-filename page))))
                                      (html-fragment
                                        (40ants-doc/utils::html-safe-name
                                         (40ants-doc/reference::reference-to-anchor reference))))
                                  (common-doc:make-document-link page-uri
                                                                 html-fragment
                                                                 (common-doc:make-code
                                                                  (common-doc:make-text text))))))

                       (cond ((= (length found-references) 1)
                              (destructuring-bind (reference . page)
                                  (first found-references)
                                (let* ((object (40ants-doc/reference::resolve reference))
                                       (text (or (40ants-doc/commondoc/xref:link-text object)
                                                 text)))
                                  (make-link reference
                                             page
                                             text))))
                             (t
                              (common-doc:make-content
                               (append (list (common-doc:make-code
                                              (common-doc:make-text text))
                                             (common-doc:make-text " ("))
                                       (loop for (reference . page) in found-references
                                             for index upfrom 1
                                             for text = (format nil "~A" index)
                                             collect (make-link reference page text)
                                             unless (= index (length found-references))
                                             collect (common-doc:make-text " "))
                                       (list (common-doc:make-text ")"))))))))
                    
                    (t
                     (warn "Unable to find target for reference ~A mentioned at ~{~A~^ / ~}"
                           node
                           (loop for section in (reverse sections)
                                 for title = (common-doc.ops:collect-all-text
                                              (common-doc:title section))
                                 collect title))
                     node))))
               (t
                node))))
    (40ants-doc/commondoc/mapper:map-nodes node #'replacer
                                           :on-going-down #'go-down
                                           :on-going-up #'go-up)))
