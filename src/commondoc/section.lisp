(uiop:define-package #:40ants-doc/commondoc/section
  (:use #:cl)
  (:import-from #:40ants-doc)
  (:import-from #:common-doc)
  (:import-from #:40ants-doc/commondoc/html
                #:with-html)
  (:import-from #:common-html.emitter
                #:*section-depth*)
  (:import-from #:common-html.emitter
                #:*section-depth*)
  (:import-from #:common-html.emitter
                #:*section-depth*)
  (:import-from #:common-html.emitter
                #:emit)
  (:import-from #:common-html.emitter
                #:emit)
  (:import-from #:40ants-doc/commondoc/xref
                #:make-xref)
  (:import-from #:40ants-doc/ignored-words)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/commondoc/piece
                #:documentation-piece)
  (:import-from #:40ants-doc/reference
                #:reference-locative
                #:reference-object)
  (:import-from #:40ants-doc/object-package
                #:object-package)
  (:export
   #:documentation-section
   #:section-definition
   #:make-section-with-reference))
(in-package 40ants-doc/commondoc/section)


(defclass section-with-reference (documentation-piece common-doc:section)
  ())


(defclass documentation-section (section-with-reference)
  ((definition :initarg :definition
               :type 40ants-doc:section
               :reader section-definition))
  (:documentation "Objects of this class bind 40ANTS-DOC:SECTION to COMMON-DOC:SECTION
                   and can be rendered as part of the documentation."))


(defun make-section-body (section)
  (loop for entry in (40ants-doc:section-entries section)
        collect (40ants-doc/commondoc/builder:to-commondoc entry)))


(defun make-section-with-reference (title children reference)
  (check-type reference 40ants-doc/reference::reference)
  
  (let ((title (loop for item in (uiop:ensure-list title)
                     collect (etypecase item
                               (string (common-doc:make-text item))
                               (common-doc:document-node item)))))
    (make-instance 'section-with-reference
                   :title title
                   :doc-reference reference
                   :children (uiop:ensure-list children))))


(defun make-documentation-section (section &key (class-name 'documentation-section))
  (check-type section 40ants-doc:section)
  
  (let* ((link-to (40ants-doc:section-link-title-to section))
         (title-text (common-doc:make-text (or (40ants-doc:section-title section)
                                               "Untitled")))
         (title (list
                 (if link-to
                     (make-xref title-text
                                :symbol (reference-object link-to)
                                :locative (reference-locative link-to))
                     title-text)))
        (children (make-section-body section)))

    (let* ((reference (40ants-doc/reference-api::canonical-reference section))
           (html-fragment (40ants-doc/utils::html-safe-name
                           (40ants-doc/reference::reference-to-anchor reference))))
    
      (make-instance class-name
                     :definition section
                     :doc-reference (40ants-doc/reference-api::canonical-reference
                                     section)
                     :title title
                     :reference html-fragment
                     :children children))))


(defmethod 40ants-doc/ignored-words:supports-ignored-words-p ((obj documentation-section))
  t)


(defmethod 40ants-doc/ignored-words:ignored-words ((obj documentation-section))
  (let ((definition (section-definition obj)))
    (40ants-doc::section-ignore-words definition)))


(defmethod 40ants-doc/commondoc/builder:to-commondoc ((obj 40ants-doc:section))
  (make-documentation-section obj))


(defgeneric emit-html-after-title (obj)
  (:method ((obj documentation-section))
    (let ((uri-fragment (common-doc:reference obj)))
      (with-html
        (:a :href (format nil "#~A" uri-fragment)
            :title "Permalink to this headline"
            :id uri-fragment
            :class "header-link"
            "¶")))))


(common-html.emitter::define-emitter (obj documentation-section)
  "Emit a documentation section with a link."
  (let (;; Here we change package to a package for which section was defined,
        ;; to make all symbols from this package be printed in their short form
        ;; without package prefix.
        (*package* (object-package obj)))
    (with-html
      (:tag :name (format nil "h~A" *section-depth*)
            (progn
              (emit (common-doc:title obj))
              (values))
            (emit-html-after-title obj))
      (incf *section-depth*)
      (emit (common-doc:children obj))
      (decf *section-depth*))))


(defmethod common-doc.format:emit-document ((format commondoc-markdown:markdown)
                                            (node documentation-section)
                                            stream)
  ;; Here we change package to a package for which section was defined,
  ;; to make all symbols from this package be printed in their short form
  ;; without package prefix.
  (let ((*package* (object-package node)))
    (call-next-method)))


(defmethod 40ants-doc/commondoc/xref:link-text ((obj 40ants-doc:section))
  (40ants-doc:section-title obj))


(defun fill-html-fragments (document)
  (flet ((fill-fragments (node)
           (when (and (typep node 'common-doc:section)
                      (not (common-doc:reference node)))
             (let* ((title (common-doc:title node))
                    (text (common-doc.ops:collect-all-text title))
                    (reference (str:param-case text)))
               (setf (common-doc:reference node)
                     reference)))
           node))
    (40ants-doc/commondoc/mapper:map-nodes document #'fill-fragments)))
