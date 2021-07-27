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
  (:import-from #:40ants-doc/commondoc/xref)
  (:import-from #:40ants-doc/ignored-words)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/commondoc/piece
                #:documentation-piece)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/object-package)
  (:export
   #:documentation-section
   #:section-definition
   #:documentation-section-uri-fragment
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


(defun make-documentation-section (definition)
  (check-type definition 40ants-doc:section)
  
  (let ((title (common-doc:make-text (40ants-doc:section-title definition)))
        (children (make-section-body definition)))

    (let* ((reference (40ants-doc/reference-api::canonical-reference definition))
           (html-fragment (40ants-doc/utils::html-safe-name
                           (40ants-doc/reference::reference-to-anchor reference))))
    
      (make-instance 'documentation-section
                     :definition definition
                     :doc-reference (40ants-doc/reference-api::canonical-reference
                                     definition)
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


(defun documentation-section-uri-fragment (obj)
  (check-type obj documentation-section)
  
  (let* ((definition (section-definition obj))
         (reference (40ants-doc/reference-api::canonical-reference definition)))
    (40ants-doc/utils::html-safe-name
     (40ants-doc/reference::reference-to-anchor reference))))


(common-html.emitter::define-emitter (obj documentation-section)
  "Emit a documentation section with a link."
  (let ((uri-fragment (documentation-section-uri-fragment obj)))
    (with-html
      (:tag :name (format nil "h~A" *section-depth*)
            (progn
              (emit (common-doc:title obj))
              (values))
            (:a :href (format nil "#~A" uri-fragment)
                :title "Permalink to this headline"
                :id uri-fragment
                :class "header-link"
                "¶"))
      (incf *section-depth*)
      (emit (common-doc:children obj))
      (decf *section-depth*))))


(defmethod 40ants-doc/commondoc/xref::link-text ((obj 40ants-doc:section))
  (40ants-doc:section-title obj))


(defmethod 40ants-doc/object-package:object-package ((obj documentation-section))
  (let* ((section (section-definition obj))
         (name (40ants-doc:section-name section)))
    (40ants-doc/object-package:object-package name)))
