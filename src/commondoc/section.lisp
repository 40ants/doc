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
  (:export
   #:documentation-section
   #:section-definition))
(in-package 40ants-doc/commondoc/section)


(defclass documentation-section (common-doc:section)
  ((definition :initarg :definition
               :type 40ants-doc:section
               :reader section-definition))
  (:documentation "Objects of this class bind 40ANTS-DOC:SECTION to COMMON-DOC:SECTION
                   and can be rendered as part of the documentation."))


(defun make-section-body (section)
  (loop for entry in (40ants-doc:section-entries section)
        collect (40ants-doc/commondoc/builder:to-commondoc entry)))


(defun make-documentation-section (definition)
  (check-type definition 40ants-doc:section)
  
  (let ((title (common-doc:make-text (40ants-doc:section-title definition)))
        (children (make-section-body definition)))
    (make-instance 'documentation-section
                   :definition definition
                   :title title
                   :children children)))


(defmethod 40ants-doc/commondoc/builder:to-commondoc ((obj 40ants-doc:section))
  (make-documentation-section obj))


(common-html.emitter::define-emitter (obj documentation-section)
  "Emit a documentation section with a link."
  (let* ((definition (section-definition obj))
         (reference (40ants-doc/reference-api::canonical-reference definition)))
    (with-html
      (let ((uri (40ants-doc/utils::html-safe-name
                  (40ants-doc/reference::reference-to-anchor reference))))
        (:tag :name (format nil "h~A" *section-depth*)
              (progn
                    (emit (common-doc:title obj))
                    (values))
              (:a :href (format nil "#~A" uri)
                  :title "Permalink to this headline"
                  :id uri
                  :class "header-link"
                  "¶"))
        (incf *section-depth*)
        (emit (common-doc:children obj))
        (decf *section-depth*)))))


(defmethod 40ants-doc/commondoc/xref::link-text ((obj 40ants-doc:section))
  (40ants-doc:section-title obj))
