(uiop:define-package #:40ants-doc/commondoc/bullet
  (:use #:cl)
  (:import-from #:common-html.emitter
                #:with-tag)
  (:import-from #:common-html.emitter
                #:with-tag)
  (:import-from #:common-html.emitter
                #:with-tag)
  (:import-from #:common-html.emitter
                #:with-tag)
  (:import-from #:common-html.emitter
                #:with-tag)
  (:import-from #:40ants-doc/render/args
                #:arglist-to-string)
  (:import-from #:40ants-doc/commondoc/arglist)
  (:import-from #:40ants-doc/ignored-words
                #:ignored-words)
  (:import-from #:40ants-doc/dislocated-symbols
                #:dislocated-symbols)
  (:import-from #:40ants-doc/commondoc/piece
                #:documentation-piece
                #:doc-reference)
  (:import-from #:40ants-doc/utils
                #:maybe-downcase)
  (:export
   #:make-bullet))
(in-package 40ants-doc/commondoc/bullet)


(defclass bullet (documentation-piece common-doc:content-node)
  ((name :initarg :name
         :initform nil
         :reader bullet-name)
   (arglist :initarg :arglist
            :initform nil
            :reader bullet-arglist)
   (ignored-words :initarg :ignore-words
                  :initform nil
                  :reader ignored-words)
   (dislocated-symbols :initarg :dislocated-symbols
                       :initform nil
                       :reader dislocated-symbols)))


(defmethod 40ants-doc/ignored-words:supports-ignored-words-p ((obj bullet))
  t)


(defmethod 40ants-doc/dislocated-symbols:supports-dislocated-symbols-p ((obj bullet))
  t)


(defun make-bullet (reference &key arglist
                                   children
                                   name
                                   ignore-words
                                   dislocated-symbols)
  "Creates a CommonDoc node to represent a documentation item.

   Documentation item can have an ARGLIST. If NAME is not given,
   then it will be made from reference's object printed representation.

   You can provide a CHILDREN arguments. It should be a list of CommonDoc nodes
   or a single node.

   IGNORE-WORDS can be a list with the same meaning as 40ANTS-DOC:DEFSECTION.

   If you want to completely ignore some symbol inside the reference's documentation,
   then use DISPLOCATED-SYMBOLS argument.
"
  ;; TODO: remove this printer format and reference resolving on this stage.
  ;; we only need to know format to render arglist to a string, but this
  ;; shouldn't be necessary on this stage.
  (let ((html-fragment (40ants-doc/utils::html-safe-name
                        (40ants-doc/reference::reference-to-anchor reference))))
    (make-instance 'bullet
                   :name name
                   :doc-reference reference
                   :ignore-words (uiop:ensure-list ignore-words)
                   :dislocated-symbols (uiop:ensure-list dislocated-symbols)
                   ;; This argument should be a list of
                   ;; ARGLIST objects.
                   :arglist (etypecase arglist
                              (list (cond
                                      ((null arglist)
                                       arglist)
                                      ((typep (first arglist) '40ants-doc/commondoc/arglist::arglist)
                                       arglist)
                                      (t (list
                                          (40ants-doc/commondoc/arglist::make-arglist
                                           (arglist-to-string arglist))))))
                              (string
                               (list (40ants-doc/commondoc/arglist::make-arglist arglist)))
                              (40ants-doc/commondoc/arglist::arglist
                               (list arglist)))
                   :reference html-fragment
                   :children (uiop:ensure-list children) )))


(common-html.emitter::define-emitter (obj bullet)
  "Emit an piece of documentation."
  (let* ((reference (doc-reference obj))
         (arglists (bullet-arglist obj))
         (locative-type (string-downcase
                         (40ants-doc/reference::reference-locative-type reference)))
         (name (or (bullet-name obj)
                   (princ-to-string (40ants-doc/reference::reference-object reference))))
         (source-uri (40ants-doc/reference-api:source-uri reference))
         (spinneret:*html* common-html.emitter::*output-stream*))
    (spinneret:with-html
      (:ul
       (:li
        (when (common-doc:reference obj)
          (:a :href (format nil "#~A"
                            (common-doc:reference obj))))
        (:span :class "reference-bullet"
               (:span :class "reference"
                      (if source-uri
                          (:a :href source-uri
                              :class "locative-type"
                              (format nil "[~A]"
                                      locative-type))
                          (:span :class "locative-type"
                                 (format nil "[~A]"
                                         locative-type)))
                      (:div :class "reference-object"
                            :style "display: inline-block"
                            (let ((uri (common-doc:reference obj)))
                              (:a :href (format nil "#~A" uri)
                                  :id uri
                                  (maybe-downcase name)))))
               
               (mapc #'common-html.emitter::emit
                     (maybe-downcase arglists))

               (mapc #'common-html.emitter::emit
                     (common-doc::children obj))))))))


(defmethod common-doc.format:emit-document ((format commondoc-markdown:markdown)
                                            (node bullet)
                                            stream)
  (let* ((reference (doc-reference node))
         (arglists (bullet-arglist node))
         (locative-type (string-downcase
                         (40ants-doc/reference::reference-locative-type reference)))
         (name (or (bullet-name node)
                   (princ-to-string (40ants-doc/reference::reference-object reference))))
         (source-uri (40ants-doc/reference-api:source-uri reference)))

    (let ((commondoc-markdown/emitter::*header-level* (or (and (boundp 'commondoc-markdown/emitter::*header-level*)
                                                               (1+ commondoc-markdown/emitter::*header-level*))
                                                          1)))


      (commondoc-markdown/emitter::write-header
       format
       (list* (if source-uri
                  (format nil "[~A](~A) `~A`"
                          locative-type
                          source-uri
                          (maybe-downcase name))
                  (format nil "[~A] `~A`"
                          locative-type
                          (maybe-downcase name)))
              (maybe-downcase arglists))
       stream)

      (format stream "~&")
      
      (call-next-method)

      (format stream "~&"))))
