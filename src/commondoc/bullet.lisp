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
  (:import-from #:40ants-doc/commondoc/arglist))
(in-package 40ants-doc/commondoc/bullet)


(defclass bullet (common-doc:content-node)
  ((bullet-reference :initarg :bullet-reference
                     :reader bullet-reference)
   (name :initarg :name
         :initform nil
         :reader bullet-name)
   (arglist :initarg :arglist
            :initform nil
            :reader bullet-arglist)))


(defun make-bullet (reference &key arglist children name)
  ;; TODO: remove this printer format and reference resolving on this stage.
  ;; we only need to know format to render arglist to a string, but this
  ;; shouldn't be necessary on this stage.
  (let ((40ants-doc/builder/printer::*format* :plain))
    (make-instance 'bullet
                   :name name
                   :bullet-reference  reference
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
                   :children (uiop:ensure-list children) )))


(common-html.emitter::define-emitter (obj bullet)
  "Emit an piece of documentation."
  (let* ((reference (bullet-reference obj))
         (arglists (bullet-arglist obj))
         (locative-type (string-downcase
                         (40ants-doc/reference::reference-locative-type reference)))
         (name (or (bullet-name obj)
                   (prin1-to-string (40ants-doc/reference::reference-object reference))))
         ;; TODO: move source-uri to reference-api
         (source-uri (uiop:symbol-call :40ants-doc/builder/bullet :source-uri reference))
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
                            (let ((uri (40ants-doc/utils::html-safe-name
                                        (40ants-doc/reference::reference-to-anchor reference))))
                              (:a :href (format nil "#~A" uri)
                                  :id uri
                                  name))))
               
               (mapc #'common-html.emitter::emit
                     arglists)

               (mapc #'common-html.emitter::emit
                     (common-doc::children obj))))))))

