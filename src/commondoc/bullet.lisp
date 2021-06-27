(uiop:define-package #:40ants-doc/commondoc/bullet
  (:use #:cl)
  (:import-from #:40ants-doc/builder/bullet)
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
   (arglist :initarg :arglist
            :reader bullet-arglist)))


(defun make-bullet (reference &key arglist children)
  (make-instance 'bullet
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
                            (40ants-doc/commondoc/arglist::arglist
                             (list arglist)))
                 :children (uiop:ensure-list children) ))


(common-html.emitter::define-emitter (obj bullet)
  "Emit an image."
  (let* ((reference (bullet-reference obj))
         (arglists (bullet-arglist obj))
         (locative-type (string-downcase
                         (40ants-doc/reference::reference-locative-type reference)))
         (name (prin1-to-string (40ants-doc/reference::reference-object reference)))
         (source-uri (40ants-doc/builder/bullet::source-uri reference))
         (spinneret:*html* common-html.emitter::*output-stream*))
    (spinneret:with-html
      (:li
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
                     (:span :class "reference-object"
                            (:a :href (format nil "#~A"
                                              (40ants-doc/utils::html-safe-name
                                               (40ants-doc/reference::reference-to-anchor reference)))
                                name)))

              (mapc #'common-html.emitter::emit
                    arglists)

              (mapc #'common-html.emitter::emit
                    (common-doc::children obj)))))))

