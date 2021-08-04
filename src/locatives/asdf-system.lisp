(defpackage #:40ants-doc/locatives/asdf-system
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc/document
                #:document-object)
  (:import-from #:40ants-doc/render/args)
  (:import-from #:40ants-doc/builder/bullet)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/args)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/builder/vars)
  (:import-from #:40ants-doc/render/print)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/page)
  (:import-from #:40ants-doc/builder/heading)
  (:import-from #:common-doc
                #:make-paragraph
                #:make-unordered-list
                #:make-section
                #:make-list-item
                #:make-content
                #:make-web-link
                #:make-text)
  (:import-from #:40ants-doc/commondoc/section
                #:make-section-with-reference))
(in-package 40ants-doc/locatives/asdf-system)

(define-locative-type asdf:system ()
  "Refers to an asdf system. The generated documentation will include
  meta information extracted from the system definition. This also
  serves as an example of a symbol that's not accessible in the
  current package and consequently is not exported.")

(defmethod locate-object (symbol (locative-type (eql 'asdf:system))
                          locative-args)
  (assert (endp locative-args))
  ;; FIXME: This is slow as hell.
  (or (asdf:find-system symbol nil)
      (locate-error)))

(defmethod canonical-reference ((system asdf:system))
  (40ants-doc/reference::make-reference (asdf::primary-system-name system) 'asdf:system))

;; (defmethod document-object ((system asdf:system) stream)
;;   (40ants-doc/builder/heading::with-heading (stream system
;;                                              (format nil "~A ASDF System Details"
;;                                                      (string-upcase
;;                                                       (asdf::primary-system-name system))))
;;     (flet ((foo (name fn &key type)
;;              (let ((value (funcall fn system)))
;;                (when value
;;                  (case type
;;                    ((:link)
;;                     (format stream "- ~A: [~A](~A)~%" name value value))
;;                    ((:mailto)
;;                     (format stream "- ~A: [~A](mailto:~A)~%"
;;                             name value value))
;;                    ((:source-control)
;;                     (format stream "- ~A: [~A](~A)"
;;                             name (first value) (second value)))
;;                    ((nil)
;;                     (format stream "- ~A: ~A~%" name value)))))))
;;       (foo "Version" 'asdf/component:component-version)
;;       (foo "Description" 'asdf/system:system-description)
;;       (foo "Licence" 'asdf/system:system-licence)
;;       (foo "Author" 'asdf/system:system-author)
;;       (foo "Maintainer" 'asdf/system:system-maintainer)
;;       (foo "Mailto" 'asdf/system:system-mailto :type :mailto)
;;       (foo "Homepage" 'asdf/system:system-homepage :type :link)
;;       (foo "Bug tracker" 'asdf/system:system-bug-tracker :type :link)
;;       (foo "Source control" 'asdf/system:system-source-control
;;            :type :source-control)
;;       (terpri stream))))


(defmethod 40ants-doc/commondoc/builder:to-commondoc ((system asdf:system))
  (let ((title (format nil "~A ASDF System Details"
                       (string-upcase
                        (asdf::primary-system-name system)))))
    (flet ((item (name getter &key type)
             (let* ((value (funcall getter system))
                    (href nil))
               (when value
                 (case type
                   (:link (setf href value))
                   (:mailto (setf href (format nil "mailto:~A"
                                               value)))
                   (:source-control (psetf value (format nil "~A"
                                                         (first value))
                                           href (second value))))
                 (make-list-item
                  (make-paragraph
                   (if href
                       (make-content
                        (list (make-text
                               (format nil "~A: "
                                       name))
                              (make-web-link href
                                             (make-text value))))
                       (make-text
                        (format nil "~A: ~A"
                                name
                                value)))))))))
      
      (let* ((items (list (item "Version" 'asdf/component:component-version)
                          (item "Description" 'asdf/system:system-description)
                          (item "Licence" 'asdf/system:system-licence)
                          (item "Author" 'asdf/system:system-author)
                          (item "Maintainer" 'asdf/system:system-maintainer)
                          (item "Mailto" 'asdf/system:system-mailto
                                :type :mailto)
                          (item "Homepage" 'asdf/system:system-homepage
                                :type :link)
                          (item "Bug tracker" 'asdf/system:system-bug-tracker
                                :type :link)
                          (item "Source control" 'asdf/system:system-source-control
                                :type :source-control)))
             (children (make-unordered-list
                        (remove nil items)))
             (reference (40ants-doc/reference-api::canonical-reference system)))
        (make-section-with-reference title
                                     children
                                     reference)))))

(defmethod find-source ((system asdf:system))
  `(:location
    (:file ,(namestring (asdf/system:system-source-file system)))
    (:position 1)
    (:snippet "")))

(defvar end-of-asdf-example)
