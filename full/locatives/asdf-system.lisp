(uiop:define-package #:40ants-doc-full/locatives/asdf-system
  (:use #:cl)
  (:import-from #:str)
  (:import-from #:40ants-doc/locatives/base
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/reference)
  (:import-from #:common-doc
                #:make-paragraph
                #:make-unordered-list
                #:make-section
                #:make-list-item
                #:make-content
                #:make-web-link
                #:make-text)
  (:import-from #:40ants-doc-full/commondoc/section
                #:make-section-with-reference)
  (:import-from #:40ants-doc-full/commondoc/builder
                #:to-commondoc))
(in-package #:40ants-doc-full/locatives/asdf-system)

(define-locative-type asdf:system ()
  "Refers to an asdf system. The generated documentation will include
  meta information extracted from the system definition. This also
  serves as an example of a symbol that's not accessible in the
  current package and consequently is not exported.")

(defmethod locate-object (symbol (locative-type (eql 'asdf:system))
                          locative-args)
  (assert (endp locative-args))
  ;; FIXME: This is slow as hell.
  ;; TODO: check if replacement of find-system with registered-system helped
  (or (asdf:registered-system symbol)
      (locate-error)))

(defmethod canonical-reference ((system asdf:system))
  (40ants-doc/reference:make-reference (asdf:primary-system-name system)
                                       'asdf:system))

(defmethod find-source ((system asdf:system))
  `(:location
    (:file ,(namestring (asdf/system:system-source-file system)))
    (:position 1)
    (:snippet "")))

(defmethod to-commondoc ((system asdf:system))
  (let ((title (format nil "~A ASDF System Details"
                       (string-upcase
                        (asdf:primary-system-name system)))))
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
                   (cond
                     ((eql type :asdf-systems)
                      (make-content
                       (list*
                        (make-text
                         (format nil "~A: "
                                 name))
                        (loop with first = t
                              for system-name in value
                              if first
                                do (setf first nil)
                              else
                                collect (make-text ", ")
                              collect (make-web-link (format nil "https://quickdocs.org/~A"
                                                             system-name)
                                                     (make-text system-name))))))
                     (href
                      (make-content
                       (list (make-text
                              (format nil "~A: "
                                      name))
                             (make-web-link href
                                            (make-text value)))))
                     (t
                      (make-text
                       (format nil "~A: ~A"
                               name
                               value))))))))))
      
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
                                :type :source-control)
                          (item "Depends on" 'asdf-system-dependencies
                                :type :asdf-systems)))
             (children (make-unordered-list
                        (remove nil items)))
             (reference (40ants-doc/reference-api:canonical-reference system)))
        (make-section-with-reference title
                                     children
                                     reference)))))

(defvar end-of-asdf-example)


(defgeneric asdf-system-dependencies (system)
  (:method ((system-name string))
    (asdf-system-dependencies (asdf:registered-system system-name)))
  (:method ((system-name symbol))
    (asdf-system-dependencies (asdf:registered-system system-name)))
  (:method ((system asdf:system))
    (loop with base-system = (asdf:primary-system-name system)
          with results = nil
          for name in (asdf:system-depends-on system)
          for subsystem = (string-equal base-system
                                        (asdf:primary-system-name name))
          do (if subsystem
                 (setf results
                       (nunion results
                               (asdf-system-dependencies name)
                               :test #'string-equal ))
                 (pushnew name results
                          :test #'string-equal))
          finally (return (sort results
                                #'string<)))))


(defun comma-separated-dependencies (system)
  (str:join ", " (asdf-system-dependencies system)))
