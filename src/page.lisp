(uiop:define-package #:40ants-doc/page
  (:use #:cl)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/object-package)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/reference-api
                #:format-reference)
  (:import-from #:40ants-doc/link)
  (:import-from #:40ants-doc/builder/printer)
  (:import-from #:40ants-doc/builder/vars)
  (:import-from #:40ants-doc/locatives/dislocated)
  (:import-from #:40ants-doc/commondoc/builder)
  ;; TODO: solve circular dependency :(
  ;; (:import-from #:40ants-doc/commondoc/page)
  (:import-from #:40ants-doc/commondoc/format
                #:ensure-format-class-name)
  (:export
   #:make-page2
   #:ensure-page)
  (:export
   #:page-format
   #:base-filename
   #:page-base-dir
   #:page-base-url
   #:page-sections))
(in-package 40ants-doc/page)


(defclass page-common-mixin ()
  ((base-filename :initarg :base-filename
                  :reader base-filename
                  :type string
                  :documentation "Keeps a filename without extension.
                                  Extension will be added later during
                                  documentation generation phase.")
   (base-dir :initform nil
             :initarg :base-dir
             :reader page-base-dir)
   (base-url :initform nil
             :initarg :base-url
             :reader page-base-url)
   (format :initform nil
           :initarg :format
           :reader page-format
           :type (or null symbol))))


(defmethod initialize-instance :after ((instance page-common-mixin) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value instance 'format)
        (ensure-format-class-name
         (slot-value instance 'format))))


(defmethod page-base-url ((page (eql :no-page)))
  nil)

(defmethod page-format ((page (eql :no-page)))
  nil)


(defclass page2 (page-common-mixin)
  ((sections :initarg :sections
             :reader page-sections)))


(defmethod 40ants-doc/object-package:object-package ((obj page2))
  nil)


(defun make-base-filename (sections)
  (unless sections
    (error "Page should have at least one section."))
  (concatenate 'string
               (string-downcase
                (string-trim "@"
                             (40ants-doc:section-name
                              (first sections))))))


(defun make-page2 (sections &key base-filename
                                 base-dir
                                 base-url
                                 format)
  (let* ((sections (uiop:ensure-list sections))
         (base-filename (or base-filename
                            (make-base-filename sections))))
    (make-instance 'page2
                   :sections sections
                   :base-filename base-filename
                   :base-dir base-dir
                   :base-url base-url
                   :format format)))


(defun ensure-page (obj)
  (check-type obj (or 40ants-doc:section page2))
  (typecase obj
    (page2 obj)
    (t (make-page2 obj))))


(defmethod 40ants-doc/commondoc/builder:to-commondoc ((obj page2))
  (uiop:symbol-call :40ants-doc/commondoc/page :make-page
                    (mapcar #'40ants-doc/commondoc/builder:to-commondoc
                            (page-sections obj))
                    (base-filename obj)
                    :format (page-format obj)
                    :base-dir (page-base-dir obj)
                    :base-url (page-base-url obj)))
