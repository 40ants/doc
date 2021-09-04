(defpackage #:40ants-doc/commondoc/image
  (:use #:cl)
  (:import-from #:common-html.emitter
                #:define-emitter)
  (:import-from #:str)
  (:import-from #:40ants-doc/commondoc/html
                #:with-html)
  (:import-from #:40ants-doc/commondoc/page
                #:make-page-uri)
  (:import-from #:40ants-doc/utils
                #:make-relative-path)
  (:import-from #:cl-ppcre
                #:do-register-groups))
(in-package 40ants-doc/commondoc/image)

(defclass local-image (common-doc:image)
  ((width :initform nil
          :initarg :width
          :reader width)
   (height :initform nil
          :initarg :height
          :reader height)))

(defun make-local-image (path &key width height)
  (unless (probe-file path)
    (error "Image file \"~A\" not found"
           path))
  (make-instance 'local-image :source path
                              :width width
                              :height height))

(defun replace-images (document)
  (flet ((replacer (node)
           (typecase node
             (common-doc:image
              (let ((source (common-doc:source node)))
                (if (or (str:starts-with-p "http:" source)
                        (str:starts-with-p "https:" source))
                    node
                    (multiple-value-bind (source width height)
                        (extract-width-and-height source)
                      (make-local-image source
                                        :width width
                                        :height height)))))
             (t node))))
    (40ants-doc/commondoc/mapper:map-nodes document #'replacer)))

(defun extract-width-and-height (path)
  "Returns 3 values, real path, width and height. Width and height might be NIL.

   For example, on \"blah.png{height=400,width=300}\" it will return:

   blah.png
   300
   400"
  
  (let ((width nil)
        (height nil))
    (do-register-groups (name value) ("[{,](.*?)=(.[^,}]*)"
                                      path)
      (cond
        ((string-equal name "width")
         (setf width
               value))
        ((string-equal name "height")
         (setf height
               value))
        (t
         (error "Parameter \"~A\" is not supported."
                name))))

    (values
     (first (str:split "{" path))
     width
     height)))

(define-emitter (obj local-image)
  "Emit a local-image and move referenced image into the HTML documentation folder."
  (let* ((original-path (common-doc:source obj))
         (target-path (uiop:merge-pathnames* original-path
                                             (uiop:merge-pathnames* #P"images/"
                                                                    (uiop:ensure-directory-pathname
                                                                     40ants-doc/builder/vars::*base-dir*))))
         (page-uri (make-page-uri 40ants-doc/builder/vars::*current-page*))
         (new-source (make-relative-path page-uri
                                         (format nil "images/~A"
                                                 original-path)))
         (src (if common-html.emitter:*image-format-control*
                  (format nil common-html.emitter:*image-format-control*
                          new-source)
                  new-source))
         (description (common-doc:description obj)))

    (ensure-directories-exist target-path)
    (uiop:copy-file original-path
                    target-path)
    (with-html
      (:img :src src
            :alt description
            :title description
            :width (width obj)
            :height (height obj)))))
