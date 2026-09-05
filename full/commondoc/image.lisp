(uiop:define-package #:40ants-doc-full/commondoc/image
  (:use #:cl)
  (:import-from #:common-html.emitter
                #:define-emitter)
  (:import-from #:str)
  (:import-from #:log)
  (:import-from #:common-doc)
  (:import-from #:40ants-doc-full/commondoc/html
                #:with-html)
  (:import-from #:40ants-doc-full/commondoc/page
                #:make-page-uri)
  (:import-from #:40ants-doc-full/utils
                #:make-relative-path)
  (:import-from #:cl-ppcre
                #:do-register-groups)
  (:import-from #:40ants-doc-full/builder/vars
                #:*current-asdf-system*)
  (:import-from #:40ants-doc-full/commondoc/mapper
                #:map-nodes)
  (:import-from #:serapeum
                #:->)
  (:export #:local-image
           #:width
           #:height))
(in-package #:40ants-doc-full/commondoc/image)


(defclass local-image (common-doc:image)
  ((target-filename :initarg :target-filename
                    :type string
                    :reader target-filename)
   (width :initform nil
          :initarg :width
          :reader width)
   (height :initform nil
           :initarg :height
           :reader height)))


(defun resolve-local-image-path (path)
  "Resolve PATH according to the current documented ASDF system."
  (cond
    ((str:starts-with-p "asdf:" path)
     (destructuring-bind (prefix asdf-system-name path)
         (str:split ":" path
                    :limit 3)
       (declare (ignore prefix))
       (asdf:system-relative-pathname asdf-system-name
                                      path)))
    ((uiop:absolute-pathname-p path)
     path)
    (*current-asdf-system*
     (asdf:system-relative-pathname *current-asdf-system*
                                    path))
    (t
     (merge-pathnames path))))


(-> relative-path (string)
    (values string &optional))

(defun relative-path (path)
  (cond
    ((str:starts-with-p "asdf:" path)
     (destructuring-bind (prefix asdf-system-name relative-path)
         (str:split ":" path
                    :limit 3)
       (declare (ignore prefix asdf-system-name))
       (values relative-path)))
    ((uiop:absolute-pathname-p path)
     (cond
       (*current-asdf-system*
        (let ((probably-relative
                (enough-namestring path
                                   (asdf:system-relative-pathname *current-asdf-system*
                                                                     "./"))))
          (cond
            ((uiop:absolute-pathname-p probably-relative)
             ;; If filename is not inside the ASDF system, then probably it is
             ;; inside the current directory
             (let ((probably-relative
                     (enough-namestring path)))
               (cond
                 ((uiop:absolute-pathname-p probably-relative)
                  ;; If filename is not inside the current directory, then there is no
                  ;; way to learn what is the relative path would be:
                  (error "Unable to figure out relative path out from ~A path."
                         path))
                 (t
                  probably-relative))))
            (t
             probably-relative))))
       (t
        (let ((probably-relative
                (enough-namestring path)))

          (cond
            ((uiop:absolute-pathname-p probably-relative)
             ;; If filename is not inside the current directory, then there is no
             ;; way to learn what is the relative path would be:
             (error "Unable to figure out relative path out from ~A path."
                    path))
            (t
             probably-relative))))))
    (t
     ;; Path already was relative:
     path)))


(defun local-image (path &key target-filename description width height)
  "Creates a note for rendering an image in the documentation.
   Could be useful if you are constructing document from CommonDoc nodes.

   The SOURCE argument should point to a file on local filesystem.

   For example, here is how this function is used in the
   new [`PlantUML` plugin](https://40ants.com/doc-plantuml/):

   ```
   (defmethod to-commondoc ((diagram diagram))
     (uiop:with-temporary-file (:pathname pathname
                                :type \"png\"
                                :keep t)
       (ensure-directories-exist pathname)
   
       (40ants-plantuml:render (diagram-code diagram)
                            pathname)
       (let ((image
               (local-image
                (namestring pathname)
                :target-filename (diagram-filename diagram))))
         (common-doc:make-paragraph image))))
   ```

   This code creates a temporary file, renders a png image into it
   and then makes a paragraph with image, pointing to this temp file.
"
     
  (let ((full-path (resolve-local-image-path path)))
    (unless (probe-file full-path)
      (error "Image file \"~A\" not found"
             full-path))

    (make-instance
     'local-image
     ;; This is the path from where we will copy image file
     :source (namestring full-path)
     ;; And this is a relative path how we will refer the file
     ;; in the documentation:
     :target-filename (cond 
                        (target-filename
                         (namestring target-filename))
                        (t
                         (namestring (relative-path path))))
     :description description
     :width width
     :height height)))


(defun unsigned-16-bit-big-endian (bytes offset)
  (+ (ash (aref bytes offset) 8)
     (aref bytes (1+ offset))))


(defun unsigned-32-bit-big-endian (bytes offset)
  (+ (ash (aref bytes offset) 24)
     (ash (aref bytes (+ offset 1)) 16)
     (ash (aref bytes (+ offset 2)) 8)
     (aref bytes (+ offset 3))))


(defun unsigned-16-bit-little-endian (bytes offset)
  (+ (aref bytes offset)
     (ash (aref bytes (1+ offset)) 8)))


(defun png-dimensions (header)
  (when (and (>= (length header) 24)
             (equalp (subseq header 0 8)
                     #(137 80 78 71 13 10 26 10)))
    (values (unsigned-32-bit-big-endian header 16)
            (unsigned-32-bit-big-endian header 20))))


(defun gif-dimensions (header)
  (when (and (>= (length header) 10)
             (or (equalp (subseq header 0 6) #(71 73 70 56 55 97))
                 (equalp (subseq header 0 6) #(71 73 70 56 57 97))))
    (values (unsigned-16-bit-little-endian header 6)
            (unsigned-16-bit-little-endian header 8))))


(defun read-octet (stream)
  (or (read-byte stream nil)
      (error "Unexpected end of image file.")))


(defun read-unsigned-16-bit-big-endian (stream)
  (+ (ash (read-octet stream) 8)
     (read-octet stream)))


(defun jpeg-dimensions (stream)
  (when (and (= (read-octet stream) #xff)
             (= (read-octet stream) #xd8))
    (loop for marker = (loop for octet = (read-octet stream)
                             until (/= octet #xff)
                             finally (return octet))
          until (= marker #xd9)
          unless (or (= marker #x01)
                     (<= #xd0 marker #xd7))
            do (let ((length (read-unsigned-16-bit-big-endian stream)))
                 (when (< length 2)
                   (error "Invalid JPEG segment length."))
                 (if (member marker '(#xc0 #xc1 #xc2 #xc3
                                      #xc5 #xc6 #xc7
                                      #xc9 #xca #xcb
                                      #xcd #xce #xcf))
                     (progn
                       (read-octet stream)
                       (let ((height (read-unsigned-16-bit-big-endian stream))
                             (width (read-unsigned-16-bit-big-endian stream)))
                         (return (values width height))))
                     (file-position stream (+ (file-position stream)
                                              (- length 2))))))))


(defun image-dimensions (path)
  "Return the pixel width and height of PNG, GIF, or JPEG PATH."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((header (make-array 24 :element-type '(unsigned-byte 8)))
          (bytes-read 0))
      (setf bytes-read (read-sequence header stream))
      (multiple-value-bind (width height)
          (png-dimensions (subseq header 0 bytes-read))
        (if width
            (values width height)
            (multiple-value-bind (width height)
                (gif-dimensions (subseq header 0 bytes-read))
              (if width
                  (values width height)
                  (progn
                    (file-position stream 0)
                    (jpeg-dimensions stream)))))))))


(defun rendered-dimensions (image)
  "Return IMAGE dimensions, inferring the missing pixel dimension when possible."
  (let ((width (width image))
        (height (height image)))
    (cond
      ((and (or width height)
            (or (null width) (null height))
            (or (integerp width) (integerp height)))
       (multiple-value-bind (source-width source-height)
           (image-dimensions (common-doc:source image))
         (unless (and source-width source-height)
           (error "Unable to determine dimensions of image ~A."
                  (common-doc:source image)))
         (if width
             (values width (round (* width source-height) source-width))
             (values (round (* height source-width) source-height) height))))
      (t
       (values width height)))))


(defun replace-images (document)
  (flet ((replacer (node)
           (typecase node
             ;; If node is already has needed class,
             ;; then leave it as is:
             (local-image
                node)
             (common-doc:image
                (let ((source (common-doc:source node)))
                  (if (or (str:starts-with-p "http:" source)
                          (str:starts-with-p "https:" source))
                    node
                    ;; We rewrite only nodes pointing to
                    ;; files on local filesystem:
                    (multiple-value-bind (source width height)
                        (extract-width-and-height source)
                      (local-image source
                                   :width width
                                   :height height)))))
             (t node))))
    (map-nodes document #'replacer)))


(defun target-path (image base-dir)
  (uiop:merge-pathnames* (target-filename image)
                         (uiop:ensure-directory-pathname base-dir)))


(defun copy-local-image (image base-dir copied-targets)
  (let* ((source-path (common-doc:source image))
         (target-path (target-path image base-dir)))
    (unless (gethash target-path copied-targets)
      (unless (equal source-path target-path)
        (log:info "Copying image from ~A to ~A" source-path target-path)
        (ensure-directories-exist target-path)
        (uiop:copy-file source-path target-path))
      (setf (gethash target-path copied-targets) t))))


(defun copy-local-images (document base-dir)
  "Copy all local images in DOCUMENT to BASE-DIR exactly once per target."
  (let ((copied-targets (make-hash-table :test #'equal)))
    (map-nodes document
               (lambda (node)
                 (when (typep node 'local-image)
                   (copy-local-image node base-dir copied-targets))
                 node))))


(defun image-source (image)
  (let* ((base-dir (uiop:ensure-directory-pathname
                    40ants-doc-full/builder/vars::*base-dir*))
         (target-path (target-path image base-dir))
         (page-uri (make-page-uri 40ants-doc-full/builder/vars::*current-page*))
         (relative-path (namestring (uiop:enough-pathname target-path base-dir))))
    (make-relative-path page-uri relative-path)))

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
  "Emit a local image already copied to the documentation output folder."
  (let* ((new-source (image-source obj))
         (src (if common-html.emitter:*image-format-control*
                (format nil common-html.emitter:*image-format-control*
                        new-source)
                new-source))
         (description (common-doc:description obj)))
    (multiple-value-bind (width height)
        (rendered-dimensions obj)
      (with-html
        (:img :src src
              :alt description
              :title description
              :width width
              :height height)))))


(defmethod common-doc.format:emit-document ((format commondoc-markdown:markdown)
                                            (obj local-image)
                                            stream)
  (multiple-value-bind (width height)
      (rendered-dimensions obj)
    (if (or width height)
        (format stream "<img src=\"~A\" alt=\"~A\"~@[ width=\"~A\"~]~@[ height=\"~A\"~]>"
                (image-source obj)
                (common-doc:description obj)
                width
                height)
        (format stream "![~A](~A)"
                (common-doc:description obj)
                (image-source obj)))))
