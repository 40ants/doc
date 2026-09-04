(uiop:define-package #:40ants-doc-full/assets
  (:use #:cl)
  (:import-from #:40ants-doc-full/commondoc/image
                #:local-image)
  (:import-from #:40ants-doc-full/commondoc/mapper
                #:map-nodes)
  (:import-from #:40ants-doc-full/commondoc/xref
                #:xref
                #:xref-locative
                #:xref-symbol)
  (:export #:defasset))
(in-package #:40ants-doc-full/assets)


(defclass asset ()
  ((name :initarg :name
         :reader asset-name)
   (source :initarg :source
           :reader asset-source)
   (target-filename :initarg :target-filename
                    :reader asset-target-filename)
   (description :initarg :description
                :reader asset-description)
   (width :initarg :width
          :initform nil
          :reader asset-width)
   (height :initarg :height
           :initform nil
           :reader asset-height)))


(defun make-asset (name source target-filename description &rest restargs
                    &key width height)
  (declare (ignore width height))
  (apply #'make-instance 'asset
         :name name
         :source source
         :target-filename target-filename
         :description description
         restargs))


(defvar *assets* (make-hash-table :test #'eq))


(defun pathname-designator-string (pathname-designator)
  (namestring (pathname pathname-designator)))


(defun register-asset (name source &key target-filename description width height)
  (check-type name symbol)
  (check-type source (or string pathname))

  (let* ((source (pathname-designator-string source))
         (target-filename (pathname-designator-string
                           (or target-filename source)))
         (description (or description
                          (symbol-name name)))
         (new-asset (make-asset name source target-filename description
                                :width width
                                :height height))
         (old-asset (gethash name *assets*)))
    (when (and old-asset
               (not (and (string= (asset-source old-asset)
                                  (asset-source new-asset))
                         (string= (asset-target-filename old-asset)
                                  (asset-target-filename new-asset))
                         (string= (asset-description old-asset)
                                  (asset-description new-asset))
                         (equal (asset-width old-asset)
                                (asset-width new-asset))
                         (equal (asset-height old-asset)
                                (asset-height new-asset)))))
      (error "Asset ~S is already registered with different properties."
             name))
    (setf (gethash name *assets*) new-asset)
    new-asset))


(defmacro defasset (name source &rest args)
  "Register NAME as a local image used in documentation.

SOURCE is a pathname designator.  By default, the file is copied to the
same relative pathname below the documentation output directory.  Use
:TARGET-FILENAME to choose another relative output pathname.

Every unqualified occurrence of NAME in prose is rendered as an image."
  (check-type name symbol)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (register-asset ',name ,source ,@args)))


(defun safe-target-filename-p (target-filename)
  (let ((pathname (pathname target-filename)))
    (and (not (uiop:absolute-pathname-p pathname))
         (not (member :up (pathname-directory pathname))))))


(defun validate-asset (asset)
  (unless (probe-file
           (40ants-doc-full/commondoc/image::resolve-local-image-path
            (asset-source asset)))
    (error "Asset ~S source file does not exist: ~A"
           (asset-name asset)
           (asset-source asset)))

  (unless (safe-target-filename-p (asset-target-filename asset))
    (error "Asset ~S target filename is not below the output directory: ~A"
           (asset-name asset)
           (asset-target-filename asset)))

  (maphash (lambda (name other-asset)
             (when (and (not (eq name (asset-name asset)))
                        (string= (asset-target-filename other-asset)
                                 (asset-target-filename asset)))
               (error "Assets ~S and ~S use the same target filename: ~A"
                      (asset-name asset)
                      name
                      (asset-target-filename asset))))
           *assets*)
  asset)


(defun asset-to-local-image (asset)
  (validate-asset asset)
  (local-image (asset-source asset)
               :target-filename (asset-target-filename asset)
               :description (asset-description asset)
               :width (asset-width asset)
               :height (asset-height asset)))


(defun replace-assets (document)
  "Replace XREF nodes naming registered assets with local image nodes."
  (map-nodes document
             (lambda (node)
               (if (and (typep node 'xref)
                        (null (xref-locative node)))
                   (let ((asset (gethash (xref-symbol node) *assets*)))
                     (if asset
                         (asset-to-local-image asset)
                         node))
                   node))))
