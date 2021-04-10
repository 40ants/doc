(uiop:define-package #:40ants-doc/locatives/include
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-and-find-source
                #:locate-and-document
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc/locatives
                #:include)
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
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader)
  (:import-from #:40ants-doc/core
                #:exportable-locative-type-p)
  (:import-from #:40ants-doc/source-api))
(in-package 40ants-doc/locatives/include)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(define-locative-type include (source &key line-prefix header footer
                                      header-nl footer-nl)
  """Refers to a region of a file. SOURCE can be a string or a
  pathname in which case the whole file is being pointed to or it can
  explicitly supply START, END locatives. INCLUDE is typically used to
  include non-lisp files in the documentation (say markdown or elisp
  as in the next example) or regions of lisp source files. This can
  reduce clutter and duplication.

  ```commonlisp
  (defsection example-section ()
    (pax.el (include #.(asdf:system-relative-pathname :40ants-doc "elisp/pax.el")
                     :header-nl "```elisp" :footer-nl "```"))
    (foo-example (include (:start (foo function)
                           :end (end-of-foo-example variable))
                          :header-nl "```commonlisp"
                          :footer-nl "```"))

  (defun foo (x)
    (1+ x))

  ;;; Since file regions are copied verbatim, comments survive.
  (defmacro bar ())

  ;;; This comment is the last thing in FOO-EXAMPLE's
  ;;; documentation since we use the dummy END-OF-FOO-EXAMPLE
  ;;; variable to mark the end location.
  (defvar end-of-foo-example)

  ;;; More irrelevant code follows.
  ```

  In the above example, pressing `M-.` on `pax.el` will open the
  `src/pax.el` file and put the cursor on its first character. `M-.`
  on `FOO-EXAMPLE` will go to the source location of the `(asdf:system
  locative)` locative.

  When documentation is generated, the entire `pax.el` file is
  included in the markdown surrounded by the strings given as
  HEADER-NL and FOOTER-NL (if any). The trailing newline character is
  assumed implicitly. If that's undesirable, then use HEADER and
  FOOTER instead. The documentation of `FOO-EXAMPLE` will be the
  region of the file from the source location of the START
  locative (inclusive) to the source location of the END
  locative (exclusive). START and END default to the beginning and end
  of the file, respectively.

  Note that the file of the source location of :START and :END must be
  the same. If SOURCE is pathname designator, then it must be absolute
  so that the locative is context independent.

  Finally, if specified LINE-PREFIX is a string that's prepended to
  each line included in the documentation. For example, a string of
  four spaces makes markdown think it's a code block.""")


(defmethod exportable-locative-type-p ((locative-type (eql 'include)))
  nil)

(defmethod locate-object (symbol (locative-type (eql 'include))
                          locative-args)
  (destructuring-bind (source &key line-prefix header footer
                       header-nl footer-nl) locative-args
    (declare (ignore source line-prefix header footer header-nl footer-nl))
    (40ants-doc/reference::make-reference symbol (cons locative-type locative-args))))


(defmethod locate-and-find-source (symbol (locative-type (eql 'include))
                                   locative-args)
  (multiple-value-bind (file start)
      (include-region (first locative-args))
    (assert file)
    `(:location
      (:file ,(namestring file))
      (:position ,(1+ start))
      nil)))

(defmethod locate-and-document (symbol (locative-type (eql 'include))
                                locative-args stream)
  (destructuring-bind (source &key (line-prefix "")
                                   header
                                   footer
                                   header-nl
                                   footer-nl)
      locative-args
    
    (when header
      (format stream "~A" header))
    (when header-nl
      (format stream "~A~%" header-nl))
    (format stream "~A"
            (40ants-doc/utils::prefix-lines line-prefix
                                            (multiple-value-call #'file-subseq
                                              (include-region source))))
    (when footer
      (format stream "~A" footer))
    (when footer-nl
      (format stream "~A~%" footer-nl))))

;;; Return the filename and start, end positions of the region to be
;;; included.
(defun include-region (source)
  (cond ((or (stringp source) (pathnamep source))
         (assert (uiop/pathname:absolute-pathname-p source) ()
                 "Pathnames given as the SOURCE argument of the ~
                 INCLUDE locative must be absolute, but ~S is not."
                 source)
         (values source 0 nil))
        ((and source (listp source))
         (destructuring-bind (&key start end) source
           (let* ((start-reference (40ants-doc/reference::resolve
                                    (40ants-doc/core::entry-to-reference start)))
                  (end-reference (40ants-doc/reference::resolve
                                  (40ants-doc/core::entry-to-reference end)))
                  (start (40ants-doc/source-api::find-source start-reference))
                  (end (40ants-doc/source-api::find-source end-reference)))
             (when start
               (check-location start))
             (when end
               (check-location end))
             (let ((start-file (when start (location-file start)))
                   (start-position (when start (location-position start)))
                   (end-file (when end (location-file end)))
                   (end-position (when end (location-position end))))
               (when (and start end)
                 (assert (string= (namestring (truename start-file))
                                  (namestring (truename end-file)))
                         () "Include starts in file ~S and ends in ~
                         another file ~S." start-file end-file))

               (when (< end-position
                        start-position)
                 (error "Something went wrong end position ~A of ~S goes before start position ~A of ~S"
                        end-position
                        (getf source :end)
                        start-position
                        (getf source :start)))
               (values (or start-file end-file) start-position end-position)))))
        (t
         (error "~@<Malformed include source ~S.~:@>" source))))

;;; Check that LOCATION looks like this:
;;;
;;;     (:location
;;;      (:file "filename")
;;;      (:position 1)
;;;      (:snippet ""))
(defun check-location (location)
  (assert (listp location) () "Location ~S is not a list." location)
  (assert (eq (first location) :location) ()
          "Location ~S does not start with ~S." location :location)
  (assert (and (location-file location)
               (location-position location))
          () "Location ~S should contain: ~S."
          location '(:file :position)))

(defun location-file (location)
  (second (find :file (rest location) :key #'first)))

(defun location-position (location)
  (1- (second (find :position (rest location) :key #'first))))

;; TODO: Find why this get called three times when I have only one include in my document :(
(defun file-subseq (pathname &optional start end)
  ;; START and END arguments contains offsets in bytes,
  ;; thus to process Unicode symbols, encoded in UTF-8,
  ;; we need to read data as bytes and transfrom into
  ;; the characters:
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let* ((*print-pretty* nil)
           (start (or start 0))
           (file-len (file-length stream))
           (end (min (or end file-len)
                     file-len))
           (buffer-size (- end start))
           (buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
      (file-position stream start)
      (read-sequence buffer stream)
      (babel:octets-to-string buffer))))

