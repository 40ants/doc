(uiop:define-package #:40ants-doc/page
  (:use #:cl)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/reference-api
                #:format-reference)
  (:import-from #:40ants-doc/link)
  (:import-from #:40ants-doc/builder/printer)
  (:import-from #:40ants-doc/builder/vars)
  (:import-from #:40ants-doc/locatives/dislocated)
  (:import-from #:40ants-doc/commondoc/builder)
  (:export
   #:make-page2
   #:ensure-page))
(in-package 40ants-doc/page)


;;; Documentation starts out being sent to a certain stream, but the
;;; output is redirected to different stream if it is for a reference
;;; among PAGE-REFERENCES. This stream is given by TEMP-STREAM-SPEC
;;; that's a stream spec to allow it to
;;;
;;; - be created lazily so that no stray files are left around and
;;;   only a small number of fds are needed even for a huge project
;;;
;;; - be opened multiple times (which is not given for string streams)
;;;
;;; So output is generated in markdown format to TEMP-STREAM-SPEC, but
;;; before we are done it is converted to the requested output format
;;; and HEADER-FN, FOOTER-FN are called to write arbitrary leading and
;;; trailing content to the final stream.
;;;
;;; Finally, URI-FRAGMENT is a string such as "doc/manual.html" that
;;; specifies where the page will be deployed on a webserver. It
;;; defines how links between pages will look. If it's not specified
;;; and OUTPUT refers to a file, then it defaults to the name of the
;;; file. If URI-FRAGMENT is NIL, then no links will be made to or
;;; from that page.
(defstruct page
  references
  temp-stream-spec
  final-stream-spec
  uri-fragment
  header-fn
  footer-fn
  source-uri-fn)

;;; The current page where output is being sent.
(defvar *page* nil)


;;;; Page specs

;;; Convert the PAGES argument of DOCUMENT to PAGE objects.
(defun translate-page-specs (pages format)
  (mapcar (lambda (page) (translate-page-spec page format))
          pages))

(defun translate-page-spec (page format)
  (destructuring-bind (&key objects output header-fn footer-fn
                       (uri-fragment nil uri-fragment-p)
                       source-uri-fn)
      page
    (let* ((stream-spec (apply #'40ants-doc/utils::make-stream-spec output))
           (uri-fragment (or uri-fragment
                             (if (and (not uri-fragment-p)
                                      (typep stream-spec '40ants-doc/utils::file-stream-spec))
                                 (40ants-doc/utils::file-stream-spec-pathname stream-spec)
                                 nil))))
      (make-page
       :references (40ants-doc/reference::reachable-canonical-references objects)
       :temp-stream-spec (if (and (eq format :markdown)
                                  (null header-fn)
                                  (null footer-fn))
                             stream-spec
                             (make-instance '40ants-doc/utils::string-stream-spec))
       :final-stream-spec stream-spec
       :uri-fragment uri-fragment
       :header-fn header-fn
       :footer-fn footer-fn
       :source-uri-fn source-uri-fn))))


;;; Pages tracking

(defvar *pages-created*)

(defmacro with-tracking-pages-created (() &body body)
  `(let ((*pages-created* ()))
     ,@body))

;;; Add a LINK to *LINKS* (and a REFERENCE to *REFERENCES*) for each
;;; reference in PAGE-REFERENCES of PAGE.
(defmacro with-pages ((pages) &body body)
  `(let ((40ants-doc/reference::*references* 40ants-doc/reference::*references*)
         (40ants-doc/link::*links* 40ants-doc/link::*links*))
     (with-standard-io-syntax
       (loop for page in ,pages
             do (dolist (reference (page-references page))
                  (unless (40ants-doc/link::find-link reference)
                    (push reference 40ants-doc/reference::*references*)
                    (push (40ants-doc/link::make-link
                           :reference reference
                           :page page
                           :id (40ants-doc/link::hash-link (40ants-doc/reference::reference-to-anchor reference)
                                                           #'40ants-doc/link::find-link-by-id)
                           :page-to-n-uses (make-hash-table))
                          40ants-doc/link::*links*)))))
     (locally ,@body)))

(defmacro do-pages-created ((page) &body body)
  `(dolist (,page (reverse *pages-created*))
     ,@body))

(defun mark-page-created (page)
  (pushnew page *pages-created*))


(defmacro with-temp-input-from-page ((stream page) &body body)
  `(40ants-doc/utils::with-open-stream-spec (,stream (page-temp-stream-spec ,page))
     ,@body))


(defun call-with-temp-output-to-page (page stream func)
  (cond
    (40ants-doc/builder/vars::*table-of-contents-stream*
     (let ((*page* page))
       (funcall func (make-broadcast-stream))))
    ((or (null page)
         (eq page *page*))
     (funcall func stream))
    (t
     ;; ATTENTION: Here  we are changing the stream we output documentation to!
     (let ((stream-spec (page-temp-stream-spec page)))
       (40ants-doc/utils::with-open-stream-spec (page-stream stream-spec
                                                 :direction :output)
         (let ((*page* page))
           (mark-page-created page)
           (funcall func page-stream )))))))


(defmacro with-temp-output-to-page ((stream page) &body body)
  `(flet ((process (,stream)
            ,@body))
     (call-with-temp-output-to-page ,page ,stream #'process)))


(defmacro with-final-output-to-page ((stream page) &body body)
  `(40ants-doc/utils::with-open-stream-spec (,stream (page-final-stream-spec ,page)
                                             :direction :output)
     ,@body))


;;; Remember the page so that linking can be done from the right
;;; context.
(defvar *table-of-contents-page* nil)


(defun relative-page-uri-fragment (page reference-page)
  (if (eq page reference-page)
      ""
      (let ((fragment (page-uri-fragment page))
            (reference-fragment (page-uri-fragment reference-page)))
        (assert (and fragment reference-fragment))
        (40ants-doc/utils::relativize-pathname fragment reference-fragment))))


(defun filter-references-by-format (refs)
  (remove-if-not (lambda (ref)
                   (and (or (and 40ants-doc/link::*document-link-sections*
                                 (typep (40ants-doc/reference::resolve ref :errorp nil)
                                        '40ants-doc/core::section))
                            40ants-doc/link::*document-link-code*)
                        (let* ((ref-page (40ants-doc/link::reference-page ref))
                               (current-page *page*)
                               (current-page-uri (when current-page
                                                  (40ants-doc/page::page-uri-fragment current-page)))
                               (ref-page-uri (when ref-page
                                               (40ants-doc/page::page-uri-fragment ref-page)))
                               (ref-locative-type (40ants-doc/reference::reference-locative-type ref)))
                          (or
                           ;; These have no pages, but won't result in
                           ;; link anyway. Keep them.
                           (member ref-locative-type
                                   '(40ants-doc/locatives/dislocated::dislocated))
                           ;; Intrapage links always work.
                           (eq current-page
                               ref-page)
                           ;; Else we need to know the URI-FRAGMENT of
                           ;; both pages. See
                           ;; RELATIVE-PAGE-URI-FRAGMENT.
                           (and current-page-uri
                                ref-page-uri)))))
                 refs))



;;; Select some references from REFS heuristically.
(defun filter-references (refs)
  (let ((refs (40ants-doc/reference::filter-asdf-system-references
               (filter-references-by-format refs))))
    (if (40ants-doc/reference::references-for-the-same-symbol-p refs)
        (40ants-doc/reference::resolve-generic-function-and-methods
         (40ants-doc/reference::resolve-dislocated refs))
        refs)))


;;; For the link to REFERENCE, increment the link counter for the
;;; current page and return the link id.
(defun link-to-reference (reference)
  (let ((link (40ants-doc/link::find-link reference)))
    (when (and link
               (or (eq *page* (40ants-doc/link::link-page link))
                   (and (page-uri-fragment *page*)
                        (page-uri-fragment (40ants-doc/link::link-page link)))))
      (incf (gethash *page* (40ants-doc/link::link-page-to-n-uses link) 0))
      (format nil "~A" (40ants-doc/link::link-id link)))))

(defun link-used-on-current-page-p (link)
  (plusp (gethash *page* (40ants-doc/link::link-page-to-n-uses link) 0)))



;;; REFS is the list of references for NAME after filtering. Mark it
;;; up as code, create link(s).
(defun format-references (name refs)
  (let ((ref-1 (first refs)))
    (cond ((endp refs)
           ;; all references were filtered out
           `(,(40ants-doc/utils::code-fragment name)))
          ((< 1 (length refs))
           ;; `name`([1][link-id-1] [2][link-id-2])
           (values `(,(40ants-doc/utils::code-fragment (40ants-doc/builder/printer::maybe-downcase name))
                     "("
                     ,@(loop
                         for i upfrom 0
                         for ref in refs
                         append `(,@(unless (zerop i)
                                      '(" "))
                                  (:reference-link
                                   :label (,(40ants-doc/utils::code-fragment i))
                                   :definition ,(link-to-reference ref))))
                     ")")
                   t))
          ((member (40ants-doc/reference::reference-locative-type ref-1) '(40ants-doc/locatives/dislocated::dislocated))
           `(,(40ants-doc/utils::code-fragment (40ants-doc/builder/printer::maybe-downcase name))))
          
          (t
           (format-reference (40ants-doc/reference::resolve ref-1)
                             name
                             ref-1
                             (link-to-reference ref-1))))))


;; NOTE: I've added link argument,
;; to not call link-to-reference function in each method implementation
(defmethod format-reference (obj name ref link)
  `((:reference-link :label (,(40ants-doc/utils::code-fragment (40ants-doc/builder/printer::maybe-downcase name)))
                     :definition ,link)))


(defmethod 40ants-doc/document::document-object :around (object stream)
  ;; This method renders object either to the current stream,
  ;; or to the stream of the page object belongs to.
  (loop
    (return
      (cond ((or (stringp object)
                 (typep object '40ants-doc/reference::reference))
             (call-next-method))
            (t
             (let* ((reference (40ants-doc/reference::canonical-reference object))
                    (40ants-doc/reference::*reference-being-documented* reference))
               (assert (eq object (40ants-doc/reference::resolve reference)))

               ;; ATTENTION: Inside with-temp-output-to-page we are changing the stream we output documentation to!
               (40ants-doc/page::with-temp-output-to-page (stream (40ants-doc/link::reference-page reference))
                 (when (and 40ants-doc/link::*document-link-code*
                            (not (typep object '40ants-doc/core::section))
                            (not (typep object 'asdf:system)))
                   (40ants-doc/utils::anchor (40ants-doc/reference::reference-to-anchor reference) stream))
                 (call-next-method object stream))))))))


;; TODO: check objects and functions above
;; if we are still need them

(defclass page2 ()
  ((sections :initarg :sections
             :reader page-sections)))


(defun make-page2 (sections)
  (make-instance 'page2
                 :sections (uiop:ensure-list sections)))


(defun ensure-page (obj)
  (check-type obj (or 40ants-doc:section page2))
  (typecase obj
    (page2 obj)
    (t (make-page2 obj))))


(defmethod 40ants-doc/commondoc/builder:to-commondoc ((obj page2))
  (40ants-doc/commondoc/page:make-page
   (mapcar #'40ants-doc/commondoc/builder:to-commondoc
           (page-sections obj))))
