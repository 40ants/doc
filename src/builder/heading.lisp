(uiop:define-package #:40ants-doc/builder/heading
  (:use #:cl)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/link)
  (:import-from #:40ants-doc/document)
  (:import-from #:40ants-doc/page)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/render/toc)
  (:import-from #:40ants-doc/render/navigation-api)
  (:import-from #:40ants-doc/builder/heading-api)
  (:import-from #:40ants-doc/builder/vars))
(in-package 40ants-doc/builder/heading)


;;; A list of HEADING objects in the order of generation.
(defvar *headings* ())

(defstruct heading
  object
  title
  level)

(defun collect-heading (object title)
  (push (make-heading :object object :title title :level 40ants-doc/builder/vars::*heading-level*)
        *headings*))

(defun collect-headings (object)
  (let ((40ants-doc/builder/vars::*collecting-headings-p* t)
        (*headings* ())
        (40ants-doc/builder/vars::*table-of-contents-stream* (make-broadcast-stream))
        (40ants-doc/builder/vars::*document-max-table-of-contents-level* 0))
    (40ants-doc/document::document-object object (make-broadcast-stream))
    (reverse *headings*)))


(defmacro with-headings ((object) &body body)
  `(let ((*headings* (collect-headings ,object)))
     ,@body))


(defun call-with-heading (stream object title link-title-to fn)
  (flet ((foo ()
           ;; Arrange for all output to go to /dev/null
           ;; (MAKE-BROADCAST-STREAM) except for the headings when we
           ;; are generating the table of contents.
           (cond
             (40ants-doc/builder/vars::*collecting-headings-p*
              (funcall fn (make-broadcast-stream)))
             (40ants-doc/builder/vars::*table-of-contents-stream*
              (when (<= 40ants-doc/builder/vars::*heading-level*
                        40ants-doc/builder/vars::*document-max-table-of-contents-level*)
                (40ants-doc/render/toc::print-table-of-contents-entry object title
                                                                      40ants-doc/builder/vars::*table-of-contents-stream*)
                (funcall fn (make-broadcast-stream))))
             (t
              (if 40ants-doc/link::*document-link-sections*
                  (let ((anchor (40ants-doc/reference::reference-to-anchor object)))
                    (40ants-doc/utils::anchor anchor stream)
                    (40ants-doc/render/navigation-api::navigation-link object stream)
                    (format stream "~A" (40ants-doc/builder/heading-api::fancy-navigation object))
                    (40ants-doc/utils::heading 40ants-doc/builder/vars::*heading-level* stream)
                    (if (eq 40ants-doc/builder/printer::*format* :html)
                        (if link-title-to
                            (format stream " [~A~A][~A]~%~%"
                                    (40ants-doc/render/toc::heading-number) title
                                    (40ants-doc/page::link-to-reference link-title-to))
                            (format stream " <a href=\"#~A\">~A~A</a>~%~%"
                                    (40ants-doc/utils::html-safe-name anchor)
                                    (40ants-doc/render/toc::heading-number)
                                    (40ants-doc/utils::escape-markdown title)))
                        (format stream " ~A~A~%~%" (40ants-doc/render/toc::heading-number)
                                (40ants-doc/utils::escape-markdown title))))
                  (progn
                    (40ants-doc/utils::heading 40ants-doc/builder/vars::*heading-level* stream)
                    (format stream " ~A~A~%~%"
                            (40ants-doc/render/toc::heading-number)
                            (40ants-doc/utils::escape-markdown title))))
              (when (and (zerop 40ants-doc/builder/vars::*heading-level*)
                         (plusp 40ants-doc/builder/vars::*document-max-table-of-contents-level*))
                (40ants-doc/utils::heading (1+ 40ants-doc/builder/vars::*heading-level*) stream)
                (format stream " Table of Contents~%~%")
                (let ((40ants-doc/builder/vars::*table-of-contents-stream* stream)
                      (40ants-doc/page::*table-of-contents-page* 40ants-doc/page::*page*)
                      (40ants-doc/builder/vars::*heading-number* (copy-list 40ants-doc/builder/vars::*heading-number*)))
                  (funcall fn (make-broadcast-stream)))
                (terpri stream))
              (funcall fn (if 40ants-doc/builder/vars::*table-of-contents-stream*
                              (make-broadcast-stream)
                              stream))))))
    (let ((level 40ants-doc/builder/vars::*heading-level*))
      (when 40ants-doc/builder/vars::*collecting-headings-p*
        (collect-heading object title))
      (when (plusp level)
        (incf (nth (1- level) 40ants-doc/builder/vars::*heading-number*)))
      (let ((40ants-doc/builder/vars::*heading-number*
              (append 40ants-doc/builder/vars::*heading-number*
                      (loop repeat (max 0 (- (1+ level)
                                             (length 40ants-doc/builder/vars::*heading-number*)))
                            collect 0))))
        (foo)))))


(defmacro with-heading ((stream object title &key link-title-to)
                        &body body)
  `(call-with-heading ,stream ,object ,title ,link-title-to
                      (lambda (,stream) ,@body)))

