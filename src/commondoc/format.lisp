(uiop:define-package #:40ants-doc/commondoc/format
  (:use #:cl)
  (:export #:with-format
           #:current-files-extension
           #:files-extension))
(in-package 40ants-doc/commondoc/format)


(defvar *current-format*)

(setf (documentation '*current-format* 'variable)
      "Keeps track a commondoc format currently generated")


(defgeneric files-extension (format)
  (:method ((format (eql 'common-html:html)))
    "html")
  (:method ((format (eql 'commondoc-markdown:markdown)))
    "md"))


(defun current-files-extension ()
  (unless (boundp '*current-format*)
    (error "Please, use WITH-FORMAT macro"))
  
  (files-extension *current-format*))


(defun call-with-format (format func)
  (let ((*current-format* format))
    (funcall func)))


(defmacro with-format ((format) &body body)
  `(call-with-format ,format
                     (lambda ()
                       ,@body)))