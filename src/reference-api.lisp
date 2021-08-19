(defpackage #:40ants-doc/reference-api
  (:use #:cl)
  (:export
   #:canonical-reference
   #:*source-uri-fn*
   #:source-uri))
(in-package 40ants-doc/reference-api)


(defgeneric canonical-reference (object)
  (:documentation "Return a 40ANTS-DOC/REFERENCE::REFERENCE that resolves to OBJECT."))

(defgeneric format-reference (obj name ref link))

(defgeneric reference-name (obj name ref link))


(defvar *source-uri-fn* nil
  "Set this to a function of one argument.

   The argument of this function will be a 40ANTS-DOC/REFERENCE:REFERENCE
   object and the result should be a full URL leading to the web page where
   referenced object can be viewed. Usually this is a GitHub's page.

   See 40ANTS-DOC/GITHUB:MAKE-GITHUB-SOURCE-URI-FN for details.")


(defun source-uri (reference)
  (let ((fn *source-uri-fn*))
    (when fn
      (funcall fn reference))))
