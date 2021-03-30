(defpackage #:40ants-doc/locatives/generic-function
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:swank-backend)
  (:import-from #:swank-mop)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/builder/bullet)
  (:import-from #:40ants-doc/render/args)
  (:import-from #:40ants-doc/args)
  (:import-from #:40ants-doc/document
                #:document-object)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/args)
  (:import-from #:40ants-doc/render/print))
(in-package 40ants-doc/locatives/generic-function)


(define-locative-type generic-function ())

(defmethod locate-object (symbol (locative-type (eql 'generic-function))
                          locative-args)
  (declare (ignore locative-args))
  (let ((function (symbol-function symbol)))
    (unless (typep function 'generic-function)
      (locate-error "#'~S is not a generic function." symbol))
    function))


(defmethod canonical-reference ((function generic-function))
  (40ants-doc/reference::make-reference (swank-mop:generic-function-name function) 'generic-function))

