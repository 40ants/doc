(defpackage #:40ants-doc/locatives/function
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
(in-package 40ants-doc/locatives/function)


(define-locative-type function ()
  "Note that the arglist in the generated documentation depends on
  the quality of SWANK-BACKEND:ARGLIST. It may be that default
  values of optional and keyword arguments are missing.")

(define-locative-type generic-function ())

(defmethod locate-object (symbol (locative-type (eql 'function)) locative-args)
  (declare (ignore locative-args))
  (when (macro-function symbol)
    (locate-error "~S is a macro, not a function." symbol))
  (let ((function (symbol-function symbol)))
    (when (typep function 'generic-function)
      (locate-error "~S is a generic function, not a plain function." symbol))
    function))

(defmethod locate-object (symbol (locative-type (eql 'generic-function))
                          locative-args)
  (declare (ignore locative-args))
  (let ((function (symbol-function symbol)))
    (unless (typep function 'generic-function)
      (locate-error "#'~S is not a generic function." symbol))
    function))

(defmethod canonical-reference ((function function))
  (40ants-doc/reference::make-reference (swank-backend:function-name function) 'function))

(defmethod canonical-reference ((function generic-function))
  (40ants-doc/reference::make-reference (swank-mop:generic-function-name function) 'generic-function))

(defmethod document-object ((function function) stream)
  (let ((reference (canonical-reference function)))
    (40ants-doc/builder/bullet::print-bullet reference stream)
    (write-char #\Space stream)
    (let ((arglist (swank-backend:arglist function)))
      (40ants-doc/render/args::print-arglist arglist stream)
      (40ants-doc/builder/bullet::print-end-bullet stream)
      (40ants-doc/args::with-dislocated-symbols ((40ants-doc/args::function-arg-names arglist))
        (40ants-doc/render/print::maybe-print-docstring (40ants-doc/reference::reference-object reference)
                                                        'function
                                                        stream)))))