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
  (:import-from #:40ants-doc/render/args)
  (:import-from #:40ants-doc/args)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/args)
  (:import-from #:40ants-doc/docstring)
  (:import-from #:40ants-doc/commondoc/markdown))
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



(defmethod 40ants-doc/commondoc/builder::to-commondoc ((obj generic-function))
  (let* ((arglist (swank-backend:arglist obj))
         (docstring (40ants-doc/docstring:get-docstring obj 'function))
         ;; TODO:  we should move text transfromation after it will be parsed
         (children (when docstring
                     (40ants-doc/commondoc/markdown:parse-markdown docstring)))
         (reference (canonical-reference obj)))

    (40ants-doc/commondoc/bullet:make-bullet reference
                                             :arglist arglist
                                             :children children
                                             :dislocated-symbols (40ants-doc/args::function-arg-names arglist))))
