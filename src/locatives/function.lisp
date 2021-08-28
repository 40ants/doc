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
  (:import-from #:40ants-doc/render/args)
  (:import-from #:40ants-doc/args)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/args)
  (:import-from #:40ants-doc/commondoc/builder)
  (:import-from #:40ants-doc/commondoc/bullet)
  (:import-from #:40ants-doc/docstring)
  (:import-from #:40ants-doc/commondoc/markdown))
(in-package 40ants-doc/locatives/function)


(define-locative-type function ()
  "Note that the arglist in the generated documentation depends on
  the quality of SWANK-BACKEND:ARGLIST. It may be that default
  values of optional and keyword arguments are missing.")

(defmethod locate-object (symbol (locative-type (eql 'function)) locative-args)
  (declare (ignore locative-args))
  (when (macro-function symbol)
    (locate-error "~S is a macro, not a function." symbol))
  (let ((function (symbol-function symbol)))
    (when (typep function 'generic-function)
      (locate-error "~S is a generic function, not a plain function." symbol))
    function))

(defmethod canonical-reference ((function function))
  (40ants-doc/reference::make-reference (swank-backend:function-name function) 'function))


(defmethod 40ants-doc/commondoc/builder::to-commondoc ((obj function))
  (let* ((arglist (swank-backend:arglist obj))
         (docstring (40ants-doc/docstring:get-docstring obj 'function))
         (children (when docstring
                     (40ants-doc/commondoc/markdown:parse-markdown docstring)))
         (reference (canonical-reference obj))
         (dislocated (40ants-doc/args::function-arg-names arglist)))

    (40ants-doc/commondoc/bullet:make-bullet reference
                                             :arglist arglist
                                             :children children
                                             :dislocated-symbols dislocated)))
