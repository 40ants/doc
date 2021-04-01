(uiop:define-package #:40ants-doc/locatives/compiler-macro
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-and-find-source
                #:locate-and-document
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc/document
                #:document-object)
  (:import-from #:40ants-doc/render/args)
  (:import-from #:40ants-doc/builder/bullet)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/args
                #:with-dislocated-symbols)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/builder/vars)
  (:import-from #:40ants-doc/render/print)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/page)
  (:import-from #:40ants-doc/source-api))
(in-package 40ants-doc/locatives/compiler-macro)


(define-locative-type compiler-macro ())

(defmethod locate-object (symbol (locative-type (eql 'compiler-macro))
                          locative-args)
  (unless (compiler-macro-function symbol)
    (locate-error))
  (40ants-doc/reference::make-reference symbol (cons locative-type locative-args)))


(defmethod locate-and-document (symbol (locative-type (eql 'compiler-macro))
                                locative-args stream)
  (40ants-doc/builder/bullet::locate-and-print-bullet locative-type locative-args symbol stream)
  (write-char #\Space stream)
  (let ((arglist (swank-backend:arglist symbol)))
    (40ants-doc/render/args::print-arglist arglist stream)
    (40ants-doc/builder/bullet::print-end-bullet stream)
    (with-dislocated-symbols ((40ants-doc/args::macro-arg-names arglist))
      (40ants-doc/render/print::maybe-print-docstring symbol 'function stream))))


(defmethod locate-and-find-source (symbol (locative-type (eql 'compiler-macro))
                                   locative-args)
  (declare (ignore locative-args))
  #-allegro
  (40ants-doc/source-api::find-source (compiler-macro-function symbol))
  #+allegro
  (find-one-location (swank-backend:find-definitions symbol)
                     '("compiler-macro")))
