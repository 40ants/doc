(uiop:define-package #:40ants-doc/locatives/macro
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc/render/args)
  (:import-from #:40ants-doc/builder/bullet)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/args)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/builder/vars)
  (:import-from #:40ants-doc/render/print)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/locatives
                #:macro)
  (:import-from #:40ants-doc/page)
  (:import-from #:swank-backend)
  (:import-from #:40ants-doc/commondoc/builder)
  (:import-from #:40ants-doc/commondoc/bullet))
(in-package 40ants-doc/locatives/macro)


(define-locative-type macro ())


(defmethod locate-object (symbol (locative-type (eql 'macro)) locative-args)
  (unless (macro-function symbol)
    (locate-error))
  (40ants-doc/reference::make-reference symbol (cons locative-type locative-args)))


(defmethod 40ants-doc/locatives/base::locate-and-document (symbol (locative-type (eql 'macro))
                                                           locative-args stream)
  (40ants-doc/builder/bullet::locate-and-print-bullet locative-type locative-args symbol stream)
  (write-char #\Space stream)
  (let ((arglist (swank-backend:arglist symbol)))
    (40ants-doc/render/args::print-arglist arglist stream)
    (40ants-doc/builder/bullet::print-end-bullet stream)
    (40ants-doc/args::with-dislocated-symbols ((40ants-doc/args::macro-arg-names arglist))
      (40ants-doc/render/print::maybe-print-docstring symbol 'function stream))))


(defmethod 40ants-doc/commondoc/builder::reference-to-commondoc ((symbol symbol) (locative-type (eql 'macro)) locative-args)
  (let* ((reference (canonical-reference
                     (40ants-doc/reference::make-reference
                      symbol (cons locative-type locative-args))))
         (arglist (swank-backend:arglist symbol))
         (docstring (40ants-doc/render/print::get-docstring symbol 'function))
         ;; TODO:  we should move text transformation out from get-docstring to after it will be parsed
         (children (40ants-doc/commondoc/builder::parse-markdown docstring)))

    (40ants-doc/commondoc/bullet::make-bullet reference
                                              :arglist locative-args
                                              :children children
                                              :ignore-words (40ants-doc/args::macro-arg-names arglist))))


(defmethod 40ants-doc/locatives/base::locate-and-find-source (symbol (locative-type (eql 'macro))
                                                              locative-args)
  (declare (ignore locative-args))
  (40ants-doc/source-api::find-source (macro-function symbol)))
