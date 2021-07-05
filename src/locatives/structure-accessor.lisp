(uiop:define-package #:40ants-doc/locatives/structure-accessor
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
  (:import-from #:40ants-doc/args)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/builder/vars)
  (:import-from #:40ants-doc/render/print)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/page)
  (:import-from #:40ants-doc/builder/printer)
  (:import-from #:40ants-doc/markdown/transform)
  (:import-from #:swank-backend)
  (:import-from #:40ants-doc/source-api)
  (:import-from #:40ants-doc/locatives
                #:structure-accessor)
  (:import-from #:40ants-doc/locatives/utils)
  (:import-from #:40ants-doc/commondoc/builder)
  (:import-from #:40ants-doc/commondoc/bullet))
(in-package 40ants-doc/locatives/structure-accessor)


(define-locative-type structure-accessor ()
  "This is a synonym of FUNCTION with the difference that the often
  ugly and certainly uninformative lambda list will not be printed.")

(defmethod locate-object ((symbol symbol)
                          (locative-type (eql 'structure-accessor))
                          locative-args)
  ;; Signal an error if it doesn't exist.
  (or (ignore-errors (symbol-function symbol))
      (locate-error))
  (40ants-doc/reference::make-reference symbol (cons locative-type locative-args)))


;; TODO: Maybe support initial value,
;;       type and read-only flag.
(defmethod locate-and-document ((symbol symbol)
                                (locative-type (eql 'structure-accessor))
                                locative-args stream)
  (40ants-doc/builder/bullet::locate-and-print-bullet locative-type locative-args symbol stream)
  (40ants-doc/builder/bullet::print-end-bullet stream)
  (40ants-doc/args::with-dislocated-symbols ((list symbol))
    (40ants-doc/render/print::maybe-print-docstring symbol 'function stream)))


(defmethod 40ants-doc/commondoc/builder::reference-to-commondoc ((symbol symbol) (locative-type (eql 'structure-accessor)) locative-args)
  (let* ((reference (canonical-reference
                     (40ants-doc/reference::make-reference
                      symbol (cons locative-type locative-args))))
         (docstring (40ants-doc/args::with-dislocated-symbols ((list symbol))
                      (40ants-doc/render/print::get-docstring symbol 'function)))
         ;; TODO:  we should move text transfromation after it will be parsed
         (children (when docstring
                     (40ants-doc/commondoc/builder::parse-markdown docstring))))

    (40ants-doc/commondoc/bullet::make-bullet reference
                                              :arglist locative-args
                                              :children children)))


(defmethod locate-and-find-source (symbol
                                   (locative-type (eql 'structure-accessor))
                                   locative-args)
  (declare (ignore locative-args))
  ;; Some implementations can not find the source location of the
  ;; accessor function, so fall back on FIND-ONE-LOCATION.
  (let ((location (40ants-doc/source-api::find-source (symbol-function symbol))))
    (if (eq :error (first location))
        (40ants-doc/locatives/utils::find-one-location (swank-backend:find-definitions symbol)
                                                       '("function" "operator"))
        location)))
