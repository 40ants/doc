(uiop:define-package #:40ants-doc/locatives/symbol-macro
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
                #:symbol-macro)
  (:import-from #:40ants-doc/locatives/utils))

(in-package 40ants-doc/locatives/symbol-macro)

(define-locative-type symbol-macro ()
  "")

(defmethod locate-object ((symbol symbol)
                          (locative-type (eql 'symbol-macro))
                          locative-args)
  (when (equalp (macroexpand-1 symbol)
		symbol)
    (locate-error))
  (40ants-doc/reference::make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document ((symbol symbol)
                                (locative-type (eql 'symbol-macro))
                                locative-args stream)
  (40ants-doc/builder/bullet::locate-and-print-bullet locative-type locative-args symbol stream)
  (40ants-doc/builder/bullet::print-end-bullet stream)
  (40ants-doc/args::with-dislocated-symbols ((list symbol))
    (40ants-doc/render/print::maybe-print-docstring symbol :symbol-macro stream)))


(defmethod locate-and-find-source (symbol
                                   (locative-type (eql 'symbol-macro))
                                   locative-args)
  (declare (ignore locative-args))
  ;; Some implementations can not find the source location of the
  ;; accessor function, so fall back on FIND-ONE-LOCATION.
  (let ((location (40ants-doc/source-api::find-source (symbol-function symbol))))
    (if (eq :error (first location))
        (40ants-doc/locatives/utils::find-one-location (swank-backend:find-definitions symbol)
                                                       '("function" "operator"))
        location)))
