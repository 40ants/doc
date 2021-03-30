(uiop:define-package #:40ants-doc/locatives/type
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
  (:import-from #:40ants-doc/builder/bullet
                #:locate-and-print-bullet)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/args)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/builder/vars)
  (:import-from #:40ants-doc/render/print)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/page)
  (:import-from #:swank-backend)
  (:import-from #:40ants-doc/locatives/utils))
(in-package 40ants-doc/locatives/type)


(define-locative-type type ()
  "TYPE can refer to classes as well, but it's better style to use the
  more specific CLASS locative type for that. Another difference to
  CLASS is that an attempt is made at printing the arguments of type
  specifiers.")

(defmethod locate-object (symbol (locative-type (eql 'type)) locative-args)
  (unless (swank-backend:type-specifier-p 'symbol)
    (locate-error))
  (40ants-doc/reference::make-reference symbol (cons locative-type locative-args)))


;; Show lambda type expansion along with docstring
;; SLY inspector shows it:
;; 
;; Type-specifier documentation: Very small integer, less or equal than 3.
;; Type-specifier expansion: (AND INTEGER (SATISFIES A-FEW-P))
(defmethod locate-and-document (symbol (locative-type (eql 'type)) locative-args
                                stream)
  (locate-and-print-bullet locative-type locative-args symbol stream)
  (let ((arglist (swank-backend:type-specifier-arglist symbol)))
    (when (and arglist (not (eq arglist :not-available)))
      (write-char #\Space stream)
      (40ants-doc/render/args::print-arglist arglist stream)))
  (40ants-doc/builder/bullet::print-end-bullet stream)
  (40ants-doc/args::with-dislocated-symbols ((list symbol))
    (40ants-doc/render/print::maybe-print-docstring symbol 'type stream)))


(defmethod locate-and-find-source (symbol (locative-type (eql 'type))
                                   locative-args)
  (declare (ignore locative-args))
  (40ants-doc/locatives/utils::find-one-location (swank-backend:find-definitions symbol)
                                                 '("type" "class")))
