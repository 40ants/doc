(uiop:define-package #:40ants-doc/locatives/type
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-and-find-source
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc/render/args)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/args)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/builder/vars)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/page)
  (:import-from #:swank-backend)
  (:import-from #:40ants-doc/locatives/utils)
  (:import-from #:40ants-doc/commondoc/builder)
  (:import-from #:40ants-doc/commondoc/bullet)
  (:import-from #:40ants-doc/docstring)
  (:import-from #:40ants-doc/commondoc/markdown))
(in-package 40ants-doc/locatives/type)


(define-locative-type type ()
  "TYPE can refer to classes as well, but it's better style to use the
  more specific CLASS locative type for that. Another difference to
  CLASS is that an attempt is made at printing the arguments of type
  specifiers.")

(defmethod locate-object (symbol (locative-type (eql 'type)) locative-args)
  (unless (swank-backend:type-specifier-p 'symbol)
    (locate-error))
  (40ants-doc/reference:make-reference symbol (cons locative-type locative-args)))


;; Show lambda type expansion along with docstring
;; SLY inspector shows it:
;; 
;; Type-specifier documentation: Very small integer, less or equal than 3.
;; Type-specifier expansion: (AND INTEGER (SATISFIES A-FEW-P))
(defmethod 40ants-doc/commondoc/builder::reference-to-commondoc ((symbol symbol) (locative-type (eql 'type)) locative-args)
  (let* ((reference (40ants-doc/reference:make-reference symbol
                                                         (cons locative-type locative-args)))
         (arglist (swank-backend:type-specifier-arglist symbol))
         (docstring (40ants-doc/docstring:get-docstring symbol 'type))
         (children (when docstring
                     (40ants-doc/commondoc/markdown:parse-markdown docstring))))

    (40ants-doc/commondoc/bullet:make-bullet reference
                                             :arglist arglist
                                             :children children
                                             :ignore-words symbol)))


(defmethod locate-and-find-source (symbol (locative-type (eql 'type))
                                   locative-args)
  (declare (ignore locative-args))
  (40ants-doc/locatives/utils::find-one-location (swank-backend:find-definitions symbol)
                                                 '("type" "class")))
