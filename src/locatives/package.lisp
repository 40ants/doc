(uiop:define-package #:40ants-doc/locatives/package
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
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
  (:import-from #:swank-backend)
  (:import-from #:swank-mop)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader)
  (:import-from #:40ants-doc/commondoc/bullet)
  (:import-from #:40ants-doc/commondoc/builder))
(in-package 40ants-doc/locatives/package)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)

(define-locative-type package ())

(defmethod locate-object (symbol (locative-type (eql 'package))
                          locative-args)
  (assert (= 0 (length locative-args)))
  (or (find-package symbol) (locate-error)))

(defmethod canonical-reference ((package package))
  (40ants-doc/reference::make-reference (package-name package) 'package))

(defmethod document-object ((package package) stream)
  (let ((symbol (package-name package)))
    (40ants-doc/builder/bullet::print-bullet package stream)
    (40ants-doc/builder/bullet::print-end-bullet stream)
    (40ants-doc/args::with-dislocated-symbols ((list symbol))
      (40ants-doc/render/print::maybe-print-docstring package t stream))))


(defmethod 40ants-doc/commondoc/builder:to-commondoc ((package package))
  (let* ((reference (canonical-reference package))
         (symbol (package-name package))
         (docstring (40ants-doc/render/print::get-docstring package t))
         (children (when docstring
                     (40ants-doc/commondoc/builder::parse-markdown docstring))))
    (40ants-doc/commondoc/bullet::make-bullet reference
                                              :children children
                                              :ignore-words symbol)))
