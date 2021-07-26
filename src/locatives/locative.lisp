(uiop:define-package #:40ants-doc/locatives/locative
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-and-find-source
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
                #:function-arg-names)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/builder/vars)
  (:import-from #:40ants-doc/locatives
                #:locative)
  (:import-from #:40ants-doc/render/print)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/page)
  (:import-from #:40ants-doc/commondoc/builder)
  (:import-from #:40ants-doc/commondoc/bullet))
(in-package 40ants-doc/locatives/locative)


(define-locative-type locative (lambda-list)
  "This is the locative for locatives. When `M-.` is pressed on
  `VARIABLE` in `(VARIABLE LOCATIVE)`, this is what makes it possible
  to land at the `(40ANTS-DOC/LOCATIVES/BASE:DEFINE-LOCATIVE-TYPE VARIABLE ...)` form.
  Similarly, `(LOCATIVE LOCATIVE)` leads to this very definition.")

(defmethod locate-object (symbol (locative-type (eql 'locative)) locative-args)
  (assert (endp locative-args))
  (or (ignore-errors (locative-lambda-list-method-for-symbol symbol))
      (locate-error))
  (40ants-doc/reference::make-reference symbol (cons locative-type locative-args)))


(defun locative-lambda-list-method-for-symbol (symbol)
  (find-method #'40ants-doc/locatives/base::locative-lambda-list () `((eql ,symbol))))


(defmethod 40ants-doc/locatives/base::locate-and-document (symbol (locative-type (eql 'locative))
                                                           locative-args stream)
  (let ((method (locative-lambda-list-method-for-symbol symbol))
        (lambda-list (40ants-doc/locatives/base::locative-lambda-list symbol)))
    (40ants-doc/builder/bullet::locate-and-print-bullet locative-type locative-args symbol stream)
    (40ants-doc/args::with-dislocated-symbols ((40ants-doc/args::macro-arg-names lambda-list))
      (when lambda-list
        (write-char #\Space stream)
        (40ants-doc/render/args::print-arglist lambda-list stream))
      (40ants-doc/builder/bullet::print-end-bullet stream)
      (40ants-doc/args::with-dislocated-symbols ((list symbol))
        (40ants-doc/render/print::maybe-print-docstring method t stream))))
  (format stream "~&"))


(defmethod 40ants-doc/commondoc/builder::reference-to-commondoc ((symbol symbol) (locative-type (eql 'locative)) locative-args)
  (let* ((reference (canonical-reference
                     (40ants-doc/reference::make-reference
                      symbol (cons locative-type locative-args))))
         (method (locative-lambda-list-method-for-symbol symbol))
         (arglist (40ants-doc/locatives/base::locative-lambda-list symbol))
         (docstring (40ants-doc/render/print::get-docstring method t))
         (children (when docstring
                     (40ants-doc/commondoc/builder::parse-markdown docstring))))

    (40ants-doc/commondoc/bullet::make-bullet reference
                                              :arglist arglist
                                              :ignore-words (list* symbol
                                                                   (function-arg-names arglist))
                                              :children children)))


(defmethod locate-and-find-source (symbol (locative-type (eql 'locative))
                                   locative-args)
  (declare (ignore locative-args))
  (40ants-doc/source-api::find-source (locative-lambda-list-method-for-symbol symbol)))
