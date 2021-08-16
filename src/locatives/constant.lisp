(uiop:define-package #:40ants-doc/locatives/constant
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-and-find-source
                #:locate-and-document
                #:locate-error
                #:locate-object
                #:define-locative-type)
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
  (:import-from #:40ants-doc/source-api)
  (:import-from #:swank-backend)
  (:import-from #:40ants-doc/locatives/utils)
  (:import-from #:40ants-doc/locatives
                #:constant)
  (:import-from #:40ants-doc/commondoc/bullet)
  (:import-from #:40ants-doc/commondoc/builder))
(in-package 40ants-doc/locatives/constant)


(define-locative-type constant (&optional initform)
  "Refers to a DEFCONSTANT. INITFORM, or if not specified,
  the value of the constant is included in the documentation.")


(defmethod locate-object (symbol (locative-type (eql 'constant)) locative-args)
  (assert (<= (length locative-args) 1))
  (assert (constantp symbol))
  (40ants-doc/reference::make-reference symbol (cons locative-type locative-args)))


(defmethod locate-and-document (symbol (locative-type (eql 'constant))
                                locative-args stream)
  (destructuring-bind (&optional (initform nil initformp)) locative-args
    (40ants-doc/builder/bullet::locate-and-print-bullet locative-type locative-args symbol stream)
    (write-char #\Space stream)
    (40ants-doc/render/args::print-arglist (prin1-to-string (cond (initformp
                                                                   initform)
                                                                  ((boundp symbol)
                                                                   (symbol-value symbol))
                                                                  (t
                                                                   "<unbound>")))
                                           stream)
    (40ants-doc/builder/bullet::print-end-bullet stream)
    (40ants-doc/args::with-dislocated-symbols ((list symbol))
      (40ants-doc/render/print::maybe-print-docstring symbol
                                                      ;; Standard does not support 'constant doc-type
                                                      ;; and defconstant objects use 'variable
                                                      'variable
                                                      ;; locative-type
                                                      stream))))



(defmethod 40ants-doc/commondoc/builder::reference-to-commondoc ((symbol symbol) (locative-type (eql 'constant)) locative-args)
  (destructuring-bind (&optional (initform nil initformp)) locative-args
    (let* ((reference (canonical-reference
                       (40ants-doc/reference::make-reference
                        symbol (cons locative-type locative-args))))
           (docstring (40ants-doc/render/print::get-docstring symbol 'variable))
           (arglist (cond (initformp
                           (prin1-to-string initform))
                          ((boundp symbol)
                           (prin1-to-string
                            (symbol-value symbol)))
                          (t "-unbound-")))
           (children (when docstring
                       (40ants-doc/commondoc/builder::parse-markdown docstring))))

      (40ants-doc/commondoc/bullet::make-bullet reference
                                                :arglist arglist
                                                :children children
                                                :ignore-words symbol))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'constant))
                                   locative-args)
  (declare (ignore locative-args))
  (40ants-doc/locatives/utils::find-one-location (swank-backend:find-definitions symbol)
                                                 '("defconstant" "constant" "variable")))
