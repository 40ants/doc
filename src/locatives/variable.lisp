(uiop:define-package #:40ants-doc/locatives/variable
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
  (:import-from #:40ants-doc/source-api)
  (:import-from #:swank-backend)
  (:import-from #:40ants-doc/locatives/utils)
  (:import-from #:40ants-doc/commondoc/builder)
  (:import-from #:40ants-doc/commondoc/bullet))
(in-package 40ants-doc/locatives/variable)


(define-locative-type variable (&optional initform)
  "Refers to a global special variable. INITFORM, or if not specified,
  the global value of the variable is included in the documentation.")

(defmethod locate-object (symbol (locative-type (eql 'variable)) locative-args)
  (assert (<= (length locative-args) 1))
  (40ants-doc/reference::make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'variable))
                                locative-args stream)
  (destructuring-bind (&optional (initform nil initformp)) locative-args
    (40ants-doc/builder/bullet::locate-and-print-bullet locative-type locative-args symbol stream)
    (write-char #\Space stream)
    (multiple-value-bind (value unboundp) (40ants-doc/utils::symbol-global-value symbol)
      (40ants-doc/render/args::print-arglist (prin1-to-string (cond (initformp initform)
                                                                    (unboundp "-unbound-")
                                                                    (t value)))
                     stream))
    (40ants-doc/builder/bullet::print-end-bullet stream)
    (with-dislocated-symbols ((list symbol))
      (40ants-doc/render/print::maybe-print-docstring symbol locative-type stream))))


(defmethod 40ants-doc/commondoc/builder::reference-to-commondoc ((symbol symbol) (locative-type (eql 'variable)) locative-args)
  (destructuring-bind (&optional (initform nil initformp)) locative-args
    (let* ((reference (canonical-reference
                       (40ants-doc/reference::make-reference
                        symbol (cons locative-type locative-args))))
           (docstring (40ants-doc/render/print::get-docstring symbol 'variable))
           (arglist (multiple-value-bind (value unboundp) (40ants-doc/utils::symbol-global-value symbol)
                      (cond (initformp
                             (prin1-to-string initform))
                            (unboundp "-unbound-")
                            (t
                             (prin1-to-string value)))))
           (children (when docstring
                       (40ants-doc/commondoc/builder::parse-markdown docstring))))

      (40ants-doc/commondoc/bullet::make-bullet reference
                                                :arglist arglist
                                                :children children
                                                :ignore-words symbol))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'variable))
                                   locative-args)
  (declare (ignore locative-args))
  (40ants-doc/locatives/utils::find-one-location (swank-backend:find-definitions symbol)
                                                 '("variable" "defvar" "defparameter"
                                                   "special-declaration")))

(defvar end-of-variable-example)
