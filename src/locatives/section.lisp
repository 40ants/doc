(defpackage #:40ants-doc/locatives/section
  (:use #:cl)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/reference-api)
  (:import-from #:40ants-doc/source)
  (:import-from #:40ants-doc/locatives/base)
  (:import-from #:40ants-doc/builder/printer)
  (:import-from #:40ants-doc/page)
  (:import-from #:40ants-doc/locatives
                #:section)
  (:import-from #:40ants-doc/core
                ;; #:section
                ))
(in-package 40ants-doc/locatives/section)


(40ants-doc/locatives/base::define-locative-type section ()
  "Refers to a section defined by 40ANTS-DOC:DEFSECTION.")

(defmethod 40ants-doc/locatives/base::locate-object (symbol (locative-type (eql 'section))
                          locative-args)
  (declare (ignore locative-args))
  (unless (typep (symbol-value symbol)
                 '40ants-doc/core::section)
    (error "Section locative works only with objects defined by 40ANTS-DOC:DEFSECTION."))
  (symbol-value symbol))

(defmethod 40ants-doc/reference-api::canonical-reference ((section 40ants-doc/core::section))
  (40ants-doc/reference::make-reference (40ants-doc/core::section-name section)
                                        'section))

(defmethod 40ants-doc/source-api:find-source ((section 40ants-doc/core::section))
  (40ants-doc/locatives/base:locate-and-find-source (40ants-doc/core::section-name section) 'variable ()))
