(defpackage #:40ants-doc/locatives/glossary
  (:use #:cl)
  ;; (:import-from #:40ants-doc/core)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/builder/printer)
  (:import-from #:40ants-doc/builder/bullet)
  (:import-from #:40ants-doc/locatives/base)
  (:import-from #:40ants-doc/locatives
                #:glossary-term)
  (:import-from #:40ants-doc/glossary)
  (:import-from #:40ants-doc/reference-api)
  (:import-from #:40ants-doc/markdown/transform))
(in-package 40ants-doc/locatives/glossary)


(40ants-doc/locatives/base::define-locative-type glossary-term ()
  "Refers to a glossary term defined by 40ANTS-DOC/GLOSSARY::DEFINE-GLOSSARY-TERM.")

(defmethod 40ants-doc/locatives/base::locate-object (symbol (locative-type (eql 'glossary-term))
                                                     locative-args)
  (declare (ignore locative-args))
  (assert (typep (symbol-value symbol) '40ants-doc/glossary::glossary-term))
  (symbol-value symbol))


(defun glossary-term-title-or-name (glossary-term)
  (check-type glossary-term 40ants-doc/glossary::glossary-term)
  (or (40ants-doc/glossary::glossary-term-title glossary-term)
      (40ants-doc/builder/printer::maybe-downcase
       (prin1-to-string (40ants-doc/glossary::glossary-term-name glossary-term)))))


;; (defmethod 40ants-doc/document::document-object ((glossary-term 40ants-doc/glossary::glossary-term) stream)
;;   (let ((symbol (40ants-doc/glossary::glossary-term-name glossary-term)))
;;     (40ants-doc/builder/bullet::locate-and-print-bullet 'glossary-term () symbol stream
;;                                                         :name (glossary-term-title-or-name glossary-term))
;;     (40ants-doc/builder/bullet::print-end-bullet stream)
;;     (40ants-doc/args::with-dislocated-symbols ((list symbol))
;;       (let ((docstring (40ants-doc/glossary::glossary-term-docstring glossary-term)))
;;         (when docstring
;;           (format stream "~%~A~%" (40ants-doc/markdown/transform::massage-docstring docstring)))))))


(defmethod 40ants-doc/commondoc/builder:to-commondoc ((glossary-term 40ants-doc/glossary::glossary-term))
  (let* ((symbol (40ants-doc/glossary::glossary-term-name glossary-term))
         (reference
           (40ants-doc/reference::canonical-reference (40ants-doc/reference::make-reference
                                                       symbol '(glossary-term))))
         (docstring (let ((docstring (40ants-doc/glossary::glossary-term-docstring glossary-term)))
                      (when docstring
                        (40ants-doc/utils::strip-docstring-indentation docstring))))
         (children (when docstring
                     (40ants-doc/commondoc/builder::parse-markdown docstring))))

    (40ants-doc/commondoc/bullet::make-bullet reference
                                              :name (glossary-term-title-or-name glossary-term)
                                              :children children
                                              :ignore-words symbol)))

(defmethod 40ants-doc/reference-api::canonical-reference ((glossary-term 40ants-doc/glossary::glossary-term))
  (40ants-doc/reference::make-reference (40ants-doc/glossary::glossary-term-name glossary-term) 'glossary-term))

(defmethod 40ants-doc/source::find-source ((glossary-term 40ants-doc/glossary::glossary-term))
  (40ants-doc/locatives/base::locate-and-find-source (40ants-doc/glossary::glossary-term-name glossary-term) 'variable ()))


(defmethod 40ants-doc/reference-api::format-reference ((obj 40ants-doc/glossary::glossary-term) name ref link)
  `((:reference-link :label (,(glossary-term-title-or-name
                               (40ants-doc/reference::resolve ref)))
                     :definition ,link)))

