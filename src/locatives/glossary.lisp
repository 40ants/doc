(defpackage #:40ants-doc/locatives/glossary
  (:use #:cl)
  ;; (:import-from #:40ants-doc/core)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/builder/printer)
  (:import-from #:40ants-doc/builder/bullet)
  (:import-from #:40ants-doc/locatives/base)
  (:import-from #:40ants-doc/reference-api)
  (:import-from #:40ants-doc/markdown/transform))
(in-package 40ants-doc/locatives/glossary)


;;;; GLOSSARY-TERM locative

(defclass glossary-term ()
  ((name
    :initarg :name :reader glossary-term-name
    :documentation "The name of the global variable whose value is
    this GLOSSARY-TERM object.")
   (title
    :initarg :title :reader glossary-term-title
    :documentation "Used in generated documentation.")
   (docstring
    :initarg :docstring :reader glossary-term-docstring)))

(defmacro define-glossary-term
    (name (&key title (discard-documentation-p 40ants-doc/core::*discard-documentation-p*))
     docstring)
  "Define a global variable with NAME and set it to a glossary term
  object. A glossary term is just a symbol to hang a docstring on. It
  is a bit like a SECTION in that, when linked to, its TITLE will be
  the link text instead of the name of the symbol. Unlike sections
  though, glossary terms are not rendered with headings, but in the
  more lightweight bullet + locative + name/title style.

  When DISCARD-DOCUMENTATION-P (defaults to *DISCARD-DOCUMENTATION-P*)
  is true, DOCSTRING will not be recorded to save memory."
  `(defparameter ,name
     (make-instance 'glossary-term
                    :name ',name :title ,title
                    :docstring ,(unless discard-documentation-p
                                  docstring))))

(defun glossary-term-title-or-name (glossary-term)
  (or (glossary-term-title glossary-term)
      (40ants-doc/builder/printer::maybe-downcase
       (prin1-to-string (glossary-term-name glossary-term)))))

(defmethod print-object ((glossary-term glossary-term) stream)
  (print-unreadable-object (glossary-term stream :type t)
    (format stream "~a" (glossary-term-name glossary-term))))

(40ants-doc/locatives/base::define-locative-type glossary-term ()
  "Refers to a glossary term defined by DEFINE-GLOSSARY-TERM.")

(defmethod locate-object (symbol (locative-type (eql 'glossary-term))
                          locative-args)
  (declare (ignore locative-args))
  (assert (typep (symbol-value symbol) 'glossary-term))
  (symbol-value symbol))

(defmethod 40ants-doc/document::document-object ((glossary-term glossary-term) stream)
  (let ((symbol (glossary-term-name glossary-term)))
    (40ants-doc/builder/bullet::locate-and-print-bullet 'glossary-term () symbol stream
                                                        :name (glossary-term-title-or-name glossary-term))
    (40ants-doc/builder/bullet::print-end-bullet stream)
    (40ants-doc/args::with-dislocated-symbols ((list symbol))
      (let ((docstring (glossary-term-docstring glossary-term)))
        (when docstring
          (format stream "~%~A~%" (40ants-doc/markdown/transform::massage-docstring docstring)))))))

(defmethod canonical-reference ((glossary-term glossary-term))
  (40ants-doc/reference::make-reference (glossary-term-name glossary-term) 'glossary-term))

(defmethod 40ants-doc/source::find-source ((glossary-term glossary-term))
  (40ants-doc/locatives/base::locate-and-find-source (glossary-term-name glossary-term) 'variable ()))


(defmethod 40ants-doc/reference-api::format-reference ((obj glossary-term) name ref link)
  `((:reference-link :label (,(40ants-doc/locatives/glossary::glossary-term-title-or-name
                               (40ants-doc/locatives/base::resolve ref)))
                     :definition ,link)))

