(defpackage #:40ants-doc/locatives/section
  (:use #:cl)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/reference-api)
  (:import-from #:40ants-doc/builder/heading-api)
  (:import-from #:40ants-doc/source)
  (:import-from #:40ants-doc/locatives/base)
  (:import-from #:40ants-doc/builder/printer)
  (:import-from #:40ants-doc/builder/heading)
  (:import-from #:40ants-doc/builder/bullet)
  (:import-from #:40ants-doc/page)
  (:import-from #:40ants-doc/locatives
                #:section)
  (:import-from #:40ants-doc/core
                ;; #:section
                ))
(in-package 40ants-doc/locatives/section)


(40ants-doc/locatives/base::define-locative-type section ()
  "Refers to a section defined by DEFSECTION.")

(defmethod 40ants-doc/locatives/base::locate-object (symbol (locative-type (eql 'section))
                          locative-args)
  (declare (ignore locative-args))
  (unless (typep (symbol-value symbol)
                 '40ants-doc/core::section)
    (error "Section locative works only with objects defined by defsection."))
  (symbol-value symbol))

(defmethod 40ants-doc/reference-api::canonical-reference ((section 40ants-doc/core::section))
  (40ants-doc/reference::make-reference (40ants-doc/core::section-name section)
                                        'section))

(defmethod 40ants-doc/reference-api::collect-reachable-objects ((section 40ants-doc/core::section))
  (mapcan (lambda (reference)
            (cons reference (40ants-doc/reference-api::collect-reachable-objects reference)))
          (remove-if-not (lambda (entry)
                           (typep entry '40ants-doc/reference::reference))
                         (40ants-doc/core::section-entries section))))

(defmethod 40ants-doc/builder/heading-api::fancy-navigation ((object 40ants-doc/core::section))
  (if (and 40ants-doc/builder/vars::*document-fancy-html-navigation*
           40ants-doc/link::*document-link-sections*
           (eq 40ants-doc/builder/printer::*format* :html))
      (let* ((position (position object 40ants-doc/builder/heading::*headings* :key #'40ants-doc/builder/heading::heading-object))
             (level (40ants-doc/builder/heading::heading-level (elt 40ants-doc/builder/heading::*headings* position)))
             (n (length 40ants-doc/builder/heading::*headings*))
             (prev (when (plusp position)
                     (elt 40ants-doc/builder/heading::*headings* (1- position))))
             (up (when (plusp level)
                   (find (1- level)
                         (subseq 40ants-doc/builder/heading::*headings* 0 position)
                         :from-end t
                         :key #'40ants-doc/builder/heading::heading-level)))
             (next (when (< position (1- n))
                     (elt 40ants-doc/builder/heading::*headings* (1+ position))))
             (source-uri (40ants-doc/builder/bullet::source-uri
                          (40ants-doc/reference-api::canonical-reference object))))
        (format nil "<span class=\"outer-navigation\">~
                    <span class=\"navigation\">~
                    ~@[ [&#8592;][~A]~]~
                    ~@[ [&#8593;][~A]~]~
                    ~@[ [&#8594;][~A]~] ~
                    [&#8634;][~A]~
                    ~A~
                    </span></span>~%"
                (when prev
                  (40ants-doc/page::link-to-reference
                   (40ants-doc/reference-api::canonical-reference (40ants-doc/builder/heading::heading-object prev))))
                (when up
                  (40ants-doc/page::link-to-reference
                   (40ants-doc/reference-api::canonical-reference (40ants-doc/builder/heading::heading-object up))))
                (when next
                  (40ants-doc/page::link-to-reference
                   (40ants-doc/reference-api::canonical-reference (40ants-doc/builder/heading::heading-object next))))
                (40ants-doc/page::link-to-reference (40ants-doc/reference-api::canonical-reference object))
                (if source-uri
                    (format nil " <a href=~S>&#955;</a>" source-uri)
                    "")))
      ""))


(defmethod 40ants-doc/source::find-source ((section 40ants-doc/core::section))
  (40ants-doc/locatives/base::locate-and-find-source (40ants-doc/core::section-name section) 'variable ()))
