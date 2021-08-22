(uiop:define-package #:40ants-doc/builder/section
  (:use #:cl)
  (:import-from #:40ants-doc/core)
  (:import-from #:40ants-doc/builder/printer)
  (:import-from #:40ants-doc/page)
  (:import-from #:40ants-doc/reference-api)
  (:import-from #:40ants-doc/warn)
  (:import-from #:40ants-doc/commondoc/format)
  (:import-from #:commondoc-markdown/emitter))
(in-package 40ants-doc/builder/section)


(defun section-title-or-name (section)
  (or (40ants-doc/core::section-title section)
      (40ants-doc/builder/printer::maybe-downcase
       (prin1-to-string (40ants-doc/core::section-name section)))))


(defmethod 40ants-doc/reference-api::format-reference ((obj 40ants-doc/core::section) name ref link)
  `((:reference-link :label (,(section-title-or-name (40ants-doc/reference::resolve ref)))
                     :definition ,link)))
