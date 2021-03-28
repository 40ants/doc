(uiop:define-package #:40ants-doc/builder/footer
  (:use #:cl)
  (:import-from #:40ants-doc/link)
  (:import-from #:40ants-doc/core)
  (:import-from #:40ants-doc/page)
  (:import-from #:40ants-doc/builder/section))
(in-package 40ants-doc/builder/footer)


;;; Emit markdown definitions for links to REFERENCE objects that were
;;; linked to.
(defun emit-footer (stream)
  (let ((used-links (sort (remove-if-not #'40ants-doc/page::link-used-on-current-page-p
                                         40ants-doc/link::*links*)
                          #'string< :key #'40ants-doc/link::link-id)))
    (when used-links
      (format stream "~%")
      (dolist (link used-links)
        (let ((anchor (40ants-doc/reference::reference-to-anchor
                       (40ants-doc/link::link-reference link))))
          (format stream "  [~A]: ~@[~A~]#~A ~S~%"
                  (40ants-doc/link::link-id link)
                  (if (40ants-doc/link::link-page link)
                      (40ants-doc/page::relative-page-uri-fragment (40ants-doc/link::link-page link)
                                                                   40ants-doc/page::*page*)
                      nil)
                  (40ants-doc/utils::html-safe-name anchor)
                  (let ((object (40ants-doc/reference::resolve (40ants-doc/link::link-reference link))))
                    (if (typep object '40ants-doc/core::section)
                        (40ants-doc/builder/section::section-title-or-name object)
                        (princ-to-string anchor)))))))))

