(uiop:define-package #:40ants-doc/builder/section
  (:use #:cl)
  (:import-from #:40ants-doc/core)
  (:import-from #:40ants-doc/render/toc)
  (:import-from #:40ants-doc/builder/printer)
  (:import-from #:40ants-doc/page)
  (:import-from #:40ants-doc/reference-api)
  (:import-from #:40ants-doc/document)
  (:import-from #:40ants-doc/warn)
  (:import-from #:40ants-doc/builder/heading)
  (:import-from #:40ants-doc/commondoc/format)
  (:import-from #:commondoc-markdown/emitter))
(in-package 40ants-doc/builder/section)


(defun section-title-or-name (section)
  (or (40ants-doc/core::section-title section)
      (40ants-doc/builder/printer::maybe-downcase
       (prin1-to-string (40ants-doc/core::section-name section)))))


(defvar *section*)

;; (defmethod 40ants-doc/document::document-object ((section 40ants-doc/core::section) stream)
;;   (40ants-doc/warn::with-ignored (40ants-doc/core::section-ignore-words section)
;;     (let ((same-package (eq *package* (40ants-doc/core::section-package section)))
;;           (*package* (if 40ants-doc/builder/printer::*document-normalize-packages*
;;                          (40ants-doc/core::section-package section)
;;                          *package*))
;;           (*readtable* (40ants-doc/core::section-readtable section))
;;           (*section* section))
;;       (40ants-doc/builder/heading::with-heading (stream section (40ants-doc/builder/section::section-title-or-name section)
;;                                                  :link-title-to (40ants-doc/core::section-link-title-to section))
;;         (when (and 40ants-doc/builder/printer::*document-normalize-packages*
;;                    (not same-package))
;;           (format stream "###### \\[in package ~A~A\\]~%" (package-name *package*)
;;                   (if (package-nicknames *package*)
;;                       (format nil " with nicknames ~{~A~^, ~}" (package-nicknames *package*))
;;                       "")))
;;         (let ((firstp t))
;;           (dolist (entry (40ants-doc/core::section-entries section))
;;             (if firstp
;;                 (setq firstp nil)
;;                 (terpri stream))
;;             (40ants-doc/render/toc::with-nested-headings ()
;;               (40ants-doc/document::document-object entry stream))))))))


(defmethod describe-object ((section 40ants-doc/core::section) stream)
  "40ANTS-DOC:SECTION objects are printed by calling 40ANTS-DOC/DOCUMENT::DOCUMENT on them
  with all 40ANTS-DOC/DOC:@DOCUMENTATION-PRINTER-VARIABLES, except for
  `40ANTS-DOC/BUILDER/PRINTER::*DOCUMENT-NORMALIZE-PACKAGES*`, turned off to reduce clutter."

  ;; TODO: check these vars, probably some of them aren't need anymore:
  (let ((40ants-doc/builder/printer::*document-uppercase-is-code* nil) ;; supported
        (40ants-doc/link::*document-link-code* nil) ;; supported
        (commondoc-markdown/emitter:*emit-section-anchors* nil) ;; supported
        (40ants-doc/builder/vars::*document-mark-up-signatures* nil)
        (40ants-doc/builder/vars::*document-max-numbering-level* 0)
        (40ants-doc/builder/vars::*document-max-table-of-contents-level* 0)
        (40ants-doc/builder/vars::*document-text-navigation* nil)
        ;; Some Lisps bind it to T in DESCRIBE, some don't.
        (*print-circle* nil))

    (write-string (40ants-doc/builder:render-to-string section
                                                       :format :markdown)
                  stream)))


(defmethod 40ants-doc/reference-api::format-reference ((obj 40ants-doc/core::section) name ref link)
  `((:reference-link :label (,(section-title-or-name (40ants-doc/reference::resolve ref)))
                     :definition ,link)))
