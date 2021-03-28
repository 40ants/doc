(defpackage #:40ants-doc/render/toc
  (:use #:cl)
  (:import-from #:40ants-doc/builder/vars)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/link)
  (:import-from #:40ants-doc/page))
(in-package 40ants-doc/render/toc)


(defun print-table-of-contents-entry (object string stream)
  (loop repeat (* 4 (1- 40ants-doc/builder/vars::*heading-level*))
        do (write-char #\Space stream))
  (let ((link-id (let ((40ants-doc/page::*page* 40ants-doc/page::*table-of-contents-page*))
                   (40ants-doc/page::link-to-reference
                    (40ants-doc/reference::canonical-reference object))))
        (string (40ants-doc/utils::escape-markdown string)))
    (if (and 40ants-doc/link::*document-link-sections* link-id)
        (format stream "- [~A~A][~A]" (heading-number) string link-id)
        (format stream "- ~A~A" (heading-number) string)))
  (terpri stream))


(defun heading-number ()
  (format nil "~@[~{~D~^.~} ~]"
          (when (<= (1- (length 40ants-doc/builder/vars::*heading-number*))
                    40ants-doc/builder/vars::*document-max-numbering-level*)
            (butlast 40ants-doc/builder/vars::*heading-number*))))

(defmacro with-nested-headings (() &body body)
  `(let ((40ants-doc/builder/vars::*heading-level* (1+ 40ants-doc/builder/vars::*heading-level*)))
     ,@body))

