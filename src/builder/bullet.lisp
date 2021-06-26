(defpackage #:40ants-doc/builder/bullet
  (:use #:cl)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/page)
  (:import-from #:40ants-doc/builder/printer))
(in-package 40ants-doc/builder/bullet)


;;; PRINT REFERENCE to STREAM as:
;;;
;;;     - [locative-type] symbol
;;;
;;; When generating HTML, link SYMBOL to its own anchor.
(defun print-reference-bullet (reference stream &key name)
  (let ((locative-type (string-downcase
                        (40ants-doc/reference::reference-locative-type reference)))
        (name (or name (prin1-to-string (40ants-doc/reference::reference-object reference)))))
    (if 40ants-doc/builder/vars::*document-mark-up-signatures*
        ;; insert self links in HTML
        (let ((locative-type (40ants-doc/utils::escape-markdown locative-type))
              (name (40ants-doc/utils::escape-markdown name)))
          (if (eq 40ants-doc/builder/printer::*format* :html)
              (let ((source-uri (source-uri reference)))
                (format stream
                        "- <span class=reference-bullet>~
                           <span class=reference>~
                           <span class=\"locative-type\">~
                           ~@[<a href=\"~A\">~]\\[~A]~:[~;</a>~]~
                           </span> ~
                        <span class=\"reference-object\">[~A](#~A)</span>~
                        </span>"
                        source-uri locative-type source-uri name
                        (40ants-doc/utils::html-safe-name (40ants-doc/reference::reference-to-anchor reference))))
              (format stream "- [~A] ~A" locative-type (40ants-doc/utils::bold name nil))))
        (format stream "- [~A] ~A" locative-type name))))

(defun print-end-bullet (stream)
  (if (eq 40ants-doc/builder/printer::*format* :html)
      ;; end "reference-bullet" span
      (format stream "</span>~%")
      (format stream "~%")))

(defun source-uri (reference)
  (let ((fn (when 40ants-doc/page::*page*
              (40ants-doc/page::page-source-uri-fn 40ants-doc/page::*page*))))
    (if fn
        (funcall fn reference)
        nil)))

(defun locate-and-print-bullet (locative-type locative-args symbol stream
                                &key name)
  (let ((reference
          (40ants-doc/reference::canonical-reference (40ants-doc/reference::make-reference
                                                      symbol (cons locative-type locative-args)))))
    (print-reference-bullet reference stream :name name)))

(defun print-bullet (object stream)
  (print-reference-bullet (40ants-doc/reference::canonical-reference object) stream))

