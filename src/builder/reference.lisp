(defpackage #:40ants-doc/builder/reference
  (:use #:cl)
  (:import-from #:40ants-doc/document)
  (:import-from #:40ants-doc/locatives/base)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/reference-api))
(in-package 40ants-doc/builder/reference)


(defmethod 40ants-doc/document::document-object ((reference 40ants-doc/reference::reference) stream)
  "If REFERENCE can be resolved to a non-reference, call
  40ANTS-DOC/DOCUMENT::DOCUMENT-OBJECT with it, else call 40ANTS-DOC/LOCATIVES/BASE::LOCATE-AND-DOCUMENT on the
  object, locative-type, locative-args of REFERENCE"
  (let* ((reference (40ants-doc/reference-api::canonical-reference reference))
         (resolved-object (40ants-doc/reference::resolve reference)))
    (if (typep resolved-object '40ants-doc/reference::reference)
        (40ants-doc/page::with-temp-output-to-page (stream (40ants-doc/link::reference-page reference))
          (when 40ants-doc/link::*document-link-code*
            (40ants-doc/utils::anchor (40ants-doc/reference::reference-to-anchor reference) stream))
          (let ((locative (40ants-doc/reference::reference-locative reference)))
            (40ants-doc/locatives/base::locate-and-document (40ants-doc/reference::reference-object reference)
                                                            (40ants-doc/locatives/base::locative-type locative)
                                                            (40ants-doc/locatives/base::locative-args locative)
                                 stream)))
        (40ants-doc/document::document-object resolved-object stream))))
