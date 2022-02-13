(defpackage #:40ants-doc/commondoc/html
  (:use #:cl)
  (:import-from #:spinneret)
  (:import-from #:common-html.emitter)
  (:export #:with-html))
(in-package #:40ants-doc/commondoc/html)


(defmacro with-html (&body body)
  "Use this macro to render HTML inside generic-functions,
   listed in the 40ANTS-DOC/THEMES/DOCS::@DEFINING-A-THEME section."
  `(let ((spinneret:*html* common-html.emitter::*output-stream*))
     (spinneret:with-html
       ,@body)))
