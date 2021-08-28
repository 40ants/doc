(defpackage #:40ants-doc/commondoc/html
  (:use #:cl)
  (:import-from #:spinneret)
  (:import-from #:common-html.emitter)
  (:export
   #:with-html))
(in-package 40ants-doc/commondoc/html)


(defmacro with-html (&body body)
  `(let ((spinneret:*html* common-html.emitter::*output-stream*))
     (spinneret:with-html
       ,@body)))
