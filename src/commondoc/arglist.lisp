(uiop:define-package #:40ants-doc/commondoc/arglist
  (:use #:cl)
  (:import-from #:common-doc)
  (:import-from #:common-html.emitter)
  (:import-from #:spinneret
                #:with-html))
(in-package 40ants-doc/commondoc/arglist)


(defclass arglist (common-doc:content-node)
  ((text :initarg :text
         :reader arglist-text)))


(defun make-arglist (text)
  (make-instance 'arglist :text text))


(common-html.emitter::define-emitter (obj arglist)
  "Emit an arglist."
  (let* ((spinneret:*html* common-html.emitter::*output-stream*))
    (with-html
      (:span :class "locative-args"
             (arglist-text obj)))))
