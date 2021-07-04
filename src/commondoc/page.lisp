(uiop:define-package #:40ants-doc/commondoc/page
  (:use #:cl)
  (:import-from #:common-doc)
  (:import-from #:common-html.emitter)
  (:import-from #:40ants-doc/commondoc/html
                #:with-html)
  (:import-from #:common-html.emitter
                #:define-emitter)
  (:export
   #:ensure-page
   #:make-page))
(in-package 40ants-doc/commondoc/page)


(defclass page (common-doc:content-node)
  ())


(defun make-page (sections)
  (make-instance 'page
                 :children (uiop:ensure-list sections)))


(defun ensure-page (obj)
  (check-type obj common-doc:document-node)
  (typecase obj
    (page obj)
    (t (make-page obj))))


(define-emitter (obj page)
  "Emit an piece of documentation."
  (with-html
    (:html
     (:head
      (:meta :name "viewport"
             :content "width=device-width, initial-scale=1")
      (:title "Example page")
      (:link :rel "stylesheet"
             :type "text/css"
             :href "theme.css"))
     (:body
      (:div :id "content"
            (mapc #'common-html.emitter::emit
                  (common-doc::children obj)))))))
