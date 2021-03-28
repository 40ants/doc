(defpackage #:40ants-doc/builder/heading-api
  (:use #:cl))
(in-package 40ants-doc/builder/heading-api)


(defgeneric fancy-navigation (object)
  (:method ((object t))))
