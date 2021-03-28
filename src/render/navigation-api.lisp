(defpackage #:40ants-doc/render/navigation-api
  (:use #:cl))
(in-package 40ants-doc/render/navigation-api)


(defgeneric navigation-link (object stream))
