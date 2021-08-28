(uiop:define-package #:40ants-doc/themes/api
  (:use #:cl)
  (:export
   #:render-css))
(in-package 40ants-doc/themes/api)


(defgeneric render-css (theme)
  (:documentation "Returns a string with CSS."))
