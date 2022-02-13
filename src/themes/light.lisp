(defpackage #:40ants-doc/themes/light
  (:use #:cl)
  (:import-from #:40ants-doc/themes/default
                #:default-theme)
  (:import-from #:lass)
  (:import-from #:40ants-doc/themes/api)
  (:export #:light-theme))
(in-package #:40ants-doc/themes/light)


(defclass light-theme (default-theme)
  ())

(defmethod 40ants-doc/themes/api:render-css ((theme light-theme))
  (let ((background "#FFFEFB")
        (font-color "#333"))
    (concatenate
     'string
     (call-next-method)
     
     (lass:compile-and-write
      `(body
        :color ,font-color

        ((:or h1 h2 h3 h4 h5 h6)
         :color ,font-color
         :border-bottom none)
        
        (.sidebar
         :background ,background
         :box-shadow inset -3px 0 3px 0px "#777"
         (.page-toc
          (a
           :color "#333"))
         (.toc-active 
          :background ,font-color
          )
         ((.toc-active > a)
          :color ,background)))))))

(defmethod 40ants-doc/themes/api:highlight-theme ((theme light-theme))
  "atom-one-light")

