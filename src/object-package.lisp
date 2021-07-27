(defpackage #:40ants-doc/object-package
  (:use #:cl)
  (:export
   #:object-package))
(in-package 40ants-doc/object-package)


(defgeneric object-package (object)
  (:method ((object t))
    (warn "Unable to figure out *package* for object ~S"
          object)
    nil))
