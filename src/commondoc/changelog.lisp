(defpackage #:40ants-doc/commondoc/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/commondoc/section
                #:documentation-section
                #:make-documentation-section)
  (:import-from #:40ants-doc/changelog))
(in-package 40ants-doc/commondoc/changelog)


(defclass changelog (documentation-section)
  ())

(defclass version (documentation-section)
  ())

(defmethod 40ants-doc/commondoc/builder:to-commondoc ((obj 40ants-doc/changelog::changelog))
  (make-documentation-section obj :class-name 'changelog))

(defmethod 40ants-doc/commondoc/builder:to-commondoc ((obj 40ants-doc/changelog::version))
  (make-documentation-section obj :class-name 'version))
