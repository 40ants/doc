(uiop:define-package #:40ants-doc/commondoc/builder
  (:use #:cl)
  (:import-from #:40ants-doc/commondoc/markdown
                #:parse-markdown)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/object-package)
  (:export
   #:to-commondoc
   #:reference-to-commondoc))
(in-package 40ants-doc/commondoc/builder)


(defgeneric to-commondoc (obj)
  (:documentation
   "Define methods for this generic function to render object's
    documentation into an intermediate CommonDoc format.

    Function should return a COMMON-DOC:DOCUMENT-NODE.

    To show a standard documentation item with locative,
    name and arguments, use 40ANTS-DOC/COMMONDOC/BULLET:MAKE-BULLET
    function.
   "))

(defgeneric reference-to-commondoc (obj locative locative-args))


(defmethod to-commondoc ((obj string))
  (parse-markdown
   (40ants-doc/utils::strip-docstring-indentation obj)))


(defmethod to-commondoc ((obj 40ants-doc/reference::reference))
  (let* ((resolved (40ants-doc/reference::resolve obj))
         (locative (40ants-doc/reference::reference-locative obj))
         (locative-name (etypecase locative
                          (list (car locative))
                          (symbol locative)))
         (locative-args (etypecase locative
                          (list (cdr locative))
                          (symbol nil))))
    (typecase resolved
      (40ants-doc/reference::reference
       (let* ((reference-obj (40ants-doc/reference::reference-object obj))
              (*package* (or (40ants-doc/object-package:object-package reference-obj)
                             *package*)))
         (reference-to-commondoc reference-obj
                                 locative-name
                                 locative-args)))
      (t (to-commondoc resolved)))))


(defmethod to-commondoc :around ((obj t))
  "This methods sets the *PACKAGE* because other TO-COMMONDOC methods might
   read symbols from docstrings, and they should be referenced
   against the package OBJ argument belongs to."
  (let ((*package* (or (40ants-doc/object-package:object-package obj)
                       *package*)))
    (call-next-method)))


(defmethod to-commondoc ((obj t))
  (parse-markdown (format nil "Don't know how to render `~S`. ~
                               Implement `TO-COMMONDOC` (`~S`) method."
                          obj
                          (type-of obj))))


(defmethod reference-to-commondoc ((obj t) (locative t) locative-args)
  (let ((locative-name (etypecase locative
                         (list (first locative))
                         (symbol locative))))
    (parse-markdown (format nil "Don't know how to render reference `~S` (`~S`). ~
                               Implement a `REFERENCE-TO-COMMONDOC` (`~S` `~S` T) method."
                            obj
                            locative
                            (type-of obj)
                            (list 'eql locative-name)))))
