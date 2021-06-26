(defpackage #:40ants-doc/commondoc/builder
  (:use #:cl)
  (:export
   #:to-commondoc))
(in-package 40ants-doc/commondoc/builder)


(defgeneric to-commondoc (obj))

(defgeneric reference-to-commondoc (obj locative))


(defun parse-markdown (text)
  (common-doc.format:parse-document (make-instance 'commondoc-markdown:markdown)
                                    text))

(defun make-section-body (section)
  (loop for entry in (40ants-doc:section-entries section)
        collect (to-commondoc entry)))


(defmethod to-commondoc ((obj 40ants-doc:section))
  (common-doc:make-section (40ants-doc:section-title obj)
                           :children (make-section-body obj)))

(defmethod to-commondoc ((obj string))
  (parse-markdown obj))


(defmethod to-commondoc ((obj 40ants-doc/reference::reference))
  (let ((resolved (40ants-doc/reference::resolve obj)))
    (typecase resolved
      (40ants-doc/reference::reference
       (reference-to-commondoc (40ants-doc/reference::reference-object obj)
                               (40ants-doc/reference::reference-locative obj)))
      (t (to-commondoc resolved)))))


(defmethod to-commondoc ((obj t))
  (parse-markdown (format nil "Don't know how to render `~S`. ~
                               Implement `TO-COMMONDOC` (`~S`) method."
                          obj
                          (type-of obj))))


(defmethod reference-to-commondoc ((obj t) (locative t))
  (let ((locative-name (etypecase locative
                         (list (first locative))
                         (symbol locative))))
    (parse-markdown (format nil "Don't know how to render reference `~S` (`~S`). ~
                               Implement a `REFERENCE-TO-COMMONDOC` (`~S` `~S`) method."
                            obj
                            locative
                            (type-of obj)
                            (list 'eql locative-name)))))
