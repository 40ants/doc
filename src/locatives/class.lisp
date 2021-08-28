(uiop:define-package #:40ants-doc/locatives/class
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc/render/args)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/args)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/builder/vars)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/page)
  (:import-from #:40ants-doc/commondoc/bullet)
  (:import-from #:40ants-doc/commondoc/arglist)
  (:import-from #:40ants-doc/docstring)
  (:import-from #:40ants-doc/commondoc/markdown))
(in-package 40ants-doc/locatives/class)

(define-locative-type class ())

(define-locative-type condition ())

(defmethod locate-object (symbol (locative-type (eql 'class)) locative-args)
  (declare (ignore locative-args))
  (or (find-class symbol :errorp nil)
      (locate-error)))

(defmethod locate-object (symbol (locative-type (eql 'condition))
                          locative-args)
  (assert (= 0 (length locative-args)))
  (let ((class (find-class symbol :errorp nil)))
    (unless (subtypep class 'condition)
      (locate-error))
    class))


(defmethod canonical-reference ((class class))
  (if (subtypep class 'condition)
      (40ants-doc/reference:make-reference (class-name class) 'condition)
      (40ants-doc/reference:make-reference (class-name class) 'class)))


(defmethod 40ants-doc/commondoc/builder:to-commondoc ((class class))
  (let* ((conditionp (subtypep class 'condition))
         (symbol (class-name class))
         (superclasses
           (remove-if (lambda (name)
                        (or (eq name 'standard-object)
                            (and conditionp (eq name 'condition))))
                      (mapcar #'class-name
                              (swank-mop:class-direct-superclasses class))))
         (docstring (40ants-doc/docstring:get-docstring class t))
         (children (when docstring
                     (40ants-doc/commondoc/markdown:parse-markdown docstring))))

    (40ants-doc/commondoc/bullet:make-bullet (canonical-reference class)
                                             ;; TODO: transform superclasses to XREFs
                                             :arglist (40ants-doc/commondoc/arglist::make-arglist superclasses)
                                             :children children
                                             :ignore-words symbol)))

(defun find-known-reference (reference)
  (find reference 40ants-doc/reference::*references* :test #'40ants-doc/reference::reference=))
