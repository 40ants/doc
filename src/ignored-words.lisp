(uiop:define-package #:40ants-doc/ignored-words
  (:use #:cl)
  (:export
   #:ignored-words
   #:supports-ignored-words-p
   #:ignore-words-in-package
   #:ignored-in-package))
(in-package 40ants-doc/ignored-words)


(defvar *package-ignore-words*
  (make-hash-table))


(defgeneric supports-ignored-words-p (obj)
  (:method ((obj t))
    nil)
  (:method ((obj package))
    (gethash obj *package-ignore-words*)))


(defgeneric ignored-words (obj)
  (:method ((obj package))
    (gethash obj *package-ignore-words*)))


(defmacro ignore-words-in-package (&rest symbols-or-strings)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash *package* *package-ignore-words*)
           (list ,@symbols-or-strings))))


(defun ignored-in-package (symbol-or-string package)
  (member symbol-or-string
          (gethash package *package-ignore-words*)
          :test #'equal))
