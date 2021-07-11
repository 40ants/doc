(uiop:define-package #:40ants-doc/ignored-words
  (:use #:cl)
  (:export
   #:ignored-words
   #:supports-ignored-words-p))
(in-package 40ants-doc/ignored-words)


(defgeneric supports-ignored-words-p (obj)
  (:method ((obj t))
    nil))

(defgeneric ignored-words (obj))
