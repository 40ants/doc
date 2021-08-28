(uiop:define-package #:40ants-doc/builder/vars
  (:use #:cl)
  (:export #:*document-max-numbering-level*))
(in-package 40ants-doc/builder/vars)


(defvar *document-max-numbering-level* 3
  "A non-negative integer. In their hierarchy, sections on levels less
  than this value get numbered in the format of `3.1.2`. Setting it to
  0 turns numbering off.

  **Is not supported yet.**")


(defvar *find-definitions-right-trim* ",:.>")
(defparameter *find-definitions-right-trim-2* ",:.>sS")


(defvar *downcase-uppercase-code* t
  "If true, then the names of symbols recognized as code (including
  those found if *DOCUMENT-UPPERCASE-IS-CODE*) are downcased in the
  output if they only consist of uppercase characters.")
