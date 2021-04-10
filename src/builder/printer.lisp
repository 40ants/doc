(uiop:define-package #:40ants-doc/builder/printer
  (:use #:cl)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/core
                #:defsection))
(in-package 40ants-doc/builder/printer)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(defvar *document-normalize-packages* t
  "If true, symbols are printed relative to `40ANTS-DOC::SECTION-PACKAGE` of the
  innermost containing section or with full package names if there is
  no containing section. To eliminate ambiguity `[in package ...]`
  messages are printed right after the section heading if necessary.
  If false, symbols are always printed relative to the current
  package.")


(defvar *document-uppercase-is-code* t
  """When true, words with at least three characters and no lowercase
  characters naming an interned symbol are assumed to be code as if
  they were marked up with backticks which is especially useful when
  combined with *DOCUMENT-LINK-CODE*. For example, this docstring:

      "`FOO` and FOO."

  is equivalent to this:

      "`FOO` and `FOO`."

  if `FOO` is an interned symbol. To suppress this behavior, add a
  backslash to the beginning of the symbol or right after the leading
  * if it would otherwise be parsed as markdown emphasis:

      "\\40ANTS-DOC *\\DOCUMENT-NORMALIZE-PACKAGES*"

  The number of backslashes is doubled above because that's how the
  example looks in a docstring. Note that the backslash is discarded
  even if *DOCUMENT-UPPERCASE-IS-CODE* is false.""")

(defvar *document-downcase-uppercase-code* nil
  "If true, then the names of symbols recognized as code (including
  those found if *DOCUMENT-UPPERCASE-IS-CODE*) are downcased in the
  output if they only consist of uppercase characters. If it is
  :ONLY-IN-MARKUP, then if the output format does not support
  markup (e.g. it's :PLAIN), then no downcasing is performed.")


;;; Bound by DOCUMENT, this allows markdown output to depend on the
;;; output format.
(defvar *format*)


(defun maybe-downcase (string)
  (if (and (or (and *document-downcase-uppercase-code*
                    (not (eq *document-downcase-uppercase-code*
                             :only-in-markup)))
               (and (eq *document-downcase-uppercase-code*
                        :only-in-markup)
                    (not (eq *format* :plain))))
           (40ants-doc/utils::no-lowercase-chars-p string))
      (string-downcase string)
      string))



