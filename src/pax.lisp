(in-package :mgl-pax)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(defsection @mgl-pax-basics (:title "Basics")
  "Now let's examine the most important pieces in detail."
  (defsection macro)
  (*discard-documentation-p* variable)
  (define-package macro)
  (document function))


(defsection @mgl-pax-locative-types (:title "Locative Types")
  "These are the locatives type supported out of the box. As all
  locative types, they are symbols and their names should make it
  obvious what kind of things they refer to. Unless otherwise noted,
  locatives take no arguments."
  (asdf:system locative)
  (section locative)
  (variable locative)
  (constant locative)
  (macro locative)
  (compiler-macro locative)
  (function locative)
  (generic-function locative)
  (method locative)
  (accessor locative)
  (reader locative)
  (writer locative)
  (structure-accessor locative)
  (class locative)
  (condition locative)
  (restart locative)
  (define-restart macro)
  (type locative)
  (package locative)
  (dislocated locative)
  (argument locative)
  (locative locative)
  (glossary-term locative)
  (define-glossary-term macro)
  (include locative))


(defsection @mgl-pax-extension-api (:title "Extension API")
  (@mgl-pax-locatives-and-references section)
  (@mgl-pax-new-object-types section)
  (@mgl-pax-reference-based-extensions section)
  (@mgl-pax-sections section))


(defsection @mgl-pax-reference-based-extensions
    (:title "Reference Based Extensions")
  "Let's see how to extend DOCUMENT and `M-.` navigation if there is
  no first class object to represent the thing of interest. Recall
  that LOCATE returns a REFERENCE object in this case. DOCUMENT-OBJECT
  and FIND-SOURCE defer to LOCATE-AND-DOCUMENT and
  LOCATE-AND-FIND-SOURCE, which have LOCATIVE-TYPE in their argument
  list for EQL specializing pleasure. Here is a stripped down example
  of how the VARIABLE locative is defined:"
  (variable-example (include (:start (variable locative)
                                     :end (end-of-variable-example variable))
                             :header-nl "```commonlisp"
                             :footer-nl "```"))
  (collect-reachable-objects (method () (reference)))
  (locate-and-collect-reachable-objects generic-function)
  (locate-and-collect-reachable-objects (method () (t t t)))
  (document-object (method () (reference t)))
  (locate-and-document generic-function)
  (find-source (method () (reference)))
  (locate-and-find-source generic-function)
  (locate-and-find-source (method () (t t t)))
  "We have covered the basic building blocks of reference based
  extensions. Now let's see how the obscure
  DEFINE-SYMBOL-LOCATIVE-TYPE and
  DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE macros work together to
  simplify the common task of associating definition and documentation
  with symbols in a certain context."
  (define-symbol-locative-type macro)
  (define-definer-for-symbol-locative-type macro))






(defsection @mgl-pax-sections (:title "Sections")
  "[Section][class] objects rarely need to be dissected since
  DEFSECTION and DOCUMENT cover most needs. However, it is plausible
  that one wants to subclass them and maybe redefine how they are
  presented."
  (section class)
  (section-name (reader section))
  (section-package (reader section))
  (section-readtable (reader section))
  (section-title (reader section))
  (section-link-title-to (reader section))
  (section-entries (reader section))
  (describe-object (method () (section t))))



(defsection @mgl-pax-new-object-types (:title "Adding New Object Types")
  "One may wish to make the DOCUMENT function and `M-.` navigation
  work with new object types. Extending DOCUMENT can be done by
  defining a DOCUMENT-OBJECT method. To allow these objects to be
  referenced from DEFSECTION, a LOCATE-OBJECT method is to be defined.
  Finally, for `M-.` FIND-SOURCE can be specialized. Finally,
  EXPORTABLE-LOCATIVE-TYPE-P may be overridden if exporting does not
  makes sense. Here is a stripped down example of how all this is done
  for ASDF:SYSTEM:"
  (asdf-example (include (:start (asdf:system locative)
                          :end (end-of-asdf-example variable))
                         :header-nl "```commonlisp"
                         :footer-nl "```"))
  (define-locative-type macro)
  (exportable-locative-type-p generic-function)
  (locate-object generic-function)
  (locate-error function)
  (canonical-reference generic-function)
  (collect-reachable-objects generic-function)
  (collect-reachable-objects (method () (t)))
  (document-object generic-function)
  (document-object (method () (string t)))
  (find-source generic-function))
