(defpackage #:40ants-doc/changelog
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:pythonic-string-reader)
  (:import-from #:named-readtables)
  (:export
   #:@changelog
   #:defchangelog))
(in-package 40ants-doc/changelog)


(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(defsection @index (:title "Changelog Generation")
  (defchangelog macro))


(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun make-version-section (version content)
   `(defsection ,version (:title ,(symbol-name version))
      ,@content)))


(defmacro defchangelog ((&key (title "Changes")
                              ignore-words)
                        &body versions)
  """
  This macro might be used to define a ChangeLog in a structured way.
  With DEFCHANGELOG you specify a body where each sublist starts with
  a version number and the rest is it's description in the markdown
  format. You can mention symbols from the rest of the documentation
  and they will be cross-linked automatically if you are using
  40ANTS-DOC/BUILDER:UPDATE-ASDF-SYSTEM-DOCS function.

  Here is an example:

  ```lisp
  (defchangelog ()
    (0.2.0
     "- Feature B implemented.
    - Bug was fixed in function FOO.")
    
    (0.1.0
     "- Project forked from [MGL-PAX](https://github.com/melisgl/mgl-pax).
    - Feature A implemented."))
  ```
  """
  (let ((section-name (intern "@CHANGELOG")))
    `(progn
       ;; Symbol should be exported, to allow DOCS-BUILDER
       ;; to discover a changelog.
       (export ',section-name)
      
       (defsection ,section-name (:title ,title
                               :ignore-words (list ,@ignore-words))
         ,@(loop for (version) in versions
                 collect `(,version section)))
       ,@(loop for (version . content) in versions
               collect (make-version-section version content)))))


(defchangelog (:ignore-words ("MGL-PAX"
                              "UPDATE-ASDF-SYSTEM-HTML-DOCS"
                              "UPDATE-ASDF-SYSTEM-README"
                              "README"
                              "HTML"
                              "URL"
                              "JS"
                              "MGL-PAX:DEFINE-PACKAGE"
                              "UIOP:DEFINE-PACKAGE"
                              "*DOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL*"
                              "*DOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL*"
                              "*DOCUMENT-LINK-SECTIONS*"
                              "*DOCUMENT-TEXT-NAVIGATION*"
                              "*DOCUMENT-FANCY-HTML-NAVIGATION*"
                              "LOCATE-AND-DOCUMENT"
                              "TOC"
                              "SLIME"
                              "SLY"
                              "COLLECT-REACHABLE-OBJECTS"
                              "LOCATE-AND-COLLECT-REACHABLE-OBJECTS"
                              "COMMONDOC-MARKDOWN:*MIN-LINK-HASH-LENGTH*"
                              "*DOCUMENT-MIN-LINK-HASH-LENGTH*"
                              "*DOCUMENT-MARK-UP-SIGNATURES*"
                              "*DOCUMENT-NORMALIZE-PACKAGES*"
                              "*DOCUMENT-DOWNCASE-UPPERCASE-CODE*"))
  (0.4.0 ;; 2021-09-05
   "- *DOCUMENT-NORMALIZE-PACKAGES* variable was replaced with FULL-PACKAGE-NAMES argument
      of 40ANTS-DOC/BUILDER:RENDER-TO-FILES function.")
  (0.3.0 ;; 2021-09-04
   "- Now images are copied to target folder together with HTML documentation
      and links are adjusted accordingly.
    - Added a protocol to define new color themes and change page layout.
      Three new themes are available out of the box.
      Read more at 40ANTS-DOC/THEMES/DOCS::@DEFINING-A-THEME section.")
  (0.2.0
   "- Now defsection does not exports symbols by default
    - You can render documents in multiple formats in a single run having cross links.
      For example shorter README.md could mention symbols and have correct
      links to the full documentation
    - \"Clean\" URLs are supported out of the box.
    - Now defsection does not export nor mentioned symbols nor the name of the section
      It is better to have explicit exports.
    - 40ANTS-DOC/LOCATIVES:INCLUDE locative now does not support :HEADER, :FOOTER and some other arguments. Use :LANG argument instead.
    - Added code highlighting using Highlight.js library.
    - Added search form which uses index in browser. JS code was taken from
      [Sphinx](https://www.sphinx-doc.org/) documentation builder.
    - Elisp code for transcriptions was fixed and now should word not
      only with SLIME, but also with SLY.
    - 40ANTS-DOC:DEFSECTION macro now does not generate export code
      if :EXPORT argument is NIL.
    - Functions UPDATE-ASDF-SYSTEM-HTML-DOCS and UPDATE-ASDF-SYSTEM-README
      were replaced with 40ANTS-DOC/BUILDER:UPDATE-ASDF-SYSTEM-DOCS, which also supports
      ChangeLog.md generation. Use 40ANTS-DOC/CHANGELOG:DEFCHANGELOG to define versions.
    - Variables *DOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL* and
      *DOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL* were removed. Probably we'll return this
      feature back in other form, to restrict TOC's size.
    - Removed LOCATE-AND-DOCUMENT generic function.
    - Links to the GitHub now are generated automatically,
      if 40ANTS-DOC/BUILDER:UPDATE-ASDF-SYSTEM-DOCS function is used
      and system definition has a :SOURCE-CONTROL slot.
    - Generic functions COLLECT-REACHABLE-OBJECTS and LOCATE-AND-COLLECT-REACHABLE-OBJECTS
      were removed.
    - Variables *DOCUMENT-LINK-SECTIONS*, *DOCUMENT-TEXT-NAVIGATION* and *DOCUMENT-FANCY-HTML-NAVIGATION* were removed.
    - Variable *DOCUMENT-MIN-LINK-HASH-LENGTH* was removed. Use COMMONDOC-MARKDOWN:*MIN-LINK-HASH-LENGTH*
    - Functions 40ANTS-DOC/BUILDER:UPDATE-ASDF-SYSTEM-DOCS and 40ANTS-DOC/BUILDER:RENDER-TO-FILES
      now accept WARN-ON-UNDOCUMENTED-PACKAGES argument and CLEAN-URLS argument.
    - Variable *DOCUMENT-MARK-UP-SIGNATURES* was removed.
    - Added DOWNCASE-UPPERCASE-CODE argument instead of *DOCUMENT-DOWNCASE-UPPERCASE-CODE*.
      This argument is true by default.
    - Added warnings on symbols, referenced like internals, using `::`.
    - Added 40ANTS-DOC:DEFSECTION-COPY macro to define copy of the section but with a different name.")
  
  (0.1.0
   "- Project forked from [MGL-PAX](https://github.com/melisgl/mgl-pax).
      Code refactored into the package inferred system and core is separated
      to have minimum dependencies.
    - Fixed displaying docstring for constant locative.
    - Include locative was fixed for files with unicode characters
      file-subseq function was rewritten.
    - Locatives can be specified without a package prefix inside the defsection
      because all locative symbols now live in [40ANTS-DOC/LOCATIVES][package] package.
    - Function update-asdf-system-readmes was renamed to update-asdf-system-readmes and now
      it generates only one README file.
    - Tests were rewritten to use Rove and to support `(asdf:test-system :40ants-doc)`.
    - Removed MGL-PAX:DEFINE-PACKAGE macro. An UIOP:DEFINE-PACKAGE can be used instead.
    - Now builder issues a warning if it wasn't able to find a symbol mentioned in the docstring.
    - Uppercase word should have at least two charaters to be resolved as a symbol.
    - Improved work with package inferred systems. For examples, when fixed the
      automatic symbol rendering for case when documentation section and
      referenced objects are in different packages.
    - Allowed to reference objects using keywords.
    - Fixed docstring extraction for compiler macro."))


