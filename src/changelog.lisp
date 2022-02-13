(defpackage #:40ants-doc/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/core
                #:defsection)
  (:import-from #:pythonic-string-reader)
  (:import-from #:named-readtables)
  (:export #:@changelog
           #:defchangelog))
(in-package #:40ants-doc/changelog)


(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(defsection @index (:title "Changelog Generation")
  (defchangelog macro))


(defclass changelog (40ants-doc:section)
  ())

(defclass version (40ants-doc:section)
  ((date :initform nil
         :initarg :date
         :type (or null string)
         :documentation "This slot will contain a date in it's unparsed form, as a string.
                         Because we don't want to introduce dependency from LOCAL-TIME system for
                         changelog definition. This value will be parsed later, when we'll generate output."
         :accessor version-date)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun looks-like-date (item)
    (typecase item
      (symbol (looks-like-date (symbol-name item)))
      (string (and (= (length item) 10)
                   (digit-char-p (elt item 0))
                   (digit-char-p (elt item 1))
                   (digit-char-p (elt item 2))
                   (digit-char-p (elt item 3))
                   (char= (elt item 4) #\-)
                   (digit-char-p (elt item 5))
                   (digit-char-p (elt item 6))
                   (char= (elt item 7) #\-)
                   (digit-char-p (elt item 8))
                   (digit-char-p (elt item 9))))))
  
  (defun split-date-if-given (content)
    "Receives a list of entities and if first entity is a date in format 2021-09-24,
   then this date is removed from the content and returned as the first value.
   Rest entries are returned as the second value."
    (let ((first (first content)))
      (cond 
        ((looks-like-date first)
         (values (typecase first
                   (symbol (symbol-name first))
                   (t first))
                 (cdr content)))
        (t
         (values nil
                 content))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-version-section (version content external-docs external-links)
    (multiple-value-bind (date content)
        (split-date-if-given content)
      `(progn
         (defsection ,version (:title ,(symbol-name version)
                               :section-class version
                               :external-docs ,external-docs
                               :external-links ,external-links)
           ,@content)
         (setf (version-date ,version)
               ,date)))))


(defmacro defchangelog ((&key (title "ChangeLog")
                           ignore-words
                           external-docs
                           external-links)
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
                                  :ignore-words (list ,@ignore-words)
                                  :section-class changelog
                                  :external-docs ,external-docs
                                  :external-links ,external-links)
         ,@(loop for (version) in versions
                 collect `(,version section)))
       ,@(loop for (version . content) in versions
               collect (make-version-section version
                                             content
                                             external-docs
                                             external-links)))))


(defchangelog (:ignore-words ("MGL-PAX"
                              "UPDATE-ASDF-SYSTEM-HTML-DOCS"
                              "UPDATE-ASDF-SYSTEM-README"
                              "README"
                              "HTML"
                              "ASDF"
                              "RSS"
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
                              "COMMONDOC:SECTION"
                              "COLLECT-REACHABLE-OBJECTS"
                              "LOCATE-AND-COLLECT-REACHABLE-OBJECTS"
                              "COMMONDOC-MARKDOWN:*MIN-LINK-HASH-LENGTH*"
                              "*DOCUMENT-MIN-LINK-HASH-LENGTH*"
                              "*DOCUMENT-MARK-UP-SIGNATURES*"
                              "*DOCUMENT-NORMALIZE-PACKAGES*"
                              "*DOCUMENT-DOWNCASE-UPPERCASE-CODE*"
                              ;; These objects are not documented yet:
                              "40ANTS-DOC/COMMONDOC/XREF:XREF"))
  (0.8.0 2022-02-14
         "* Now 40ANTS-DOC/COMMONDOC/MAPPER:MAP-NODES generic-function
            supports any node type which defines a method for
            generic-function 40ANTS-DOC/COMMONDOC/MAPPER:NODE-SUPPORTS-CHILDREN.
          * Default theme was fixed to work with latest Spinneret, which now
            escapes single quotes inside HTML nodes.")
  (0.7.0 2021-12-31
         "* 40ANTS/CHANGELOG:DEFCHANGELOG now supports EXTERNAL-LINKS argument.
          * Automatic symbol extraction now ignores dates like 2021-12-31. Now
            to make it work, the symbol should contain at least one alpha character.")
  (0.6.0 2021-12-05
         "* Fixed the issue, when we tried to find uppercased xrefs inside inline code and links.
          * Added EXTERNAL-LINKS argument to DEFSECTION macro. It can be useful, if you have a multiple
            text sections having the same external link.
          * Now it is possible to use 40ANTS-DOC/IGNORED-WORDS:IGNORE-WORDS-IN-PACKAGE
            to suppress warning on symbols which are exported but not documented.")
  (0.5.8 2021-10-23
         "* Fixed the way of how a TOC is built. Previosly it incorrectly shown nested COMMONDOC:SECTION objects.")
  (0.5.7 2021-10-21
         "* DEFSECTION macro now supports EXTERNAL-DOCS argument.")
  (0.5.6 2021-10-21
         "* Blockquotes markup is supported now.
          * Now external references are rendered as code and downcased according to the settings.")
  (0.5.5 2021-09-26
         "Warning messages like:

          ```
          Unable to find target for reference #<XREF 40ANTS-DOC/COMMONDOC/XREF:XREF> mentioned at ChangeLog / 0.5.3  (2021-09-08)
          ```

          were rewritten to be more actionable:

          ```
          Object referenced as #<XREF 40ANTS-DOC/COMMONDOC/XREF:XREF> in ChangeLog / 0.5.3  (2021-09-08) is not documented.
          ```
          ")
  (0.5.4 2021-09-11
         "- Included changelog.lisp into the 40ANTS-DOC system.")
  (0.5.3 2021-09-08
         "- Fixed locatives parsing in case if there are more than one 40ANTS-DOC/COMMONDOC/XREF:XREF in the text.")
  (0.5.2 2021-09-08
         "- Now default theme removes underline from images nested into the `<a>` HTML tag.
          - Fixed images collection for case when current directory is different from the
            ASDF system's directory.")
  (0.5.1 2021-09-07
         "- Fixed reference index generation and comparison of usual references with references where locative is a string.")
  (0.5.0 2021-09-06
         "- Now 40ANTS-DOC:DEFSECTION macro accepts EXTERNAL-DOCS argument.
            Together with HTML pages, `references.json` file is rendered, and you can
            provide a list of urls of external libraries' documentation to have an automatic
            cross-referencing between them.")
  (0.4.1 2021-09-05
         "- Function 40ANTS-DOC/BUILDER:GET-CURRENT-ASDF-SYSTEM was added. Now you can use it to do something interesting
            like showing \"Fork me on the GitHub\" stripe [as my own theme do][commit].
          - Markdown files now will have a footer saying that a file was generated by 40ANTS-DOC.
          - An RSS feed is generated for changelog. Also, changelog items can have a date now.

          [commit]: https://github.com/40ants/40ants-doc-theme-40ants/commit/917a4c1e72b0379f509bdee4864531e641c9ec4e#diff-47d16baea2d4ef710747f19c24df8cf7ef4f6bbbfd1dbb0ade55f47457b1e8feR155-R161")
  (0.4.0 2021-09-05
         "- *DOCUMENT-NORMALIZE-PACKAGES* variable was replaced with FULL-PACKAGE-NAMES argument
      of 40ANTS-DOC/BUILDER:RENDER-TO-FILES function.")
  (0.3.0 2021-09-04
         "- Now images are copied to target folder together with HTML documentation
      and links are adjusted accordingly.
    - Added a protocol to define new color themes and change page layout.
      Three new themes are available out of the box.
      Read more at 40ANTS-DOC/THEMES/DOCS::@DEFINING-A-THEME section.")
  (0.2.0 2021-09-01
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
  
  (0.1.0 2021-05-01
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


