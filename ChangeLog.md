<a id="x-2840ANTS-DOC-2FCHANGELOG-3A-40CHANGELOG-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# Changes

<a id="x-2840ANTS-DOC-2FCHANGELOG-3A-3A-7C0-2E2-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.2.0

* Now defsection does not exports symbols by default

* You can render documents in multiple formats in a single run having cross links.
  For example shorter `README`.md could mention symbols and have correct
  links to the full documentation

* "Clean" `URL`s are supported out of the box.

* Now defsection does not export nor mentioned symbols nor the name of the section
  It is better to have explicit exports.

* [`40ants-doc/locatives:include`](index.html#x-2840ANTS-DOC-2FLOCATIVES-3AINCLUDE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) locative now does not support `:HEADER`, `:FOOTER` and some other arguments. Use `:LANG` argument instead.

* Added code highlighting using Highlight.js library.

* Added search form which uses index in browser. `JS` code was taken from
  [Sphinx][140c] documentation builder.

* Elisp code for transcriptions was fixed and now should word not
  only with `SLIME`, but also with `SLY`.

* [`40ants-doc:defsection`](index.html#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29) macro now does not generate export code
  if `:EXPORT` argument is `NIL`.

* Functions `UPDATE-ASDF-SYSTEM-HTML-DOCS` and `UPDATE-ASDF-SYSTEM-README`
  were replaced with [`40ants-doc/builder:update-asdf-system-docs`](index.html#x-2840ANTS-DOC-2FBUILDER-3AUPDATE-ASDF-SYSTEM-DOCS-20FUNCTION-29), which also supports
  ChangeLog.md generation. Use [`40ants-doc/changelog:defchangelog`](index.html#x-2840ANTS-DOC-2FCHANGELOG-3ADEFCHANGELOG-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29) to define versions.

* Variables `*DOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL*` and
  `*DOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL*` were removed. Probably we'll return this
  feature back in other form, to restrict `TOC`'s size.

* Removed `LOCATE-AND-DOCUMENT` generic function.

* Links to the GitHub now are generated automatically,
  if [`40ants-doc/builder:update-asdf-system-docs`](index.html#x-2840ANTS-DOC-2FBUILDER-3AUPDATE-ASDF-SYSTEM-DOCS-20FUNCTION-29) function is used
  and system definition has a `:SOURCE-CONTROL` slot.

* Generic functions `COLLECT-REACHABLE-OBJECTS` and `LOCATE-AND-COLLECT-REACHABLE-OBJECTS`
  were removed.

* Variables `*DOCUMENT-LINK-SECTIONS*`, `*DOCUMENT-TEXT-NAVIGATION*` and `*DOCUMENT-FANCY-HTML-NAVIGATION*` were removed.

* Variable `*DOCUMENT-MIN-LINK-HASH-LENGTH*` was removed. Use `COMMONDOC-MARKDOWN:*MIN-LINK-HASH-LENGTH*`

* Functions [`40ants-doc/builder:update-asdf-system-docs`](index.html#x-2840ANTS-DOC-2FBUILDER-3AUPDATE-ASDF-SYSTEM-DOCS-20FUNCTION-29) and [`40ants-doc/builder:render-to-files`](index.html#x-2840ANTS-DOC-2FBUILDER-3ARENDER-TO-FILES-20FUNCTION-29)
  now accept `WARN-ON-UNDOCUMENTED-PACKAGES` argument and `CLEAN-URLS` argument.

* Variable `*DOCUMENT-MARK-UP-SIGNATURES*` was removed.

* Added `DOWNCASE-UPPERCASE-CODE` argument instead of `*DOCUMENT-DOWNCASE-UPPERCASE-CODE*`.
  This argument is true by default.

<a id="x-2840ANTS-DOC-2FCHANGELOG-3A-3A-7C0-2E1-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.1.0

* Project forked from [`MGL-PAX`][7927].
  Code refactored into the package inferred system and core is separated
  to have minimum dependencies.

* Fixed displaying docstring for constant locative.

* Include locative was fixed for files with unicode characters
  file-subseq function was rewritten.

* Locatives can be specified without a package prefix inside the defsection
  because all locative symbols now live in [`40ants-doc/locatives`](index.html#x-28-23A-28-2820-29-20BASE-CHAR-20-2E-20-2240ANTS-DOC-2FLOCATIVES-22-29-20PACKAGE-29) package.

* Function update-asdf-system-readmes was renamed to update-asdf-system-readmes and now
  it generates only one `README` file.

* Tests were rewritten to use Rove and to support `(asdf:test-system :40ants-doc)`.

* Removed `MGL-PAX:DEFINE-PACKAGE` macro. An `UIOP:DEFINE-PACKAGE` can be used instead.

* Now builder issues a warning if it wasn't able to find a symbol mentioned in the docstring.

* Uppercase word should have at least two charaters to be resolved as a symbol.

* Improved work with package inferred systems. For examples, when fixed the
  automatic symbol rendering for case when documentation section and
  referenced objects are in different packages.

* Allowed to reference objects using keywords.

* Fixed docstring extraction for compiler macro.


[140c]: https://www.sphinx-doc.org/
[7927]: https://github.com/melisgl/mgl-pax