<a id="x-2840ANTS-DOC-2FCHANGELOG-3A-40CHANGELOG-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# Changes

<a id="x-2840ANTS-DOC-2FCHANGELOG-3A-3A-7C0-2E4-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.4.0

* `*DOCUMENT-NORMALIZE-PACKAGES*` variable was replaced with `FULL-PACKAGE-NAMES` argument
of [`40ants-doc/builder:render-to-files`][05c0] function.

<a id="x-2840ANTS-DOC-2FCHANGELOG-3A-3A-7C0-2E3-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.3.0

* Now images are copied to target folder together with `HTML` documentation
  and links are adjusted accordingly.

* Added a protocol to define new color themes and change page layout.
  Three new themes are available out of the box.
  Read more at [`Defining a Custom Theme`][bcaa] section.

<a id="x-2840ANTS-DOC-2FCHANGELOG-3A-3A-7C0-2E2-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.2.0

* Now defsection does not exports symbols by default

* You can render documents in multiple formats in a single run having cross links.
  For example shorter `README`.md could mention symbols and have correct
  links to the full documentation

* "Clean" `URL`s are supported out of the box.

* Now defsection does not export nor mentioned symbols nor the name of the section
  It is better to have explicit exports.

* [`40ants-doc/locatives:include`][359f] locative now does not support `:HEADER`, `:FOOTER` and some other arguments. Use `:LANG` argument instead.

* Added code highlighting using Highlight.js library.

* Added search form which uses index in browser. `JS` code was taken from
  [Sphinx][140c] documentation builder.

* Elisp code for transcriptions was fixed and now should word not
  only with `SLIME`, but also with `SLY`.

* [`40ants-doc:defsection`][4e8b] macro now does not generate export code
  if `:EXPORT` argument is `NIL`.

* Functions `UPDATE-ASDF-SYSTEM-HTML-DOCS` and `UPDATE-ASDF-SYSTEM-README`
  were replaced with [`40ants-doc/builder:update-asdf-system-docs`][0983], which also supports
  ChangeLog.md generation. Use [`40ants-doc/changelog:defchangelog`][8c40] to define versions.

* Variables `*DOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL*` and
  `*DOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL*` were removed. Probably we'll return this
  feature back in other form, to restrict `TOC`'s size.

* Removed `LOCATE-AND-DOCUMENT` generic function.

* Links to the GitHub now are generated automatically,
  if [`40ants-doc/builder:update-asdf-system-docs`][0983] function is used
  and system definition has a `:SOURCE-CONTROL` slot.

* Generic functions `COLLECT-REACHABLE-OBJECTS` and `LOCATE-AND-COLLECT-REACHABLE-OBJECTS`
  were removed.

* Variables `*DOCUMENT-LINK-SECTIONS*`, `*DOCUMENT-TEXT-NAVIGATION*` and `*DOCUMENT-FANCY-HTML-NAVIGATION*` were removed.

* Variable `*DOCUMENT-MIN-LINK-HASH-LENGTH*` was removed. Use `COMMONDOC-MARKDOWN:*MIN-LINK-HASH-LENGTH*`

* Functions [`40ants-doc/builder:update-asdf-system-docs`][0983] and [`40ants-doc/builder:render-to-files`][05c0]
  now accept `WARN-ON-UNDOCUMENTED-PACKAGES` argument and `CLEAN-URLS` argument.

* Variable `*DOCUMENT-MARK-UP-SIGNATURES*` was removed.

* Added `DOWNCASE-UPPERCASE-CODE` argument instead of `*DOCUMENT-DOWNCASE-UPPERCASE-CODE*`.
  This argument is true by default.

* Added warnings on symbols, referenced like internals, using `::`.

* Added [`40ants-doc:defsection-copy`][ad02] macro to define copy of the section but with a different name.

<a id="x-2840ANTS-DOC-2FCHANGELOG-3A-3A-7C0-2E1-2E0-7C-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## 0.1.0

* Project forked from [`MGL-PAX`][7927].
  Code refactored into the package inferred system and core is separated
  to have minimum dependencies.

* Fixed displaying docstring for constant locative.

* Include locative was fixed for files with unicode characters
  file-subseq function was rewritten.

* Locatives can be specified without a package prefix inside the defsection
  because all locative symbols now live in [`40ants-doc/locatives`][d889] package.

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


[d889]: https://40ants.com/doc/#x-28-23A-28-2820-29-20BASE-CHAR-20-2E-20-2240ANTS-DOC-2FLOCATIVES-22-29-20PACKAGE-29
[05c0]: https://40ants.com/doc/#x-2840ANTS-DOC-2FBUILDER-3ARENDER-TO-FILES-20FUNCTION-29
[0983]: https://40ants.com/doc/#x-2840ANTS-DOC-2FBUILDER-3AUPDATE-ASDF-SYSTEM-DOCS-20FUNCTION-29
[8c40]: https://40ants.com/doc/#x-2840ANTS-DOC-2FCHANGELOG-3ADEFCHANGELOG-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[359f]: https://40ants.com/doc/#x-2840ANTS-DOC-2FLOCATIVES-3AINCLUDE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29
[bcaa]: https://40ants.com/doc/#x-2840ANTS-DOC-2FTHEMES-2FDOCS-3A-3A-40DEFINING-A-THEME-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[4e8b]: https://40ants.com/doc/#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[ad02]: https://40ants.com/doc/#x-2840ANTS-DOC-3ADEFSECTION-COPY-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29
[7927]: https://github.com/melisgl/mgl-pax
[140c]: https://www.sphinx-doc.org/