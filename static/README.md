How to update highlight.js
==========================

This folder contains subfolder *highlight*, downloaded from
https://highlightjs.org/download/. Version, included into
the repository, supports these languages:

* bash
* css
* json
* yaml
* plain-text
* html
* markdown
* lisp

By default, *atom-one-dark.min.css* theme is used.

If you want to include other languages or themes, go to the
https://highlightjs.org/download/, download archive and unpack it
into this *static* directory. Strip down unnecessary files,
to make ASDF system smaller. You can remove everything except
*highlight.min.js*, *styles/<theme>.min.css* and *LICENSE*.

Different themes can be previewed at https://highlightjs.org/static/demo/.
