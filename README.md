<a id="x-2840ANTS-DOC-2FDOC-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# 40ANTS-DOC Documentation Generator


<table>
<tr>
<td><a href="https://github.com/40ants/doc/actions/workflows/ci.yml"><img src="http://github-actions.40ants.com/40ants/doc/matrix.svg?only=ci.run-tests"/></a></td>

<td><a href="https://github.com/40ants/doc/actions/workflows/linter.yml"><img src="http://github-actions.40ants.com/40ants/doc/matrix.svg?only=linter.linter"/></a></td>

<td><a href="https://coveralls.io/github/40ants/doc?branch=master"><img src="https://coveralls.io/repos/github/40ants/doc/badge.svg?branch=master"></a></td>
</tr>
</table>

<a id="x-2840ANTS-DOC-2FDOC-3A-3A-40ABOUT-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## About this fork

This system is a fork of [`MGL-PAX`](https://github.com/melisgl/mgl-pax).

There are a few reasons, why I've created the fork.

The main goal is to extract a core features into the `40ANTS-DOC` system
with as little dependencies as possible. This is important, because with `MGL-PAX`'s
style, you define documentation sections in your library's code, which makes
it dependent on the documentation system. However, heavy weight dependencies
like `IRONCLAD`, `3BMD` or `SWANK` should not be required.

The seconds goal was to refactor a 3.5k lines of `pax.lisp` file into
a smaller modules to make navigation easier. This will help any person
who will decide to learn how the documentation builder works. Also,
granular design will make it possible loading subsystems like `SLIME` or `SLY`
integration.

<a id="x-2840ANTS-DOC-2FDOC-3A-3A-40DIFFERENCE-FROM-MGL-PAX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Why this fork is different

Here is features already implemented in this fork:

* Core system `40ANTS-DOC` now has only two dependencies on `NAMED-READTABLES`
  and `PYTHONIC-STRING-READER`. If you want to compile a documentation, load
  `40ANTS-DOC-FULL` system which will download such dependencies as markdown
  parser and more.

* Now you don't have to import any locative symbols into your package. Import
  only a `DEFSECTION` macro and it will be enough to define documentation for
  your library!

* Added a warning mechanism, which will issue such warnings on words which looks
  like a symbol, but when real symbol or reference is absent:

```
WARNING: Unable to find symbol "API" mentioned in (CL-INFO:@INDEX SECTION)
```
I'm planning to extend this fork even more. Read [`TODO`](#x-2840ANTS-DOC-2FDOC-3A-3A-40TODO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29) section to learn about
proposed features or [start a new discussion](https://github.com/40ants/doc/discussions)
on the GitHub to suggest a new feature.

<a id="x-2840ANTS-DOC-2FDOC-3A-3A-40FULL-DOC-LINK-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Full Documentation

Read full documentation at [site 40ants.com/doc/](https://40ants.com/doc/).

<a id="x-2840ANTS-DOC-2FDOC-3A-3A-40TODO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## TODO

* <s>Refactor code and make a core package with only a few dependencies.</s>

* <s>Add warnings on `UPPERCASED` symbols in docstrings which aren't found in the package and can't be cross referenced.</s>

* Make some warnings compile-time for defsection and show them in the Emacs, if possible.

* Support custom `HTML` themes.

* Support `SLY` and make both `SLIME` and `SLY` integrations optional.

* Add a search facility which will build an index for static file like Sphinx does.

* Separate markup parsing and result rendering code to support markups other than Markdown and `HTML`.

* Add a new section type to render ChangeLog.

