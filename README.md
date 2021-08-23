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

The main goal is to extract a core features into the [`40ANTS-DOC`](#x-28-23A-28-2810-29-20BASE-CHAR-20-2E-20-2240ants-doc-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29) system
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

* Core system [`40ANTS-DOC`](#x-28-23A-28-2810-29-20BASE-CHAR-20-2E-20-2240ants-doc-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29) now has only two dependencies on `NAMED-READTABLES`
  and `PYTHONIC-STRING-READER`. If you want to compile a documentation, load
  [`40ANTS-DOC-FULL`](#x-28-23A-28-2815-29-20BASE-CHAR-20-2E-20-2240ants-doc-full-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29) system which will download such dependencies as markdown
  parser and more.

* Now you don't have to import any locative symbols into your package. Import
  only a [`DEFSECTION`](#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29) macro and it will be enough to define documentation for
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

<a id="x-2840ANTS-DOC-2FDOC-3A-3A-40TUTORIAL-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Tutorial

[`40ANTS-DOC`](#x-28-23A-28-2810-29-20BASE-CHAR-20-2E-20-2240ants-doc-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29) provides an extremely poor man's Explorable Programming
environment. Narrative primarily lives in so called sections that
mix markdown docstrings with references to functions, variables,
etc, all of which should probably have their own docstrings.

The primary focus is on making code easily explorable by using
`SLIME`'s `M-.` (`slime-edit-definition`). See how to enable some
fanciness in [`Emacs Integration`](#x-2840ANTS-DOC-2FDOC-3A-3A-40EMACS-INTEGRATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29). Generating documentation
from sections and all the referenced items in Markdown or `HTML`
format is also implemented.

With the simplistic tools provided, one may accomplish similar
effects as with Literate Programming, but documentation is generated
from code, not vice versa and there is no support for chunking yet.
Code is first, code must look pretty, documentation is code.

When the code is loaded into the lisp, pressing `M-.` in `SLIME` on
the name of the section will take you there. Sections can also refer
to other sections, packages, functions, etc and you can keep exploring.

Here is an example of how it all works together:

```commonlisp
(uiop:define-package #:foo-random
  (:documentation "This package provides various utilities for
                   random. See @FOO-RANDOM-MANUAL.")
  (:use #:common-lisp
        #:40ants-doc)
  (:import-from #:40ants-doc/ignored-words
                #:ignore-words-in-package)
  (:export #:foo-random-state
           #:state
           #:*foo-state*
           #:gaussian-random
           #:uniform-random))

(in-package foo-random)

(defsection @foo-random-manual (:title "Foo Random manual")
  "Here you describe what's common to all the referenced (and
   exported) functions that follow. They work with *FOO-STATE*,
   and have a :RANDOM-STATE keyword arg. Also explain when to
   choose which."
  (foo-random-state class)
  (state (reader foo-random-state))
  
  "Hey we can also print states!"
  
  (print-object (method () (foo-random-state t)))
  (*foo-state* variable)
  (gaussian-random function)
  (uniform-random function)
  ;; this is a subsection
  (@foo-random-examples section))

(defclass foo-random-state ()
  ((state :reader state
          :documentation "Returns random foo's state.")))

(defmethod print-object ((object foo-random-state) stream)
  (print-unreadable-object (object stream :type t)))

(defvar *foo-state* (make-instance 'foo-random-state)
  "Much like *RANDOM-STATE* but uses the FOO algorithm.")

(defun uniform-random (limit &key (random-state *foo-state*))
  "Return a random number from the between 0 and LIMIT (exclusive)
   uniform distribution."
  (declare (ignore limit random-state))
  nil)

(defun gaussian-random (stddev &key (random-state *foo-state*))
  "Return not a random number from a zero mean normal distribution with
   STDDEV."
  (declare (ignore stddev random-state))
  nil)

(defsection @foo-random-examples (:title "Examples")
  "Let's see the transcript of a real session of someone working
   with FOO:

   ```cl-transcript
   (values (princ :hello) (list 1 2))
   .. HELLO
   => :HELLO
   => (1 2)

   (make-instance 'foo-random-state)
   ==> #<FOO-RANDOM-STATE >
   ```")

```
Generating documentation in a very stripped down markdown format is
easy:

```commonlisp
(describe @foo-random-manual)
```
For this example, the generated markdown would look like this:

```markdown
#<40ANTS-DOC:SECTION FOO-RANDOM::@FOO-RANDOM-MANUAL>
  [standard-object]

Slots with :INSTANCE allocation:
  NAME                           = FOO-RANDOM::@FOO-RANDOM-MANUAL
  PACKAGE                        = #<PACKAGE "FOO-RANDOM">
  READTABLE                      = #<NAMED-READTABLE PYTHONIC-STRING-READER:PYTHONIC-STRING-SYNTAX {1002C..
  TITLE                          = "Foo Random manual"
  LINK-TITLE-TO                  = NIL
  ENTRIES                        = ("Here you describe what's common to all the referenced (and..
  IGNORE-WORDS                   = NIL
```
More fancy markdown or `HTML` output with automatic markup and linking
of uppercase symbol names found in docstrings, section numbering,
table of contents, etc is possible by calling the
[`40ANTS-DOC/BUILDER:RENDER-TO-STRING`](#x-2840ANTS-DOC-2FBUILDER-3ARENDER-TO-STRING-20FUNCTION-29) or [`40ANTS-DOC/BUILDER:RENDER-TO-FILES`](#x-2840ANTS-DOC-2FBUILDER-3ARENDER-TO-FILES-20FUNCTION-29)
functions.

Last one can even generate documentation for different, but related
libraries at the same time with the output going to different files,
but with cross-page links being automatically added for symbols
mentioned in docstrings. See [`Generating Documentation`](#x-2840ANTS-DOC-2FBUILDER-3A-3A-40GENERATING-DOCUMENTATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29) for
some convenience functions to cover the most common cases.

Note how `(*FOO-STATE* [VARIABLE](#x-28VARIABLE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29))` in the [`DEFSECTION`](#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29) form includes its documentation in
`@FOO-RANDOM-MANUAL`. The symbols [`VARIABLE`](#x-28VARIABLE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) and [`FUNCTION`](#x-28FUNCTION-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) are just two
instances of 'locatives' which are used in [`DEFSECTION`](#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29) to refer to
definitions tied to symbols. See [`Locative Types`](#x-2840ANTS-DOC-2FDOC-3A-3A-40LOCATIVE-TYPES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29).

The transcript in the code block tagged with `cl-transcript` is
automatically checked for up-to-dateness. See
[`Transcripts`](#x-2840ANTS-DOC-2FTRANSCRIBE-3A-3A-40TRANSCRIPT-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29).

<a id="x-2840ANTS-DOC-2FDOC-3A-3A-40TODO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## TODO

* <s>Refactor code and make a core package with only a few dependencies.</s>

* <s>Add warnings on `UPPERCASED` symbols in docstrings which aren't found in the package and can't be cross referenced.</s>

* <s>Support `SLY` and make both `SLIME` and `SLY` integrations optional.</s>

* <s>Add a search facility which will build an index for static file like Sphinx does.</s>

* <s>Separate markup parsing and result rendering code to support markups other than Markdown and `HTML`.</s>

* <s>Add a new section type to render ChangeLog.</s>

* Support custom `HTML` themes.

* Make some warnings compile-time for defsection and show them in the Emacs, if possible.

