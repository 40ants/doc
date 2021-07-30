<a id="x-2840ANTS-DOC-2FDOC-3A-40INDEX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# 40Ants Doc Manual


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

`
   WARNING: Unable to find symbol "API" mentioned in ([40Ants Doc Manual](#x-2840ANTS-DOC-2FDOC-3A-40INDEX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29) [SECTION](#x-2840ANTS-DOC-2FLOCATIVES-3ASECTION-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29))
`

I'm planning to extend this fork even more. Read [`TODO`](#x-2840ANTS-DOC-2FDOC-3A-3A-40TODO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29) section to learn about
proposed features or [start a new discussion](https://github.com/40ants/doc/discussions)
on the GitHub to suggest a new feature.

## 40ANTS-DOC ASDF System Details

* Version: 0.1.0

* Description: Allows to put documentation inside lisp files and cross-reference between different entities. Based on `MGL-PAX`.

* Licence: `MIT`

* Author: Alexander Artemenko

* Mailto: [svetlyak.40wt@gmail.com](mailto:svetlyak.40wt@gmail.com)

* Homepage: [http://40ants.com/doc](http://40ants.com/doc)

* Bug tracker: [https://github.com/40ants/doc/issues](https://github.com/40ants/doc/issues)

* Source control: [`GIT`](https://github.com/40ants/doc)

## 40ANTS-DOC-FULL ASDF System Details

* Version: 0.1.0

* Description: Documentation generator. You will need to load this system, to build documentation for a library which uses [`40ANTS-DOC`](#x-28-23A-28-2810-29-20BASE-CHAR-20-2E-20-2240ants-doc-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29) system.

* Licence: `MIT`

* Author: Alexander Artemenko

* Mailto: [svetlyak.40wt@gmail.com](mailto:svetlyak.40wt@gmail.com)

* Homepage: [http://40ants.com/doc](http://40ants.com/doc)

* Bug tracker: [https://github.com/40ants/doc/issues](https://github.com/40ants/doc/issues)

* Source control: [`GIT`](https://github.com/40ants/doc.git)

<a id="x-2840ANTS-DOC-2FDOC-3A-3A-40LINKS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Links

Here is the [official repository](https://github.com/40ants/doc) and
the [`HTML` documentation](https://40ants.com/doc) for the latest version.

This system is a fork of the [`MGL-PAX`](https://github.com/melisgl/mgl-pax).
Because of massive refactoring, it is incompatible with original repository.

<a id="x-2840ANTS-DOC-2FDOC-3A-3A-40BACKGROUND-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Background

As a user, I frequently run into documentation that's incomplete
and out of date, so I tend to stay in the editor and explore the
code by jumping around with `SLIME`'s [`M-.`](http://common-lisp.net/project/slime/doc/html/Finding-definitions.html#Finding-definitions). As a library
author, I spend a great deal of time polishing code, but precious
little writing documentation.

In fact, I rarely write anything more comprehensive than docstrings
for exported stuff. Writing docstrings feels easier than writing a
separate user manual and they are always close at hand during
development. The drawback of this style is that users of the library
have to piece the big picture together themselves.

That's easy to solve, I thought, let's just put all the narrative
that holds docstrings together in the code and be a bit like a
Literate Programming weenie turned inside out. The original
prototype which did almost everything I wanted was this:

```
(defmacro defsection (name docstring)
  `(defun ,name () ,docstring))
```
Armed with [`DEFSECTION`](#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29), I soon found myself organizing code following
the flow of user level documentation and relegated comments to
implementational details entirely. However, some portions of
[`DEFSECTION`](#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29) docstrings were just listings of all the functions,
macros and variables related to the narrative, and this list was
effectively repeated in the `DEFPACKAGE` form complete with little
comments that were like section names. A clear violation of
[`OAOO`](http://c2.com/cgi/wiki?OnceAndOnlyOnce), one of them had to go, so [`DEFSECTION`](#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29) got a list of
symbols to export.

That was great, but soon I found that the listing of symbols is
ambiguous if, for example, a function, a compiler macro and a class
are named by the same symbol. This did not concern exporting, of
course, but it didn't help readability. Distractingly, on such
symbols, `M-.` was popping up selection dialogs. There were two
birds to kill, and the symbol got accompanied by a type which was
later generalized into the concept of locatives:

```commonlisp
(defsection @introduction ()
  "A single line for one man ..."
  (foo class)
  (bar function))
```
After a bit of elisp hacking, `M-.` was smart enough to disambiguate
based on the locative found in the vicinity of the symbol and
everything was good for a while.

Then I realized that sections could refer to other sections if there
were a [`SECTION`](#x-2840ANTS-DOC-2FLOCATIVES-3ASECTION-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) locative. Going down that path, I soon began to feel
the urge to generate pretty documentation as all the necessary
information was manifest in the [`DEFSECTION`](#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29) forms. The design
constraint imposed on documentation generation was that following
the typical style of upcasing symbols in docstrings there should be
no need to explicitly mark up links: if `M-.` works, then the
documentation generator shall also be able find out what's being
referred to.

I settled on [Markdown](https://daringfireball.net/projects/markdown/) as a reasonably non-intrusive
format, and a few thousand lines later `MGL-PAX` was born.

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

In typical use, using [`40ANTS-DOC`](#x-28-23A-28-2810-29-20BASE-CHAR-20-2E-20-2240ants-doc-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29), packages have no `:EXPORT`'s defined.
Instead the `UIOP:DEFINE-PACKAGE` form gets a docstring which may mention section
names (defined with [`DEFSECTION`](#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29)). When the code is loaded into the
lisp, pressing `M-.` in `SLIME` on the name of the section will take
you there. Sections can also refer to other sections, packages,
functions, etc and you can keep exploring.

Here is an example of how it all works together:

```commonlisp
(uiop:define-package #:foo-random
  (:documentation "This package provides various utilities for
random. See @FOO-RANDOM-MANUAL.")
  (:use #:common-lisp #:40ants-doc))

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
  ((state :reader state)))

(defmethod print-object ((object foo-random-state) stream)
  (print-unreadable-object (object stream :type t)))

(defvar *foo-state* (make-instance 'foo-random-state)
  "Much like *RANDOM-STATE* but uses the FOO algorithm.")

(defun uniform-random (limit &key (random-state *foo-state*))
  "Return a random number from the between 0 and LIMIT (exclusive)
uniform distribution."
  nil)

(defun gaussian-random (stddev &key (random-state *foo-state*))
  "Return a random number from a zero mean normal distribution with
STDDEV."
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

```text
# Foo Random manual

###### \[in package FOO-RANDOM\]
Here you describe what's common to all the referenced (and
exported) functions that follow. They work with *FOO-STATE*,
and have a :RANDOM-STATE keyword arg. Also explain when to
choose which.

- [class] FOO-RANDOM-STATE

- [reader] STATE FOO-RANDOM-STATE

Hey we can also print states!

- [method] PRINT-OBJECT (OBJECT FOO-RANDOM-STATE) STREAM

- [variable] *FOO-STATE* #<FOO-RANDOM-STATE >

    Much like *RANDOM-STATE* but uses the FOO algorithm.

- [function] GAUSSIAN-RANDOM STDDEV &KEY (RANDOM-STATE *FOO-STATE*)

    Return a random number from a zero mean normal distribution with
    STDDEV.

- [function] UNIFORM-RANDOM LIMIT &KEY (RANDOM-STATE *FOO-STATE*)

    Return a random number from the between 0 and LIMIT (exclusive)
    uniform distribution.

## Examples

Let's see the transcript of a real session of someone working
with FOO:

```cl-transcript
(values (princ :hello) (list 1 2))
.. HELLO
=> :HELLO
=> (1 2)

(make-instance 'foo-random-state)
==> #<FOO-RANDOM-STATE >

```
```
More fancy markdown or `HTML` output with automatic markup and linking
of uppercase symbol names found in docstrings, section numbering,
table of contents, etc is possible by calling the [`40ANTS-DOC/DOCUMENT::DOCUMENT`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-20GENERIC-FUNCTION-29)
generic.

One can even generate documentation for different, but related
libraries at the same time with the output going to different files,
but with cross-page links being automatically added for symbols
mentioned in docstrings. See [`Generating Documentation`](#x-2840ANTS-DOC-2FBUILDER-3A-3A-40GENERATING-DOCUMENTATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29) for
some convenience functions to cover the most common cases.

Note how `([VARIABLE](#x-28VARIABLE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) *FOO-STATE*)` in the [`DEFSECTION`](#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29) form both
exports `*FOO-STATE*` and includes its documentation in
`@FOO-RANDOM-MANUAL`. The symbols [`VARIABLE`](#x-28VARIABLE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) and [`FUNCTION`](#x-28FUNCTION-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) are just two
instances of 'locatives' which are used in [`DEFSECTION`](#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29) to refer to
definitions tied to symbols. See [`Locative Types`](#x-2840ANTS-DOC-2FDOC-3A-3A-40LOCATIVE-TYPES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29).

The transcript in the code block tagged with `cl-transcript` is
automatically checked for up-to-dateness. See
[`Transcripts`](#x-2840ANTS-DOC-2FTRANSCRIBE-3A-3A-40TRANSCRIPT-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29).

<a id="x-2840ANTS-DOC-2FDOC-3A-3A-40EMACS-INTEGRATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Emacs Integration

Integration into `SLIME`'s `M-.` (`slime-edit-definition`) allows one
to visit the source location of the thing that's identified by a
symbol and the locative before or after the symbol in a buffer. With
this extension, if a locative is the previous or the next expression
around the symbol of interest, then `M-.` will go straight to the
definition which corresponds to the locative. If that fails, `M-.`
will try to find the definitions in the normal way which may involve
popping up an xref buffer and letting the user interactively select
one of possible definitions.

Note that the this feature is implemented in terms of
`SWANK-BACKEND:FIND-SOURCE-LOCATION` and
`SWANK-BACKEND:FIND-DEFINITIONS` whose support varies across the Lisp
implementations.

In the following examples, pressing `M-.` when the cursor is on one
of the characters of `FOO` or just after `FOO`, will visit the
definition of function `FOO`:

```text
function foo
foo function
(function foo)
(foo function)
```
In particular, references in a [`DEFSECTION`](#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29) form are in (`SYMBOL`
[`LOCATIVE`](#x-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29)) format so `M-.` will work just fine there.

Just like vanilla `M-.`, this works in comments and docstrings. In
this example pressing `M-.` on `FOO` will visit `FOO`'s default
method:

```commonlisp
;;;; See FOO `(method () (t t t))` for how this all works.
;;;; But if the locative has semicolons inside: FOO `(method
;;;; () (t t t))`, then it won't, so be wary of line breaks
;;;; in comments.
```
With a prefix argument (`C-u M-.`), one can enter a symbol plus a
locative separated by whitespace to preselect one of the
possibilities.

The `M-.` extensions can be enabled by adding this to your Emacs
initialization file (or loading `src/pax.el`):

```
;;; MGL-PAX M-. integration

(defun slime-edit-locative-definition (name &optional where)
  (or (slime-locate-definition name (slime-locative-before))
      (slime-locate-definition name (slime-locative-after))
      (slime-locate-definition name (slime-locative-after-in-brackets))
      ;; support "foo function" and "function foo" syntax in
      ;; interactive use
      (let ((pos (cl-position ?\s name)))
        (when pos
          (or (slime-locate-definition (cl-subseq name 0 pos)
                                       (cl-subseq name (1+ pos)))
              (slime-locate-definition (cl-subseq name (1+ pos))
                                       (cl-subseq name 0 pos)))))))

(defun slime-locative-before ()
  (ignore-errors (save-excursion
                   (slime-beginning-of-symbol)
                   (slime-last-expression))))

(defun slime-locative-after ()
  (ignore-errors (save-excursion
                   (slime-end-of-symbol)
                   (slime-forward-sexp)
                   (slime-last-expression))))

(defun slime-locative-after-in-brackets ()
  (ignore-errors (save-excursion
                   (slime-end-of-symbol)
                   (skip-chars-forward "`" (+ (point) 1))
                   (when (and (= 1 (skip-chars-forward "\\]" (+ (point) 1)))
                              (= 1 (skip-chars-forward "\\[" (+ (point) 1))))
                     (buffer-substring-no-properties
                      (point)
                      (progn (search-forward "]" nil (+ (point) 1000))
                             (1- (point))))))))

(defun slime-locate-definition (name locative)
  (when locative
    (let ((location
           (slime-eval
            ;; Silently fail if mgl-pax is not loaded.
            `(cl:when (cl:find-package :mgl-pax)
                      (cl:funcall
                       (cl:find-symbol
                        (cl:symbol-name :locate-definition-for-emacs) :mgl-pax)
                       ,name ,locative)))))
      (when (and (consp location)
                 (not (eq (car location) :error)))
        (slime-edit-definition-cont
         (list (make-slime-xref :dspec `(,name)
                                :location location))
         "dummy name"
         where)))))

(add-hook 'slime-edit-definition-hooks 'slime-edit-locative-definition)
```
<a id="x-2840ANTS-DOC-2FSWANK-3ALOCATE-DEFINITION-FOR-EMACS-20FUNCTION-29"></a>

### [function] `LOCATE-DEFINITION-FOR-EMACS` NAME LOCATIVE-STRING



<a id="x-2840ANTS-DOC-2FDOC-3A-3A-40BASICS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Basics

Now let's examine the most important pieces in detail.

<a id="x-2840ANTS-DOC-3A-2ADISCARD-DOCUMENTATION-P-2A-20-28VARIABLE-29-29"></a>

### [variable] `*DISCARD-DOCUMENTATION-P*` NIL


The default value of [`DEFSECTION`](#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29)'s `DISCARD-DOCUMENTATION-P` argument.
One may want to set [`*DISCARD-DOCUMENTATION-P*`](#x-2840ANTS-DOC-3A-2ADISCARD-DOCUMENTATION-P-2A-20-28VARIABLE-29-29) to true before
building a binary application.


<a id="x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

### [macro] `DEFSECTION`


Define a documentation section and maybe export referenced symbols.
A bit behind the scenes, a global variable with `NAME` is defined and
is bound to a [`SECTION`](#x-2840ANTS-DOC-3ASECTION-20CLASS-29) object. By convention, section names
start with the character `@`. See [`Tutorial`](#x-2840ANTS-DOC-2FDOC-3A-3A-40TUTORIAL-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29) for an example.

`ENTRIES` consists of docstrings and references. Docstrings are
arbitrary strings in markdown format, references are defined in the
form:

```text
(symbol locative)
```
For example, `(FOO [FUNCTION](#x-28FUNCTION-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29))` refers to the function `FOO`, `(@BAR
[SECTION](#x-2840ANTS-DOC-3ASECTION-20CLASS-29))` says that `@BAR` is a subsection of this
one. `(BAZ ([METHOD](#x-28METHOD-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) () (T T T)))` refers to the default method of the
three argument generic function `BAZ`. `(FOO [FUNCTION](#x-28FUNCTION-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29))` is
equivalent to `(FOO ([FUNCTION](#x-28FUNCTION-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29)))`.

A locative in a reference can either be a symbol or it can be a list
whose `CAR` is a symbol. In either case, the symbol is the called the
type of the locative while the rest of the elements are the locative
arguments. See [`Locative Types`](#x-2840ANTS-DOC-2FDOC-3A-3A-40LOCATIVE-TYPES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29) for the list of locative
types available out of the box.

The same symbol can occur multiple times in a reference, typically
with different locatives, but this is not required.

The references are not looked up (see [`40ANTS-DOC/REFERENCE::RESOLVE`](#x-2840ANTS-DOC-2FREFERENCE-3ARESOLVE-20FUNCTION-29) in the
[`Extension API`](#x-2840ANTS-DOC-2FDOC-3A-3A-40EXTENSION-API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29)) until documentation is generated, so it is
allowed to refer to things yet to be defined.

If `:EXPORT` is true (the default), the referenced symbols and `NAME` are
candidates for exporting. A candidate symbol is exported if

* it is accessible in [`PACKAGE`](#x-28PACKAGE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) (it's not `OTHER-PACKAGE:SOMETHING`)
  and

* there is a reference to it in the section being defined with a
  locative whose type is approved by [`EXPORTABLE-LOCATIVE-TYPE-P`](#x-2840ANTS-DOC-3AEXPORTABLE-LOCATIVE-TYPE-P-20GENERIC-FUNCTION-29).

The idea with confounding documentation and exporting is to force
documentation of all exported symbols. `:EXPORT` argument will cause
[package variance](http://www.sbcl.org/manual/#Package-Variance)
error on `SBCL`. To prevent it, use `UIOP:DEFINE-PACKAGE` instead
of `CL:DEFPACKAGE`.

`:TITLE` is a non-marked-up string or `NIL`. If non-`NIL`, it determines
the text of the heading in the generated output. `:LINK-TITLE-TO` is a
reference given as an
`(OBJECT LOCATIVE)` pair or `NIL`, to which the heading will link when
generating `HTML`. If not specified, the heading will link to its own
anchor.

When `:DISCARD-DOCUMENTATION-P` (defaults to [`*DISCARD-DOCUMENTATION-P*`](#x-2840ANTS-DOC-3A-2ADISCARD-DOCUMENTATION-P-2A-20-28VARIABLE-29-29))
is true, `ENTRIES` will not be recorded to save memory.

`:IGNORE-WORDS` allows to pass a list of string which will not cause
warnings. Usually these as uppercased words which are not symbols
in the current package, like `SLIME`, `LISP`, etc.


<a id="x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-20GENERIC-FUNCTION-29"></a>

### [generic-function] `DOCUMENT` OBJECT &KEY STREAM PAGES FORMAT


Write `OBJECT` in `FORMAT` to `STREAM` diverting some output to `PAGES`.
`FORMAT` can be anything `3BMD` supports which is
currently `:MARKDOWN`, `:HTML` and `:PLAIN`. `STREAM` may be a stream
object, T or `NIL` as with `CL:FORMAT`.

Most often, this function is called on section objects
like `([DOCUMENT](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-20GENERIC-FUNCTION-29) @MANUAL)`, but it supports all kinds of
objects for which `DOCUMENT-OBJECT` ([`1`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20T-29-29-29) [`2`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-28STRING-20T-29-29-29) [`3`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20GENERIC-FUNCTION-29)) is defined. To look up the
documentation of function [`DOCUMENT`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-20GENERIC-FUNCTION-29):

```text
(document #'document)
```
To generate the documentation for separate libraries with automatic
cross-links:

```text
(document (list @cube-manual @mat-manual))
```
Note that not only first class objects can have documentation. For
instance, variables and deftypes are not represented by objects.
That's why `CL:DOCUMENTATION` has a `:DOC-TYPE` argument. [`DOCUMENT`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-20GENERIC-FUNCTION-29) does
not have anything like that, instead it relies on [`40ANTS-DOC/REFERENCE::REFERENCE`](#x-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20CLASS-29) objects
to carry the extra information. We are going to see later how
references and locatives work. Until then, here is an example on how
to look up the documentation of type `FOO`:

```text
(document (locate 'foo 'type))
```
One can call `DESCRIBE` on [`40ANTS-DOC:SECTION`](#x-2840ANTS-DOC-3ASECTION-20CLASS-29) objects to get
documentation in markdown format with less markup than the default.
See [`DESCRIBE-OBJECT`](#x-28DESCRIBE-OBJECT-20-28METHOD-20NIL-20-2840ANTS-DOC-3ASECTION-20T-29-29-29).

There are quite a few special variables that affect how output is
generated, see [`Documentation Printer Variables`](#x-2840ANTS-DOC-2FDOC-3A-3A-40DOCUMENTATION-PRINTER-VARIABLES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29).

The rest of this description deals with how to generate multiple
pages.

The `PAGES` argument is to create multi-page documents by routing some
of the generated output to files, strings or streams. `PAGES` is a
list of page specification elements. A page spec is a plist with
keys `:OBJECTS`, `:OUTPUT`, `:URI-FRAGMENT`, `:SOURCE-URI-FN`, `:HEADER-FN`
and `:FOOTER-FN`. `:OBJECTS` is a list of objects (references are allowed
but not required) whose documentation is to be sent to `:OUTPUT`.

When documentation for an object is generated, the first matching
page spec is used, where the object matches the page spec if it is
contained in one of its `:OBJECTS` in the sense of
`40ANTS-DOC/REFERENCE-API::COLLECT-REACHABLE-OBJECTS` ([`1`](#x-2840ANTS-DOC-2FREFERENCE-API-3ACOLLECT-REACHABLE-OBJECTS-20-28METHOD-20NIL-20-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-29-29-29) [`2`](#x-2840ANTS-DOC-2FREFERENCE-API-3ACOLLECT-REACHABLE-OBJECTS-20-28METHOD-20NIL-20-28T-29-29-29) [`3`](#x-2840ANTS-DOC-2FREFERENCE-API-3ACOLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29)).

`:OUTPUT` can be a number things:

* If it's a list whose first element is a string or a pathname, then
  output will be sent to the file denoted by that and the rest of
  the elements of the list are passed on as arguments to `CL:OPEN`.
  One extra keyword argument is `:ENSURE-DIRECTORIES-EXIST`. If it's
  true, `ENSURE-DIRECTORIES-EXIST` will be called on the pathname
  before it's opened.

* If it's `NIL`, then output will be collected in a string.

* If it's T, then output will be sent to `*STANDARD-OUTPUT*`.

* If it's a stream, then output will be sent to that stream.

If some pages are specified, [`DOCUMENT`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-20GENERIC-FUNCTION-29) returns a list of designators
for generated output. If a page whose `:OUTPUT` refers to a file that
was created (which doesn't happen if nothing would be written to
it), then the corresponding pathname is included in the list. For
strings the string itself, while for streams the stream object is
included in the list. This way it's possible to write some pages to
files and some to strings and have the return value indicate what
was created. The output designators in the returned list are ordered
by creation time.

If no `PAGES` are specified, [`DOCUMENT`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-20GENERIC-FUNCTION-29) returns a single pathname,
string or stream object according to the value of the `STREAM`
argument.

Note that even if `PAGES` is specified, `STREAM` acts as a catch all
taking the generated documentation for references not claimed by any
pages. Also, the filename, string or stream corresponding to `STREAM`
is always the first element in list of generated things that is the
return value.

`:HEADER-FN`, if not `NIL`, is a function of a single stream argument
which is called just before the first write to the page.
Since `:FORMAT` `:HTML` only generates `HTML` fragments, this makes it
possible to print arbitrary headers, typically setting the title,
css stylesheet, or charset.

`:FOOTER-FN` is similar to `:HEADER-FN`, but it's called after the last
write to the page. For `HTML`, it typically just closes the body.

`:URI-FRAGMENT` is a string such as `"doc/manual.html"` that specifies
where the page will be deployed on a webserver. It defines how links
between pages will look. If it's not specified and `:OUTPUT` refers
to a file, then it defaults to the name of the file. If `:URI-FRAGMENT`
is `NIL`, then no links will be made to or from that page.

Finally, `:SOURCE-URI-FN` is a function of a single, `40ANTS-DOC/REFERENCE::REFERENCE`
argument. If it returns a value other than `NIL`, then it must be a
string representing an `URI`. If `FORMAT` is `:HTML` and
[`40ANTS-DOC/BUILDER/VARS::*DOCUMENT-MARK-UP-SIGNATURES*`](#x-2840ANTS-DOC-2FBUILDER-2FVARS-3A-2ADOCUMENT-MARK-UP-SIGNATURES-2A-20-28VARIABLE-29-29) is true, then the locative as
displayed in the signature will be a link to this uri. See
[`40ANTS-DOC/GITHUB::MAKE-GITHUB-SOURCE-URI-FN`](#x-2840ANTS-DOC-2FGITHUB-3AMAKE-GITHUB-SOURCE-URI-FN-20FUNCTION-29).

`:PAGES` may look something like this:

```commonlisp
`((;; The section about SECTIONs and everything below it ...
   :objects (, @mgl-pax-sections)
   ;; ... is so boring that it's not worth the disk space, so
   ;; send it to a string.
   :output (nil)
   ;; Explicitly tell other pages not to link to these guys.
   :uri-fragment nil)
  ;; Send the @MGL-PAX-EXTENSION-API section and everything reachable
  ;; from it ...
  (:objects (, @mgl-pax-extension-api)
   ;; ... to build/tmp/pax-extension-api.html.
   :output ("build/tmp/pax-extension-api.html")
   ;; However, on the web server html files will be at this
   ;; location relative to some common root, so override the
   ;; default:
   :uri-fragment "doc/dev/pax-extension-api.html"
   ;; Set html page title, stylesheet, charset.
   :header-fn 'write-html-header
   ;; Just close the body.
   :footer-fn 'write-html-footer)
  ;; Catch the reference that were not reachable from the above. It
  ;; is important for this page spec to be last.
  (:objects (, @mgl-pax-manual)
   :output ("build/tmp/manual.html")
   ;; Links from the extension api page to the manual page will
   ;; be to ../user/pax-manual#<anchor>, while links going to
   ;; the opposite direction will be to
   ;; ../dev/pax-extension-api.html#<anchor>.
   :uri-fragment "doc/user/pax-manual.html"
   :header-fn 'write-html-header
   :footer-fn 'write-html-footer))
```

<a id="x-2840ANTS-DOC-2FBUILDER-3ADOCUMENT-TO-STRING-20FUNCTION-29"></a>

### [function] `DOCUMENT-TO-STRING` DOCUMENT &KEY (FORMAT 'COMMON-HTML:HTML)


Renders given CommonDoc node into the string using specified format.

This function is useful for debugging [`40ANTS-DOC`](#x-28-23A-28-2810-29-20BASE-CHAR-20-2E-20-2240ants-doc-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29) itself.


<a id="x-2840ANTS-DOC-2FBUILDER-3ARENDER-TO-FILES-20FUNCTION-29"></a>

### [function] `RENDER-TO-FILES` SECTIONS &KEY (THEME '40ANTS-DOC/THEMES/DEFAULT:DEFAULT-THEME) (BASE-DIR #P"./") (FORMAT 'COMMON-HTML:HTML)


Renders given sections or pages into a files on disk.

By default, it renders in to `HTML`, but you can specify `FORMAT` argument,
and pass other CommonDoc's format class, supported by the [`40ANTS-DOC`](#x-28-23A-28-2810-29-20BASE-CHAR-20-2E-20-2240ants-doc-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29) system.

Returns an absolute pathname to the output directory as the first value
and pathnames corresponding to each of given sections.


<a id="x-2840ANTS-DOC-2FBUILDER-3A-3A-40GENERATING-DOCUMENTATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Generating Documentation

Two convenience functions are provided to serve the common case of
having an `ASDF` system with some readmes and a directory for the
`HTML` documentation and the default css stylesheet.

<a id="x-2840ANTS-DOC-2FBUILDER-3AUPDATE-ASDF-SYSTEM-HTML-DOCS-20FUNCTION-29"></a>

### [function] `UPDATE-ASDF-SYSTEM-HTML-DOCS` SECTIONS ASDF-SYSTEM &KEY PAGES (TARGET-DIR (ASDF/SYSTEM:SYSTEM-RELATIVE-PATHNAME ASDF-SYSTEM "doc/")) (UPDATE-CSS-P T)


Generate pretty `HTML` documentation for a single `ASDF` system,
possibly linking to github. If `UPDATE-CSS-P`, copy the `CSS` style
sheet to `TARGET-DIR`, as well. Example usage:

```commonlisp
(update-asdf-system-html-docs @manual :40ants-doc)
```
The same, linking to the sources on github:

```commonlisp
(update-asdf-system-html-docs
  @manual :40ants-doc
  :pages
  (list (list :objects (list @manual)
              :source-uri-fn (make-github-source-uri-fn
                               :40ants-doc
                               "https://github.com/40ants/doc"))))
```

<a id="x-2840ANTS-DOC-2FBUILDER-3AUPDATE-ASDF-SYSTEM-README-20FUNCTION-29"></a>

### [function] `UPDATE-ASDF-SYSTEM-README` SECTIONS ASDF-SYSTEM &KEY (FORMAT :MARKDOWN)


Convenience function to generate readme file in the directory
holding the `ASDF-SYSTEM` definition.

By default, `README`.md is generated. It has anchors, links, inline code,
and other markup added. Not necessarily the easiest on the eye in an editor,
but looks good on github.

You can provide `:FORMAT :PLAIN` argument to generate `README` instead.
It will be optimized for reading in text format. Has no links and
cluttery markup.

Example usage:

```
(update-asdf-system-readme @40ants-doc-manual :40ants-doc)
```

<a id="x-2840ANTS-DOC-2FBUILDER-3A-2ADOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL-2A-20-28VARIABLE-29-29"></a>

### [variable] `*DOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL*` NIL


`NIL` or a non-negative integer. If non-`NIL`, it overrides
[`40ANTS-DOC/BUILDER/VARS::*DOCUMENT-MAX-NUMBERING-LEVEL*`](#x-2840ANTS-DOC-2FBUILDER-2FVARS-3A-2ADOCUMENT-MAX-NUMBERING-LEVEL-2A-20-28VARIABLE-29-29) in dynamic `HTML` table of contents on
the left of the page.


<a id="x-2840ANTS-DOC-2FBUILDER-3A-2ADOCUMENT-HTML-TOP-BLOCKS-OF-LINKS-2A-20-28VARIABLE-29-29"></a>

### [variable] `*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS*` NIL


A list of blocks of links to be display on the sidebar on the left,
above the table of contents. A block is of the form
`(&KEY TITLE ID LINKS)`, where `TITLE` will be displayed at the top of the block in a
`HTML` `div` with `id`, followed by the links. `LINKS` is a list
of `(URI LABEL)` elements.`


<a id="x-2840ANTS-DOC-2FBUILDER-3A-2ADOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS-2A-20-28VARIABLE-29-29"></a>

### [variable] `*DOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS*` NIL


Like [`*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS*`](#x-2840ANTS-DOC-2FBUILDER-3A-2ADOCUMENT-HTML-TOP-BLOCKS-OF-LINKS-2A-20-28VARIABLE-29-29), only it is displayed
below the table of contents.


<a id="x-2840ANTS-DOC-2FGITHUB-3A-3A-40GITHUB-WORKFLOW-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Github Workflow

It is generally recommended to commit generated readmes (see
[`40ANTS-DOC/BUILDER::UPDATE-ASDF-SYSTEM-README`](#x-2840ANTS-DOC-2FBUILDER-3AUPDATE-ASDF-SYSTEM-README-20FUNCTION-29)) so that users have something to read
without reading the code and sites like github can display them.

`HTML` documentation can also be committed, but there is an issue with
that: when linking to the sources (see [`MAKE-GITHUB-SOURCE-URI-FN`](#x-2840ANTS-DOC-2FGITHUB-3AMAKE-GITHUB-SOURCE-URI-FN-20FUNCTION-29)),
the commit id is in the link. This means that code changes need to
be committed first, then `HTML` documentation regenerated and
committed in a followup commit.

The second issue is that github is not very good at serving `HTML`s
files from the repository itself (and
[http://htmlpreview.github.io](http://htmlpreview.github.io) chokes
on links to the sources).

The recommended workflow is to use
[gh-pages](https://pages.github.com/), which can be made relatively
painless with the `git workflow` command. The gist of it is to make
the `doc/` directory a checkout of the branch named `gh-pages`. A
good description of this process is
[http://sangsoonam.github.io/2019/02/08/using-git-worktree-to-deploy-github-pages.html](http://sangsoonam.github.io/2019/02/08/using-git-worktree-to-deploy-github-pages.html).
Two commits needed still, but it is somewhat less painful.

This way the `HTML` documentation will be available at
`http://<username>.github.io/<repo-name>`. It is probably a good
idea to add section like the [`Links`](#x-2840ANTS-DOC-2FDOC-3A-3A-40LINKS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29) section to allow jumping
between the repository and the gh-pages site.

<a id="x-2840ANTS-DOC-2FGITHUB-3AMAKE-GITHUB-SOURCE-URI-FN-20FUNCTION-29"></a>

#### [function] `MAKE-GITHUB-SOURCE-URI-FN` ASDF-SYSTEM GITHUB-URI &KEY GIT-VERSION


Return a function suitable as `:SOURCE-URI-FN` of a page spec (see
the `:PAGES` argument of [`40ANTS-DOC/DOCUMENT::DOCUMENT`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-20GENERIC-FUNCTION-29)). The function looks the source
location of the reference passed to it, and if the location is
found, the path is made relative to the root directory of
`ASDF-SYSTEM` and finally an `URI` pointing to github is returned. The
`URI` looks like this:

```text
https://github.com/melisgl/mgl-pax/blob/master/src/pax-early.lisp#L12
```
"master" in the above link comes from `GIT-VERSION`.

If `GIT-VERSION` is `NIL`, then an attempt is made to determine to
current commit id from the `.git` in the directory holding
`ASDF-SYSTEM`. If no `.git` directory is found, then no links to
github will be generated.

A separate warning is signalled whenever source location lookup
fails or if the source location points to a directory not below the
directory of `ASDF-SYSTEM`.


<a id="x-2840ANTS-DOC-2FWORLD-3A-3A-40WORLD-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### PAX World

`MGL-PAX` supported a "World" which was a registry of documents, which can generate
cross-linked `HTML` documentation pages for all the registered
documents.

But I decided to drop this feature for now, because usually build libraries documentation
separately as part of their `CI` pipline.

If somebody want's cross referencing between different libraries, then instead
of building their docs simultaneously, I'd suggest to create an index of entities,
provided by libraries and to store them as a `JSON` file along with a library documentation.

This way it will be possible to enumerate such sources of cross references as usual `URL`s.

Such feature is not implemented in the [`40ANTS-DOC`](#x-28-23A-28-2810-29-20BASE-CHAR-20-2E-20-2240ants-doc-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29) system yet, but probably it will be
useful for libraries built around the [Weblocks](https://40ants.com/weblocks/).
If you want to help and implement the feature, please, let me know.

<a id="x-2840ANTS-DOC-2FMARKDOWN-3A-3A-40MARKDOWN-SUPPORT-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Markdown Support

The Markdown in docstrings is processed with the
`3BMD` library.

<a id="x-2840ANTS-DOC-2FMARKDOWN-3A-3A-40MARKDOWN-INDENTATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Indentation

Docstrings can be indented in any of the usual styles.
[`40ANTS-DOC`](#x-28-23A-28-2810-29-20BASE-CHAR-20-2E-20-2240ants-doc-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29) normalizes indentation by converting:

```text
(defun foo ()
  "This is
```
indented
differently")

to

```text
(defun foo ()
  "This is
```
indented
differently")

See `40ANTS-DOC/DOCUMENT::DOCUMENT-OBJECT` ([`1`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20T-29-29-29) [`2`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-28STRING-20T-29-29-29) [`3`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20GENERIC-FUNCTION-29)) for the details.

<a id="x-2840ANTS-DOC-2FMARKDOWN-3A-3A-40MARKDOWN-SYNTAX-HIGHLIGHTING-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Syntax highlighting

For syntax highlighting, github's [fenced code
blocks](https://help.github.com/articles/github-flavored-markdown#fenced-code-blocks) markdown extension to mark up code
blocks with triple backticks is enabled so all you need to do is
write:

```text
```elisp
(defun foo ())
```
```
to get syntactically marked up `HTML` output. Copy `src/style.css`
from [`40ANTS-DOC`](#x-28-23A-28-2810-29-20BASE-CHAR-20-2E-20-2240ants-doc-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29) and you are set. The language tag, `elisp` in this example,
is optional and defaults to `common-lisp`.

See the documentation of [`3BMD`](https://github.com/3b/3bmd) and [colorize](https://github.com/redline6561/colorize/) for
the details.

<a id="x-2840ANTS-DOC-2FMARKDOWN-3A-3A-40MATHJAX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### MathJax

Displaying pretty mathematics in TeX format is supported via
MathJax. It can be done inline with `$` like this:

```text
$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$
```
which is diplayed as $\int_0^\infty e^{-x^2}
dx=\frac{\sqrt{\pi}}{2}$, or it can be delimited by `$$` like this:

```text
$$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$
```
to get: $$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$

MathJax will leave code blocks (including those inline with
backticks) alone. Outside code blocks, escape `$` by prefixing it
with a backslash to scare MathJax off.

Escaping all those backslashes in TeX fragments embedded in Lisp
strings can be a pain. [Pythonic String
Reader](https://github.com/smithzvk/pythonic-string-reader) can help
with that.

<a id="x-2840ANTS-DOC-2FDOC-3A-3A-40DOCUMENTATION-PRINTER-VARIABLES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Documentation Printer Variables

Docstrings are assumed to be in markdown format and they are pretty
much copied verbatim to the documentation subject to a few knobs
described below.

<a id="x-2840ANTS-DOC-2FBUILDER-2FPRINTER-3A-2ADOCUMENT-UPPERCASE-IS-CODE-2A-20-28VARIABLE-29-29"></a>

### [variable] `*DOCUMENT-UPPERCASE-IS-CODE*` T


When true, words with at least three characters and no lowercase
characters naming an interned symbol are assumed to be code as if
they were marked up with backticks which is especially useful when
combined with [`40ANTS-DOC/LINK::*DOCUMENT-LINK-CODE*`](#x-2840ANTS-DOC-2FLINK-3A-2ADOCUMENT-LINK-CODE-2A-20-28VARIABLE-29-29). For example, this docstring:

```text
"`FOO` and FOO."
```
is equivalent to this:

```text
"`FOO` and `FOO`."
```
if `FOO` is an interned symbol. To suppress this behavior, add a
backslash to the beginning of the symbol or right after the leading
* if it would otherwise be parsed as markdown emphasis:

```text
"\\40ANTS-DOC *\\DOCUMENT-NORMALIZE-PACKAGES*"
```
The number of backslashes is doubled above because that's how the
example looks in a docstring. Note that the backslash is discarded
even if [`*DOCUMENT-UPPERCASE-IS-CODE*`](#x-2840ANTS-DOC-2FBUILDER-2FPRINTER-3A-2ADOCUMENT-UPPERCASE-IS-CODE-2A-20-28VARIABLE-29-29) is false.


<a id="x-2840ANTS-DOC-2FBUILDER-2FPRINTER-3A-2ADOCUMENT-DOWNCASE-UPPERCASE-CODE-2A-20-28VARIABLE-29-29"></a>

### [variable] `*DOCUMENT-DOWNCASE-UPPERCASE-CODE*` NIL


If true, then the names of symbols recognized as code (including
those found if [`*DOCUMENT-UPPERCASE-IS-CODE*`](#x-2840ANTS-DOC-2FBUILDER-2FPRINTER-3A-2ADOCUMENT-UPPERCASE-IS-CODE-2A-20-28VARIABLE-29-29)) are downcased in the
output if they only consist of uppercase characters. If it is
`:ONLY-IN-MARKUP`, then if the output format does not support
markup (e.g. it's `:PLAIN`), then no downcasing is performed.


<a id="x-2840ANTS-DOC-2FBUILDER-2FPRINTER-3A-2ADOCUMENT-NORMALIZE-PACKAGES-2A-20-28VARIABLE-29-29"></a>

### [variable] `*DOCUMENT-NORMALIZE-PACKAGES*` T


If true, symbols are printed relative to [`40ANTS-DOC::SECTION-PACKAGE`](#x-2840ANTS-DOC-3ASECTION-PACKAGE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-DOC-3ASECTION-29-29) of the
innermost containing section or with full package names if there is
no containing section. To eliminate ambiguity `[in package ...]`
messages are printed right after the section heading if necessary.
If false, symbols are always printed relative to the current
package.


<a id="x-2840ANTS-DOC-2FLINK-3A-2ADOCUMENT-LINK-CODE-2A-20-28VARIABLE-29-29"></a>

### [variable] `*DOCUMENT-LINK-CODE*` T


When true, during the process of generating documentation for a
[`40ANTS-DOC::SECTION`](#x-2840ANTS-DOC-3ASECTION-20CLASS-29) class, `HTML` anchors are added before the documentation of
every reference that's not to a section. Also, markdown style
reference links are added when a piece of inline code found in a
docstring refers to a symbol that's referenced by one of the
sections being documented. Assuming `BAR` is defined, the
documentation for:

```commonlisp
(defsection @foo
  (foo function)
  (bar function))

(defun foo (x)
  "Calls `BAR` on `X`."
  (bar x))
```
would look like this:

```text
- [function] FOO X

    Calls [`BAR`][1] on `X`.
```
Instead of `BAR`, one can write `[bar][]` or `[`bar`][]` as well.
Since symbol names are parsed according to `READTABLE-CASE`, character
case rarely matters.

Now, if `BAR` has references with different locatives:

```commonlisp
(defsection @foo
  (foo function)
  (bar function)
  (bar type))

(defun foo (x)
  "Calls `BAR` on `X`."
  (bar x))
```
then documentation would link to all interpretations:

```text
- [function] FOO X

    Calls `BAR`([`1`][link-id-1] [`2`][link-id-2]) on `X`.
```
This situation occurs with [`40ANTS-DOC::SECTION`](#x-2840ANTS-DOC-3ASECTION-20CLASS-29) which is both a class (see
[`40ANTS-DOC::SECTION`](#x-2840ANTS-DOC-3ASECTION-20CLASS-29) class) and a locative type denoted by a symbol (see
[`40ANTS-DOC/LOCATIVES:SECTION`](#x-2840ANTS-DOC-2FLOCATIVES-3ASECTION-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) locative). Back in the example above, clearly,
there is no reason to link to type `BAR`, so one may wish to select
the function locative. There are two ways to do that. One is to
specify the locative explicitly as the id of a reference link:

```text
"Calls [BAR][function] on X."
```
However, if in the text there is a locative immediately before or
after the symbol, then that locative is used to narrow down the
range of possibilities. This is similar to what the `M-.` extension
does. In a nutshell, if `M-.` works without questions then the
documentation will contain a single link. So this also works without
any markup:

```text
"Calls function `BAR` on X."
```
This last option needs backticks around the locative if it's not a
single symbol.

Note that [`*DOCUMENT-LINK-CODE*`](#x-2840ANTS-DOC-2FLINK-3A-2ADOCUMENT-LINK-CODE-2A-20-28VARIABLE-29-29) can be combined with
[`40ANTS-DOC/BUILDER/PRINTER::*DOCUMENT-UPPERCASE-IS-CODE*`](#x-2840ANTS-DOC-2FBUILDER-2FPRINTER-3A-2ADOCUMENT-UPPERCASE-IS-CODE-2A-20-28VARIABLE-29-29) to have links generated for
uppercase names with no quoting required.


<a id="x-2840ANTS-DOC-2FLINK-3A-2ADOCUMENT-LINK-SECTIONS-2A-20-28VARIABLE-29-29"></a>

### [variable] `*DOCUMENT-LINK-SECTIONS*` T


When true, `HTML` anchors are generated before the heading of
sections which allows the table of contents to contain links and
also code-like references to sections (like `@FOO-MANUAL`) to be
translated to links with the section title being the name of the
link.


<a id="x-2840ANTS-DOC-2FLINK-3A-2ADOCUMENT-MIN-LINK-HASH-LENGTH-2A-20-28VARIABLE-29-29"></a>

### [variable] `*DOCUMENT-MIN-LINK-HASH-LENGTH*` 4


Recall that markdown reference style links (like `[label][id]`) are
used for linking to sections and code. It is desirable to have ids
that are short to maintain legibility of the generated markdown, but
also stable to reduce the spurious diffs in the generated
documentation which can be a pain in a version control system.

Clearly, there is a tradeoff here. This variable controls how many
characters of the md5 sum of the full link id (the reference as a
string) are retained. If collisions are found due to the low number
of characters, then the length of the hash of the colliding
reference is increased.

This variable has no effect on the `HTML` generated from markdown, but
it can make markdown output more readable.


<a id="x-2840ANTS-DOC-2FBUILDER-2FVARS-3A-2ADOCUMENT-MARK-UP-SIGNATURES-2A-20-28VARIABLE-29-29"></a>

### [variable] `*DOCUMENT-MARK-UP-SIGNATURES*` T


When true, some things such as function names and arglists are
rendered as bold and italic. In `:HTML` output, locative types become
links to sources (if `:SOURCE-URI-FN` is provided, see [`40ANTS-DOC/DOCUMENT::DOCUMENT`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-20GENERIC-FUNCTION-29)), and
the symbol becomes a self-link for your permalinking pleasure.

For example, a reference is rendered in markdown roughly as:

```text
- [function] foo x y
```
With this option on, the above becomes:

```text
- [function] **foo** *x y*
```
Also, in `HTML` `**foo**` will be a link to that very entry and
`[function]` may turn into a link to sources.


<a id="x-2840ANTS-DOC-2FBUILDER-2FVARS-3A-2ADOCUMENT-MAX-NUMBERING-LEVEL-2A-20-28VARIABLE-29-29"></a>

### [variable] `*DOCUMENT-MAX-NUMBERING-LEVEL*` 3


A non-negative integer. In their hierarchy, sections on levels less
than this value get numbered in the format of `3.1.2`. Setting it to
0 turns numbering off.


<a id="x-2840ANTS-DOC-2FBUILDER-2FVARS-3A-2ADOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL-2A-20-28VARIABLE-29-29"></a>

### [variable] `*DOCUMENT-MAX-TABLE-OF-CONTENTS-LEVEL*` 3


A non-negative integer. Top-level sections are given a table of
contents which includes a nested tree of section titles whose depth
is limited by this value. Setting it to 0 turns generation of the
table of contents off. If [`40ANTS-DOC/LINK::*DOCUMENT-LINK-SECTIONS*`](#x-2840ANTS-DOC-2FLINK-3A-2ADOCUMENT-LINK-SECTIONS-2A-20-28VARIABLE-29-29) is true, then the
table of contents will link to the sections.


<a id="x-2840ANTS-DOC-2FBUILDER-2FVARS-3A-2ADOCUMENT-TEXT-NAVIGATION-2A-20-28VARIABLE-29-29"></a>

### [variable] `*DOCUMENT-TEXT-NAVIGATION*` NIL


If true, then before each heading a line is printed with links to
the previous, parent and next section. Needs
[`40ANTS-DOC/LINK::*DOCUMENT-LINK-SECTIONS*`](#x-2840ANTS-DOC-2FLINK-3A-2ADOCUMENT-LINK-SECTIONS-2A-20-28VARIABLE-29-29) to be on to work.


<a id="x-2840ANTS-DOC-2FBUILDER-2FVARS-3A-2ADOCUMENT-FANCY-HTML-NAVIGATION-2A-20-28VARIABLE-29-29"></a>

### [variable] `*DOCUMENT-FANCY-HTML-NAVIGATION*` T


If true and the output format is `HTML`, then headings get a
navigation component that consists of links to the previous, parent,
next section and a permalink. This component is normally hidden, it
is visible only when the mouse is over the heading. Needs
[`40ANTS-DOC/LINK::*DOCUMENT-LINK-SECTIONS*`](#x-2840ANTS-DOC-2FLINK-3A-2ADOCUMENT-LINK-SECTIONS-2A-20-28VARIABLE-29-29) to be on to work.


<a id="x-2840ANTS-DOC-2FDOC-3A-3A-40LOCATIVE-TYPES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Locative Types

These are the locatives type supported out of the box. As all
locative types, they are symbols and their names should make it
obvious what kind of things they refer to. Unless otherwise noted,
locatives take no arguments.

<a id="x-28ASDF-2FSYSTEM-3ASYSTEM-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `SYSTEM`


Refers to an asdf system. The generated documentation will include
meta information extracted from the system definition. This also
serves as an example of a symbol that's not accessible in the
current package and consequently is not exported.


<a id="x-2840ANTS-DOC-2FLOCATIVES-3ASECTION-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `SECTION`


Refers to a section defined by [`40ANTS-DOC::DEFSECTION`](#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29).


<a id="x-28VARIABLE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `VARIABLE` &OPTIONAL INITFORM


Refers to a global special variable. `INITFORM`, or if not specified,
the global value of the variable is included in the documentation.


<a id="x-2840ANTS-DOC-2FLOCATIVES-3ACONSTANT-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `CONSTANT` &OPTIONAL INITFORM


Refers to a `DEFCONSTANT`. `INITFORM`, or if not specified,
the value of the constant is included in the documentation.


<a id="x-2840ANTS-DOC-2FLOCATIVES-3AMACRO-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `MACRO`



<a id="x-28COMPILER-MACRO-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `COMPILER-MACRO`



<a id="x-28FUNCTION-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `FUNCTION`


Note that the arglist in the generated documentation depends on
the quality of `SWANK-BACKEND:ARGLIST`. It may be that default
values of optional and keyword arguments are missing.


<a id="x-28GENERIC-FUNCTION-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `GENERIC-FUNCTION`



<a id="x-28METHOD-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `METHOD` METHOD-QUALIFIERS METHOD-SPECIALIZERS


See `CL:FIND-METHOD` for the description of the arguments.
To refer to the default method of the three argument generic
function `FOO`:

```text
(foo (method () (t t t)))
```

<a id="x-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `ACCESSOR` CLASS-NAME


To refer to an accessor named `FOO-SLOT` of class
`FOO`:

```text
(foo-slot (accessor foo))
```

<a id="x-2840ANTS-DOC-2FLOCATIVES-3AREADER-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `READER` CLASS-NAME


To refer to a reader named `FOO-SLOT` of class
`FOO`:

```text
(foo-slot (reader foo))
```

<a id="x-2840ANTS-DOC-2FLOCATIVES-3AWRITER-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `WRITER` CLASS-NAME


To refer to a writer named `FOO-SLOT` of class
`FOO`:

```text
(foo-slot (writer foo))
```

<a id="x-2840ANTS-DOC-2FLOCATIVES-3ASTRUCTURE-ACCESSOR-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `STRUCTURE-ACCESSOR`


This is a synonym of [`FUNCTION`](#x-28FUNCTION-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) with the difference that the often
ugly and certainly uninformative lambda list will not be printed.


<a id="x-28CLASS-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `CLASS`



<a id="x-28CONDITION-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `CONDITION`



<a id="x-28TYPE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `TYPE`


[`TYPE`](#x-28TYPE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) can refer to classes as well, but it's better style to use the
more specific [`CLASS`](#x-28CLASS-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) locative type for that. Another difference to
[`CLASS`](#x-28CLASS-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) is that an attempt is made at printing the arguments of type
specifiers.


<a id="x-28PACKAGE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `PACKAGE`



<a id="x-2840ANTS-DOC-2FLOCATIVES-3ADISLOCATED-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `DISLOCATED`


Refers to a symbol in a non-specific context. Useful for preventing
autolinking. For example, if there is a function called `FOO` then

```text
`FOO`
```
will be linked to (if [`40ANTS-DOC/LINK::*DOCUMENT-LINK-CODE*`](#x-2840ANTS-DOC-2FLINK-3A-2ADOCUMENT-LINK-CODE-2A-20-28VARIABLE-29-29)) its definition. However,

```text
[`FOO`][dislocated]
```
will not be. On a dislocated locative function [`40ANTS-DOC/LOCATIVES/BASE::LOCATE`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-20FUNCTION-29) always fails with a
`40ANTS-DOC/LOCATIVES/BASE:LOCATE-ERROR` ([`1`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-ERROR-20FUNCTION-29) [`2`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-ERROR-20CONDITION-29)) condition.


<a id="x-2840ANTS-DOC-2FLOCATIVES-3AARGUMENT-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `ARGUMENT`


An alias for [`40ANTS-DOC/LOCATIVES::DISLOCATED`](#x-2840ANTS-DOC-2FLOCATIVES-3ADISLOCATED-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29), so the one can refer to an argument of a
macro without accidentally linking to a class that has the same name
as that argument. In the following example, `FORMAT` may link to
`CL:FORMAT` (if we generated documentation for it):

```
"See the FORMAT in DOCUMENT."
```
Since [`ARGUMENT`](#x-2840ANTS-DOC-2FLOCATIVES-3AARGUMENT-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) is a locative, we can prevent that linking by writing:

```
"See the FORMAT argument of DOCUMENT."
```

<a id="x-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `LOCATIVE` LAMBDA-LIST


This is the locative for locatives. When `M-.` is pressed on
[`VARIABLE`](#x-28VARIABLE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) in `([VARIABLE](#x-28VARIABLE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) [LOCATIVE](#x-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29))`, this is what makes it possible
to land at the `([40ANTS-DOC/LOCATIVES/BASE:DEFINE-LOCATIVE-TYPE](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ADEFINE-LOCATIVE-TYPE-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29) [VARIABLE](#x-28VARIABLE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) ...)` form.
Similarly, `([LOCATIVE](#x-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) [LOCATIVE](#x-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29))` leads to this very definition.


<a id="x-2840ANTS-DOC-2FLOCATIVES-3AINCLUDE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `INCLUDE` SOURCE &KEY LINE-PREFIX HEADER FOOTER HEADER-NL FOOTER-NL


Refers to a region of a file. `SOURCE` can be a string or a
pathname in which case the whole file is being pointed to or it can
explicitly supply `START`, `END` locatives. [`INCLUDE`](#x-2840ANTS-DOC-2FLOCATIVES-3AINCLUDE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) is typically used to
include non-lisp files in the documentation (say markdown or elisp
as in the next example) or regions of lisp source files. This can
reduce clutter and duplication.

```commonlisp
(defsection example-section ()
  (pax.el (include #.(asdf:system-relative-pathname :40ants-doc "elisp/pax.el")
                   :header-nl "```elisp" :footer-nl "```"))
  (foo-example (include (:start (foo function)
                         :end (end-of-foo-example variable))
                        :header-nl "```commonlisp"
                        :footer-nl "```"))

(defun foo (x)
  (1+ x))

;;; Since file regions are copied verbatim, comments survive.
(defmacro bar ())

;;; This comment is the last thing in FOO-EXAMPLE's
;;; documentation since we use the dummy END-OF-FOO-EXAMPLE
;;; variable to mark the end location.
(defvar end-of-foo-example)

;;; More irrelevant code follows.
```
In the above example, pressing `M-.` on `pax.el` will open the
`src/pax.el` file and put the cursor on its first character. `M-.`
on `FOO-EXAMPLE` will go to the source location of the `(asdf:system
locative)` locative.

When documentation is generated, the entire `pax.el` file is
included in the markdown surrounded by the strings given as
`HEADER-NL` and `FOOTER-NL` (if any). The trailing newline character is
assumed implicitly. If that's undesirable, then use `HEADER` and
`FOOTER` instead. The documentation of `FOO-EXAMPLE` will be the
region of the file from the source location of the `START`
locative (inclusive) to the source location of the `END`
locative (exclusive). `START` and `END` default to the beginning and end
of the file, respectively.

Note that the file of the source location of `:START` and `:END` must be
the same. If `SOURCE` is pathname designator, then it must be absolute
so that the locative is context independent.

Finally, if specified `LINE-PREFIX` is a string that's prepended to
each line included in the documentation. For example, a string of
four spaces makes markdown think it's a code block.


<a id="x-2840ANTS-DOC-2FRESTART-3ADEFINE-RESTART-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

### [macro] `DEFINE-RESTART`


A definer macro to hang the documentation of a restart on a
symbol.

```
(define-restart my-ignore-error ()
  "Available when MY-ERROR is signalled, MY-IGNORE-ERROR unsafely continues.")
```
Note that while there is a `CL:RESTART` class, there is no
corresponding source location or docstring like for [`CONDITION`](#x-28CONDITION-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29)s.


<a id="x-28RESTART-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `RESTART`



<a id="x-2840ANTS-DOC-2FGLOSSARY-3ADEFINE-GLOSSARY-TERM-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

### [macro] `DEFINE-GLOSSARY-TERM`


Define a global variable with `NAME` and set it to a glossary term
object. A glossary term is just a symbol to hang a docstring on. It
is a bit like a [`40ANTS-DOC::SECTION`](#x-2840ANTS-DOC-3ASECTION-20CLASS-29) in that, when linked to, its `TITLE` will be
the link text instead of the name of the symbol. Unlike sections
though, glossary terms are not rendered with headings, but in the
more lightweight bullet + locative + name/title style.

When `DISCARD-DOCUMENTATION-P` (defaults to [`40ANTS-DOC::*DISCARD-DOCUMENTATION-P*`](#x-2840ANTS-DOC-3A-2ADISCARD-DOCUMENTATION-P-2A-20-28VARIABLE-29-29))
is true, `DOCSTRING` will not be recorded to save memory.


<a id="x-2840ANTS-DOC-2FLOCATIVES-3AGLOSSARY-TERM-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

### [locative] `GLOSSARY-TERM`


Refers to a glossary term defined by [`40ANTS-DOC/GLOSSARY::DEFINE-GLOSSARY-TERM`](#x-2840ANTS-DOC-2FGLOSSARY-3ADEFINE-GLOSSARY-TERM-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29).


<a id="x-2840ANTS-DOC-2FDOC-3A-3A-40EXTENSION-API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Extension API

<a id="x-2840ANTS-DOC-2FDOC-3A-3A-40LOCATIVES-AND-REFERENCES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Locatives and References

While Common Lisp has rather good introspective abilities, not
everything is first class. For example, there is no object
representing the variable defined with `(DEFVAR
FOO)`. `([40ANTS-DOC/REFERENCE:MAKE-REFERENCE](#x-2840ANTS-DOC-2FREFERENCE-3AMAKE-REFERENCE-20FUNCTION-29) 'FOO '[VARIABLE](#x-28VARIABLE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29))` constructs a [`40ANTS-DOC/REFERENCE::REFERENCE`](#x-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20CLASS-29) that
captures the path to take from an object (the symbol `FOO`) to an
entity of interest (for example, the documentation of the variable).
The path is called the locative. A locative can be applied to an
object like this:

```
(locate 'foo 'variable)
```
which will return the same reference as `([40ANTS-DOC/REFERENCE:MAKE-REFERENCE](#x-2840ANTS-DOC-2FREFERENCE-3AMAKE-REFERENCE-20FUNCTION-29) 'FOO
'[VARIABLE](#x-28VARIABLE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29))`. Operations need to know how to deal with references
which we will see in `40ANTS-DOC/LOCATIVES/BASE::LOCATE-AND-COLLECT-REACHABLE-OBJECTS` ([`1`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-AND-COLLECT-REACHABLE-OBJECTS-20-28METHOD-20NIL-20-28T-20T-20T-29-29-29) [`2`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-AND-COLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29)),
[`40ANTS-DOC/LOCATIVES/BASE::LOCATE-AND-DOCUMENT`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-AND-DOCUMENT-20GENERIC-FUNCTION-29) and `40ANTS-DOC/LOCATIVES/BASE::LOCATE-AND-FIND-SOURCE` ([`1`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-AND-FIND-SOURCE-20-28METHOD-20NIL-20-28T-20T-20T-29-29-29) [`2`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-AND-FIND-SOURCE-20GENERIC-FUNCTION-29)).

Naturally, `([40ANTS-DOC/LOCATIVES/BASE:LOCATE](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-20FUNCTION-29) 'FOO '[FUNCTION](#x-28FUNCTION-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29))` will simply return `#'FOO`, no
need to muck with references when there is a perfectly good object.

<a id="x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-20FUNCTION-29"></a>

#### [function] `LOCATE` OBJECT LOCATIVE &KEY (ERRORP T)


Follow `LOCATIVE` from `OBJECT` and return the object it leads to or a
[`40ANTS-DOC/REFERENCE::REFERENCE`](#x-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20CLASS-29) if there is no first class object corresponding to the
location. If `ERRORP`, then a `LOCATE-ERROR` ([`1`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-ERROR-20FUNCTION-29) [`2`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-ERROR-20CONDITION-29)) condition is signaled when
the lookup fails.


<a id="x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-ERROR-20CONDITION-29"></a>

#### [condition] `LOCATE-ERROR` (ERROR)


Signaled by [`LOCATE`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-20FUNCTION-29) when the lookup fails and `ERRORP`
is true.


<a id="x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-ERROR-MESSAGE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-ERROR-29-29"></a>

#### [reader] `LOCATE-ERROR-MESSAGE` (LOCATE-ERROR) (:MESSAGE)



<a id="x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-ERROR-OBJECT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-ERROR-29-29"></a>

#### [reader] `LOCATE-ERROR-OBJECT` (LOCATE-ERROR) (:OBJECT)



<a id="x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-ERROR-LOCATIVE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-ERROR-29-29"></a>

#### [reader] `LOCATE-ERROR-LOCATIVE` (LOCATE-ERROR) (:LOCATIVE)



<a id="x-2840ANTS-DOC-2FREFERENCE-3ARESOLVE-20FUNCTION-29"></a>

#### [function] `RESOLVE` REFERENCE &KEY (ERRORP T)


A convenience function to [`40ANTS-DOC/LOCATIVES/BASE::LOCATE`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-20FUNCTION-29) [`REFERENCE`](#x-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20CLASS-29)'s object with its
locative.


<a id="x-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20CLASS-29"></a>

#### [class] `REFERENCE` ()


A [`REFERENCE`](#x-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20CLASS-29) represents a path ([`REFERENCE-LOCATIVE`](#x-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-LOCATIVE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-DOC-2FREFERENCE-3AREFERENCE-29-29))
to take from an object ([`REFERENCE-OBJECT`](#x-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-OBJECT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-DOC-2FREFERENCE-3AREFERENCE-29-29)).


<a id="x-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-OBJECT-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-DOC-2FREFERENCE-3AREFERENCE-29-29"></a>

#### [reader] `REFERENCE-OBJECT` (REFERENCE) (:OBJECT)



<a id="x-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-LOCATIVE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-DOC-2FREFERENCE-3AREFERENCE-29-29"></a>

#### [reader] `REFERENCE-LOCATIVE` (REFERENCE) (:LOCATIVE)



<a id="x-2840ANTS-DOC-2FREFERENCE-3AMAKE-REFERENCE-20FUNCTION-29"></a>

#### [function] `MAKE-REFERENCE` OBJECT LOCATIVE



<a id="x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATIVE-TYPE-20FUNCTION-29"></a>

#### [function] `LOCATIVE-TYPE` LOCATIVE


The first element of `LOCATIVE` if it's a list. If it's a symbol then
it's that symbol itself. Typically, methods of generic functions
working with locatives take locative type and locative args as
separate arguments to allow methods have eql specializers on the
type symbol.


<a id="x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATIVE-ARGS-20FUNCTION-29"></a>

#### [function] `LOCATIVE-ARGS` LOCATIVE


The `REST` of `LOCATIVE` if it's a list. If it's a symbol then
it's ().


<a id="x-2840ANTS-DOC-2FDOC-3A-3A-40NEW-OBJECT-TYPES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Adding New Object Types

One may wish to make the [`40ANTS-DOC/DOCUMENT::DOCUMENT`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-20GENERIC-FUNCTION-29) generic-function and `M-.` navigation
work with new object types. Extending [`40ANTS-DOC/DOCUMENT::DOCUMENT`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-20GENERIC-FUNCTION-29) can be done by
defining a method of [`40ANTS-DOC/DOCUMENT::DOCUMENT-OBJECT`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20GENERIC-FUNCTION-29) generic-function. To allow these objects to be
referenced from [`DEFSECTION`](#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29), a method of [`40ANTS-DOC/LOCATIVES/BASE::LOCATE-OBJECT`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-OBJECT-20GENERIC-FUNCTION-29) generic-function is to be defined.
Finally, for `M-.` `40ANTS-DOC/SOURCE-API::FIND-SOURCE` ([`1`](#x-2840ANTS-DOC-2FSOURCE-API-3AFIND-SOURCE-20-28METHOD-20NIL-20-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-29-29-29) [`2`](#x-2840ANTS-DOC-2FSOURCE-API-3AFIND-SOURCE-20GENERIC-FUNCTION-29)) can be specialized. Finally,
[`40ANTS-DOC::EXPORTABLE-LOCATIVE-TYPE-P`](#x-2840ANTS-DOC-3AEXPORTABLE-LOCATIVE-TYPE-P-20GENERIC-FUNCTION-29) may be overridden if exporting does not
makes sense. Here is a stripped down example of how all this is done
for [`ASDF:SYSTEM`](#x-28ASDF-2FSYSTEM-3ASYSTEM-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29):

```
(define-locative-type asdf:system ()
  "Refers to an asdf system. The generated documentation will include
  meta information extracted from the system definition. This also
  serves as an example of a symbol that's not accessible in the
  current package and consequently is not exported.")

(defmethod locate-object (symbol (locative-type (eql 'asdf:system))
                          locative-args)
  (assert (endp locative-args))
  ;; FIXME: This is slow as hell.
  (or (asdf:find-system symbol nil)
      (locate-error)))

(defmethod canonical-reference ((system asdf:system))
  (40ants-doc/reference::make-reference (asdf::primary-system-name system) 'asdf:system))

(defmethod document-object ((system asdf:system) stream)
  (40ants-doc/builder/heading::with-heading (stream system
                                             (format nil "~A ASDF System Details"
                                                     (string-upcase
                                                      (asdf::primary-system-name system))))
    (flet ((foo (name fn &key type)
             (let ((value (funcall fn system)))
               (when value
                 (case type
                   ((:link)
                    (format stream "- ~A: [~A](~A)~%" name value value))
                   ((:mailto)
                    (format stream "- ~A: [~A](mailto:~A)~%"
                            name value value))
                   ((:source-control)
                    (format stream "- ~A: [~A](~A)"
                            name (first value) (second value)))
                   ((nil)
                    (format stream "- ~A: ~A~%" name value)))))))
      (foo "Version" 'asdf/component:component-version)
      (foo "Description" 'asdf/system:system-description)
      (foo "Licence" 'asdf/system:system-licence)
      (foo "Author" 'asdf/system:system-author)
      (foo "Maintainer" 'asdf/system:system-maintainer)
      (foo "Mailto" 'asdf/system:system-mailto :type :mailto)
      (foo "Homepage" 'asdf/system:system-homepage :type :link)
      (foo "Bug tracker" 'asdf/system:system-bug-tracker :type :link)
      (foo "Source control" 'asdf/system:system-source-control
           :type :source-control)
      (terpri stream))))


(defmethod 40ants-doc/commondoc/builder:to-commondoc ((system asdf:system))
  (let ((title (format nil "~A ASDF System Details"
                       (string-upcase
                        (asdf::primary-system-name system)))))
    (flet ((item (name getter &key type)
             (let* ((value (funcall getter system))
                    (href nil))
               (when value
                 (case type
                   (:link (setf href value))
                   (:mailto (setf href (format nil "mailto:~A"
                                               value)))
                   (:source-control (psetf value (format nil "~A"
                                                         (first value))
                                           href (second value))))
                 (make-list-item
                  (make-paragraph
                   (if href
                       (make-content
                        (list (make-text
                               (format nil "~A: "
                                       name))
                              (make-web-link href
                                             (make-text value))))
                       (make-text
                        (format nil "~A: ~A"
                                name
                                value)))))))))
      
      (let* ((items (list (item "Version" 'asdf/component:component-version)
                          (item "Description" 'asdf/system:system-description)
                          (item "Licence" 'asdf/system:system-licence)
                          (item "Author" 'asdf/system:system-author)
                          (item "Maintainer" 'asdf/system:system-maintainer)
                          (item "Mailto" 'asdf/system:system-mailto
                                :type :mailto)
                          (item "Homepage" 'asdf/system:system-homepage
                                :type :link)
                          (item "Bug tracker" 'asdf/system:system-bug-tracker
                                :type :link)
                          (item "Source control" 'asdf/system:system-source-control
                                :type :source-control)))
             (children (make-unordered-list
                        (remove nil items)))
             (reference (40ants-doc/reference-api::canonical-reference system)))
        (make-section-with-reference title
                                     children
                                     reference)))))

(defmethod find-source ((system asdf:system))
  `(:location
    (:file ,(namestring (asdf/system:system-source-file system)))
    (:position 1)
    (:snippet "")))

```
<a id="x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ADEFINE-LOCATIVE-TYPE-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

#### [macro] `DEFINE-LOCATIVE-TYPE`


Declare [`LOCATIVE-TYPE`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATIVE-TYPE-20FUNCTION-29) as a locative. One gets two
things in return: first, a place to document the format and
semantics of [`LOCATIVE-TYPE`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATIVE-TYPE-20FUNCTION-29) (in `LAMBDA-LIST` and `DOCSTRING`); second,
being able to reference `([LOCATIVE-TYPE](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATIVE-TYPE-20FUNCTION-29) LOCATIVE)`. For example, if
you have:

```common-lisp
(define-locative-type variable (&optional initform)
  "Dummy docstring.")
```
then `([VARIABLE](#x-28VARIABLE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) LOCATIVE)` refers to this form.


<a id="x-2840ANTS-DOC-3AEXPORTABLE-LOCATIVE-TYPE-P-20GENERIC-FUNCTION-29"></a>

#### [generic-function] `EXPORTABLE-LOCATIVE-TYPE-P` LOCATIVE-TYPE


Return true iff symbols in references with
`LOCATIVE-TYPE` are to be exported by default when they occur in a
[`DEFSECTION`](#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29). The default method returns T, while the methods for
[`PACKAGE`](#x-28PACKAGE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29), [`ASDF:SYSTEM`](#x-28ASDF-2FSYSTEM-3ASYSTEM-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) and [`METHOD`](#x-28METHOD-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) return `NIL`.

[`DEFSECTION`](#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29) calls this function to decide what symbols to export when
its `EXPORT` argument is true.


<a id="x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-OBJECT-20GENERIC-FUNCTION-29"></a>

#### [generic-function] `LOCATE-OBJECT` OBJECT LOCATIVE-TYPE LOCATIVE-ARGS


Return the object, to which `OBJECT` and the locative
refer. For example, if [`LOCATIVE-TYPE`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATIVE-TYPE-20FUNCTION-29) is the symbol [`PACKAGE`](#x-28PACKAGE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29), this
returns `(FIND-PACKAGE SYMBOL)`. Signal a `LOCATE-ERROR` ([`1`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-ERROR-20FUNCTION-29) [`2`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-ERROR-20CONDITION-29)) condition by
calling the [`LOCATE-ERROR`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-ERROR-20FUNCTION-29) function if the lookup fails. Signal other
errors if the types of the argument are bad, for instance
[`LOCATIVE-ARGS`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATIVE-ARGS-20FUNCTION-29) is not the empty list in the package example. If a
[`40ANTS-DOC/REFERENCE::REFERENCE`](#x-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20CLASS-29) is returned then it must be canonical in the sense that
calling [`40ANTS-DOC/REFERENCE-API::CANONICAL-REFERENCE`](#x-2840ANTS-DOC-2FREFERENCE-API-3ACANONICAL-REFERENCE-20GENERIC-FUNCTION-29) on it will return the same reference.
For extension only, don't call this directly.


<a id="x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-ERROR-20FUNCTION-29"></a>

#### [function] `LOCATE-ERROR` &REST FORMAT-AND-ARGS


Call this function to signal a `LOCATE-ERROR` ([`1`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-ERROR-20FUNCTION-29) [`2`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-ERROR-20CONDITION-29)) condition from a
[`LOCATE-OBJECT`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-OBJECT-20GENERIC-FUNCTION-29) generic-function. `FORMAT-AND-ARGS` contains a format string and
args suitable for `FORMAT` from which the [`LOCATE-ERROR-MESSAGE`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-ERROR-MESSAGE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-ERROR-29-29) is
constructed. If `FORMAT-AND-ARGS` is `NIL`, then the message will be `NIL`
too.

The object and the locative are not specified, they are added by
[`LOCATE`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-20FUNCTION-29) when it resignals the condition.


<a id="x-2840ANTS-DOC-2FREFERENCE-API-3ACANONICAL-REFERENCE-20GENERIC-FUNCTION-29"></a>

#### [generic-function] `CANONICAL-REFERENCE` OBJECT


Return a [`40ANTS-DOC/REFERENCE::REFERENCE`](#x-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20CLASS-29) that resolves to `OBJECT`.


<a id="x-2840ANTS-DOC-2FREFERENCE-API-3ACOLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29"></a>

#### [generic-function] `COLLECT-REACHABLE-OBJECTS` OBJECT


Return a list of objects representing all things
that would be documented in a ([`40ANTS-DOC/DOCUMENT::DOCUMENT`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-20GENERIC-FUNCTION-29) `OBJECT`) call.
For sections this is simply the union of references reachable from
references in [`40ANTS-DOC::SECTION-ENTRIES`](#x-2840ANTS-DOC-3ASECTION-ENTRIES-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-DOC-3ASECTION-29-29). The returned objects can be anything
provided that [`CANONICAL-REFERENCE`](#x-2840ANTS-DOC-2FREFERENCE-API-3ACANONICAL-REFERENCE-20GENERIC-FUNCTION-29) works on them. The list need not
include `OBJECT` itself.

One only has to specialize this for new container-like objects.


<a id="x-2840ANTS-DOC-2FREFERENCE-API-3ACOLLECT-REACHABLE-OBJECTS-20-28METHOD-20NIL-20-28T-29-29-29"></a>

#### [method] `COLLECT-REACHABLE-OBJECTS` OBJECT


This default implementation returns the empty list. This means that
nothing is reachable from `OBJECT`.


<a id="x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20GENERIC-FUNCTION-29"></a>

#### [generic-function] `DOCUMENT-OBJECT` OBJECT STREAM



<a id="x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-28STRING-20T-29-29-29"></a>

#### [method] `DOCUMENT-OBJECT` (STRING STRING) STREAM


Print `STRING` verbatim to `STREAM` after cleaning up indentation.

Docstrings in sources are indented in various ways which can easily
mess up markdown. To handle the most common cases leave the first
line alone, but from the rest of the lines strip the longest run of
leading spaces that is common to all non-blank lines.


<a id="x-2840ANTS-DOC-2FSOURCE-API-3AFIND-SOURCE-20GENERIC-FUNCTION-29"></a>

#### [generic-function] `FIND-SOURCE` OBJECT


Like `SWANK:FIND-DEFINITION-FOR-THING`, but this
one is a generic function to be extensible. In fact, the default
implementation simply defers to `SWANK:FIND-DEFINITION-FOR-THING`.
This function is called by [`40ANTS-DOC/SWANK::LOCATE-DEFINITION-FOR-EMACS`](#x-2840ANTS-DOC-2FSWANK-3ALOCATE-DEFINITION-FOR-EMACS-20FUNCTION-29) which lies
behind the `M-.` extension (see [`Emacs Integration`](#x-2840ANTS-DOC-2FDOC-3A-3A-40EMACS-INTEGRATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29)).

If successful, the return value looks like this:

```commonlisp
(:location (:file "/home/mega/own/mgl/pax/test/test.lisp")
           (:position 24) nil)
```
The `NIL` is the source snippet which is optional. Note that position
1 is the first character. If unsuccessful, the return values is
like:

```commonlisp
(:error "Unknown source location for SOMETHING")
```

<a id="x-2840ANTS-DOC-2FDOC-3A-3A-40REFERENCE-BASED-EXTENSIONS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Reference Based Extensions

Let's see how to extend [`40ANTS-DOC/DOCUMENT::DOCUMENT`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-20GENERIC-FUNCTION-29) and `M-.` navigation if there is
no first class object to represent the thing of interest. Recall
that [`40ANTS-DOC/LOCATIVES/BASE::LOCATE`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-20FUNCTION-29) returns a [`40ANTS-DOC/REFERENCE::REFERENCE`](#x-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20CLASS-29) object in this case. `40ANTS-DOC/DOCUMENT::DOCUMENT-OBJECT` ([`1`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20T-29-29-29) [`2`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-28STRING-20T-29-29-29) [`3`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20GENERIC-FUNCTION-29))
and `40ANTS-DOC/SOURCE-API::FIND-SOURCE` ([`1`](#x-2840ANTS-DOC-2FSOURCE-API-3AFIND-SOURCE-20-28METHOD-20NIL-20-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-29-29-29) [`2`](#x-2840ANTS-DOC-2FSOURCE-API-3AFIND-SOURCE-20GENERIC-FUNCTION-29)) defer to [`40ANTS-DOC/LOCATIVES/BASE::LOCATE-AND-DOCUMENT`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-AND-DOCUMENT-20GENERIC-FUNCTION-29) and
`40ANTS-DOC/LOCATIVES/BASE::LOCATE-AND-FIND-SOURCE` ([`1`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-AND-FIND-SOURCE-20-28METHOD-20NIL-20-28T-20T-20T-29-29-29) [`2`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-AND-FIND-SOURCE-20GENERIC-FUNCTION-29)), which have [`40ANTS-DOC/LOCATIVES/BASE::LOCATIVE-TYPE`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATIVE-TYPE-20FUNCTION-29) in their argument
list for `EQL` specializing pleasure. Here is a stripped down example
of how the [`VARIABLE`](#x-28VARIABLE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29) locative is defined:

```
(define-locative-type variable (&optional initform)
  "Refers to a global special variable. INITFORM, or if not specified,
  the global value of the variable is included in the documentation.")

(defmethod locate-object (symbol (locative-type (eql 'variable)) locative-args)
  (assert (<= (length locative-args) 1))
  (40ants-doc/reference::make-reference symbol (cons locative-type locative-args)))

(defmethod locate-and-document (symbol (locative-type (eql 'variable))
                                locative-args stream)
  (destructuring-bind (&optional (initform nil initformp)) locative-args
    (40ants-doc/builder/bullet::locate-and-print-bullet locative-type locative-args symbol stream)
    (write-char #\Space stream)
    (multiple-value-bind (value unboundp) (40ants-doc/utils::symbol-global-value symbol)
      (40ants-doc/render/args::print-arglist (prin1-to-string (cond (initformp initform)
                                                                    (unboundp "-unbound-")
                                                                    (t value)))
                     stream))
    (40ants-doc/builder/bullet::print-end-bullet stream)
    (with-dislocated-symbols ((list symbol))
      (40ants-doc/render/print::maybe-print-docstring symbol locative-type stream))))


(defmethod 40ants-doc/commondoc/builder::reference-to-commondoc ((symbol symbol) (locative-type (eql 'variable)) locative-args)
  (destructuring-bind (&optional (initform nil initformp)) locative-args
    (let* ((reference (canonical-reference
                       (40ants-doc/reference::make-reference
                        symbol (cons locative-type locative-args))))
           (docstring (40ants-doc/render/print::get-docstring symbol 'variable))
           (arglist (multiple-value-bind (value unboundp) (40ants-doc/utils::symbol-global-value symbol)
                      (cond (initformp
                             (prin1-to-string initform))
                            (unboundp "-unbound-")
                            (t
                             (prin1-to-string value)))))
           (children (when docstring
                       (40ants-doc/commondoc/builder::parse-markdown docstring))))

      (40ants-doc/commondoc/bullet::make-bullet reference
                                                :arglist arglist
                                                :children children
                                                :ignore-words symbol))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'variable))
                                   locative-args)
  (declare (ignore locative-args))
  (40ants-doc/locatives/utils::find-one-location (swank-backend:find-definitions symbol)
                                                 '("variable" "defvar" "defparameter"
                                                   "special-declaration")))

```
<a id="x-2840ANTS-DOC-2FREFERENCE-API-3ACOLLECT-REACHABLE-OBJECTS-20-28METHOD-20NIL-20-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-29-29-29"></a>

#### [method] `COLLECT-REACHABLE-OBJECTS` (REFERENCE REFERENCE)


If `REFERENCE` can be resolved to a non-reference, call
`COLLECT-REACHABLE-OBJECTS` ([`1`](#x-2840ANTS-DOC-2FREFERENCE-API-3ACOLLECT-REACHABLE-OBJECTS-20-28METHOD-20NIL-20-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-29-29-29) [`2`](#x-2840ANTS-DOC-2FREFERENCE-API-3ACOLLECT-REACHABLE-OBJECTS-20-28METHOD-20NIL-20-28T-29-29-29) [`3`](#x-2840ANTS-DOC-2FREFERENCE-API-3ACOLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29)) with it, else call
`40ANTS-DOC/LOCATIVES/BASE::LOCATE-AND-COLLECT-REACHABLE-OBJECTS` ([`1`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-AND-COLLECT-REACHABLE-OBJECTS-20-28METHOD-20NIL-20-28T-20T-20T-29-29-29) [`2`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-AND-COLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29)) on the object, locative-type,
locative-args of `REFERENCE`


<a id="x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-AND-COLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29"></a>

#### [generic-function] `LOCATE-AND-COLLECT-REACHABLE-OBJECTS` OBJECT LOCATIVE-TYPE LOCATIVE-ARGS


Called by `40ANTS-DOC/REFERENCE-API::COLLECT-REACHABLE-OBJECTS` ([`1`](#x-2840ANTS-DOC-2FREFERENCE-API-3ACOLLECT-REACHABLE-OBJECTS-20-28METHOD-20NIL-20-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-29-29-29) [`2`](#x-2840ANTS-DOC-2FREFERENCE-API-3ACOLLECT-REACHABLE-OBJECTS-20-28METHOD-20NIL-20-28T-29-29-29) [`3`](#x-2840ANTS-DOC-2FREFERENCE-API-3ACOLLECT-REACHABLE-OBJECTS-20GENERIC-FUNCTION-29)) on
[`40ANTS-DOC/REFERENCE::REFERENCE`](#x-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20CLASS-29) objects, this function has essentially the same purpose as its
caller but it has different arguments to allow specializing on
[`LOCATIVE-TYPE`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATIVE-TYPE-20FUNCTION-29).


<a id="x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-AND-COLLECT-REACHABLE-OBJECTS-20-28METHOD-20NIL-20-28T-20T-20T-29-29-29"></a>

#### [method] `LOCATE-AND-COLLECT-REACHABLE-OBJECTS` OBJECT LOCATIVE-TYPE LOCATIVE-ARGS


This default implementation returns the empty list. This means that
nothing is reachable from the reference.


<a id="x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20T-29-29-29"></a>

#### [method] `DOCUMENT-OBJECT` (REFERENCE REFERENCE) STREAM


If `REFERENCE` can be resolved to a non-reference, call
`40ANTS-DOC/DOCUMENT::DOCUMENT-OBJECT` ([`1`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20T-29-29-29) [`2`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-28STRING-20T-29-29-29) [`3`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20GENERIC-FUNCTION-29)) with it, else call [`40ANTS-DOC/LOCATIVES/BASE::LOCATE-AND-DOCUMENT`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-AND-DOCUMENT-20GENERIC-FUNCTION-29) on the
object, locative-type, locative-args of `REFERENCE`


<a id="x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-AND-DOCUMENT-20GENERIC-FUNCTION-29"></a>

#### [generic-function] `LOCATE-AND-DOCUMENT` OBJECT LOCATIVE-TYPE LOCATIVE-ARGS STREAM


Called by `40ANTS-DOC/DOCUMENT::DOCUMENT-OBJECT` ([`1`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20T-29-29-29) [`2`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-28STRING-20T-29-29-29) [`3`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20GENERIC-FUNCTION-29)) on [`40ANTS-DOC/REFERENCE::REFERENCE`](#x-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20CLASS-29) objects,
this function has essentially the same purpose as `40ANTS-DOC/DOCUMENT::DOCUMENT-OBJECT` ([`1`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20T-29-29-29) [`2`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20-28METHOD-20NIL-20-28STRING-20T-29-29-29) [`3`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-OBJECT-20GENERIC-FUNCTION-29))
but it has different arguments to allow specializing on
[`LOCATIVE-TYPE`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATIVE-TYPE-20FUNCTION-29).


<a id="x-2840ANTS-DOC-2FSOURCE-API-3AFIND-SOURCE-20-28METHOD-20NIL-20-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-29-29-29"></a>

#### [method] `FIND-SOURCE` (REFERENCE REFERENCE)


If `REFERENCE` can be resolved to a non-reference, call `40ANTS-DOC/SOURCE-API::FIND-SOURCE` ([`1`](#x-2840ANTS-DOC-2FSOURCE-API-3AFIND-SOURCE-20-28METHOD-20NIL-20-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-29-29-29) [`2`](#x-2840ANTS-DOC-2FSOURCE-API-3AFIND-SOURCE-20GENERIC-FUNCTION-29))
with it, else call `40ANTS-DOC/LOCATIVES/BASE::LOCATE-AND-FIND-SOURCE` ([`1`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-AND-FIND-SOURCE-20-28METHOD-20NIL-20-28T-20T-20T-29-29-29) [`2`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-AND-FIND-SOURCE-20GENERIC-FUNCTION-29)) on the object,
locative-type, locative-args of `REFERENCE`.


<a id="x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-AND-FIND-SOURCE-20GENERIC-FUNCTION-29"></a>

#### [generic-function] `LOCATE-AND-FIND-SOURCE` OBJECT LOCATIVE-TYPE LOCATIVE-ARGS


Called by `40ANTS-DOC/SOURCE-API::FIND-SOURCE` ([`1`](#x-2840ANTS-DOC-2FSOURCE-API-3AFIND-SOURCE-20-28METHOD-20NIL-20-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-29-29-29) [`2`](#x-2840ANTS-DOC-2FSOURCE-API-3AFIND-SOURCE-20GENERIC-FUNCTION-29)) on [`40ANTS-DOC/REFERENCE::REFERENCE`](#x-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20CLASS-29) objects, this
function has essentially the same purpose as [`40ANTS-DOC/SOURCE-API::FIND-SOURCE`](#x-2840ANTS-DOC-2FSOURCE-API-3AFIND-SOURCE-20GENERIC-FUNCTION-29) generic-function but it has
different arguments to allow specializing on [`LOCATIVE-TYPE`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATIVE-TYPE-20FUNCTION-29).


<a id="x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ALOCATE-AND-FIND-SOURCE-20-28METHOD-20NIL-20-28T-20T-20T-29-29-29"></a>

#### [method] `LOCATE-AND-FIND-SOURCE` OBJECT LOCATIVE-TYPE LOCATIVE-ARGS


This default implementation simply calls `40ANTS-DOC/SOURCE-API::FIND-SOURCE` ([`1`](#x-2840ANTS-DOC-2FSOURCE-API-3AFIND-SOURCE-20-28METHOD-20NIL-20-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-29-29-29) [`2`](#x-2840ANTS-DOC-2FSOURCE-API-3AFIND-SOURCE-20GENERIC-FUNCTION-29)) with `OBJECT`
which should cover the common case of a macro expanding to, for
instance, a defun but having its own locative type.


We have covered the basic building blocks of reference based
extensions. Now let's see how the obscure
[`40ANTS-DOC/LOCATIVES/DEFINERS::DEFINE-SYMBOL-LOCATIVE-TYPE`](#x-2840ANTS-DOC-2FLOCATIVES-2FDEFINERS-3ADEFINE-SYMBOL-LOCATIVE-TYPE-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29) and
[`40ANTS-DOC/LOCATIVES/DEFINE-DEFINER::DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE`](#x-2840ANTS-DOC-2FLOCATIVES-2FDEFINE-DEFINER-3ADEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29) macros work together to
simplify the common task of associating definition and documentation
with symbols in a certain context.

<a id="x-2840ANTS-DOC-2FLOCATIVES-2FDEFINERS-3ADEFINE-SYMBOL-LOCATIVE-TYPE-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

#### [macro] `DEFINE-SYMBOL-LOCATIVE-TYPE`


Similar to [`40ANTS-DOC/LOCATIVES/BASE::DEFINE-LOCATIVE-TYPE`](#x-2840ANTS-DOC-2FLOCATIVES-2FBASE-3ADEFINE-LOCATIVE-TYPE-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29) but it assumes that all things
locatable with `LOCATIVE-TYPE` are going to be just symbols defined
with a definer defined with [`40ANTS-DOC/LOCATIVES/DEFINE-DEFINER::DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE`](#x-2840ANTS-DOC-2FLOCATIVES-2FDEFINE-DEFINER-3ADEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29).
It is useful to attach documentation and source location to symbols
in a particular context. An example will make everything clear:

```commonlisp
(define-symbol-locative-type direction ()
  "A direction is a symbol. (After this `M-.` on `DIRECTION LOCATIVE`
                                   works and it can also be included in DEFSECTION forms.)")

(define-definer-for-symbol-locative-type define-direction direction
  "With DEFINE-DIRECTION one can document what a symbol means when
interpreted as a direction.")

(define-direction up ()
  "UP is equivalent to a coordinate delta of (0, -1).")
```
After all this, `(UP DIRECTION)` refers to the `DEFINE-DIRECTION`
form above.


<a id="x-2840ANTS-DOC-2FLOCATIVES-2FDEFINE-DEFINER-3ADEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

#### [macro] `DEFINE-DEFINER-FOR-SYMBOL-LOCATIVE-TYPE`


Define a macro with `NAME` which can be used to attach documentation,
a lambda-list and source location to a symbol in the context of
`LOCATIVE-TYPE`. The defined macro's arglist is (`SYMBOL` `LAMBDA-LIST`
`&OPTIONAL` `DOCSTRING`). `LOCATIVE-TYPE` is assumed to have been defined
with [`40ANTS-DOC/LOCATIVES/DEFINERS::DEFINE-SYMBOL-LOCATIVE-TYPE`](#x-2840ANTS-DOC-2FLOCATIVES-2FDEFINERS-3ADEFINE-SYMBOL-LOCATIVE-TYPE-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29).


<a id="x-2840ANTS-DOC-2FDOC-3A-3A-40SECTIONS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Sections

[`40ANTS-DOC:SECTION`](#x-2840ANTS-DOC-3ASECTION-20CLASS-29) objects rarely need to be dissected since
[`40ANTS-DOC::DEFSECTION`](#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29) and [`40ANTS-DOC/DOCUMENT::DOCUMENT`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-20GENERIC-FUNCTION-29) cover most needs. However, it is plausible
that one wants to subclass them and maybe redefine how they are
presented.

<a id="x-2840ANTS-DOC-3ASECTION-20CLASS-29"></a>

#### [class] `SECTION` ()


[`DEFSECTION`](#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29) stores its `:NAME`, `:TITLE`, `:PACKAGE`,
`:READTABLE` and `:ENTRIES` in [`SECTION`](#x-2840ANTS-DOC-3ASECTION-20CLASS-29) objects.


<a id="x-2840ANTS-DOC-3ASECTION-NAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-DOC-3ASECTION-29-29"></a>

#### [reader] `SECTION-NAME` (SECTION) (:NAME)


The name of the global variable whose value is
this [`SECTION`](#x-2840ANTS-DOC-3ASECTION-20CLASS-29) object.


<a id="x-2840ANTS-DOC-3ASECTION-PACKAGE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-DOC-3ASECTION-29-29"></a>

#### [reader] `SECTION-PACKAGE` (SECTION) (:PACKAGE)


`*PACKAGE*` will be bound to this package when
generating documentation for this section.


<a id="x-2840ANTS-DOC-3ASECTION-READTABLE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-DOC-3ASECTION-29-29"></a>

#### [reader] `SECTION-READTABLE` (SECTION) (:READTABLE)


`*READTABLE*` will be bound to this when generating
documentation for this section.


<a id="x-2840ANTS-DOC-3ASECTION-TITLE-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-DOC-3ASECTION-29-29"></a>

#### [reader] `SECTION-TITLE` (SECTION) (:TITLE)


`STRING` or `NIL`. Used in generated documentation.


<a id="x-2840ANTS-DOC-3ASECTION-LINK-TITLE-TO-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-DOC-3ASECTION-29-29"></a>

#### [reader] `SECTION-LINK-TITLE-TO` (SECTION) (:LINK-TITLE-TO = `NIL`)


A [`40ANTS-DOC/REFERENCE::REFERENCE`](#x-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20CLASS-29) or `NIL`. Used in generated documentation.


<a id="x-2840ANTS-DOC-3ASECTION-ENTRIES-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-2040ANTS-DOC-3ASECTION-29-29"></a>

#### [reader] `SECTION-ENTRIES` (SECTION) (:ENTRIES)


A list of strings and [`40ANTS-DOC/REFERENCE::REFERENCE`](#x-2840ANTS-DOC-2FREFERENCE-3AREFERENCE-20CLASS-29) objects in the
order they occurred in [`DEFSECTION`](#x-2840ANTS-DOC-3ADEFSECTION-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29).


<a id="x-28DESCRIBE-OBJECT-20-28METHOD-20NIL-20-2840ANTS-DOC-3ASECTION-20T-29-29-29"></a>

#### [method] `DESCRIBE-OBJECT` (SECTION SECTION) STREAM


[`40ANTS-DOC:SECTION`](#x-2840ANTS-DOC-3ASECTION-20CLASS-29) objects are printed by calling [`40ANTS-DOC/DOCUMENT::DOCUMENT`](#x-2840ANTS-DOC-2FDOCUMENT-3ADOCUMENT-20GENERIC-FUNCTION-29) on them
with all [`Documentation Printer Variables`](#x-2840ANTS-DOC-2FDOC-3A-3A-40DOCUMENTATION-PRINTER-VARIABLES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29), except for
[`40ANTS-DOC/BUILDER/PRINTER::*DOCUMENT-NORMALIZE-PACKAGES*`](#x-2840ANTS-DOC-2FBUILDER-2FPRINTER-3A-2ADOCUMENT-NORMALIZE-PACKAGES-2A-20-28VARIABLE-29-29), turned off to reduce clutter.


<a id="x-2840ANTS-DOC-2FTRANSCRIBE-3A-3A-40TRANSCRIPT-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Transcripts

What are transcripts for? When writing a tutorial, one often wants
to include a `REPL` session with maybe a few defuns and a couple of
forms whose output or return values are shown. Also, in a function's
docstring an example call with concrete arguments and return values
speaks volumes. A transcript is a text that looks like a repl
session, but which has a light markup for printed output and return
values, while no markup (i.e. prompt) for lisp forms. The `PAX`
transcripts may include output and return values of all forms, or
only selected ones. In either case the transcript itself can be
easily generated from the source code.

The main worry associated with including examples in the
documentation is that they tend to get out-of-sync with the code.
This is solved by being able to parse back and update transcripts.
In fact, this is exactly what happens during documentation
generation with `PAX`. Code sections tagged `cl-transcript` are
retranscribed and checked for inconsistency (that is, any difference
in output or return values). If the consistency check fails, an
error is signalled that includes a reference to the object being
documented.

Going beyond documentation, transcript consistency checks can be
used for writing simple tests in a very readable form. For example:

```cl-transcript
(+ 1 2)
=> 3

(values (princ :hello) (list 1 2))
.. HELLO
=> :HELLO
=> (1 2)
```
All in all, transcripts are a handy tool especially when combined
with the Emacs support to regenerate them and with
`PYTHONIC-STRING-READER` and its triple-quoted strings that allow one
to work with nested strings with less noise. The triple-quote syntax
can be enabled with:

```text
(in-readtable pythonic-string-syntax)
```
<a id="x-2840ANTS-DOC-2FTRANSCRIBE-3A-3A-40TRANSCRIPT-EMACS-INTEGRATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Transcribing with Emacs

Typical transcript usage from within Emacs is simple: add a lisp
form to a docstring or comment at any indentation level. Move the
cursor right after the end of the form as if you were to evaluate it
with `C-x C-e`. The cursor is marked by `#\^`:

```text
This is part of a docstring.

```cl-transcript
(values (princ :hello) (list 1 2))^
```
```
Note that the use of fenced code blocks with the language tag
`cl-transcript` is only to tell `PAX` to perform consistency checks at
documentation generation time.

Now invoke the elisp function `40ants-doc-transcribe` where the cursor
is and the fenced code block from the docstring becomes:

```text
(values (princ :hello) (list 1 2))
.. HELLO
=> :HELLO
=> (1 2)
^
```
Then you change the printed message and add a comment to the second
return value:

```text
(values (princ :hello-world) (list 1 2))
.. HELLO
=> :HELLO
=> (1
    ;; This value is arbitrary.
    2)
```
When generating the documentation you get a
[`TRANSCRIPTION-CONSISTENCY-ERROR`](#x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIPTION-CONSISTENCY-ERROR-20CONDITION-29) because the printed output and the
first return value changed so you regenerate the documentation by
marking the region of bounded by `#\|` and the cursor at `#\^` in
the example:

```text
|(values (princ :hello-world) (list 1 2))
.. HELLO
=> :HELLO
=> (1
    ;; This value is arbitrary.
    2)
^
```
then invoke the elisp function `40ANTS-DOC-RETRANSCRIBE-REGION` to get:

```text
(values (princ :hello-world) (list 1 2))
.. HELLO-WORLD
=> :HELLO-WORLD
=> (1
    ;; This value is arbitrary.
    2)
^
```
Note how the indentation and the comment of `(1 2)` was left alone
but the output and the first return value got updated.

Alternatively, `C-u 1 40ants-doc-transcribe` will emit commented markup:

```text
(values (princ :hello) (list 1 2))
;.. HELLO
;=> :HELLO
;=> (1 2)
```
`C-u 0 40ants-doc-retranscribe-region` will turn commented into
non-commented markup. In general, the numeric prefix argument is the
index of the syntax to be used in 40ants-doc:[`*SYNTAXES*`](#x-2840ANTS-DOC-2FTRANSCRIBE-3A-2ASYNTAXES-2A-20-28VARIABLE-29-29). Without a
prefix argument `40ants-doc-retranscribe-region` will not change the
markup style.

Finally, not only do both functions work at any indentation level,
but in comments too:

```text
;;;; (values (princ :hello) (list 1 2))
;;;; .. HELLO
;;;; => :HELLO
;;;; => (1 2)
```
Transcription support in emacs can be enabled by adding this to your
Emacs initialization file (or loading `elisp/transcribe.el`):

```
;;; MGL-PAX transcription

(defun mgl-pax-transcribe-last-expression ()
  "A bit like C-u C-x C-e (slime-eval-last-expression) that
inserts the output and values of the sexp before the point, this
does the same but with MGL-PAX:TRANSCRIBE. Use a numeric prefix
argument as in index to select one of the Common Lisp
MGL-PAX:*SYNTAXES* as the SYNTAX argument to MGL-PAX:TRANSCRIBE.
Without a prefix argument, the first syntax is used."
  (interactive)
  (insert
   (save-excursion
     (let* ((end (point))
            (start (progn (backward-sexp)
                          (move-beginning-of-line nil)
                          (point))))
       (mgl-pax-transcribe start end (mgl-pax-transcribe-syntax-arg)
                           nil nil nil)))))

(defun mgl-pax-retranscribe-region (start end)
  "Updates the transcription in the current region (as in calling
MGL-PAX:TRANSCRIBE with :UPDATE-ONLY T). Use a numeric prefix
argument as in index to select one of the Common Lisp
MGL-PAX:*SYNTAXES* as the SYNTAX argument to MGL-PAX:TRANSCRIBE.
Without a prefix argument, the syntax of the input will not be
changed."
  (interactive "r")
  (let* ((point-at-start-p (= (point) start))
         (point-at-end-p (= (point) end))
         (transcript (mgl-pax-transcribe start end
                                         (mgl-pax-transcribe-syntax-arg)
                                         t t nil)))
    (if point-at-start-p
        (save-excursion
          (goto-char start)
          (delete-region start end)
          (insert transcript))
      (save-excursion
          (goto-char start)
          (delete-region start end))
      (insert transcript))))

(defun mgl-pax-transcribe-syntax-arg ()
  (if current-prefix-arg
      (prefix-numeric-value current-prefix-arg)
    nil))

(defun mgl-pax-transcribe (start end syntax update-only echo
                                 first-line-special-p)
  (let ((transcription
         (slime-eval
          `(cl:if (cl:find-package :mgl-pax)
                  (cl:funcall
                   (cl:find-symbol
                    (cl:symbol-name :transcribe-for-emacs) :mgl-pax)
                   ,(buffer-substring-no-properties start end)
                   ',syntax ',update-only ',echo ',first-line-special-p)
                  t))))
    (if (eq transcription t)
        (error "MGL-PAX is not loaded.")
      transcription)))
```
<a id="x-2840ANTS-DOC-2FTRANSCRIBE-3A-3A-40TRANSCRIPT-API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Transcript API

<a id="x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIBE-20FUNCTION-29"></a>

#### [function] `TRANSCRIBE` INPUT OUTPUT &KEY UPDATE-ONLY (INCLUDE-NO-OUTPUT UPDATE-ONLY) (INCLUDE-NO-VALUE UPDATE-ONLY) (ECHO T) CHECK-CONSISTENCY DEFAULT-SYNTAX (INPUT-SYNTAXES \*SYNTAXES\*) (OUTPUT-SYNTAXES \*SYNTAXES\*)


Read forms from `INPUT` and write them (if `ECHO`) to `OUTPUT`
followed by any output and return values produced by calling `EVAL` on
the form. `INPUT` can be a stream or a string, while `OUTPUT` can be a
stream or `NIL` in which case transcription goes into a string. The
return value is the `OUTPUT` stream or the string that was
constructed.

A simple example is this:

```cl-transcript
(transcribe "(princ 42) " nil)
=> "(princ 42)
.. 42
=> 42
"
```
However, the above may be a bit confusing since this documentation
uses [`TRANSCRIBE`](#x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIBE-20FUNCTION-29) markup syntax in this very example, so let's do it
differently. If we have a file with these contents:

```commonlisp
(values (princ 42) (list 1 2))
```
it is transcribed to:

```commonlisp
(values (princ 42) (list 1 2))
.. 42
=> 42
=> (1 2)
```
Output to all standard streams is captured and printed with
the `:OUTPUT` prefix (`".."`). The return values above are printed
with the `:READABLE` prefix (`"=>"`). Note how these prefixes are
always printed on a new line to facilitate parsing.

Updating

[`TRANSCRIBE`](#x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIBE-20FUNCTION-29) is able to parse its own output. If we transcribe the
previous output above, we get it back exactly. However, if we remove
all output markers, leave only a placeholder value marker and
pass `:UPDATE-ONLY` T with source:

```commonlisp
(values (princ 42) (list 1 2))
=>
```
we get this:

```commonlisp
(values (princ 42) (list 1 2))
=> 42
=> (1 2)
```
With `UPDATE-ONLY`, printed output of a form is only transcribed if
there were output markers in the source. Similarly, with
`UPDATE-ONLY`, return values are only transcribed if there were value
markers in the source.

No Output/Values

If the form produces no output or returns no values, then whether or
not output and values are transcribed is controlled by
`INCLUDE-NO-OUTPUT` and `INCLUDE-NO-VALUE`, respectively. By default,
neither is on so:

```commonlisp
(values)
..
=>
```
is transcribed to

```commonlisp
(values)
```
With `UPDATE-ONLY` true, we probably wouldn't like to lose those
markers since they were put there for a reason. Hence, with
`UPDATE-ONLY`, `INCLUDE-NO-OUTPUT` and `INCLUDE-NO-VALUE` default to true.
So with `UPDATE-ONLY` the above example is transcribed to:

```commonlisp
(values)
..
=> ; No value
```
where the last line is the `:NO-VALUE` prefix.

Consistency Checks

If `CHECK-CONSISTENCY` is true, then [`TRANSCRIBE`](#x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIBE-20FUNCTION-29) signals a continuable
[`TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR`](#x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR-20CONDITION-29) whenever a form's output as a
string is different from what was in `INPUT`, provided that `INPUT`
contained the output. Similary, for values, a continuable
[`TRANSCRIPTION-VALUES-CONSISTENCY-ERROR`](#x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIPTION-VALUES-CONSISTENCY-ERROR-20CONDITION-29) is signalled if a value read
from the source does not print as the as the value returned by `EVAL`.
This allows readable values to be hand-indented without failing
consistency checks:

```commonlisp
(list 1 2)
=> (1
      2)
```
Unreadable Values

The above scheme involves `READ`, so consistency of unreadable values
cannot be treated the same. In fact, unreadable values must even be
printed differently for transcribe to be able to read them back:

```commonlisp
(defclass some-class () ())

(defmethod print-object ((obj some-class) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream \"~%~%end\")))

(make-instance 'some-class)
==> #<SOME-CLASS 
-->
--> end>
```
where `"==>"` is the `:UNREADABLE` prefix and `"-->"` is
the `:UNREADABLE-CONTINUATION` prefix. As with outputs, a consistency
check between an unreadable value from the source and the value from
`EVAL` is performed with `STRING`=. That is, the value from `EVAL` is
printed to a string and compared to the source value. Hence, any
change to unreadable values will break consistency checks. This is
most troublesome with instances of classes with the default
`PRINT-OBJECT` method printing the memory address. There is currently
no remedy for that, except for customizing `PRINT-OBJECT` or not
transcribing that kind of stuff.

Syntaxes

Finally, a transcript may employ different syntaxes for the output
and values of different forms. When `INPUT` is read, the syntax for
each form is determined by trying to match all prefixes from all
syntaxes in `INPUT-SYNTAXES` against a line. If there are no output or
values for a form in `INPUT`, then the syntax remains undetermined.

When `OUTPUT` is written, the prefixes to be used are looked up in
`DEFAULT-SYNTAX` of `OUTPUT-SYNTAXES`, if `DEFAULT-SYNTAX` is not `NIL`. If
`DEFAULT-SYNTAX` is `NIL`, then the syntax used by the same form in the
`INPUT` is used or (if that could not be determined) the syntax of the
previous form. If there was no previous form, then the first syntax
if `OUTPUT-SYNTAXES` is used.

To produce a transcript that's executable Lisp code,
use `:DEFAULT-SYNTAX` `:COMMENTED-1`:

```commonlisp
(make-instance 'some-class)
;==> #<SOME-CLASS
;-->
;--> end>

(list 1 2)
;=> (1
;->    2)
```
To translate the above to uncommented syntax,
use `:DEFAULT-SYNTAX` `:DEFAULT`. If `DEFAULT-SYNTAX` is `NIL` (the
default), the same syntax will be used in the output as in the input
as much as possible.


<a id="x-2840ANTS-DOC-2FTRANSCRIBE-3A-2ASYNTAXES-2A-20-28VARIABLE-29-29"></a>

#### [variable] `*SYNTAXES*` ((:DEFAULT (:OUTPUT "..") (:NO-VALUE "=> ; No value") (:READABLE "=>")
  (:UNREADABLE "==>") (:UNREADABLE-CONTINUATION "-->"))
 (:COMMENTED-1 (:OUTPUT ";..") (:NO-VALUE ";=> ; No value") (:READABLE ";=>")
  (:READABLE-CONTINUATION ";->") (:UNREADABLE ";==>")
  (:UNREADABLE-CONTINUATION ";-->"))
 (:COMMENTED-2 (:OUTPUT ";;..") (:NO-VALUE ";;=> ; No value")
  (:READABLE ";;=>") (:READABLE-CONTINUATION ";;->") (:UNREADABLE ";;==>")
  (:UNREADABLE-CONTINUATION ";;-->")))


The default syntaxes used by [`TRANSCRIBE`](#x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIBE-20FUNCTION-29) for reading and writing
lines containing output and values of an evaluated form.

A syntax is a list of of the form `(SYNTAX-ID &REST PREFIXES)` where
`prefixes` is a list of `(PREFIX-ID PREFIX-STRING)` elements. For
example the syntax `:COMMENTED-1` looks like this:

```commonlisp
(:commented-1
 (:output ";..")
 (:no-value ";=>  No value")
 (:readable ";=>")
 (:readable-continuation ";->")
 (:unreadable ";==>")
 (:unreadable-continuation ";-->"))
```
All of the above prefixes must be defined for every syntax except
for `:READABLE-CONTINUATION`. If that's missing (as in the `:DEFAULT`
syntax), then the following value is read with `READ` and printed with
`PRIN1` (hence no need to mark up the following lines).

When writing, an extra space is added automatically if the line to
be prefixed is not empty. Similarly, the first space following the
prefix discarded when reading.

See [`TRANSCRIBE`](#x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIBE-20FUNCTION-29) for how the actual syntax to be used is selected.


<a id="x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIPTION-ERROR-20CONDITION-29"></a>

#### [condition] `TRANSCRIPTION-ERROR` (ERROR)


Represents syntactic errors in the `INPUT` argument
of [`TRANSCRIBE`](#x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIBE-20FUNCTION-29) and also serves as the superclass of
[`TRANSCRIPTION-CONSISTENCY-ERROR`](#x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIPTION-CONSISTENCY-ERROR-20CONDITION-29).


<a id="x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIPTION-CONSISTENCY-ERROR-20CONDITION-29"></a>

#### [condition] `TRANSCRIPTION-CONSISTENCY-ERROR` (TRANSCRIPTION-ERROR)


A common superclass for
[`TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR`](#x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR-20CONDITION-29) and
[`TRANSCRIPTION-VALUES-CONSISTENCY-ERROR`](#x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIPTION-VALUES-CONSISTENCY-ERROR-20CONDITION-29).


<a id="x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR-20CONDITION-29"></a>

#### [condition] `TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR` (TRANSCRIPTION-CONSISTENCY-ERROR)


Signaled (with `CERROR`) by [`TRANSCRIBE`](#x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIBE-20FUNCTION-29) when invoked
with `:CHECK-CONSISTENCY` and the output of a form is not the same as
what was parsed.


<a id="x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIPTION-VALUES-CONSISTENCY-ERROR-20CONDITION-29"></a>

#### [condition] `TRANSCRIPTION-VALUES-CONSISTENCY-ERROR` (TRANSCRIPTION-CONSISTENCY-ERROR)


Signaled (with `CERROR`) by [`TRANSCRIBE`](#x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIBE-20FUNCTION-29) when invoked
with `:CHECK-CONSISTENCY` and the values of a form are inconsistent
with their parsed representation.


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

