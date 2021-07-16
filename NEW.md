# Documentation

<a id="x-28PLAYGROUND-3A-40INDEX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Playground

Hello World!

And here is a link to [`Generic and methods`](#x-28PLAYGROUND-3A-40METHODS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29) section.

And there can be the [`Second Page`](#x-28PLAYGROUND-3A-40SECOND-PAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29) section.

<a id="x-28PLAYGROUND-3A-40ASDF-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### ASDF System

#### 40ANTS-DOC ASDF System Details

* Version: 0.1.0

* Description: Documentation generator, based on MGL-PAX. Allows to put documentation inside lisp files and cross-reference between different entities.

* Licence: MIT

* Author: Alexander Artemenko

* Maintainer: `NIL`

* Mailto: [svetlyak.40wt@gmail.com](mailto:svetlyak.40wt@gmail.com)

* Homepage: [http://40ants.com/doc](http://40ants.com/doc)

* Bug tracker: [https://github.com/40ants/doc/issues](https://github.com/40ants/doc/issues)

* Source control: [GIT](https://github.com/40ants/doc)

<a id="x-28PLAYGROUND-3A-40FUNCTION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Functions

<a id="x-28PLAYGROUND-3AFOO-20FUNCTION-29"></a>

#### [function] `FOO` ARG


Cool! It calls [`BAR`](#x-28PLAYGROUND-3ABAR-20FUNCTION-29) function!


<a id="x-28PLAYGROUND-3AUSER-20FUNCTION-29"></a>

#### [function] `USER`


Just to check locatives in docstrings


<a id="x-28PLAYGROUND-3ABAR-20FUNCTION-29"></a>

#### [function] `BAR` USER


Cool! This function prints its USER argument. It is also exists as [`BAR`](#x-28PLAYGROUND-3ABAR-20-28COMPILER-MACRO-29-29) compiler-macro.


<a id="x-28PLAYGROUND-3A-40CLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Classes

<a id="x-28PLAYGROUND-3ATHE-OBJECT-20CLASS-29"></a>

#### [class] `THE-OBJECT` ()


Base class for all objects in the system


<a id="x-28PLAYGROUND-3AUSER-20CLASS-29"></a>

#### [class] `USER` (THE-OBJECT)


Class for all users except admins.

```python
def foo():
    pass
```
This was the `Python` code.


<a id="x-28PLAYGROUND-3AUSER-NICKNAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20PLAYGROUND-3AUSER-29-29"></a>

#### [reader] `USER-NICKNAME` (USER) (= `:UNAUTHORIZED`)


User's nickname


<a id="x-28PLAYGROUND-3AUSER-EMAIL-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20PLAYGROUND-3AUSER-29-29"></a>

#### [accessor] `USER-EMAIL` (USER) (= `NIL`)


User's Email. Can be empty


<a id="x-28PLAYGROUND-3AUSER-PROCESSED-20-2840ANTS-DOC-2FLOCATIVES-3AWRITER-20PLAYGROUND-3AUSER-29-29"></a>

#### [writer] `USER-PROCESSED` (USER) (= `NIL`)



<a id="x-28PLAYGROUND-3A-40STRUCTURE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Structures

No support for structure type yet (`MGL-PAX` lack it too)

<a id="x-28PLAYGROUND-3ABOX-WIDTH-20-2840ANTS-DOC-2FLOCATIVES-3ASTRUCTURE-ACCESSOR-29-29"></a>

#### [structure-accessor] `BOX-WIDTH`



<a id="x-28PLAYGROUND-3ABOX-HEIGHT-20-2840ANTS-DOC-2FLOCATIVES-3ASTRUCTURE-ACCESSOR-29-29"></a>

#### [structure-accessor] `BOX-HEIGHT`



<a id="x-28PLAYGROUND-3A-40COMPILER-MACRO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Compiler macro

<a id="x-28PLAYGROUND-3ABAR-20-28COMPILER-MACRO-29-29"></a>

#### [compiler-macro] `BAR`


A custom dostring for a compiler macro. Optimizes a call to the [`BAR`](#x-28PLAYGROUND-3ABAR-20FUNCTION-29) function, when arg is an atom.


<a id="x-28PLAYGROUND-3A-40CONSTANT-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Constants

<a id="x-28PLAYGROUND-3A-2BTHE-QUESTION-2B-20-2840ANTS-DOC-2FLOCATIVES-3ACONSTANT-29-29"></a>

#### [constant] `+THE-QUESTION+` NIL



<a id="x-28PLAYGROUND-3A-2BTHE-ANSWER-2B-20-2840ANTS-DOC-2FLOCATIVES-3ACONSTANT-29-29"></a>

#### [constant] `+THE-ANSWER+` 42


The answer to everything


<a id="x-28PLAYGROUND-3A-40VARS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Variables

<a id="x-28PLAYGROUND-3A-2AVAR-A-2A-20-28VARIABLE-29-29"></a>

#### [variable] `*VAR-A*` -unbound-



<a id="x-28PLAYGROUND-3A-2AVAR-B-2A-20-28VARIABLE-29-29"></a>

#### [variable] `*VAR-B*` 100500


Just a var with docstring.


<a id="x-28PLAYGROUND-3A-2AVAR-C-2A-20-28VARIABLE-29-29"></a>

#### [variable] `*VAR-C*` -unbound-


Unbound var with docstring. [`LISP`](#x-28PLAYGROUND-3ALISP-2040ANTS-DOC-2FLOCATIVES-3AGLOSSARY-TERM-29) allows us to define docstring separately.


<a id="x-28PLAYGROUND-3A-40GLOSSARY-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Glossary

<a id="x-28PLAYGROUND-3ALISP-2040ANTS-DOC-2FLOCATIVES-3AGLOSSARY-TERM-29"></a>

#### [glossary-term] `The Best Programming Language`


You really should use LISP!


<a id="x-28PLAYGROUND-3A-40LOCATIVE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Locatives

<a id="x-28VARIABLE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29"></a>

#### [locative] `VARIABLE` &OPTIONAL INITFORM


Refers to a global special variable. INITFORM, or if not specified,
the global value of the variable is included in the documentation.


<a id="x-28PLAYGROUND-3A-40MACRO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Macro

<a id="x-28PLAYGROUND-3ATHE-MACRO-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29"></a>

#### [macro] `THE-MACRO`


Macro's docstring.

We can refer [`FOO`](#x-28PLAYGROUND-3AFOO-20FUNCTION-29) function from here.


<a id="x-28PLAYGROUND-3A-40METHODS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Generic and methods

<a id="x-28PLAYGROUND-3AGET-ADDRESS-20GENERIC-FUNCTION-29"></a>

#### [generic-function] `GET-ADDRESS` ENTITY


Docstring of the generic function.


<a id="x-28PLAYGROUND-3AGET-ADDRESS-20-28METHOD-20NIL-20-28PLAYGROUND-3AUSER-29-29-29"></a>

#### [method] `GET-ADDRESS` (USER USER)


Returns user's address.


<a id="x-28PLAYGROUND-3A-40PACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Package

<a id="x-28-23A-28-2810-29-20BASE-CHAR-20-2E-20-2240ANTS-DOC-22-29-20PACKAGE-29"></a>

#### [package] `40ANTS-DOC`


See `40ANTS-DOC:@INDEX`.


<a id="x-28-23A-28-2815-29-20BASE-CHAR-20-2E-20-2240ANTS-DOC-FULL-22-29-20PACKAGE-29"></a>

#### [package] `40ANTS-DOC-FULL`



<a id="x-28-23A-28-2810-29-20BASE-CHAR-20-2E-20-22PLAYGROUND-22-29-20PACKAGE-29"></a>

#### [package] `PLAYGROUND`



<a id="x-28PLAYGROUND-3A-40RESTART-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Restarts

<a id="x-28PLAYGROUND-3ARETRY-THIS-ERROR-20-28RESTART-29-29"></a>

#### [restart] `RETRY-THIS-ERROR`


Some docstring for restart


<a id="x-28PLAYGROUND-3A-40TYPE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Types

<a id="x-28PLAYGROUND-3AA-FEW-20-28TYPE-29-29"></a>

#### [type] `A-FEW` &OPTIONAL (TYPE 'INTEGER)


Very small integer, less or equal than 3.


<a id="x-28PLAYGROUND-3A-40INCLUDE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Inclusions

```lisp
(defun user ()
  "Just to check locatives in docstrings"
  (values))

(define-compiler-macro bar (&whole form arg)
  "A custom dostring for a compiler macro. Optimizes a call to the [`BAR`](#x-28PLAYGROUND-3ABAR-20FUNCTION-29) function, when arg is an atom."
  (if (atom arg)
      arg
      form))

;; Надо разобраться почему не работает явное указание locatives
```
<a id="x-28PLAYGROUND-3A-40TODO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### TODO

Here what I need to [check](https://yandex.ru) and fix:

1. enable all locatives

2. check dependencies of core

3. reenable tests suite

4. fix how do `M-.` work in `SLIME`

5. fix transcribe

6. create integration with `SLY`

Finally the other [`Second Page`](#x-28PLAYGROUND-3A-40SECOND-PAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29) section link.

<a id="x-28PLAYGROUND-3A-40SECOND-PAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Second Page

<a id="x-28PLAYGROUND-3AUSER-20CLASS-29"></a>

### [class] `USER` (THE-OBJECT)


Class for all users except admins.

```python
def foo():
    pass
```
This was the `Python` code.


