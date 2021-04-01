<a id='x-28PLAYGROUND-3A-40INDEX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

# Playground

## Table of Contents

- [1 ASDF System][d08e]
    - [1.1 40ants-doc ASDF System Details][b62a]
- [2 Functions][2e93]
- [3 Classes][cb9f]
- [4 Structures][015a]
- [5 Compiler macro][e31f]
- [6 Constants][99cb]
- [7 Variables][132d]
- [8 Glossary][92bd]
- [9 Locatives][c883]
- [10 Macro][3811]
- [11 Generic and methods][e736]
- [12 Package][43b4]
- [13 Restarts][b532]
- [14 Types][f79c]
- [15 Inclusions][9b46]
- [16 TODO][2ba2]

###### \[in package PLAYGROUND\]
Hello World!

<a id='x-28PLAYGROUND-3A-40ASDF-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 1 ASDF System

<a id='x-28-23A-28-2810-29-20BASE-CHAR-20-2E-20-2240ants-doc-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29'></a>

### 1.1 40ants-doc ASDF System Details

- Version: 0.1.0
- Description: Exploratory programming tool and documentation generator, based on MGL-PAX.
- Licence: MIT
- Author: Alexander Artemenko
- Mailto: [svetlyak.40wt@gmail.com](mailto:svetlyak.40wt@gmail.com)
- Homepage: [http://40ants.com/doc](http://40ants.com/doc)
- Bug tracker: [https://github.com/40ants/doc/issues](https://github.com/40ants/doc/issues)
- Source control: [GIT](https://github.com/40ants/doc.git)

<a id='x-28PLAYGROUND-3A-40FUNCTION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 2 Functions

<a id='x-28PLAYGROUND-3AFOO-20FUNCTION-29'></a>

- [function] **FOO** *ARG*

    Cool! It calls `BAR`([`0`][35da] [`1`][e57d]) function!

<a id='x-28PLAYGROUND-3AUSER-20FUNCTION-29'></a>

- [function] **USER** 

<a id='x-28PLAYGROUND-3ABAR-20FUNCTION-29'></a>

- [function] **BAR** *USER*

    Cool! This function prints its `USER` argument of [`BAR`][e57d] function.

<a id='x-28PLAYGROUND-3A-40CLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 3 Classes

<a id='x-28PLAYGROUND-3AUSER-20CLASS-29'></a>

- [class] **USER**

<a id='x-28PLAYGROUND-3AUSER-NICKNAME-20-2840ANTS-DOC-2FLOCATIVES-3AREADER-20PLAYGROUND-3AUSER-29-29'></a>

- [reader] **USER-NICKNAME** *USER* *(= :UNAUTHORIZED)*

    User's nickname

<a id='x-28PLAYGROUND-3AUSER-EMAIL-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-20PLAYGROUND-3AUSER-29-29'></a>

- [accessor] **USER-EMAIL** *USER* *(= NIL)*

    User's Email. Can be empty

<a id='x-28PLAYGROUND-3A-40STRUCTURE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 4 Structures

<a id='x-28PLAYGROUND-3ABOX-WIDTH-20-2840ANTS-DOC-2FLOCATIVES-3ASTRUCTURE-ACCESSOR-29-29'></a>

- [structure-accessor] **BOX-WIDTH**

<a id='x-28PLAYGROUND-3ABOX-HEIGHT-20-2840ANTS-DOC-2FLOCATIVES-3ASTRUCTURE-ACCESSOR-29-29'></a>

- [structure-accessor] **BOX-HEIGHT**

<a id='x-28PLAYGROUND-3A-40COMPILER-MACRO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 5 Compiler macro

<a id='x-28PLAYGROUND-3ABAR-20-28COMPILER-MACRO-29-29'></a>

- [compiler-macro] **BAR** *USER*

    Cool! This function prints its `USER` argument of [`BAR`][e57d] function.

<a id='x-28PLAYGROUND-3A-40CONSTANT-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 6 Constants

<a id='x-28PLAYGROUND-3A-2BTHE-QUESTION-2B-20-2840ANTS-DOC-2FLOCATIVES-3ACONSTANT-29-29'></a>

- [constant] **+THE-QUESTION+** *NIL*

<a id='x-28PLAYGROUND-3A-2BTHE-ANSWER-2B-20-2840ANTS-DOC-2FLOCATIVES-3ACONSTANT-29-29'></a>

- [constant] **+THE-ANSWER+** *42*

    The answer to everything

<a id='x-28PLAYGROUND-3A-40VARS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 7 Variables

<a id='x-28PLAYGROUND-3A-2AVAR-A-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*VAR-A\*** *"-unbound-"*

<a id='x-28PLAYGROUND-3A-2AVAR-B-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*VAR-B\*** *100500*

    Just a var with docstring.

<a id='x-28PLAYGROUND-3A-2AVAR-C-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*VAR-C\*** *"-unbound-"*

    Unbound var with docstring. [The Best Programmin Language][de87] allows us to define docstring separately.

<a id='x-28PLAYGROUND-3A-40GLOSSARY-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 8 Glossary

<a id='x-28PLAYGROUND-3ALISP-2040ANTS-DOC-2FLOCATIVES-3AGLOSSARY-TERM-29'></a>

- [glossary-term] **The Best Programmin Language**

    You really should use LISP!

<a id='x-28PLAYGROUND-3A-40LOCATIVE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 9 Locatives

<a id='x-28VARIABLE-20-2840ANTS-DOC-2FLOCATIVES-3ALOCATIVE-29-29'></a>

- [locative] **VARIABLE** *&OPTIONAL INITFORM*

    Refers to a global special variable. INITFORM, or if not specified,
    the global value of the variable is included in the documentation.

<a id='x-28PLAYGROUND-3A-40MACRO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 10 Macro

<a id='x-28PLAYGROUND-3ATHE-MACRO-20-2840ANTS-DOC-2FLOCATIVES-3AMACRO-29-29'></a>

- [macro] **THE-MACRO** *(TITLE) &BODY BODY*

    Macro's docstring.
    
    We can refer [`FOO`][9617] function from here.

<a id='x-28PLAYGROUND-3A-40METHODS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 11 Generic and methods

<a id='x-28PLAYGROUND-3AGET-ADDRESS-20GENERIC-FUNCTION-29'></a>

- [generic-function] **GET-ADDRESS** *ENTITY*

    Docstring of the generic function.

<a id='x-28PLAYGROUND-3AGET-ADDRESS-20-28METHOD-20NIL-20-28PLAYGROUND-3AUSER-29-29-29'></a>

- [method] **GET-ADDRESS** *(USER USER)*

    Returns user's address.

<a id='x-28PLAYGROUND-3A-40PACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 12 Package

<a id='x-28-23A-28-2810-29-20BASE-CHAR-20-2E-20-2240ANTS-DOC-22-29-20PACKAGE-29'></a>

- [package] **"40ANTS-DOC"**

    See 40ants-doc:@index.

<a id='x-28-23A-28-2815-29-20BASE-CHAR-20-2E-20-2240ANTS-DOC-FULL-22-29-20PACKAGE-29'></a>

- [package] **"40ANTS-DOC-FULL"**

<a id='x-28PLAYGROUND-3A-40RESTART-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 13 Restarts

<a id='x-28PLAYGROUND-3ARETRY-THIS-ERROR-20-28RESTART-29-29'></a>

- [restart] **RETRY-THIS-ERROR**

    Some docstring for restart

<a id='x-28PLAYGROUND-3A-40TYPE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 14 Types

<a id='x-28PLAYGROUND-3AA-FEW-20-28TYPE-29-29'></a>

- [type] **A-FEW** *&OPTIONAL (TYPE 'INTEGER)*

    Very small integer, less or equal than 3.

<a id='x-28PLAYGROUND-3A-40INCLUDE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 15 Inclusions

<a id='x-28PLAYGROUND-3AFUNCTION-LOCATIVE-EXAMPLE-20-2840ANTS-DOC-2FLOCATIVES-3AINCLUDE-20-28-3ASTART-20-28PLAYGROUND-3AUSER-20FUNCTION-29-20-3AEND-20-28PLAYGROUND-3ABAR-20FUNCTION-29-29-20-3AHEADER-NL-20-22-60-60-60commonlisp-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29'></a>

```commonlisp
(defun user ()
  "Just to check locatives in docstrings"
  )

(define-compiler-macro bar (&whole form arg)
  "A custom dostring for a compiler macro"
  (if (atom arg)
      arg
      form))

;; Надо разобраться почему не работает явное указание locatives
```

<a id='x-28PLAYGROUND-3A-40TODO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 16 TODO

Here what I need to check and fix:

- enable all locatives

- check dependencies of core

- reenable tests suite

- fix how do M-. work in SLIME

- fix transcribe

- create integration with SLY


  [015a]: #x-28PLAYGROUND-3A-40STRUCTURE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Structures"
  [132d]: #x-28PLAYGROUND-3A-40VARS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Variables"
  [2ba2]: #x-28PLAYGROUND-3A-40TODO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "TODO"
  [2e93]: #x-28PLAYGROUND-3A-40FUNCTION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Functions"
  [35da]: #x-28PLAYGROUND-3ABAR-20-28COMPILER-MACRO-29-29 "(PLAYGROUND:BAR (COMPILER-MACRO))"
  [3811]: #x-28PLAYGROUND-3A-40MACRO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Macro"
  [43b4]: #x-28PLAYGROUND-3A-40PACKAGE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Package"
  [92bd]: #x-28PLAYGROUND-3A-40GLOSSARY-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Glossary"
  [9617]: #x-28PLAYGROUND-3AFOO-20FUNCTION-29 "(PLAYGROUND:FOO FUNCTION)"
  [99cb]: #x-28PLAYGROUND-3A-40CONSTANT-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Constants"
  [9b46]: #x-28PLAYGROUND-3A-40INCLUDE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Inclusions"
  [b532]: #x-28PLAYGROUND-3A-40RESTART-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Restarts"
  [b62a]: #x-28-23A-28-2810-29-20BASE-CHAR-20-2E-20-2240ants-doc-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29 "(#A((10) BASE-CHAR . \"40ants-doc\") ASDF/SYSTEM:SYSTEM)"
  [c883]: #x-28PLAYGROUND-3A-40LOCATIVE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Locatives"
  [cb9f]: #x-28PLAYGROUND-3A-40CLASS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Classes"
  [d08e]: #x-28PLAYGROUND-3A-40ASDF-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "ASDF System"
  [de87]: #x-28PLAYGROUND-3ALISP-2040ANTS-DOC-2FLOCATIVES-3AGLOSSARY-TERM-29 "(PLAYGROUND:LISP 40ANTS-DOC/LOCATIVES:GLOSSARY-TERM)"
  [e31f]: #x-28PLAYGROUND-3A-40COMPILER-MACRO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Compiler macro"
  [e57d]: #x-28PLAYGROUND-3ABAR-20FUNCTION-29 "(PLAYGROUND:BAR FUNCTION)"
  [e736]: #x-28PLAYGROUND-3A-40METHODS-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Generic and methods"
  [f79c]: #x-28PLAYGROUND-3A-40TYPE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Types"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
