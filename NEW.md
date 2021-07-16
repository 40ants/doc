# Documentation

## Playground

Hello World!

And here is a link to `Generic and methods` section.

And there can be the `Second Page` section.

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


### Functions

#### [function] `FOO` ARG



Cool! It calls `BAR` function!



#### [function] `USER`



Just to check locatives in docstrings



#### [function] `BAR` USER



Cool! This function prints its USER argument. It is also exists as `BAR` compiler-macro.



### Classes

#### [class] `THE-OBJECT` ()



Base class for all objects in the system



#### [class] `USER` (THE-OBJECT)



Class for all users except admins.

```python
def foo():
    pass
```
This was the `Python` code.



#### [reader] `USER-NICKNAME` (USER) (= `:UNAUTHORIZED`)



User's nickname



#### [accessor] `USER-EMAIL` (USER) (= `NIL`)



User's Email. Can be empty



#### [writer] `USER-PROCESSED` (USER) (= `NIL`)





### Structures

No support for structure type yet (`MGL-PAX` lack it too)

#### [structure-accessor] `BOX-WIDTH`





#### [structure-accessor] `BOX-HEIGHT`





### Compiler macro

#### [compiler-macro] `BAR`



A custom dostring for a compiler macro. Optimizes a call to the `BAR` function, when arg is an atom.



### Constants

#### [constant] `+THE-QUESTION+` NIL





#### [constant] `+THE-ANSWER+` 42



The answer to everything



### Variables

#### [variable] `*VAR-A*` -unbound-





#### [variable] `*VAR-B*` 100500



Just a var with docstring.



#### [variable] `*VAR-C*` -unbound-



Unbound var with docstring. `LISP` allows us to define docstring separately.



### Glossary

#### [glossary-term] `The Best Programming Language`



You really should use LISP!



### Locatives

#### [locative] `VARIABLE` &OPTIONAL INITFORM



Refers to a global special variable. INITFORM, or if not specified,
the global value of the variable is included in the documentation.



### Macro

#### [macro] `THE-MACRO`



Macro's docstring.

We can refer `FOO` function from here.



### Generic and methods

#### [generic-function] `GET-ADDRESS` ENTITY



Docstring of the generic function.



#### [method] `GET-ADDRESS` (USER USER)



Returns user's address.



### Package

#### [package] `40ANTS-DOC`



See `40ANTS-DOC:@INDEX`.



#### [package] `40ANTS-DOC-FULL`





#### [package] `PLAYGROUND`





### Restarts

#### [restart] `RETRY-THIS-ERROR`



Some docstring for restart



### Types

#### [type] `A-FEW` &OPTIONAL (TYPE 'INTEGER)



Very small integer, less or equal than 3.



### Inclusions

```lisp
(defun user ()
  "Just to check locatives in docstrings"
  (values))

(define-compiler-macro bar (&whole form arg)
  "A custom dostring for a compiler macro. Optimizes a call to the `BAR` function, when arg is an atom."
  (if (atom arg)
      arg
      form))

;; Надо разобраться почему не работает явное указание locatives
```
### TODO

Here what I need to [check](https://yandex.ru) and fix:

1. enable all locatives

2. check dependencies of core

3. reenable tests suite

4. fix how do `M-.` work in `SLIME`

5. fix transcribe

6. create integration with `SLY`


Finally the other `Second Page` section link.

## Second Page

### [class] `USER` (THE-OBJECT)



Class for all users except admins.

```python
def foo():
    pass
```
This was the `Python` code.



