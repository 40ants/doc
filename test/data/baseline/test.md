<a id='x-2840ANTS-DOC-TEST-2FTEST-3A-3A-40TEST-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

# @TEST

## Table of Contents

- 1 @TEST-EXAMPLES
- [2 test other title][44c9]
- [3 Link to @TEST-OTHER][5c25]
- [4 Link to @TEST][ba0a]
- [5 \`CODE\` \*italic\* \_italic2\_ \*bold\* \[link\]\[sdf\] \<thing\>][a290]

###### \[in package 40ANTS-DOC-TEST/TEST\]
[`*TEST-VARIABLE*`][7eec]
[`*TEST-VARIABLE*`][7eec]
[`*test-variable*`][7eec]
[`*test-variable*`][7eec]
[`40ants-doc-test/test:*test-variable*`][]
[`FOO`][351d] function, function [`FOO`][351d],
[`FOO`][351d] function, function [`FOO`][351d],
[`FOO`][351d] `function`, `function` [`FOO`][351d],
[`FOO`][351d] `function`, `function` [`FOO`][351d],
[`foo`][351d],
[`foo`][351d],
[`FOO`][351d],
[`FOO`][351d],
[`foo`][351d],
[`foo`][351d],
[`FOO`][351d],
[`FOO`][351d],

[`FOO-A`][be17] `(accessor foo)`, `(accessor foo)` [`FOO-A`][be17],
[`FOO-A`][be17] `(accessor foo)`, `(accessor foo)` [`FOO-A`][be17],
[`foo-a`][be17],
[`foo-a`][be17],
[`FOO-A`][be17],
[`FOO-A`][be17],
[`foo-a`][be17],
[`foo-a`][be17],
[`FOO-A`][be17],
[`FOO-A`][be17]

`->MAX`

Escaped: FOO `FOO` *NAVIGATION-TEST-CASES*
Non escaped: `FOO`([`0`][dd4d] [`1`][351d]) [`*TEST-VARIABLE*`][7eec]
[test other title][44c9]

This should be no link because the page of `@TEST-EXAMPLES`
has `:URI-FRAGMENT` `NIL`.

This is code: T

Plural uppercase ambiguous symbol: see `FOO`([`0`][dd4d] [`1`][351d])s

Plural uppercase symbol: [`TEST-GF`][8437]s

Plural uppercase dislocated symbol: `->MAX`s

See
[`FOO`][dd4d] compiler-macro

See [`FOO`][dd4d]
compiler-macro

See
compiler-macro [`FOO`][dd4d]

See compiler-macro
[`FOO`][dd4d]

See
compiler-macro 
[`FOO`][dd4d]

See
`FOO`([`0`][dd4d] [`1`][351d])

```cl-transcript
(values (print (1+ 2)) :aaa)
..
.. 3 
=> 3
=> :AAA

```

```cl-transcript
(values '(1 2) '(3 4))
;=> (1 2)
;=> (3
;->  4)

```

```cl-transcript
(make-array 12 :initial-element 0d0)
=> #(0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
     0.0d0)

```

In documentation, when the only ambiguity is between a generic
function and its methods, it's resolved in favor if the gf:
[`TEST-GF`][8437].

<a id='x-2840ANTS-DOC-TEST-2FTEST-3A-3AFOO-20FUNCTION-29'></a>

- [function] **FOO** 

<a id='x-2840ANTS-DOC-TEST-2FTEST-3A-3AFOO-20-28COMPILER-MACRO-29-29'></a>

- [compiler-macro] **FOO** 

<a id='x-2840ANTS-DOC-TEST-2FTEST-3A-3AFOO-A-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-2040ANTS-DOC-TEST-2FTEST-3A-3AFOO-29-29'></a>

- [accessor] **FOO-A** *FOO*

<a id='x-2840ANTS-DOC-TEST-2FTEST-3A-3A-2ATEST-VARIABLE-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*TEST-VARIABLE\*** *(XXX 34)*



<a id='x-2840ANTS-DOC-TEST-2FTEST-3A-3ATEST-GF-20GENERIC-FUNCTION-29'></a>

- [generic-function] **TEST-GF** *X*

<a id='x-2840ANTS-DOC-TEST-2FTEST-3A-3ATEST-GF-20-28METHOD-20NIL-20-28NUMBER-29-29-29'></a>

- [method] **TEST-GF** *(X NUMBER)*

<a id='x-2840ANTS-DOC-TEST-2FTEST-3A-3ATEST-GF-20-28METHOD-20NIL-20-28-28EQL-207-29-29-29-29'></a>

- [method] **TEST-GF** *(X (EQL 7))*

<a id='x-2840ANTS-DOC-TEST-2FTEST-3A-40TEST-SECTION-WITH-LINK-TO-OTHER-PAGE-IN-TITLE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 3 Link to @TEST-OTHER

Same link in docstring to [test other title][44c9].

<a id='x-2840ANTS-DOC-TEST-2FTEST-3A-40TEST-SECTION-WITH-LINK-TO-SAME-PAGE-IN-TITLE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 4 Link to @TEST

Same link in docstring to [@TEST][914a].

<a id='x-2840ANTS-DOC-TEST-2FTEST-3A-3A-40TEST-TRICKY-TITLE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 5 \`CODE\` \*italic\* \_italic2\_ \*bold\* \[link\]\[sdf\] \<thing\>

backlink [@TEST][914a]

  [351d]: #x-2840ANTS-DOC-TEST-2FTEST-3A-3AFOO-20FUNCTION-29 "(40ANTS-DOC-TEST/TEST::FOO FUNCTION)"
  [44c9]: other/test-other.md#x-2840ANTS-DOC-TEST-2FTEST-3A-3A-40TEST-OTHER-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "test other title"
  [5c25]: #x-2840ANTS-DOC-TEST-2FTEST-3A-40TEST-SECTION-WITH-LINK-TO-OTHER-PAGE-IN-TITLE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Link to @TEST-OTHER"
  [7eec]: #x-2840ANTS-DOC-TEST-2FTEST-3A-3A-2ATEST-VARIABLE-2A-20-28VARIABLE-29-29 "(40ANTS-DOC-TEST/TEST::*TEST-VARIABLE* (VARIABLE))"
  [8437]: #x-2840ANTS-DOC-TEST-2FTEST-3A-3ATEST-GF-20GENERIC-FUNCTION-29 "(40ANTS-DOC-TEST/TEST::TEST-GF GENERIC-FUNCTION)"
  [914a]: #x-2840ANTS-DOC-TEST-2FTEST-3A-3A-40TEST-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "40ANTS-DOC-TEST/TEST::@TEST"
  [a290]: #x-2840ANTS-DOC-TEST-2FTEST-3A-3A-40TEST-TRICKY-TITLE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "`CODE` *italic* _italic2_ *bold* [link][sdf] <thing>"
  [ba0a]: #x-2840ANTS-DOC-TEST-2FTEST-3A-40TEST-SECTION-WITH-LINK-TO-SAME-PAGE-IN-TITLE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Link to @TEST"
  [be17]: #x-2840ANTS-DOC-TEST-2FTEST-3A-3AFOO-A-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-2040ANTS-DOC-TEST-2FTEST-3A-3AFOO-29-29 "(40ANTS-DOC-TEST/TEST::FOO-A (40ANTS-DOC/LOCATIVES:ACCESSOR 40ANTS-DOC-TEST/TEST::FOO))"
  [dd4d]: #x-2840ANTS-DOC-TEST-2FTEST-3A-3AFOO-20-28COMPILER-MACRO-29-29 "(40ANTS-DOC-TEST/TEST::FOO (COMPILER-MACRO))"
