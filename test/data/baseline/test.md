<a id="x-2840ANTS-DOC-TEST-2FTEST-3A-3A-40TEST-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# Untitled

[`*TEST-VARIABLE*`](0ac1)
[`*TEST-VARIABLE*`](0ac1)
`test-variable`
[`*test-variable*`](0ac1)
`40ants-doc-test/test:*test-variable*`
[`FOO`](9ca9) function, function `FOO` ([`1`](d5eb) [`2`](9ca9)),
[`FOO`](9ca9) function, function `FOO` ([`1`](d5eb) [`2`](9ca9)),
`FOO` ([`1`](d5eb) [`2`](9ca9)) `function`, `function` `FOO` ([`1`](d5eb) [`2`](9ca9)),
`FOO` ([`1`](d5eb) [`2`](9ca9)) `function`, `function` `FOO` ([`1`](d5eb) [`2`](9ca9)),
[`foo`](9ca9),
[`foo`](9ca9),
[`FOO`](9ca9),
[`FOO`](9ca9),
[`foo`](9ca9),
[`foo`](9ca9),
[`FOO`](9ca9),
[`FOO`](9ca9),

[`FOO-A`](742b) `(accessor foo)`, `(accessor foo)` [`FOO-A`](742b),
[`FOO-A`](742b) `(accessor foo)`, `(accessor foo)` [`FOO-A`](742b),
[`foo-a`](742b),
[`foo-a`](742b),
[`FOO-A`](742b),
[`FOO-A`](742b),
[`foo-a`](742b),
[`foo-a`](742b),
[`FOO-A`](742b),
[`FOO-A`](742b)

->`MAX`

Escaped: \`FOO` ([`1`](d5eb) [`2`](9ca9)) `FOO` \`NAVIGATION-TEST-CASES`
Non escaped: `FOO` ([`1`](d5eb) [`2`](9ca9)) [`*TEST-VARIABLE*`](0ac1)
[`Test other title`](bec1)

This should be no link because the page of [`@TEST-EXAMPLES`](3bc3)
has `:URI-FRAGMENT` `NIL`.

This is code: T

Plural uppercase ambiguous symbol: see `FOO` ([`1`](d5eb) [`2`](9ca9))s

Plural uppercase symbol: `TEST-GF` ([`1`](97c8) [`2`](7f0a) [`3`](57ea))s

Plural uppercase dislocated symbol: ->`MAX`s

See
[`FOO`](d5eb) compiler-macro

See [`FOO`](d5eb)
compiler-macro

See
compiler-macro [`FOO`](d5eb)

See compiler-macro
[`FOO`](d5eb)

See
compiler-macro 
[`FOO`](d5eb)

See
`FOO` ([`1`](d5eb) [`2`](9ca9))

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
`TEST-GF` ([`1`](97c8) [`2`](7f0a) [`3`](57ea)).

<a id="x-2840ANTS-DOC-TEST-2FTEST-3A-3AFOO-20FUNCTION-29"></a>

## [function] `FOO`

<a id="x-2840ANTS-DOC-TEST-2FTEST-3A-3AFOO-20-28COMPILER-MACRO-29-29"></a>

## [compiler-macro] `FOO`

<a id="x-2840ANTS-DOC-TEST-2FTEST-3A-3AFOO-A-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-2040ANTS-DOC-TEST-2FTEST-3A-3AFOO-29-29"></a>

## [accessor] `FOO-A` (FOO) ()

<a id="x-2840ANTS-DOC-TEST-2FTEST-3A-3A-2ATEST-VARIABLE-2A-20-28VARIABLE-29-29"></a>

## [variable] `*TEST-VARIABLE*` (XXX 34)

<a id="x-2840ANTS-DOC-TEST-2FTEST-3A-3A-40TEST-EXAMPLES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Untitled

example section

<a id="x-2840ANTS-DOC-TEST-2FTEST-3A-3ATEST-GF-20GENERIC-FUNCTION-29"></a>

## [generic-function] `TEST-GF` X

<a id="x-2840ANTS-DOC-TEST-2FTEST-3A-3ATEST-GF-20-28METHOD-20NIL-20-28NUMBER-29-29-29"></a>

## [method] `TEST-GF` (X NUMBER)

<a id="x-2840ANTS-DOC-TEST-2FTEST-3A-3ATEST-GF-20-28METHOD-20NIL-20-28-28EQL-207-29-29-29-29"></a>

## [method] `TEST-GF` (X (EQL 7))

<a id="x-2840ANTS-DOC-TEST-2FTEST-3A-3A-40TEST-SECTION-WITH-LINK-TO-OTHER-PAGE-IN-TITLE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## [`Test other title`](bec1)

Same link in docstring to [`Test other title`](bec1).

<a id="x-2840ANTS-DOC-TEST-2FTEST-3A-3A-40TEST-SECTION-WITH-LINK-TO-SAME-PAGE-IN-TITLE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## [Link to @TEST](e40d)

Same link in docstring to [`@TEST`](e40d).

<a id="x-2840ANTS-DOC-TEST-2FTEST-3A-3A-40TEST-TRICKY-TITLE-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## `CODE` *italic* _italic2_ *bold* [link][sdf] <thing>

backlink [`@TEST`](e40d)


[0ac1]: #x-2840ANTS-DOC-TEST-2FTEST-3A-3A-2ATEST-VARIABLE-2A-20-28VARIABLE-29-29
[e40d]: #x-2840ANTS-DOC-TEST-2FTEST-3A-3A-40TEST-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[3bc3]: #x-2840ANTS-DOC-TEST-2FTEST-3A-3A-40TEST-EXAMPLES-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29
[d5eb]: #x-2840ANTS-DOC-TEST-2FTEST-3A-3AFOO-20-28COMPILER-MACRO-29-29
[9ca9]: #x-2840ANTS-DOC-TEST-2FTEST-3A-3AFOO-20FUNCTION-29
[742b]: #x-2840ANTS-DOC-TEST-2FTEST-3A-3AFOO-A-20-2840ANTS-DOC-2FLOCATIVES-3AACCESSOR-2040ANTS-DOC-TEST-2FTEST-3A-3AFOO-29-29
[97c8]: #x-2840ANTS-DOC-TEST-2FTEST-3A-3ATEST-GF-20-28METHOD-20NIL-20-28-28EQL-207-29-29-29-29
[7f0a]: #x-2840ANTS-DOC-TEST-2FTEST-3A-3ATEST-GF-20-28METHOD-20NIL-20-28NUMBER-29-29-29
[57ea]: #x-2840ANTS-DOC-TEST-2FTEST-3A-3ATEST-GF-20GENERIC-FUNCTION-29
[bec1]: other/test-other.md#x-2840ANTS-DOC-TEST-2FTEST-3A-3A-40TEST-OTHER-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29