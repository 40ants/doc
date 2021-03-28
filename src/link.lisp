(uiop:define-package #:40ants-doc/link
  (:use #:cl)
  (:import-from #:40ants-doc/reference)
  (:import-from #:named-readtables)
  (:import-from #:ironclad)
  (:import-from #:babel)
  (:import-from #:pythonic-string-reader))
(in-package 40ants-doc/link)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


;;; This is a link target. REFERENCE is the thing it is about, PAGE is
;;; where its documentation will go, ID is the markdown reference link
;;; id and PAGE-TO-N-USES is a hash table that counts how many times
;;; this was linked to for each page.
(defstruct link
  reference
  page
  id
  page-to-n-uses)

;;; A list of LINK objects. If a reference occurs multiple times,
;;; earlier links have precedence.
(defparameter *links* ())

(defun find-link-by-id (id)
  (find id *links* :key #'link-id :test #'equal))


(defun reference-page (reference)
  (let ((link (find-link reference)))
    (when link
      (link-page link))))


(defvar *document-link-code* t
  """When true, during the process of generating documentation for a
  [SECTION][class], HTML anchors are added before the documentation of
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

      - [function] FOO X

          Calls [`BAR`][1] on `X`.

  Instead of `BAR`, one can write `[bar][]` or ``[`bar`][]`` as well.
  Since symbol names are parsed according to READTABLE-CASE, character
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

      - [function] FOO X

          Calls `BAR`([`1`][link-id-1] [`2`][link-id-2]) on `X`.

  This situation occurs in PAX with SECTION which is both a class (see
  [SECTION][class]) and a locative type denoted by a symbol (see
  [SECTION][locative]). Back in the example above, clearly,
  there is no reason to link to type `BAR`, so one may wish to select
  the function locative. There are two ways to do that. One is to
  specify the locative explicitly as the id of a reference link:

      "Calls [BAR][function] on X."

  However, if in the text there is a locative immediately before or
  after the symbol, then that locative is used to narrow down the
  range of possibilities. This is similar to what the `M-.` extension
  does. In a nutshell, if `M-.` works without questions then the
  documentation will contain a single link. So this also works without
  any markup:

      "Calls function `BAR` on X."

  This last option needs backticks around the locative if it's not a
  single symbol.

  Note that [*DOCUMENT-LINK-CODE*][variable] can be combined with
  [`*DOCUMENT-UPPERCASE-IS-CODE*`][] to have links generated for
  uppercase names with no quoting required.""")

(defvar *document-link-sections* t
  "When true, HTML anchors are generated before the heading of
  sections which allows the table of contents to contain links and
  also code-like references to sections (like `@FOO-MANUAL`) to be
  translated to links with the section title being the name of the
  link.")

(defparameter *document-min-link-hash-length* 4
  "Recall that markdown reference style links (like `[label][id]`) are
  used for linking to sections and code. It is desirable to have ids
  that are short to maintain legibility of the generated markdown, but
  also stable to reduce the spurious diffs in the generated
  documentation which can be a pain in a version control system.

  Clearly, there is a tradeoff here. This variable controls how many
  characters of the md5 sum of the full link id (the reference as a
  string) are retained. If collisions are found due to the low number
  of characters, then the length of the hash of the colliding
  reference is increased.

  This variable has no effect on the HTML generated from markdown, but
  it can make markdown output more readable.")

(defun hash-link (string detect-collision-fn
                  &key (min-n-chars *document-min-link-hash-length*))
  (let ((hex (ironclad:byte-array-to-hex-string
              (ironclad:digest-sequence 'ironclad:md5
                                        (babel:string-to-octets string)))))
    (loop for i upfrom min-n-chars below 32
          do (let ((hash (subseq hex 0 (min 32 i))))
               (unless (funcall detect-collision-fn hash)
                 (return-from hash-link hash))))
    (assert nil () "MD5 collision collision detected.")))


(defun find-link (reference)
  (find reference *links* :key #'link-reference :test #'40ants-doc/reference::reference=))

