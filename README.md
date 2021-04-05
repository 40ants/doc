<a id='x-2840ANTS-DOC-2FTRANSCRIBE-3A-40TRANSCRIPT-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

# Transcripts

## Table of Contents

- [1 Transcribing with Emacs][a18b]
- [2 Transcript API][650c]

###### \[in package 40ANTS-DOC/TRANSCRIBE\]
What are transcripts for? When writing a tutorial, one often wants
to include a REPL session with maybe a few defuns and a couple of
forms whose output or return values are shown. Also, in a function's
docstring an example call with concrete arguments and return values
speaks volumes. A transcript is a text that looks like a repl
session, but which has a light markup for printed output and return
values, while no markup (i.e. prompt) for lisp forms. The PAX
transcripts may include output and return values of all forms, or
only selected ones. In either case the transcript itself can be
easily generated from the source code.

The main worry associated with including examples in the
documentation is that they tend to get out-of-sync with the code.
This is solved by being able to parse back and update transcripts.
In fact, this is exactly what happens during documentation
generation with PAX. Code sections tagged `cl-transcript` are
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

    (in-readtable pythonic-string-syntax)


<a id='x-2840ANTS-DOC-2FTRANSCRIBE-3A-40TRANSCRIPT-EMACS-INTEGRATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 1 Transcribing with Emacs

Typical transcript usage from within Emacs is simple: add a lisp
form to a docstring or comment at any indentation level. Move the
cursor right after the end of the form as if you were to evaluate it
with `C-x C-e`. The cursor is marked by `#\^`:

    This is part of a docstring.
    
    ```cl-transcript
    (values (princ :hello) (list 1 2))^
    ```

Note that the use of fenced code blocks with the language tag
`cl-transcript` is only to tell PAX to perform consistency checks at
documentation generation time.

Now invoke the elisp function `40ants-doc-transcribe` where the cursor
is and the fenced code block from the docstring becomes:

    (values (princ :hello) (list 1 2))
    .. HELLO
    => :HELLO
    => (1 2)
    ^

Then you change the printed message and add a comment to the second
return value:

    (values (princ :hello-world) (list 1 2))
    .. HELLO
    => :HELLO
    => (1
        ;; This value is arbitrary.
        2)

When generating the documentation you get a
[`TRANSCRIPTION-CONSISTENCY-ERROR`][0333] because the printed output and the
first return value changed so you regenerate the documentation by
marking the region of bounded by `#\|` and the cursor at `#\^` in
the example:

    |(values (princ :hello-world) (list 1 2))
    .. HELLO
    => :HELLO
    => (1
        ;; This value is arbitrary.
        2)
    ^

then invoke the elisp function `40ants-doc-retranscribe-region` to get:

    (values (princ :hello-world) (list 1 2))
    .. HELLO-WORLD
    => :HELLO-WORLD
    => (1
        ;; This value is arbitrary.
        2)
    ^

Note how the indentation and the comment of `(1 2)` was left alone
but the output and the first return value got updated.

Alternatively, `C-u 1 40ants-doc-transcribe` will emit commented markup:

    (values (princ :hello) (list 1 2))
    ;.. HELLO
    ;=> :HELLO
    ;=> (1 2)

`C-u 0 40ants-doc-retranscribe-region` will turn commented into
non-commented markup. In general, the numeric prefix argument is the
index of the syntax to be used in 40ants-doc:[`*SYNTAXES*`][83f3]. Without a
prefix argument `40ants-doc-retranscribe-region` will not change the
markup style.

Finally, not only do both functions work at any indentation level,
but in comments too:

    ;;;; (values (princ :hello) (list 1 2))
    ;;;; .. HELLO
    ;;;; => :HELLO
    ;;;; => (1 2)

Transcription support in emacs can be enabled by adding this to your
Emacs initialization file (or loading `elisp/transcribe.el`):

<a id='x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIBE-2EEL-20-2840ANTS-DOC-2FLOCATIVES-3AINCLUDE-20-23P-22-2Fhome-2Frunner-2Fwork-2Fdoc-2Fdoc-2Felisp-2Ftranscribe-2Eel-22-20-3AHEADER-NL-20-22-60-60-60elisp-22-20-3AFOOTER-NL-20-22-60-60-60-22-29-29'></a>

```elisp
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

<a id='x-2840ANTS-DOC-2FTRANSCRIBE-3A-40TRANSCRIPT-API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 2 Transcript API

<a id='x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIBE-20FUNCTION-29'></a>

- [function] **TRANSCRIBE** *INPUT OUTPUT &KEY UPDATE-ONLY (INCLUDE-NO-OUTPUT UPDATE-ONLY) (INCLUDE-NO-VALUE UPDATE-ONLY) (ECHO T) CHECK-CONSISTENCY DEFAULT-SYNTAX (INPUT-SYNTAXES \*SYNTAXES\*) (OUTPUT-SYNTAXES \*SYNTAXES\*)*

    Read forms from `INPUT` and write them (iff `ECHO`) to `OUTPUT`
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
    uses [`TRANSCRIBE`][55eb] markup syntax in this very example, so let's do it
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
    
    **Updating**
    
    [`TRANSCRIBE`][55eb] is able to parse its own output. If we transcribe the
    previous output above, we get it back exactly. However, if we remove
    all output markers, leave only a placeholder value marker and
    pass `:UPDATE-ONLY` `T` with source:
    
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
    
    **No Output/Values**
    
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
    
    **Consistency Checks**
    
    If `CHECK-CONSISTENCY` is true, then [`TRANSCRIBE`][55eb] signals a continuable
    [`TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR`][66b8] whenever a form's output as a
    string is different from what was in `INPUT`, provided that `INPUT`
    contained the output. Similary, for values, a continuable
    [`TRANSCRIPTION-VALUES-CONSISTENCY-ERROR`][6b8a] is signalled if a value read
    from the source does not print as the as the value returned by `EVAL`.
    This allows readable values to be hand-indented without failing
    consistency checks:
    
    ```commonlisp
    (list 1 2)
    => (1
          2)
    ```
    
    **Unreadable Values**
    
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
    `EVAL` is performed with `STRING=`. That is, the value from `EVAL` is
    printed to a string and compared to the source value. Hence, any
    change to unreadable values will break consistency checks. This is
    most troublesome with instances of classes with the default
    `PRINT-OBJECT` method printing the memory address. There is currently
    no remedy for that, except for customizing `PRINT-OBJECT` or not
    transcribing that kind of stuff.
    
    **Syntaxes**
    
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
    use `:DEFAULT-SYNTAX` `:COMMENTED-1:`
    
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

<a id='x-2840ANTS-DOC-2FTRANSCRIBE-3A-2ASYNTAXES-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*SYNTAXES\*** *((:DEFAULT (:OUTPUT "..") (:NO-VALUE "=> ; No value") (:READABLE "=>")
  (:UNREADABLE "==>") (:UNREADABLE-CONTINUATION "-->"))
 (:COMMENTED-1 (:OUTPUT ";..") (:NO-VALUE ";=> ; No value") (:READABLE ";=>")
  (:READABLE-CONTINUATION ";->") (:UNREADABLE ";==>")
  (:UNREADABLE-CONTINUATION ";-->"))
 (:COMMENTED-2 (:OUTPUT ";;..") (:NO-VALUE ";;=> ; No value")
  (:READABLE ";;=>") (:READABLE-CONTINUATION ";;->") (:UNREADABLE ";;==>")
  (:UNREADABLE-CONTINUATION ";;-->")))*

    The default syntaxes used by [`TRANSCRIBE`][55eb] for reading and writing
    lines containing output and values of an evaluated form.
    
    A syntax is a list of of the form `(SYNTAX-ID &REST PREFIXES)` where
    `PREFIXES` is a list of `(PREFIX-ID PREFIX-STRING)` elements. For
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
    
    See [`TRANSCRIBE`][55eb] for how the actual syntax to be used is selected.

<a id='x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIPTION-ERROR-20CONDITION-29'></a>

- [condition] **TRANSCRIPTION-ERROR** *ERROR*

    Represents syntactic errors in the SOURCE argument
    of [`TRANSCRIBE`][55eb] and also serves as the superclass of
    [`TRANSCRIPTION-CONSISTENCY-ERROR`][0333].

<a id='x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIPTION-CONSISTENCY-ERROR-20CONDITION-29'></a>

- [condition] **TRANSCRIPTION-CONSISTENCY-ERROR** *TRANSCRIPTION-ERROR*

    A common superclass for
    [`TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR`][66b8] and
    [`TRANSCRIPTION-VALUES-CONSISTENCY-ERROR`][6b8a].

<a id='x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR-20CONDITION-29'></a>

- [condition] **TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR** *TRANSCRIPTION-CONSISTENCY-ERROR*

    Signaled (with `CERROR`) by [`TRANSCRIBE`][55eb] when invoked
    with `:CHECK-CONSISTENCY` and the output of a form is not the same as
    what was parsed.

<a id='x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIPTION-VALUES-CONSISTENCY-ERROR-20CONDITION-29'></a>

- [condition] **TRANSCRIPTION-VALUES-CONSISTENCY-ERROR** *TRANSCRIPTION-CONSISTENCY-ERROR*

    Signaled (with `CERROR`) by [`TRANSCRIBE`][55eb] when invoked
    with `:CHECK-CONSISTENCY` and the values of a form are inconsistent
    with their parsed representation.
<a id='x-2840ANTS-DOC-2FBUILDER-3A-40GENERATING-DOCUMENTATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

# Generating Documentation

## Table of Contents

- [1 Github Workflow][21a6]
- [2 PAX World][90f8]

###### \[in package 40ANTS-DOC/BUILDER\]
Two convenience functions are provided to serve the common case of
having an `ASDF` system with some readmes and a directory with for the
HTML documentation and the default css stylesheet.

<a id='x-2840ANTS-DOC-2FBUILDER-3AUPDATE-ASDF-SYSTEM-HTML-DOCS-20FUNCTION-29'></a>

- [function] **UPDATE-ASDF-SYSTEM-HTML-DOCS** *SECTIONS ASDF-SYSTEM &KEY PAGES (TARGET-DIR (ASDF/SYSTEM:SYSTEM-RELATIVE-PATHNAME ASDF-SYSTEM "doc/")) (UPDATE-CSS-P T)*

    Generate pretty HTML documentation for a single `ASDF` system,
    possibly linking to github. If `UPDATE-CSS-P`, copy the CSS style
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


<a id='x-2840ANTS-DOC-2FBUILDER-3AUPDATE-ASDF-SYSTEM-README-20FUNCTION-29'></a>

- [function] **UPDATE-ASDF-SYSTEM-README** *SECTIONS ASDF-SYSTEM &KEY (FORMAT :MARKDOWN)*

    Convenience function to generate readme file in the directory
    holding the `ASDF-SYSTEM` definition.
    
    By default, README.md is generated. It has anchors, links, inline code,
    and other markup added. Not necessarily the easiest on the eye in an editor,
    but looks good on github.
    
    You can provide `:FORMAT :PLAIN` argument to generate README instead.
    It will be optimized for reading in text format. Has no links and
    cluttery markup.
    
    Example usage:
    
    ```
    (update-asdf-system-readme @40ants-doc-manual :40ants-doc)
    ```


<a id='x-2840ANTS-DOC-2FBUILDER-3A-2ADOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-HTML-MAX-NAVIGATION-TABLE-OF-CONTENTS-LEVEL\*** *NIL*

    `NIL` or a non-negative integer. If non-NIL, it overrides
    *DOCUMENT-MAX-NUMBERING-LEVEL* in dynamic HTML table of contents on
    the left of the page.

<a id='x-2840ANTS-DOC-2FBUILDER-3A-2ADOCUMENT-HTML-TOP-BLOCKS-OF-LINKS-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS\*** *NIL*

    A list of blocks of links to be display on the sidebar on the left,
    above the table of contents. A block is of the form `(&KEY TITLE ID
    LINKS)`, where `TITLE` will be displayed at the top of the block in a
    HTML `DIV` with `ID`, followed by the links. `LINKS` is a list
    of `(URI LABEL) elements.`

<a id='x-2840ANTS-DOC-2FBUILDER-3A-2ADOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*DOCUMENT-HTML-BOTTOM-BLOCKS-OF-LINKS\*** *NIL*

    Like [`*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS*`][d72c], only it is displayed
    below the table of contents.

<a id='x-2840ANTS-DOC-2FGITHUB-3A-40GITHUB-WORKFLOW-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 1 Github Workflow

###### \[in package 40ANTS-DOC/GITHUB\]
It is generally recommended to commit generated readmes (see
UPDATE-ASDF-SYSTEM-README) so that users have something to read
without reading the code and sites like github can display them.

HTML documentation can also be committed, but there is an issue with
that: when linking to the sources (see [`MAKE-GITHUB-SOURCE-URI-FN`][8e3c]),
the commit id is in the link. This means that code changes need to
be committed first, then HTML documentation regenerated and
committed in a followup commit.

The second issue is that github is not very good at serving HTMLs
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

This way the HTML documentation will be available at
`http://<username>.github.io/<repo-name>`. It is probably a good
idea to add section like the @MGL-PAX-LINKS section to allow jumping
between the repository and the gh-pages site.

<a id='x-2840ANTS-DOC-2FGITHUB-3AMAKE-GITHUB-SOURCE-URI-FN-20FUNCTION-29'></a>

- [function] **MAKE-GITHUB-SOURCE-URI-FN** *ASDF-SYSTEM GITHUB-URI &KEY GIT-VERSION*

    Return a function suitable as `:SOURCE-URI-FN` of a page spec (see
    the PAGES argument of DOCUMENT). The function looks the source
    location of the reference passed to it, and if the location is
    found, the path is made relative to the root directory of
    `ASDF-SYSTEM` and finally an URI pointing to github is returned. The
    URI looks like this:
    
        https://github.com/melisgl/mgl-pax/blob/master/src/pax-early.lisp#L12
    
    "master" in the above link comes from `GIT-VERSION`.
    
    If `GIT-VERSION` is `NIL`, then an attempt is made to determine to
    current commit id from the `.git` in the directory holding
    `ASDF-SYSTEM`. If no `.git` directory is found, then no links to
    github will be generated.
    
    A separate warning is signalled whenever source location lookup
    fails or if the source location points to a directory not below the
    directory of `ASDF-SYSTEM`.

<a id='x-2840ANTS-DOC-2FWORLD-3A-40WORLD-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 2 PAX World

###### \[in package 40ANTS-DOC/WORLD\]
MGL-PAX supported a "World" which was a registry of documents, which can generate
cross-linked HTML documentation pages for all the registered
documents.

But I decided to drop this feature for now, because usually build libraries documentation
separately as part of their CI pipline.

If somebody want's cross referencing between different libraries, then instead
of building their docs simultaneously, I'd suggest to create an index of entities,
provided by libraries and to store them as a JSON file along with a library documentation.

This way it will be possible to enumerate such sources of cross references as usual URLS.

Such feature is not implemented in the `40ANTS-DOC` system yet, but probably it will be
useful for libraries built around the Weblocks. If you want to help and implement the
feature, please, let me know.
<a id='x-2840ANTS-DOC-2FMARKDOWN-3A-40MARKDOWN-SUPPORT-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

# Markdown Support

## Table of Contents

- [1 Indentation][10a2]
- [2 Syntax highlighting][d4a5]
- [3 MathJax][4a92]

###### \[in package 40ANTS-DOC/MARKDOWN\]
The [Markdown][markdown] in docstrings is processed with the
[3BMD][3bmd] library.

<a id='x-2840ANTS-DOC-2FMARKDOWN-3A-40MARKDOWN-INDENTATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 1 Indentation

Docstrings can be indented in any of the usual styles. PAX
normalizes indentation by converting:

    (defun foo ()
      "This is

indented
differently")

to

    (defun foo ()
      "This is

indented
differently")

See [DOCUMENT-OBJECT][(method () (string t))] for the details.

<a id='x-2840ANTS-DOC-2FMARKDOWN-3A-40MARKDOWN-SYNTAX-HIGHLIGHTING-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 2 Syntax highlighting

For syntax highlighting, github's [fenced code
blocks][fenced-code-blocks] markdown extension to mark up code
blocks with triple backticks is enabled so all you need to do is
write:

    ```elisp
    (defun foo ())
    ```

to get syntactically marked up HTML output. Copy `src/style.css`
from PAX and you are set. The language tag, `elisp` in this example,
is optional and defaults to `common-lisp`.

See the documentation of [3BMD][3bmd] and [colorize][colorize] for
the details.

[3bmd]: https://github.com/3b/3bmd 

[colorize]: https://github.com/redline6561/colorize/ 

[fenced-code-blocks]: https://help.github.com/articles/github-flavored-markdown#fenced-code-blocks 


<a id='x-2840ANTS-DOC-2FMARKDOWN-3A-40MATHJAX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29'></a>

## 3 MathJax

Displaying pretty mathematics in TeX format is supported via
MathJax. It can be done inline with `$` like this:

    $\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$

which is diplayed as $\int\_0^\infty e^{-x^2}
dx=\frac{\sqrt{\pi}}{2}$, or it can be delimited by `$$` like this:

    $$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$

to get: $$\int\_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$

MathJax will leave code blocks (including those inline with
backticks) alone. Outside code blocks, escape `$` by prefixing it
with a backslash to scare MathJax off.

Escaping all those backslashes in TeX fragments embedded in Lisp
strings can be a pain. [Pythonic String
Reader](https://github.com/smithzvk/pythonic-string-reader) can help
with that.

  [0333]: #x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIPTION-CONSISTENCY-ERROR-20CONDITION-29 "(40ANTS-DOC/TRANSCRIBE:TRANSCRIPTION-CONSISTENCY-ERROR CONDITION)"
  [10a2]: #x-2840ANTS-DOC-2FMARKDOWN-3A-40MARKDOWN-INDENTATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Indentation"
  [21a6]: #x-2840ANTS-DOC-2FGITHUB-3A-40GITHUB-WORKFLOW-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Github Workflow"
  [4a92]: #x-2840ANTS-DOC-2FMARKDOWN-3A-40MATHJAX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "MathJax"
  [55eb]: #x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIBE-20FUNCTION-29 "(40ANTS-DOC/TRANSCRIBE:TRANSCRIBE FUNCTION)"
  [650c]: #x-2840ANTS-DOC-2FTRANSCRIBE-3A-40TRANSCRIPT-API-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Transcript API"
  [66b8]: #x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR-20CONDITION-29 "(40ANTS-DOC/TRANSCRIBE:TRANSCRIPTION-OUTPUT-CONSISTENCY-ERROR CONDITION)"
  [6b8a]: #x-2840ANTS-DOC-2FTRANSCRIBE-3ATRANSCRIPTION-VALUES-CONSISTENCY-ERROR-20CONDITION-29 "(40ANTS-DOC/TRANSCRIBE:TRANSCRIPTION-VALUES-CONSISTENCY-ERROR CONDITION)"
  [83f3]: #x-2840ANTS-DOC-2FTRANSCRIBE-3A-2ASYNTAXES-2A-20-28VARIABLE-29-29 "(40ANTS-DOC/TRANSCRIBE:*SYNTAXES* (VARIABLE))"
  [8e3c]: #x-2840ANTS-DOC-2FGITHUB-3AMAKE-GITHUB-SOURCE-URI-FN-20FUNCTION-29 "(40ANTS-DOC/GITHUB:MAKE-GITHUB-SOURCE-URI-FN FUNCTION)"
  [90f8]: #x-2840ANTS-DOC-2FWORLD-3A-40WORLD-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "PAX World"
  [a18b]: #x-2840ANTS-DOC-2FTRANSCRIBE-3A-40TRANSCRIPT-EMACS-INTEGRATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Transcribing with Emacs"
  [d4a5]: #x-2840ANTS-DOC-2FMARKDOWN-3A-40MARKDOWN-SYNTAX-HIGHLIGHTING-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29 "Syntax highlighting"
  [d72c]: #x-2840ANTS-DOC-2FBUILDER-3A-2ADOCUMENT-HTML-TOP-BLOCKS-OF-LINKS-2A-20-28VARIABLE-29-29 "(40ANTS-DOC/BUILDER:*DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS* (VARIABLE))"

* * *
###### \[generated by [40ANTS-DOC](https://40ants.com/doc)\]
