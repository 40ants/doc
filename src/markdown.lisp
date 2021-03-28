(defpackage #:40ants-doc/markdown
  (:use #:cl)
  (:import-from #:40ants-doc/core
                #:defsection)
  (:import-from #:40ants-doc/utils)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader))
(in-package 40ants-doc/markdown)


(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(defsection @mgl-pax-markdown-support (:title "Markdown Support")
  "The [Markdown][markdown] in docstrings is processed with the
  [3BMD][3bmd] library."
  (@mgl-pax-markdown-indentation section)
  (@mgl-pax-markdown-syntax-highlighting section)
  (@mgl-pax-mathjax section))

(defsection @mgl-pax-markdown-indentation (:title "Indentation")
  """Docstrings can be indented in any of the usual styles. PAX
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

  See [DOCUMENT-OBJECT][(method () (string t))] for the details.""")

(defsection @mgl-pax-markdown-syntax-highlighting (:title "Syntax highlighting")
  "For syntax highlighting, github's [fenced code
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
  [fenced-code-blocks]: https://help.github.com/articles/github-flavored-markdown#fenced-code-blocks")

(defsection @mgl-pax-mathjax (:title "MathJax")
  """Displaying pretty mathematics in TeX format is supported via
  MathJax. It can be done inline with `$` like this:

      $\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$

  which is diplayed as $\int_0^\infty e^{-x^2}
  dx=\frac{\sqrt{\pi}}{2}$, or it can be delimited by `$$` like this:

      $$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$

  to get: $$\int_0^\infty e^{-x^2} dx=\frac{\sqrt{\pi}}{2}$$

  MathJax will leave code blocks (including those inline with
  backticks) alone. Outside code blocks, escape `$` by prefixing it
  with a backslash to scare MathJax off.

  Escaping all those backslashes in TeX fragments embedded in Lisp
  strings can be a pain. [Pythonic String
  Reader](https://github.com/smithzvk/pythonic-string-reader) can help
  with that.""")


(defun map-markdown-parse-tree (tags stop-tags handle-strings fn string)
  (let* ((3bmd-grammar:*smart-quotes* nil)
         (parse-tree
           ;; To be able to recognize symbols like FOO* join (...
           ;; "FOO" "*" ...) to look like (... "FOO*" ...).
           (40ants-doc/utils::join-consecutive-non-blank-strings-in-parse-tree
            (3bmd-grammar:parse-doc string))))
    (with-output-to-string (out)
      (3bmd::print-doc-to-stream-using-format
       (40ants-doc/utils::transform-tree
        (lambda (parent tree)
          (40ants-doc/utils::defer-tag-handling tags stop-tags handle-strings
            fn parent tree))
        parse-tree)
       out :markdown))))

