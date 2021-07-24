(defpackage #:40ants-doc/document
  (:use #:cl)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader)
  (:export
   #:document
   #:document-object))
(in-package 40ants-doc/document)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(defgeneric document-object (object stream)
  (:method (object stream)
    (format stream "Object ~S has no 40ants-doc/document:document-object method."
            object)))


(defgeneric document (object &key stream pages format)
  (:documentation
     """Write OBJECT in FORMAT to STREAM diverting some output to PAGES.
  FORMAT can be anything [3BMD][3bmd] supports which is
  currently :MARKDOWN, :HTML and :PLAIN. STREAM may be a stream
  object, T or NIL as with CL:FORMAT.

  Most often, this function is called on section objects
  like `(DOCUMENT @MANUAL)`, but it supports all kinds of
  objects for which DOCUMENT-OBJECT is defined. To look up the
  documentation of function DOCUMENT:

      (document #'document)

  To generate the documentation for separate libraries with automatic
  cross-links:

      (document (list @cube-manual @mat-manual))

  Note that not only first class objects can have documentation. For
  instance, variables and deftypes are not represented by objects.
  That's why CL:DOCUMENTATION has a :DOC-TYPE argument. DOCUMENT does
  not have anything like that, instead it relies on 40ANTS-DOC/REFERENCE::REFERENCE objects
  to carry the extra information. We are going to see later how
  references and locatives work. Until then, here is an example on how
  to look up the documentation of type `FOO`:

      (document (locate 'foo 'type))

  One can call DESCRIBE on [40ANTS-DOC:SECTION][class] objects to get
  documentation in markdown format with less markup than the default.
  See [DESCRIBE-OBJECT][(METHOD () (40ANTS-DOC:SECTION T))].

  There are quite a few special variables that affect how output is
  generated, see 40ANTS-DOC/DOC:@DOCUMENTATION-PRINTER-VARIABLES.

  The rest of this description deals with how to generate multiple
  pages.

  The PAGES argument is to create multi-page documents by routing some
  of the generated output to files, strings or streams. PAGES is a
  list of page specification elements. A page spec is a plist with
  keys :OBJECTS, :OUTPUT, :URI-FRAGMENT, :SOURCE-URI-FN, :HEADER-FN
  and :FOOTER-FN. :OBJECTS is a list of objects (references are allowed
  but not required) whose documentation is to be sent to :OUTPUT.

  When documentation for an object is generated, the first matching
  page spec is used, where the object matches the page spec if it is
  contained in one of its :OBJECTS in the sense of
  40ANTS-DOC/REFERENCE-API::COLLECT-REACHABLE-OBJECTS.

  :OUTPUT can be a number things:

  - If it's a list whose first element is a string or a pathname, then
    output will be sent to the file denoted by that and the rest of
    the elements of the list are passed on as arguments to CL:OPEN.
    One extra keyword argument is :ENSURE-DIRECTORIES-EXIST. If it's
    true, ENSURE-DIRECTORIES-EXIST will be called on the pathname
    before it's opened.

  - If it's NIL, then output will be collected in a string.

  - If it's T, then output will be sent to `*STANDARD-OUTPUT*`.

  - If it's a stream, then output will be sent to that stream.

  If some pages are specified, DOCUMENT returns a list of designators
  for generated output. If a page whose :OUTPUT refers to a file that
  was created (which doesn't happen if nothing would be written to
  it), then the corresponding pathname is included in the list. For
  strings the string itself, while for streams the stream object is
  included in the list. This way it's possible to write some pages to
  files and some to strings and have the return value indicate what
  was created. The output designators in the returned list are ordered
  by creation time.

  If no PAGES are specified, DOCUMENT returns a single pathname,
  string or stream object according to the value of the STREAM
  argument.

  Note that even if PAGES is specified, STREAM acts as a catch all
  taking the generated documentation for references not claimed by any
  pages. Also, the filename, string or stream corresponding to STREAM
  is always the first element in list of generated things that is the
  return value.

  :HEADER-FN, if not NIL, is a function of a single stream argument
  which is called just before the first write to the page.
  Since :FORMAT :HTML only generates HTML fragments, this makes it
  possible to print arbitrary headers, typically setting the title,
  css stylesheet, or charset.

  :FOOTER-FN is similar to :HEADER-FN, but it's called after the last
  write to the page. For HTML, it typically just closes the body.

  :URI-FRAGMENT is a string such as `"doc/manual.html"` that specifies
  where the page will be deployed on a webserver. It defines how links
  between pages will look. If it's not specified and :OUTPUT refers
  to a file, then it defaults to the name of the file. If :URI-FRAGMENT
  is NIL, then no links will be made to or from that page.

  Finally, :SOURCE-URI-FN is a function of a single, 40ANTS-DOC/REFERENCE::REFERENCE
  argument. If it returns a value other than NIL, then it must be a
  string representing an URI. If FORMAT is :HTML and
  `40ANTS-DOC/BUILDER/VARS::*DOCUMENT-MARK-UP-SIGNATURES*` is true, then the locative as
  displayed in the signature will be a link to this uri. See
  40ANTS-DOC/GITHUB::MAKE-GITHUB-SOURCE-URI-FN.

  :PAGES may look something like this:

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
  ```"""))
