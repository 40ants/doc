(defpackage #:40ants-doc/builder/vars
  (:use #:cl))
(in-package 40ants-doc/builder/vars)


;;;; Section numbering, table of contents and navigation links

(defvar *document-max-numbering-level* 3
  "A non-negative integer. In their hierarchy, sections on levels less
  than this value get numbered in the format of `3.1.2`. Setting it to
  0 turns numbering off.")

(defvar *document-max-table-of-contents-level* 3
  "A non-negative integer. Top-level sections are given a table of
  contents which includes a nested tree of section titles whose depth
  is limited by this value. Setting it to 0 turns generation of the
  table of contents off. If *DOCUMENT-LINK-SECTIONS* is true, then the
  table of contents will link to the sections.")

(defvar *document-text-navigation* nil
  "If true, then before each heading a line is printed with links to
  the previous, parent and next section. Needs
  *DOCUMENT-LINK-SECTIONS* to be on to work.")

(defvar *document-fancy-html-navigation* t
  "If true and the output format is HTML, then headings get a
  navigation component that consists of links to the previous, parent,
  next section and a permalink. This component is normally hidden, it
  is visible only when the mouse is over the heading. Needs
  *DOCUMENT-LINK-SECTIONS* to be on to work.")

(defvar *heading-number* ())

(defvar *heading-level* 0)

(defvar *collecting-headings-p* nil)

(defvar *find-definitions-right-trim* ",:.>")
(defparameter *find-definitions-right-trim-2* ",:.>sS")


(defvar *document-mark-up-signatures* t
  "When true, some things such as function names and arglists are
  rendered as bold and italic. In :HTML output, locative types become
  links to sources (if :SOURCE-URI-FN is provided, see DOCUMENT), and
  the symbol becomes a self-link for your permalinking pleasure.

  For example, a reference is rendered in markdown roughly as:

      - [function] foo x y

  With this option on, the above becomes:

      - [function] **foo** *x y*

  Also, in HTML `**foo**` will be a link to that very entry and
  `[function]` may turn into a link to sources.")


;;; Remember the stream so that it can be restored in time for the
;;; printing of table of contents entries even if the stream is
;;; changed by paging.
(defvar *table-of-contents-stream* nil)


