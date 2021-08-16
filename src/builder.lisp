(uiop:define-package #:40ants-doc/builder
  (:use #:cl)
  (:import-from #:alexandria)
  (:import-from #:3bmd)
  (:import-from #:3bmd-code-blocks)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader)
  (:import-from #:40ants-doc
                #:section-name/builder/heading)
  (:import-from #:40ants-doc/builder/footer)
  (:import-from #:40ants-doc/builder/vars)
  (:import-from #:40ants-doc/page
                #:page-base-dir
                #:page-format)
  (:import-from #:40ants-doc/document)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/builder/printer)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:40ants-doc/github)
  (:import-from #:40ants-doc/world)
  (:import-from #:40ants-doc/themes/default)
  (:import-from #:40ants-doc/commondoc/page)
  (:import-from #:40ants-doc/commondoc/toc)
  (:import-from #:40ants-doc/commondoc/format)
  (:import-from #:40ants-doc/search)
  (:import-from #:40ants-doc/commondoc/transcribe)
  (:import-from #:40ants-doc/changelog)
  (:export
   #:*document-html-max-navigation-table-of-contents-level*
   #:*document-html-top-blocks-of-links*
   #:*document-html-bottom-blocks-of-links*
   #:render-to-string
   #:render-to-files
   #:update-asdf-system-docs))
(in-package 40ants-doc/builder)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)

(defsection @generating-documentation
    (:title "Generating Documentation"
     :ignore-words ("LABEL"))
  "Besides RENDER-TO-STRING and RENDER-TO-FILES a convenience function is provided
   to serve the common case of having an ASDF system with a readme and a directory for the
  HTML documentation."
  (update-asdf-system-docs function)
  (*document-html-max-navigation-table-of-contents-level* variable)
  (*document-html-top-blocks-of-links* variable)
  (*document-html-bottom-blocks-of-links* variable)
  (40ants-doc/changelog::@index section)
  (40ants-doc/github::@github-workflow section)
  (40ants-doc/world::@world section))


(defparameter *default-output-options*
  '(:if-does-not-exist :create
    :if-exists :supersede
    :ensure-directories-exist t))


(defun update-asdf-system-docs (sections-or-pages
                                asdf-system
                                &key
                                (readme-sections nil)
                                (changelog-sections nil)
                                (theme '40ants-doc/themes/default:default-theme)
                                (base-url nil)
                                (docs-dir #P"docs/"))
  "Generate pretty HTML documentation for a single ASDF system,
  possibly linking to github. If you are migrating from MGL-PAX,
  then note, this function replaces UPDATE-ASDF-SYSTEM-HTML-DOCS
  and UPDATE-ASDF-SYSTEM-README while making it possible to generate
  a crosslinks between README.md and HTML docs. The same way you
  can generate a ChangeLog.md file using :CHANGELOG-SECTIONS argument.
  See 40ANTS-DOC/CHANGELOG::@INDEX section to learn about
  40ANTS-DOC/CHANGELOG:DEFCHANGELOG helper.

  Both :README-SECTIONS and :CHANGELOG-SECTIONS arguments may be a single
  item or a list.

  Example usage:

  ```commonlisp
  (40ants-doc/builder:update-asdf-system-docs 40ants-doc/doc:@index
                                              :40ants-doc
                                              :readme-sections 40ants-doc/doc:@readme)
  ```

  This is just a shorthand to call RENDER-TO-FILES for ASDF system.

  All sections, listed in :README-SECTIONS argment will be concantenated into the README.md.
  Some symbols, referenced in the :README-SECTIONS but not documented there will be
  linked to the HTML documentation. To make this work for a hosted static sites,
  then provide :BASE-URL of the site, otherwise, links will be relative.

  In MGL-PAX this function supported such parameters as :UPDATE-CSS-P and :PAGES,
  but in 40ANTS-DOC javascript and CSS files are updated automatically. See documentation
  on RENDER-TO-FILES to learn how does page separation and other parameters work.

  If you want a more generic wrapper for building documentation for your projects,
  take a look at [DOCS-BUILDER](https://40ants.com/docs-builder/)."
  (render-to-files (append (uiop:ensure-list sections-or-pages)
                           (when readme-sections
                             (uiop:ensure-list
                              (40ants-doc/page:make-page2 readme-sections
                                                          :base-filename "README"
                                                          :base-dir (asdf:system-relative-pathname
                                                                     asdf-system
                                                                     "./")
                                                          :format :markdown))))
                   :base-dir (asdf:system-relative-pathname
                              asdf-system
                              (uiop:ensure-directory-pathname docs-dir))
                   :base-url base-url
                   :theme theme
                   :format :html))

;;; Generate with the default HTML look

(defun append1 (s n)
  (format nil "~A-~A"
          s n))

(defun process-document (document &key base-url)
  (let* ((references (40ants-doc/commondoc/page::collect-references document))
         (document (40ants-doc/commondoc/page:warn-on-missing-exports document))
         (document (40ants-doc/commondoc/page:warn-on-undocumented-exports document
                                                                           references))
         (document (40ants-doc/commondoc/transcribe::warn-on-differences-in-transcriptions document))
         (document (if 40ants-doc/builder/printer::*document-uppercase-is-code*
                       (40ants-doc/commondoc/xref::extract-symbols document)
                       document))
         (document (40ants-doc/commondoc/xref:fill-locatives document))
         (document (if 40ants-doc/link::*document-link-code*
                       (40ants-doc/commondoc/page::replace-xrefs document references
                                                                 :base-url base-url)
                       document)))
    document))


(defun render-to-string (object &key (format :html))
  "Renders given CommonDoc node into the string using specified format.
   Supported formats are :HTML and :MARKDOWN.

   This function is useful for debugging 40ANTS-DOC itself."
  (let ((format
          (40ants-doc/commondoc/format::ensure-format-class-name format)))
    
    (40ants-doc/commondoc/format:with-format (format)
      (let* ((document
               (40ants-doc/commondoc/builder:to-commondoc object))
             (processed-document
               (process-document document)))
        (uiop/cl:with-output-to-string (stream)
          (common-doc.format:emit-document (make-instance format)
                                           processed-document
                                           stream))))))


(defun render-to-files (sections &key (theme '40ants-doc/themes/default:default-theme)
                                      (base-dir #P"./")
                                      (base-url nil)
                                      (format :html))
  "Renders given sections or pages into a files on disk.

   By default, it renders in to HTML, but you can specify FORMAT argument.
   Supported formats are :HTML and :MARKDOWN.

   Returns an absolute pathname to the output directory as the first value
   and pathnames corresponding to each of given sections."

  (setf format
        (40ants-doc/commondoc/format::ensure-format-class-name format))
  
  (let ((num-warnings 0)
        ;; By default it uses "~A.html/#~A" which is wrong because there shouldn't
        ;; be a slash after the .html
        (common-html.emitter:*document-section-format-control* "~A#~A"))
    (handler-bind ((warning (lambda (c)
                              (declare (ignore c))
                              (incf num-warnings))))
      (40ants-doc/commondoc/format:with-format (format)
        (let* ((theme (make-instance theme))
               (sections (uiop:ensure-list sections))
               (pages (mapcar #'40ants-doc/page:ensure-page sections))
               (page-documents (mapcar
                                #'40ants-doc/commondoc/builder:to-commondoc
                                pages))
               (full-document (process-document
                               (common-doc:make-document "Documentation"
                                                         :children page-documents)
                               :base-url base-url))
               (absolute-dir (uiop:ensure-absolute-pathname base-dir
                                                            (probe-file ".")))
               (css-filename (uiop:merge-pathnames* #P"theme.css" absolute-dir))
               (40ants-doc/commondoc/toc::*full-document* full-document)
               (output-paths nil))

          (ensure-directories-exist absolute-dir)

          ;; 
          ;; Кстати, ещё нужно как-то генерить ChangeLog.md и changelog.html раздел в HTML
          ;;
          ;; Надо сделать обработку base-url:
          ;; 
          ;; И может ещё задавать base-url? И использовать его для построения ссылок на
          ;; страницы из документов не в HTML формате?
          ;; 
          (flet ((make-full-filename (page)
                   ;; PAGE argument could be either PAGE object or string denoting a relative path
                   ;; of HTML page.
                   (let* ((page-base-dir (or (when (typep page '40ants-doc/commondoc/page:page)
                                               (page-base-dir page))
                                             base-dir))
                          (absolute-dir (uiop:ensure-absolute-pathname page-base-dir
                                                                       (probe-file ".")))
                          (filename (etypecase page
                                      (40ants-doc/commondoc/page:page
                                       (40ants-doc/commondoc/page::full-filename page))
                                      (string
                                       page))))
                     (uiop:merge-pathnames* filename absolute-dir))))
            (loop with global-format = format
                  for document in page-documents
                  for full-filename = (make-full-filename document)
                  for format = (or
                                ;; Page may override global format setting
                                (page-format document)
                                global-format)
                  do (ensure-directories-exist full-filename)
                     (uiop:with-output-file (stream full-filename
                                                    :if-exists :supersede)
                       (common-doc.format:emit-document (make-instance format)
                                                        document
                                                        stream)
                       (push full-filename output-paths)))
         
            (when (eql format
                       'common-html:html)
              (uiop:with-output-file (stream css-filename
                                             :if-exists :supersede)
                (write-string (40ants-doc/themes/api:render-css theme)
                              stream)
                (terpri stream))

              (let* ((page (40ants-doc/commondoc/page:make-page nil "search/index"
                                                                :format :html))
                     (filename (make-full-filename page)))
                (ensure-directories-exist filename)
                (uiop:with-output-file (common-html.emitter::*output-stream*
                                        filename
                                        :if-exists :supersede)
                  (40ants-doc/commondoc/page::emit-search-page page))

                (uiop:with-output-file (stream (uiop:merge-pathnames* #P"searchindex.js" absolute-dir)
                                               :if-exists :supersede)
                  (write-string (40ants-doc/search::generate-search-index full-document page)
                                stream)
                  (terpri stream)))

              (loop with paths = '(("toc.js" "toc.js")
                                   ("highlight/highlight.min.js" "highlight.min.js")
                                   ("highlight/styles/atom-one-dark.min.css" "highlight.min.css")
                                   ("search/searchtools.js" "searchtools.js")
                                   ("search/language_data.js" "language_data.js")
                                   ("search/doctools.js" "doctools.js")
                                   ("underscore.js" "underscore.js")
                                   ("jquery.js" "jquery.js"))
                    for (from to) in paths
                    do (uiop:copy-file (asdf:system-relative-pathname :40ants-doc
                                                                      (concatenate 'string
                                                                                   "static/" from))
                                       (uiop:merge-pathnames* to absolute-dir)))))

          (unless (zerop num-warnings)
            (warn "~A warning~:P ~A caught"
                  num-warnings
                  (if (= num-warnings 1)
                      "was"
                      "were")))
          (apply #'values
                 absolute-dir
                 (nreverse output-paths)))))))


(defmethod describe-object ((section 40ants-doc/core::section) stream)
  "40ANTS-DOC:SECTION objects are printed by calling 40ANTS-DOC/BUILDER::RENDER-TO-STRING on them
  with all 40ANTS-DOC/DOC:@DOCUMENTATION-PRINTER-VARIABLES, except for
  `40ANTS-DOC/BUILDER/PRINTER::*DOCUMENT-NORMALIZE-PACKAGES*`, turned off to reduce clutter."

  ;; TODO: check these vars, probably some of them aren't need anymore:
  (let ((40ants-doc/builder/printer::*document-uppercase-is-code* nil) ;; supported
        (40ants-doc/link::*document-link-code* nil) ;; supported
        (commondoc-markdown/emitter:*emit-section-anchors* nil) ;; supported
        (40ants-doc/builder/vars::*document-mark-up-signatures* nil)
        (40ants-doc/builder/vars::*document-max-numbering-level* 0)
        (40ants-doc/builder/vars::*document-max-table-of-contents-level* 0)
        (40ants-doc/builder/vars::*document-text-navigation* nil)
        ;; Some Lisps bind it to T in DESCRIBE, some don't.
        (*print-circle* nil))

    (write-string (40ants-doc/builder:render-to-string section
                                                       :format :markdown)
                  stream)))


;; (defun page-to-markdown (sections filename)
;;   (let* ((parts (mapcar
;;                  #'40ants-doc/commondoc/builder:to-commondoc
;;                  (uiop:ensure-list sections)))
;;          (full-document (process-document
;;                          (common-doc:make-document "Documentation"
;;                                                    :children parts))))

;;     (uiop:with-output-file (stream filename
;;                                    :if-exists :supersede)
;;       (common-doc.format:emit-document (make-instance 'commondoc-markdown:markdown)
;;                                        full-document
;;                                        stream))
    
;;     (values)))


(defun add-html-defaults-to-page-specs (sections page-specs dir
                                        link-to-pax-world-p)
  (flet ((section-has-page-spec-p (section)
           (some (lambda (page-spec)
                   (member section (getf page-spec :objects)))
                 page-specs)))
    (mapcar (lambda (page-spec)
              (add-html-defaults-to-page-spec page-spec dir
                                              link-to-pax-world-p))
            (append page-specs
                    (mapcar (lambda (section)
                              `(:objects (,section)))
                            (remove-if #'section-has-page-spec-p sections))))))

(defun add-html-defaults-to-page-spec (page-spec dir link-to-pax-world-p)
  (let* ((objects (getf page-spec :objects))
         (section (if (and (= 1 (length objects))
                           (typep (first objects) '40ants-doc:section))
                      (first objects)
                      nil))
         (title (if section
                    (40ants-doc:section-title section)
                    nil))
         (filename (sections-to-filename objects dir)))
    (flet ((header (stream)
             (html-header stream :title title
                          :stylesheet "style.css" :charset "UTF-8"
                          :link-to-pax-world-p link-to-pax-world-p))
           (footer (stream)
             (html-footer stream)))
      `(,@page-spec
        ,@(unless (getf page-spec :output)
            `(:output (,filename ,@*default-output-options*)))
        ,@(unless (getf page-spec :header-fn)
            `(:header-fn ,#'header))
        ,@(unless (getf page-spec :footer-fn)
            `(:footer-fn ,#'footer))))))

(defun sections-to-filename (sections dir)
  (flet ((name (section)
           (string-downcase
            (remove-special-chars (symbol-name (40ants-doc:section-name section))))))
    (merge-pathnames (format nil "~{~A~^-~}.html"
                             (mapcar #'name sections))
                     dir)))

(defun remove-special-chars (string)
  (remove-if (lambda (char)
               (find char "!@#$%^&*"))
             string))

(defun copy-css (target-dir)
  (ensure-directories-exist target-dir)
  (loop for file in '("js/jquery.min.js" "js/toc.min.js" "css/style.css")
        do (uiop:copy-file (asdf:system-relative-pathname :40ants-doc file)
                           (merge-pathnames (file-namestring file)
                                            target-dir))))

(defvar *document-html-top-blocks-of-links* ()
  "A list of blocks of links to be display on the sidebar on the left,
  above the table of contents. A block is of the form
  `(&KEY TITLE ID LINKS)`, where `TITLE` will be displayed at the top of the block in a
  HTML `div` with `id`, followed by the links. LINKS is a list
  of `(URI LABEL)` elements.`")

(defvar *document-html-bottom-blocks-of-links* ()
  "Like *DOCUMENT-HTML-TOP-BLOCKS-OF-LINKS*, only it is displayed
  below the table of contents.")

(defun html-header
    (stream &key title stylesheet (charset "UTF-8")
     link-to-pax-world-p
     (top-blocks-of-links *document-html-top-blocks-of-links*)
     (bottom-blocks-of-links *document-html-bottom-blocks-of-links*))
  (format
   stream
   """<!DOCTYPE html>~%~
   <html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en' lang='en'>~%~
   <head>~%~
   ~@[<title>~A</title>~]~%~
   ~@[<link type='text/css' href='~A' rel='stylesheet'/>~]~%~
   ~@[<meta http-equiv="Content-Type" ~
            content="text/html; ~
   charset=~A"/>~]~%~
   <script src="jquery.min.js"></script>~%~
   <script src="toc.min.js"></script>~%~
   <script type="text/x-mathjax-config">
     MathJax.Hub.Config({
       tex2jax: {
         inlineMath: [['$','$']],
         processEscapes: true
       }
     });
   </script>
   <script type="text/javascript" ~
    src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML">
   </script>
   </head>~%~
   <body>~%~
   <div id="content-container">~%~
     <div id="toc">~%~
       ~A~
       ~:[~;<div id="toc-header"><ul><li><a href="index.html">~
            PAX World</a></li></ul></div>~%~]~
       <div id="page-toc">~%~
       </div>~%~
       ~A~
       <div id="toc-footer">~
         <ul><li><a href="https://40ants.com/doc">[generated by 40ANTS-DOC]</a></li></ul>~
       </div>~%~
     </div>~%~
     <div id="content">~%"""
   title stylesheet charset
   (blocks-of-links-to-html-string top-blocks-of-links)
   link-to-pax-world-p
   (blocks-of-links-to-html-string bottom-blocks-of-links)))

(defun blocks-of-links-to-html-string (blocks-of-links)
  (format nil "~{~A~}" (mapcar #'block-of-links-to-html-string
                               blocks-of-links)))

(defun block-of-links-to-html-string (block-of-links)
  (destructuring-bind (&key title id links) block-of-links
    (with-output-to-string (stream)
      (format stream "<div class=\"menu-block\"")
      (when id
        (format stream " id=\"~A\"" id))
      (format stream ">")
      (when title
        (format stream "<span class=\"menu-block-title\">~A</span>" title))
      (format stream "<ul>")
      (dolist (link links)
        (format stream "<li><a href=\"~A\">~A</a></li>"
                (first link)
                (second link)))
      (princ "</ul></div>" stream))))

(defvar *google-analytics-id* nil)

(defun html-footer (stream &key (google-analytics-id *google-analytics-id*))
  (format
   stream
   "  </div>~%~
   </div>~%~
   <script>$('#page-toc').toc(~A);</script>~%~
   ~:[~;<script>
   (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){~
   (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement~
   (o),m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.~
   insertBefore(a,m)})(window,document,'script','//www.google-analytics.com/~
   analytics.js','ga');ga('create', '~A', 'auto');ga('send', 'pageview');~
   </script>~%~]</body>~%</html>~%"
   (toc-options)
   google-analytics-id google-analytics-id))

(defvar *document-html-max-navigation-table-of-contents-level* nil
  "NIL or a non-negative integer. If non-NIL, it overrides
  40ANTS-DOC/BUILDER/VARS::*DOCUMENT-MAX-NUMBERING-LEVEL* in dynamic HTML table of contents on
  the left of the page.")

(defun toc-options ()
  (let ((max-level (or 40ants-doc/builder::*document-html-max-navigation-table-of-contents-level*
                       40ants-doc/builder/vars::*document-max-table-of-contents-level*)))
    (format nil "{'selectors': '~{~A~^,~}'}"
            (loop for i upfrom 1 upto (1+ max-level)
                  collect (format nil "h~S" i)))))


;; (defmethod 40ants-doc/document::document (object &key stream pages (format :markdown))
;;   ;; TODO: delete??
;;   (let ((40ants-doc/builder/printer::*format* format)
;;         (*print-right-margin* (or *print-right-margin* 80))
;;         (*package* (if 40ants-doc/builder/printer::*document-normalize-packages*
;;                        (find-package :keyword)
;;                        *package*))
;;         (default-page (40ants-doc/page::translate-page-spec
;;                        (list :objects (alexandria:ensure-list object)
;;                              :output (list stream))
;;                        format))
;;         (3bmd-code-blocks:*code-blocks* t)
;;         (3bmd-code-blocks:*code-blocks-default-colorize* :common-lisp)
;;         (3bmd-code-blocks::*colorize-name-map*
;;           (alexandria:plist-hash-table
;;            `("cl-transcript" :common-lisp
;;                              ,@(alexandria:hash-table-plist
;;                                 3bmd-code-blocks::*colorize-name-map*))
;;            :test #'equal)))
;;     (40ants-doc/page::with-tracking-pages-created ()
;;       (40ants-doc/page::with-pages ((append (40ants-doc/page::translate-page-specs pages format)
;;                                             (list default-page)))
;;         ;; Here we output documentation for all objects.
;;         ;; Don't be misleaded by DEFAULT-PAGE here.
;;         ;; DOCUMENT-OBJECT will write data to all pages
;;         ;; depending on a page reference belong to.
;;         ;; It does this in 40ANTS-DOC/DOCUMENT::DOCUMENT-OBJECT :AROUND method
;;         (40ants-doc/page::with-temp-output-to-page (stream default-page)
;;           (dolist (object (alexandria:ensure-list object))
;;             (40ants-doc/builder/heading::with-headings (object)
;;               (40ants-doc/document::document-object object stream))))
        
;;         (let ((outputs ()))
;;           (40ants-doc/page::do-pages-created (page)
;;             (40ants-doc/page::with-temp-output-to-page (stream page)
;;               (40ants-doc/builder/footer::emit-footer stream))
;;             (unless (eq format :markdown)
;;               (let ((markdown-string (40ants-doc/page::with-temp-input-from-page (stream page)
;;                                        (uiop:slurp-stream-string stream))))
;;                 (40ants-doc/utils::delete-stream-spec (40ants-doc/page::page-temp-stream-spec page))
;;                 (40ants-doc/page::with-final-output-to-page (stream page)
;;                   (when (40ants-doc/page::page-header-fn page)
;;                     (funcall (40ants-doc/page::page-header-fn page) stream))
;;                   (3bmd:parse-string-and-print-to-stream markdown-string
;;                                                          stream :format format)
;;                   (when (40ants-doc/page::page-footer-fn page)
;;                     (funcall (40ants-doc/page::page-footer-fn page) stream)))))
;;             (push (40ants-doc/utils::unmake-stream-spec (40ants-doc/page::page-final-stream-spec page))
;;                   outputs))

;;           (if (and stream
;;                    (endp pages))
;;               (first outputs)
;;               (reverse outputs)))
;;         ))))


