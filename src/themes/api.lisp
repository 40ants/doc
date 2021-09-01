(uiop:define-package #:40ants-doc/themes/api
  (:use #:cl)
  (:export #:render-css
           #:render-page
           #:render-html-head
           #:render-toc
           #:render-search-form
           #:render-sidebar-footer
           #:render-sidebar-header
           #:render-sidebar
           #:render-sidebar-content
           #:render-content
           #:render-page-header
           #:render-page-footer))
(in-package 40ants-doc/themes/api)


(defvar *theme*)


(defmacro with-theme ((theme) &body body)
  (alexandria:once-only (theme)
    `(let ((*theme* (typecase ,theme
                      (symbol (make-instance ,theme))
                      (t ,theme))))
       ,@body)))


(defgeneric render-css (theme)
  (:documentation "Returns a string with CSS."))

(defgeneric render-page (theme uri title &key toc content)
  (:documentation "Renders whole page using theme and callable CONTENT-FUNC."))

(defgeneric render-page-header (theme uri title)
  (:documentation "Renders whole page header. Does nothing by default."))

(defgeneric render-page-footer (theme uri)
  (:documentation "Renders whole page footer. Does nothing by default."))

(defgeneric render-html-head (theme uri title)
  (:documentation "Renders content of the HTML HEAD tag."))

(defgeneric render-content (theme uri toc content-func)
  (:documentation "Renders page's content"))

(defgeneric render-sidebar (theme uri toc)
  (:documentation "Renders page's sidebar"))

(defgeneric render-sidebar-header (theme uri toc)
  (:documentation "Renders sidebar's header. Usually it contains a search input."))

(defgeneric render-sidebar-footer (theme uri toc)
  (:documentation "Renders sidebar's header. By default it contains a link to the 40ANTS-DOC system."))

(defgeneric render-sidebar-content (theme uri toc)
  (:documentation "Renders sidebar's content. By default it calls RENDER-TOC generic-function."))

(defgeneric render-toc (theme uri toc)
  (:documentation "Renders documentation TOC."))

(defgeneric render-search-form (theme uri toc)
  (:documentation "Renders a search form."))


(defun check-theme ()
  (unless (boundp '*theme*)
    (error "Please, use WITH-THEME macro around the call")))


(defun render-static (absolute-dir)
  (check-theme)
  
  (let ((css-filename (uiop:merge-pathnames* #P"theme.css" absolute-dir)))
    (uiop:with-output-file (stream css-filename
                                   :if-exists :supersede)
      (write-string (render-css *theme*)
                    stream)
      (terpri stream))

    ;; TODO: Probably let to override these files too
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


(defun call-with-page-template (func uri title toc)
  (check-type uri string)
  (check-theme)
  (render-page *theme* uri title
               :toc toc
               :content func))


(defmacro with-page-template ((uri title &key toc) &body body)
  `(call-with-page-template
    (lambda ()
      ,@body)
    ,uri
    ,title
    ,toc))



