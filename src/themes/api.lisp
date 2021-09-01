(uiop:define-package #:40ants-doc/themes/api
  (:use #:cl)
  (:export #:render-css))
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
