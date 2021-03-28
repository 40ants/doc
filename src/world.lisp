;;; TODO: unsupported yet   

(defsection @mgl-pax-world (:title "PAX World")
  "PAX World is a registry of documents, which can generate
  cross-linked HTML documentation pages for all the registered
  documents."
  (register-doc-in-pax-world function)
  "For example, this is how PAX registers itself:"
  (register-doc-example (include (:start (pax-sections function)
                                  :end (end-of-register-doc-example variable))
                                 :header-nl "```commonlisp"
                                 :footer-nl "```"))
  (update-pax-world function))

(defvar *registered-pax-world-docs* ())

(defun register-doc-in-pax-world (name sections page-specs)
  "Register SECTIONS and PAGE-SPECS under NAME in PAX World. By
  default, UPDATE-PAX-WORLD generates documentation for all of these."
  (setq *registered-pax-world-docs*
        (remove name *registered-pax-world-docs* :key #'first))
  (push (list name sections page-specs) *registered-pax-world-docs*))

;;; Register PAX itself.
(defun pax-sections ()
  (list 40ants-docs/@))


(defun pax-pages ()
  `((:objects
     (,mgl-pax:@mgl-pax-manual)
     :source-uri-fn ,(make-github-source-uri-fn
                      :mgl-pax
                      "https://github.com/melisgl/mgl-pax"))))
(register-doc-in-pax-world :mgl-pax (pax-sections) (pax-pages))


(defvar end-of-register-doc-example)

(defvar *pax-world-dir* nil
  "The default location to which to write the generated documentation.
  Defaults to:

  ```commonlisp
  (asdf:system-relative-pathname :mgl-pax \"world/\")
  ```")

(defun update-pax-world (&key docs dir)
  "Generate HTML documentation for all DOCS. By default, files are
  created in *PAX-WORLD-DIR* or `(asdf:system-relative-pathname
  :mgl-pax \"world/\")`, if NIL. DOCS is a list of entries of the
  form (NAME SECTIONS PAGE-SPECS). The default for DOCS is all the
  sections and pages registered with REGISTER-DOC-IN-PAX-WORLD.

  In the absence of :HEADER-FN :FOOTER-FN, :OUTPUT, every spec in
  PAGE-SPECS is augmented with HTML headers, footers and output
  location specifications (based on the name of the section).

  If necessary a default page spec is created for every section."
  (let ((dir (or dir (asdf:system-relative-pathname :mgl-pax "world/")))
        (docs (or docs (sort (copy-seq *registered-pax-world-docs*) #'string<
                             :key (lambda (entry)
                                    (string (first entry)))))))
    (multiple-value-bind (sections pages) (sections-and-pages docs)
      (create-pax-world sections pages dir t))))

(defun sections-and-pages (registered-docs)
  (values (apply #'append (mapcar #'second registered-docs))
          (apply #'append (mapcar #'third registered-docs))))

;;; This section is not in the documentation of PAX-WORLD itself. It
;;; is dynamically extended with the list of sections for which
;;; UPDATE-PAX-WORLD was called. FIXME: this is not thread-safe.
(defsection @mgl-pax-world-dummy (:title "PAX World")
  "This is a list of documents generated with MGL-PAX in the default
  style. The documents are cross-linked: links to other documents are
  added automatically when a reference is found. Note that clicking on
  the locative type (e.g. `[function]`) will take you to the sources
  on github if possible.")

(defun create-pax-world (sections page-specs dir update-css-p)
  (set-pax-world-list sections)
  (document-html (cons @mgl-pax-world-dummy sections)
                 (cons `(:objects
                         ,(list @mgl-pax-world-dummy)
                         :output (,(merge-pathnames "index.html" dir)
                                  ,@*default-output-options*))
                       page-specs)
                 dir update-css-p t))

(defun set-pax-world-list (objects)
  (setf (slot-value @mgl-pax-world-dummy 'mgl-pax::entries)
        (list (first (section-entries @mgl-pax-world-dummy))
              (with-output-to-string (stream)
                (dolist (object objects)
                  (format stream "- ~S~%~%" (section-name object)))))))

#+nil
(progn
  (update-asdf-system-readmes (pax-sections) :mgl-pax)
  (update-asdf-system-html-docs (pax-sections) :mgl-pax :pages (pax-pages)))

#+nil
(progn
  (asdf:load-system :mgl-mat)
  (asdf:load-system :named-readtables/doc)
  (asdf:load-system :micmac)
  (asdf:load-system :mgl-gpr)
  (asdf:load-system :mgl)
  (asdf:load-system :journal)
  (asdf:load-system :trivial-utf-8/doc))

#+nil
(update-pax-world)
