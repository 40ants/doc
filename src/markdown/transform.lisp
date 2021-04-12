(defpackage #:40ants-doc/markdown/transform
  (:use #:cl)
  (:import-from #:alexandria)
  (:import-from #:3bmd-code-blocks)
  (:import-from #:40ants-doc/builder/vars)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/markdown)
  (:import-from #:40ants-doc/transcribe)
  (:import-from #:40ants-doc/definitions)
  (:import-from #:40ants-doc/page)
  (:import-from #:40ants-doc/builder/printer)
  (:import-from #:40ants-doc/locatives/dislocated)
  (:import-from #:40ants-doc/swank)
  (:import-from #:40ants-doc/core))
(in-package 40ants-doc/markdown/transform)


(defun references-for-similar-names (name refs)
  (multiple-value-bind (symbol n-chars-read)
      (40ants-doc/definitions::find-definitions-find-symbol-or-package name)
    (when n-chars-read
      (values (40ants-doc/reference::references-for-symbol symbol refs n-chars-read) n-chars-read))))


;;; This is called by MAP-NAMES so the return values are NEW-TREE,
;;; SLICE, N-CHARS-READ. Also called by TRANSLATE-TAGGED that expects
;;; only a single return value: the new tree.
(defun translate-uppercase-name (parent tree name known-references &key (min-length 2))
  (declare (ignore parent))
  (when (40ants-doc/utils::no-lowercase-chars-p name
                                                :min-length min-length)
    (flet ((process (name)
             (multiple-value-bind (refs n-chars-read)
                 (references-for-similar-names name known-references)
               (cond
                 (refs
                  (values `(,(40ants-doc/utils::code-fragment (40ants-doc/builder/printer::maybe-downcase name)))
                          t n-chars-read))
                 (t
                  (unless (40ants-doc/warn::ignore-p name)
                    (let* ((reference 40ants-doc/reference::*reference-being-documented*)
                           (obj (40ants-doc/reference::reference-object reference))
                           (locative (40ants-doc/reference::reference-locative reference))
                           ;; To print symbols with their packages
                           (*package* (find-package "COMMON-LISP")))
                      (warn "Unable to find symbol ~S mentioned in (~S ~A)"
                            name
                            obj
                            locative))))))))
      (let ((emph (and (listp tree) (eq :emph (first tree)))))
        (cond ((and emph (eql #\\ (alexandria:first-elt name)))
               (values (list `(:emph ,(40ants-doc/builder/printer::maybe-downcase (subseq name 1))))
                       t
                       (length name)))
              ((eql #\\ (alexandria:first-elt name))
               ;; Discard the leading backslash escape.
               (values (list (40ants-doc/builder/printer::maybe-downcase (subseq name 1)))
                       t
                       (length name)))
              ((not 40ants-doc/builder/printer::*document-uppercase-is-code*)
               nil)
              (emph
               (process (format nil "*~A*" name)))
              (t
               (process name)))))))


(defun translate-emph (parent tree known-references)
  (if (= 2 (length tree))
      (let ((translation (translate-uppercase-name parent tree (second tree)
                                                   known-references)))
        (if translation
            ;; Replace TREE with TRANSLATION, don't process
            ;; TRANSLATION again recursively, slice the return value
            ;; into the list of children of PARENT.
            (values translation nil t)
            ;; leave it alone, don't recurse, don't slice
            (values tree nil nil)))
      ;; leave it alone, recurse, don't slice
      (values tree t nil)))


;;; CODE-BLOCK looks like this:
;;;
;;;     (3BMD-CODE-BLOCKS::CODE-BLOCK :LANG "commonlisp" :CONTENT "42")
(defun translate-code-block (parent code-block)
  (declare (ignore parent))
  (let ((lang (getf (rest code-block) :lang)))
    (if (equal lang "cl-transcript")
        `(3bmd-code-blocks::code-block
          :lang ,lang
          :content ,(40ants-doc/transcribe::transcribe (getf (rest code-block) :content)
                                                       nil
                                                       :update-only t
                                                       :check-consistency t))
        code-block)))



(defun translate-to-code (parent tree known-references)
  (cond ((stringp tree)
         (let ((string tree))
           (values (40ants-doc/utils::map-names string
                                                (lambda (string start end)
                                                  (let ((name (subseq string start end)))
                                                    (translate-uppercase-name parent string name
                                                                              known-references))))
                   ;; don't recurse, do slice
                   nil t)))
        ((eq :emph (first tree))
         (translate-emph parent tree known-references))
        ((eq '3bmd-code-blocks::code-block (first tree))
         (translate-code-block parent tree))
        (t
         (error "~@<Unexpected tree type ~S.~:@>" (first tree)))))


;;;; Automatic markup of symbols

(defun find-reference-by-locative-string (locative-string possible-references
                                          &key if-dislocated)
  (let ((locative (40ants-doc/swank::read-locative-from-string locative-string)))
    (when locative
      ;; This won't find [SECTION][TYPE] because SECTION is a class.
      ;;
      ;; Reference lookup could look for a different locative which
      ;; would lead to the same object/reference, but there is no sane
      ;; generalization of that to locative-types. Do we need
      ;; something like LOCATIVE-SUBTYPE-P?
      (if (and if-dislocated (eq locative '40ants-doc/locatives/dislocated::dislocated))
          (40ants-doc/reference::make-reference if-dislocated '40ants-doc/locatives/dislocated::dislocated)
          (find locative possible-references
                :key #'40ants-doc/reference::reference-locative
                :test #'40ants-doc/core::locative-equal)))))


(defun translate-to-links (parent tree known-references)
  (cond
    ;; (:CODE "something")
    ((and (eq :code (first tree))
          (= 2 (length tree))
          (stringp (second tree)))
     (let* ((name (second tree))
            (translation (translate-name parent tree name known-references)))
       (if translation
           (values translation nil t)
           (progn
             ;; (warn "Reference not found 1: ~A"
             ;;       name)
             tree))))
    ;; [section][type], [`section`][type], [*var*][variable], [section][]
    ((and (eq :reference-link (first tree)))
     ;; For example, the tree for [`section`][type] is
     ;; (:REFERENCE-LINK :LABEL ((:CODE "SECTION")) :DEFINITION "type")
     (destructuring-bind (&key label definition tail) (rest tree)
       (let* ((name (extract-name-from-label label))
              (symbol (if name
                          (40ants-doc/definitions::find-definitions-find-symbol-or-package name)
                          nil)))
         (if (not symbol)
             (progn
               ;; (warn "Reference not found 2: ~A"
               ;;       name)
               tree)
             (let* ((references (remove symbol known-references
                                        :test-not #'eq
                                        :key #'40ants-doc/reference::reference-object))
                    (references (if (and (zerop (length definition))
                                         (equal tail "[]"))
                                    (40ants-doc/page::filter-references references)
                                    (alexandria:ensure-list
                                     (find-reference-by-locative-string
                                      definition
                                      ;; Explicit references don't
                                      ;; need heuristic conflict
                                      ;; resolution so we don't call
                                      ;; FILTER-REFERENCES.
                                      (40ants-doc/page::filter-references-by-format
                                       references)
                                      :if-dislocated symbol)))))
               (if references
                   (values (40ants-doc/page::format-references name references) nil t)
                   (progn
                     ;; (warn "Reference not found 3: ~A"
                     ;;       name)
                     tree)))))))
    (t
     ;; (warn "Reference not found 4: ~S"
     ;;       tree)
     tree)))

(defun extract-name-from-label (label)
  (let ((e (first label)))
    (cond
      ((stringp e)
       e)
      ((and (eq :emph (first e))
            (= 2 (length e))
            (stringp (second e)))
       (format nil "*~A*" (second e)))
      ((and (eq :code (first e))
            (= 2 (length e))
            (stringp (second e)))
       (second e)))))


;;; NAME-ELEMENT is a child of TREE. It is the name of the symbol or
;;; it contains the name. Find a locative before or after NAME-ELEMENT
;;; with which NAME occurs in KNOWN-REFERENCES. Return the matching
;;; REFERENCE, if found. KNOWN-REFERENCES must only contain references
;;; to the symbol.
(defun find-locative-around (tree name-element possible-references)
  (labels ((try (element)
             (let ((reference
                     (cond ((stringp element)
                            (find-reference-by-locative-string
                             element possible-references))
                           ((eq :code (first element))
                            (find-reference-by-locative-string
                             (second element) possible-references))
                           ;; (:REFERENCE-LINK :LABEL ((:CODE
                           ;; "CLASS")) :DEFINITION "0524")
                           ((eq :reference-link (first element))
                            (try (first (third element)))))))
               (when reference
                 (return-from find-locative-around reference)))))
    ;; For example, (:PLAIN "See" "function" " " "FOO")
    (loop for rest on tree
          do (when (and (eq (third rest) name-element)
                        (stringp (second rest))
                        (40ants-doc/utils::blankp (second rest)))
               (try (first rest))
               (return)))
    ;; For example, (:PLAIN "See" "the" "FOO" " " "function")
    (loop for rest on tree
          do (when (and (eq (first rest) name-element)
                        (stringp (second rest))
                        (40ants-doc/utils::blankp (second rest)))
               (try (third rest))
               (return)))))


;;; Translate NAME (a string) that's part of TREE (e.g. it's "xxx"
;;; from (:CODE "xxx") or from "xxx,yyy"), or it's constructed from
;;; TREE (e.g. it's "*SYM*" from (:EMPH "SYM")).
(defun translate-name (parent tree name known-references)
  (multiple-value-bind (refs n-chars-read)
      (references-for-similar-names name known-references)
    (when refs
      (let ((filtered-refs (40ants-doc/page::filter-references refs)))

        ;; If necessary, try to find a locative before or after NAME
        ;; to disambiguate.
        (when (and (> (length filtered-refs)
                      1)
                   (40ants-doc/reference::references-for-the-same-symbol-p filtered-refs))
          (let ((reference (find-locative-around parent tree filtered-refs)))
            (when reference
              (setq filtered-refs (list reference)))))

        (unless filtered-refs
          (let* ((reference 40ants-doc/reference::*reference-being-documented*)
                 (obj (40ants-doc/reference::reference-object reference))
                 (locative (40ants-doc/reference::reference-locative reference))
                 ;; To print symbols with their packages
                 (*package* (find-package "COMMON-LISP")))
            (warn "Unable to find a reference for ~S mentioned at (~S ~A)"
                  name
                  obj
                  locative)))

        (values (40ants-doc/page::format-references
                 (40ants-doc/builder/printer::maybe-downcase (subseq name 0 n-chars-read))
                 filtered-refs)
                t
                n-chars-read)))))


(defun replace-upcased-package-qualified-names (string)
  ;; Replaces FOO:BAR names with `FOO:BAR`. Also works with double :.
  ;; If name is already wrapped with `, it will be wrapped again,
  ;; because Markdown allow multiple backticks.
  (cl-ppcre:regex-replace-all
   "(?m)((?: |\\(|^)+)([\\p{UppercaseLetter}\\/0-9-]+[:]{1,2}[*+]?[\\p{UppercaseLetter}0-9-]+[*+]?)((?: |\\.|!|\\?|\\)|$)+)"
   string
   "\\1`\\2`\\3"))


;;; Take a string in markdown format and a list of KNOWN-REFERENCES.
;;; Markup symbols as code (if *DOCUMENT-UPPERCASE-IS-CODE*), autolink
;;; (if *DOCUMENT-LINK-SECTIONS*, *DOCUMENT-LINK-CODE*) and handle
;;; explicit links with locatives (always). Return the transformed
;;; string.
(defun replace-known-references (string &key (known-references 40ants-doc/reference::*references*))
  (when string
    (let* ((string (replace-upcased-package-qualified-names string))
           (string
             ;; Handle *DOCUMENT-UPPERCASE-IS-CODE* in normal strings
             ;; and :EMPH (to recognize *VAR*).
             (40ants-doc/markdown::map-markdown-parse-tree
              '(:emph 3bmd-code-blocks::code-block)
              '(:code :verbatim 3bmd-code-blocks::code-block
                :reference-link :explicit-link :image :mailto)
              t
              (alexandria:rcurry #'translate-to-code known-references)
              string)))
      ;; Handle *DOCUMENT-LINK-CODE* (:CODE for `SYMBOL` and
      ;; :REFERENCE-LINK for [symbol][locative]). Don't hurt links.
      (40ants-doc/markdown::map-markdown-parse-tree
       '(:code :reference-link)
       '(:explicit-link :image :mailto)
       nil
       (alexandria:rcurry #'translate-to-links known-references)
       string))))


(defun massage-docstring (docstring &key (indentation "    "))
  ;; (when (str:contains? "BAR function" docstring)
  ;;   (break))
  (if 40ants-doc/builder/vars::*table-of-contents-stream*
      ;; The output is going to /dev/null and this is a costly
      ;; operation, skip it.
      ""
      (let ((docstring (40ants-doc/utils::strip-docstring-indentation docstring)))
        (40ants-doc/utils::prefix-lines indentation
                                        (replace-known-references docstring)))))
