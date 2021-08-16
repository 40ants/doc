(uiop:define-package #:40ants-doc/locatives/slots
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-and-find-source
                #:locate-and-document
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc/render/args)
  (:import-from #:40ants-doc/builder/bullet)
  (:import-from #:40ants-doc/reference-api
                #:canonical-reference)
  (:import-from #:40ants-doc/args)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/builder/vars)
  (:import-from #:40ants-doc/render/print)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc/page)
  (:import-from #:swank-mop)
  (:import-from #:40ants-doc/builder/printer)
  (:import-from #:40ants-doc/locatives
                #:accessor
                #:reader
                #:writer)
  (:import-from #:40ants-doc/markdown/transform)
  (:import-from #:40ants-doc/commondoc/bullet)
  (:import-from #:40ants-doc/commondoc/arglist
                #:make-arglist))
(in-package 40ants-doc/locatives/slots)


(define-locative-type accessor (class-name)
  "To refer to an accessor named `FOO-SLOT` of class
  `FOO`:

      (foo-slot (accessor foo))")

(define-locative-type reader (class-name)
  "To refer to a reader named `FOO-SLOT` of class
  `FOO`:

      (foo-slot (reader foo))")

(define-locative-type writer (class-name)
  "To refer to a writer named `FOO-SLOT` of class
  `FOO`:

      (foo-slot (writer foo))")

(defmethod locate-object (symbol (locative-type (eql 'accessor))
                          locative-args)
  (assert (= 1 (length locative-args)) ()
          "The syntax of the ACCESSOR locative is (ACCESSOR <CLASS-NAME>).")
  (find-accessor-slot-definition symbol (first locative-args))
  (40ants-doc/reference::make-reference symbol (cons locative-type locative-args)))


(defun find-accessor-slot-definition (accessor-symbol class-symbol)
  (dolist (slot-def (swank-mop:class-direct-slots (find-class class-symbol)))
    (when (and (find accessor-symbol
                     (swank-mop:slot-definition-readers slot-def))
               (find `(setf ,accessor-symbol)
                     (swank-mop:slot-definition-writers slot-def)
                     :test #'equal))
      (return-from find-accessor-slot-definition slot-def)))
  (locate-error "Could not find accessor ~S for class ~S." accessor-symbol
                class-symbol))

(defmethod locate-object (symbol (locative-type (eql 'reader))
                          locative-args)
  (assert (= 1 (length locative-args)) ()
          "The syntax of the READER locative is (READER <CLASS-NAME>).")
  (find-reader-slot-definition symbol (first locative-args))
  (40ants-doc/reference::make-reference symbol (cons locative-type locative-args)))

(defun find-reader-slot-definition (accessor-symbol class-symbol)
  (dolist (slot-def (swank-mop:class-direct-slots (find-class class-symbol)))
    (when (find accessor-symbol (swank-mop:slot-definition-readers slot-def))
      (return-from find-reader-slot-definition slot-def)))
  (locate-error "Could not find reader ~S for class ~S." accessor-symbol
                class-symbol))

(defmethod locate-object (symbol (locative-type (eql 'writer))
                          locative-args)
  (assert (= 1 (length locative-args)) ()
          "The syntax of the WRITER locative is (WRITER <CLASS-NAME>).")
  (find-writer-slot-definition symbol (first locative-args))
  (40ants-doc/reference::make-reference symbol (cons locative-type locative-args)))

(defun find-writer-slot-definition (accessor-symbol class-symbol)
  (dolist (slot-def (swank-mop:class-direct-slots (find-class class-symbol)))
    (when (find accessor-symbol (swank-mop:slot-definition-writers slot-def))
      (return-from find-writer-slot-definition slot-def)))
  (locate-error "Could not find writer ~S for class ~S." accessor-symbol
                class-symbol))

(defmethod locate-and-document (symbol (locative-type (eql 'accessor))
                                locative-args stream)
  (generate-documentation-for-slot-definition
   symbol (find-accessor-slot-definition symbol (first locative-args))
   locative-type locative-args stream))

(defmethod locate-and-document (symbol (locative-type (eql 'reader))
                                locative-args stream)
  (generate-documentation-for-slot-definition
   symbol (find-reader-slot-definition symbol (first locative-args))
   locative-type locative-args stream))

(defmethod locate-and-document (symbol (locative-type (eql 'writer))
                                locative-args stream)
  (generate-documentation-for-slot-definition
   symbol (find-writer-slot-definition symbol (first locative-args))
   locative-type locative-args stream))


(defvar *definition-finders*
  (list 'reader #'find-reader-slot-definition
        'writer #'find-writer-slot-definition
        'accessor #'find-accessor-slot-definition))


(defun inner-reference-to-commondoc (symbol locative-type locative-args)
  (let* ((reference (canonical-reference
                     (40ants-doc/reference::make-reference
                      symbol (cons locative-type locative-args))))
         (definition-finder (getf *definition-finders* locative-type))
         (slot-def (funcall definition-finder symbol (first locative-args)))
         (initarg-strings
           (when (swank-mop:slot-definition-initargs slot-def)
             (mapcar #'40ants-doc/utils::prin1-and-escape-markdown
                     (swank-mop:slot-definition-initargs slot-def))))
         (arglist (list (make-arglist locative-args)
                        ;; TODO: make a special node for this kind of data where NAME = SOME-DATA or DEFAULT IS SOME-DATA
                        (make-arglist (format nil "(~{~A~^ ~}~A)"
                                              initarg-strings
                                              (if (swank-mop:slot-definition-initfunction slot-def)
                                                  (format nil "~A= ~A"
                                                          (if initarg-strings " " "")
                                                          ;; TODO: remove this replace-known-references and replace
                                                          ;;       it with a separate reference replacing step.
                                                          (40ants-doc/markdown/transform::replace-known-references
                                                           (40ants-doc/utils::prin1-and-escape-markdown
                                                            (swank-mop:slot-definition-initform
                                                             slot-def))))
                                                  "")))))
         (docstring (unless (subtypep (find-class (first locative-args)) 'condition)
                      (let ((docstring (swank-mop:slot-definition-documentation slot-def)))
                        (when docstring
                          (40ants-doc/markdown/transform::massage-docstring2 docstring)))))
         (children (when docstring
                     (40ants-doc/commondoc/builder::parse-markdown docstring))))
    (40ants-doc/commondoc/bullet::make-bullet reference
                                              :arglist arglist
                                              :children children
                                              :ignore-words symbol)))


(defmethod 40ants-doc/commondoc/builder::reference-to-commondoc ((symbol symbol) (locative-type (eql 'reader)) locative-args)
  (inner-reference-to-commondoc symbol locative-type locative-args))

(defmethod 40ants-doc/commondoc/builder::reference-to-commondoc ((symbol symbol) (locative-type (eql 'writer)) locative-args)
  (inner-reference-to-commondoc symbol locative-type locative-args))

(defmethod 40ants-doc/commondoc/builder::reference-to-commondoc ((symbol symbol) (locative-type (eql 'accessor)) locative-args)
  (inner-reference-to-commondoc symbol locative-type locative-args))


;; TODO: remove after refactoring
(defun generate-documentation-for-slot-definition
    (symbol slot-def locative-type locative-args stream)
  (40ants-doc/builder/bullet::locate-and-print-bullet locative-type locative-args symbol stream)
  (write-char #\Space stream)
  (40ants-doc/render/args::print-arglist locative-args stream)

  ;; TODO: Show type using (swank-mop:slot-definition-type slot-def)
  
  (when (or (swank-mop:slot-definition-initargs slot-def)
            (swank-mop:slot-definition-initfunction slot-def))
    (write-char #\Space stream)
    (if (and 40ants-doc/builder/vars::*document-mark-up-signatures*
             (eq 40ants-doc/builder/printer::*format* :html))
        (let ((initarg-strings
                (when (swank-mop:slot-definition-initargs slot-def)
                  (mapcar #'40ants-doc/utils::prin1-and-escape-markdown
                          (swank-mop:slot-definition-initargs slot-def)))))
          (40ants-doc/render/args::print-arglist
           (format nil "(~{~A~^ ~}~A)" initarg-strings
                   (if (swank-mop:slot-definition-initfunction slot-def)
                       (format nil "~A= ~A"
                               (if initarg-strings " " "")
                               (40ants-doc/markdown/transform::replace-known-references
                                (40ants-doc/utils::prin1-and-escape-markdown
                                 (swank-mop:slot-definition-initform
                                  slot-def))))
                       ""))
           stream))
        (40ants-doc/render/args::print-arglist
         (prin1-to-string
          `(,@(when (swank-mop:slot-definition-initargs slot-def)
                (swank-mop:slot-definition-initargs slot-def))
            ,@(when (swank-mop:slot-definition-initfunction slot-def)
                `(=
                  ,(swank-mop:slot-definition-initform slot-def)))))
         stream)))
  (40ants-doc/builder/bullet::print-end-bullet stream)
  ;; No documentation for condition accessors, and some
  ;; implementations signal warnings.
  (40ants-doc/args::with-dislocated-symbols ((list symbol))
    (unless (subtypep (find-class (first locative-args)) 'condition)
      (let ((docstring (swank-mop:slot-definition-documentation slot-def)))
        (when docstring
          (format stream "~%~A~%" (40ants-doc/markdown/transform::massage-docstring docstring)))))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'accessor))
                                   locative-args)
  (40ants-doc/source-api::find-source (find-method (symbol-function symbol)
                                                   '() (list (find-class (first locative-args))))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'reader))
                                   locative-args)
  (40ants-doc/source-api::find-source (find-method (symbol-function symbol)
                                                   '() (list (find-class (first locative-args))))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'writer))
                                   locative-args)
  (40ants-doc/source-api::find-source (find-method (symbol-function symbol)
                                                   '() (mapcar #'find-class
                                                               (list t (first locative-args))))))
