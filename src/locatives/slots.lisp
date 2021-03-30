(defpackage #:40ants-doc/locatives/clos
  (:use #:cl)
  (:import-from #:40ants-doc/builder/printer))
(in-package 40ants-doc/locatives/clos)


;; TODO: Fix it

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
  (make-reference symbol (cons locative-type locative-args)))

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
  (make-reference symbol (cons locative-type locative-args)))

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
  (make-reference symbol (cons locative-type locative-args)))

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

(defun generate-documentation-for-slot-definition
    (symbol slot-def locative-type locative-args stream)
  (locate-and-print-bullet locative-type locative-args symbol stream)
  (write-char #\Space stream)
  (print-arglist locative-args stream)
  (when (or (swank-mop:slot-definition-initargs slot-def)
            (swank-mop:slot-definition-initfunction slot-def))
    (write-char #\Space stream)
    (if (and *document-mark-up-signatures* (eq 40ants-doc/builder/printer::*format* :html))
        (let ((initarg-strings
                (when (swank-mop:slot-definition-initargs slot-def)
                  (mapcar #'prin1-and-escape-markdown
                          (swank-mop:slot-definition-initargs slot-def)))))
          (print-arglist
           (format nil "(~{~A~^ ~}~A)" initarg-strings
                   (if (swank-mop:slot-definition-initfunction slot-def)
                       (format nil "~A= ~A"
                               (if initarg-strings " " "")
                               (replace-known-references
                                (prin1-and-escape-markdown
                                 (swank-mop:slot-definition-initform
                                  slot-def))))
                       ""))
           stream))
        (print-arglist
         (prin1-to-string
          `(,@(when (swank-mop:slot-definition-initargs slot-def)
                (swank-mop:slot-definition-initargs slot-def))
            ,@(when (swank-mop:slot-definition-initfunction slot-def)
                `(=
                  ,(swank-mop:slot-definition-initform slot-def)))))
         stream)))
  (print-end-bullet stream)
  ;; No documentation for condition accessors, and some
  ;; implementations signal warnings.
  (with-dislocated-symbols ((list symbol))
    (unless (subtypep (find-class (first locative-args)) 'condition)
      (let ((docstring (swank-mop:slot-definition-documentation slot-def)))
        (when docstring
          (format stream "~%~A~%" (massage-docstring docstring)))))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'accessor))
                                   locative-args)
  (find-source (find-method (symbol-function symbol)
                            '() (list (find-class (first locative-args))))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'reader))
                                   locative-args)
  (find-source (find-method (symbol-function symbol)
                            '() (list (find-class (first locative-args))))))

(defmethod locate-and-find-source (symbol (locative-type (eql 'writer))
                                   locative-args)
  (find-source (find-method (symbol-function symbol)
                            '() (mapcar #'find-class
                                        (list t (first locative-args))))))
