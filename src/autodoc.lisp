(uiop:define-package #:40ants-doc/autodoc
  (:use #:cl)
  (:import-from #:40ants-doc
                #:section
                #:defsection)
  (:import-from #:alexandria
                #:symbolicate)
  (:import-from #:40ants-doc/locatives
                #:system)
  (:import-from #:closer-mop
                #:slot-definition-writers
                #:slot-definition-readers
                #:class-direct-slots
                #:class-slots)
  (:export #:defautodoc))
(in-package #:40ants-doc/autodoc)


(defun class-readers-and-accessors (class-name)
  (let* ((class (find-class class-name))
         (slots (class-direct-slots class)))
    (loop for slot in slots
          for readers = (slot-definition-readers slot)
          for writers = (slot-definition-writers slot)
          append readers into all-readers
          append (mapcar #'second writers) into all-writers
          finally (return (values (sort all-readers
                                        #'string<)
                                  (sort all-writers
                                        #'string<))))))


(defun system-packages (system-name)
  (loop for package in (list-all-packages)
        for name = (package-name package)
        when (or (string-equal name system-name)
                 (str:starts-with? (concatenate 'string (string-downcase system-name) "/")
                                   (string-downcase name)))
          collect package))


(defun package-accessors-and-writers (package)
  (loop with result = nil
        for symbol being the external-symbols of package
        when (find-class symbol nil)
          do (multiple-value-bind (readers accessors)
                 (class-readers-and-accessors symbol)
               (setf result
                     (nunion result
                             (nunion readers accessors))))
        finally (return result)))


(defun make-class-entry-with-accessors-and-readers (class-name)
  (multiple-value-bind (readers accessors)
      (class-readers-and-accessors class-name)
    (nconc
     (list (format nil "# Class ~S"
                   class-name))
     (list (list class-name 'class))
     (when readers
       (list "## Readers"))
     (loop for reader in readers
           collect `(,reader (reader ,class-name)))
     (when accessors
       (list "## Accessors"))
     (loop for accessor in accessors
           collect `(,accessor (accessor ,class-name))))))


(defun make-package-section (section-name package)
  (declare (optimize (debug 3)))
  (let* ((package-name (package-name package))
         (title package-name)
         (accessors-and-readers (package-accessors-and-writers package))
         (entries (loop for symbol being the external-symbols of package
                        when (and (fboundp symbol)
                                  (not (macro-function symbol))
                                  (not (typep (symbol-function symbol) 'generic-function)))
                          collect (list symbol 'function) into functions
                        
                        when (and (fboundp symbol)
                                  (typep (symbol-function symbol) 'generic-function)
                                  (not (member symbol accessors-and-readers
                                               :test 'eql)))
                          collect (list symbol 'generic-function) into generics
                        
                        when (and (fboundp symbol)
                                  (macro-function symbol))
                          collect (list symbol 'macro) into macros
                        
                        when (find-class symbol nil)
                          append (make-class-entry-with-accessors-and-readers symbol) into classes
                        
                        finally (return
                                  (append (when macros
                                            (list* "## Macros"
                                                   macros))
                                          (when functions
                                            (list* "## Functions"
                                                   functions))
                                          (when functions
                                            (list* "## Generic Functions"
                                                   generics))
                                          (when classes
                                            (list* "## Classes"
                                                   classes)))))))
    `(defsection ,section-name (:title ,title)
       (,(symbolicate package-name) package)
       ,@entries)))


(defun make-entries (system &key (show-system-description-p nil))
  (loop for package in (system-packages system)
        for package-name = (package-name package)
        for section-name = (symbolicate "@" (string-upcase package-name) "-PACKAGE")
        collect (list section-name 'section) into entries
        collect (make-package-section section-name package) into sections
        finally (return (values sections 
                                (append
                                 (when show-system-description-p
                                   (list (list system 'system)))
                                 entries)))))


(defmacro defautodoc (name (&key system
                              (title "API")
                              (show-system-description-p nil)
                              (readtable-symbol '*readtable*)
                              (section-class '40ants-doc:section)
                              (external-docs nil)
                              (external-links nil)
                              (ignore-words nil)))

  (multiple-value-bind (subsections entries)
      (make-entries system :show-system-description-p show-system-description-p)
    `(progn
      (defsection ,name (:title ,title
                         :readtable-symbol ,readtable-symbol
                         :section-class ,section-class
                         :external-docs ,external-docs
                         :external-links ,external-links
                         :ignore-words ,ignore-words)
        ,@entries)

      ,@subsections)))
