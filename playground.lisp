(uiop:define-package #:playground
  (:use #:cl))
(in-package playground)

(defun user ()
  "Just to check locatives in docstrings"
  (values))

(define-compiler-macro bar (&whole form arg)
  "A custom dostring for a compiler macro. Optimizes a call to the BAR function, when arg is an atom."
  (if (atom arg)
      arg
      form))

;; Надо разобраться почему не работает явное указание locatives
(defun bar (user)
  "Cool! This function prints its USER argument. It is also exists as BAR compiler-macro."
  (format t "BAR: ~S~%"
          user))

(defun foo (arg)
  "Cool! It calls BAR function!"
  (bar arg))


(40ants-doc:defsection @index (:title "Playground"
                               :ignore-words ("MGL-PAX"
                                              "GIT"
                                              "MIT"))
  "Hello World!

   And here is a link to @METHODS section.

   And there can be the @SECOND-PAGE section."
  (@asdf section)
  (@function section)
  (@class section)
  (@structure section)
  (@compiler-macro section)
  (@constant section)
  (@vars section)
  (@glossary section)
  (@locative section)
  (@macro section)
  (@METHODS section)
  (@package section)
  (@restart section)
  (@type section)
  (@include section)
  (@todo section)
  "Finally the other @SECOND-PAGE section link.")


(40ants-doc:defsection @function (:title "Functions")
  (foo function)
  (user function)
  ;; Это и не должно работать:
  ;; (user 40ants-doc/locatives/argument::argument)
  (bar function))


(40ants-doc:defsection @asdf (:title "ASDF System")
  (40ants-doc asdf:system))


(40ants-doc:defsection @compiler-macro (:title "Compiler macro")
  (bar compiler-macro))


(defconstant +the-question+ nil)

(defconstant +the-answer+ 42
  "The answer to everything")

(40ants-doc:defsection @constant (:title "Constants")
  (+the-question+ constant)
  (+the-answer+ constant))


(defvar *var-a*)

(defvar *var-b* 100500
  "Just a var with docstring.")

(defvar *var-c*)

(setf (documentation '*var-c* 'variable)
      "Unbound var with docstring. LISP allows us to define docstring separately.")


(40ants-doc:defsection @vars (:title "Variables")
  (*var-a* variable)
  (*var-b* variable)
  (*var-c* variable))


(40ants-doc/glossary::define-glossary-term lisp (:title "The Best Programming Language")
                                           "You really should use LISP!")

(40ants-doc:defsection @glossary (:title "Glossary")
  (lisp glossary-term))


(40ants-doc:defsection @locative (:title "Locatives")
  (variable locative))


(defmacro the-macro ((title) &body body)
  "Macro's docstring.

   We can refer FOO function from here.
"
  (declare (ignore title))
  `(progn ,@body))


(40ants-doc:defsection @macro (:title "Macro")
  (the-macro macro))


(defclass the-object ()
  ()
  (:documentation "Base class for all objects in the system"))


(defclass user (the-object)
  ((nickname :reader user-nickname
             :initform :unauthorized
             :documentation "User's nickname")
   (email :accessor user-email
          :type (or string null)
          :initform nil
          :documentation "User's Email. Can be empty")
   (processed :writer user-processed
              :initform nil
              ;; :documentation "Sets a \"PROCESSED\" flag."
              ))
  (:documentation "Class for all users except admins."))


(40ants-doc:defsection @class (:title "Classes")
  (the-object class)
  (user class)
  (user-nickname (reader user))
  (user-email (accessor user))
  (user-processed (writer user)))

(defgeneric get-address (entity)
  (:documentation "Docstring of the generic function."))


(defstruct box
  x
  y
  width
  (height 0
   :type integer
   :read-only t))


(40ants-doc:defsection @structure (:title "Structures")
  "No support for structure type yet (`MGL-PAX` lack it too)"
  ;; (box structure)
  (box-width structure-accessor)
  (box-height structure-accessor))


(defun a-few-p (value)
  (and (> value 0)
       (<= value 3)))


(deftype a-few (&optional (type 'integer))
  "Very small integer, less or equal than 3."
  `(and ,type
        (satisfies a-few-p)))


(40ants-doc:defsection @type (:title "Types")
  (a-few type))


(defmethod get-address ((user user))
  "Returns user's address."
  :foo-bar)


(40ants-doc:defsection @METHODS (:title "Generic and methods")
  (get-address generic-function)
  (get-address (method () (user))))


(40ants-doc:defsection @package (:title "Package")
  (40ants-doc package)
  (40ants-doc/full package)
  (playground package))



;; TODO: make this public
(40ants-doc/restart::define-restart retry-this-error ()
  "Some docstring for restart")


(40ants-doc:defsection @restart (:title "Restarts")
  (retry-this-error restart))


(40ants-doc:defsection @include (:title "Inclusions")
  (function-locative-example
   (include
    (:start (user function)
     :end (bar function))
    :lang "commonlist"
    ;; TODO: remove after refactoring
    :header-nl "```commonlisp"
    :footer-nl "```")))

(40ants-doc:defsection @todo (:title "TODO"
                              :ignore-words ("SLIME"
                                             "SLY"
                                             "M-."))
  "Here what I need to check and fix:

- enable all locatives
- check dependencies of core
- reenable tests suite
- fix how do `M-.` work in `SLIME`
- fix transcribe
- create integration with `SLY`
"
  )



(defun print-dependency-graph (system-name &key (level 0)
                                                (started-from nil)) 
  (loop for i upto level do (format t "  "))
  (format t "~A~%" system-name)
  
  (typecase system-name
    ((or string symbol)
     (when (and started-from
                (string-equal started-from
                              system-name))
       (format t "Circular dependency detected~%")
       (return-from print-dependency-graph))
     
     (let ((system (asdf/system:find-system system-name)))
       (loop for dep in (asdf/system:system-depends-on system)
             do (print-dependency-graph dep
                                        :level (1+ level)
                                        :started-from (or started-from
                                                          system-name)))))))

(defun external-dependencies (system-name) 
  (let ((primary-name (asdf:primary-system-name system-name))
        (processed nil))
    (labels ((rec (system-name &optional collected)
               (cond
                 ((member system-name processed
                          :test #'string-equal)
                  collected)
                 (t
                  (push system-name processed)
                  ;; (format t "Processing ~S system~%" system-name)
                  
                  (let* ((system (asdf/system:find-system system-name))
                         (dependencies (asdf/system:system-depends-on system)))
                    (loop for dep in dependencies
                          for dep-primary = (asdf:primary-system-name dep)
                          unless (or (string-equal primary-name dep-primary)
                                     (member dep collected
                                             :test #'string-equal))
                          collect dep into new-deps
                          finally (setf collected
                                        (append new-deps
                                                collected)))
                    (loop for dep in dependencies
                          do (setf collected
                                   (rec dep collected)))
                    collected)))))
      (sort (rec system-name)
            #'string<))))

;; To load:
#+nil
(load (asdf:system-relative-pathname :40ants-doc "playground.lisp"))

;; #+nil
(defun render ()
  (40ants-doc/builder::update-asdf-system-html-docs
   playground::@index :40ants-doc
   :pages
   `((:objects
      (,playground::@index)
      :source-uri-fn ,(40ants-doc/github::make-github-source-uri-fn
                       :40ants-doc
                       "https://github.com/40ants/doc")))))


(40ants-doc:defsection @second-page (:title "Second Page")
  "This is a second page.

It mentions only the:
"
  (playground package)
  
  "But can also refer @INDEX section or @MACRO.")


(defun render-multi ()
  (40ants-doc/builder::update-asdf-system-html-docs
   (list playground::@index
         playground::@second-page)
   :40ants-doc
   :pages
   `((:objects
      (,playground::@index)
      :source-uri-fn ,(40ants-doc/github::make-github-source-uri-fn
                       :40ants-doc
                       "https://github.com/40ants/doc"))
     (:objects
      (,playground::@second-page)
      :source-uri-fn ,(40ants-doc/github::make-github-source-uri-fn
                       :40ants-doc
                       "https://github.com/40ants/doc")))))

(defun new-render ()
  (40ants-doc/builder:single-page-to-html @index
                                          :base-dir "./new-docs/"))

(defun new-render-multi ()
  (40ants-doc/builder::multi-page-to-html (list @index
                                                @second-page)
                                          :base-dir "./new-docs/"))


#+nil
(defun render-readme ()
  (40ants-doc/builder::update-asdf-system-readme playground::@index :40ants-doc)
  (40ants-doc/builder::update-asdf-system-readme playground::@index :40ants-doc
                                                  :format :plain))
