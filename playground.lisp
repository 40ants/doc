(defpackage #:playground
  (:use #:cl))
(in-package playground)

(defun foo (arg)
  "Cool! It calls BAR function!"
  (bar arg))

(defun user ()
  "Just to check locatives in docstrings"
  )

;; Надо разобраться почему не работает явное указание locatives
(defun bar (user)
  "Cool! This function prints its USER argument of BAR function."
  (format t "BAR: ~S~%"
          user))


(40ants-doc:defsection @index (:title "Playground")
  "Hello World!"
  (@asdf 40ants-doc/core::section)
  (@function 40ants-doc/core::section)
  (@class 40ants-doc/core::section)
  (@compiler-macro 40ants-doc/core::section)
  (@constant 40ants-doc/core::section)
  (@vars 40ants-doc/core::section)
  (@glossary 40ants-doc/core::section)
  (@locative 40ants-doc/core::section)
  (@macro 40ants-doc/core::section)
  (@todo 40ants-doc/core::section))


(40ants-doc:defsection @function (:title "Functions")
  (foo function)
  (user function)
  ;; Это и не должно работать:
  ;; (user 40ants-doc/locatives/argument::argument)
  (bar function))


(40ants-doc:defsection @asdf (:title "ASDF System")
  (40ants-doc asdf:system))


(define-compiler-macro bar (&whole form arg)
  "A custom dostring for a compiler macro"
  (format t "Expanding: ~S" form)
  (if (atom arg)
      arg
      form))

(40ants-doc:defsection @compiler-macro (:title "Compiler macro")
  (bar compiler-macro))


(defconstant +the-question+ nil)

(defconstant +the-answer+ 42
  "The answer to everything")

(40ants-doc:defsection @constant (:title "Constants")
  (+the-question+ 40ants-doc/locatives/constant::constant)
  (+the-answer+ 40ants-doc/locatives/constant::constant))


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


(40ants-doc/locatives/glossary::define-glossary-term lisp (:title "The Best Programmin Language")
                                                     "A glossary docstring")

(40ants-doc:defsection @glossary (:title "Glossary")
  (lisp 40ants-doc/locatives/glossary::glossary-term))


(40ants-doc:defsection @locative (:title "Locatives")
  (40ants-doc/locatives/glossary::glossary-term 40ants-doc/locatives/locative::locative)
  (variable 40ants-doc/locatives/locative::locative))


(defmacro the-macro ((title) &body body)
  "Macro's docstring.

   We can refer FOO function from here.
"
  (declare (ignore title))
  `(progn ,@body))


(40ants-doc:defsection @macro (:title "Macro")
  (the-macro 40ants-doc/locatives/macro::macro))


(defclass user ()
  ())

(40ants-doc:defsection @class (:title "Classes")
  (user class))


(40ants-doc:defsection @todo (:title "TODO")
  "Here what I need to check and fix:

- enable all locatives
- check dependencies of core
- reenable tests suite
- fix how do M-. work in SLIME
- fix transcribe
- create integration with SLY
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


(defun render ()
  (40ants-doc/builder::update-asdf-system-html-docs
   playground::@index :40ants-doc
   :pages
   `((:objects
      (,playground::@index)
      :source-uri-fn ,(40ants-doc/github::make-github-source-uri-fn
                       :40ants-doc
                       "https://github.com/40ants/doc")))))
