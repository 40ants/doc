(defpackage #:playground
  (:use #:cl))
(in-package playground)

(defun foo (arg)
  "Cool! It calls BAR function!"
  (bar arg))

(defun bar (arg)
  "Cool! This function prints its argument"
  (format t "BAR: ~S~%"
          arg))


(40ants-doc:defsection @index (:title "Playground")
  "Hello World!"
  (foo function)
  (bar function))



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
