(uiop:define-package #:40ants-doc/commondoc/utils
  (:use #:cl)
  (:import-from #:40ants-doc/swank)
  (:import-from #:40ants-doc/utils
                #:*whitespace-chars*)
  (:import-from #:str)
  (:export))
(in-package 40ants-doc/commondoc/utils)


(defun left-word (node)
  (check-type node common-doc:text-node)
  (let* ((text (str:replace-all '(#\Newline) " "
                                (common-doc:text node)))
         (stripped (string-left-trim *whitespace-chars*
                                     text))
         (parts (str:split #\Space stripped :limit 2)))
    (first parts)))


(defun right-word (node)
  (check-type node common-doc:text-node)
  (let* ((text (str:replace-all '(#\Newline) " "
                                (common-doc:text node)))
         (stripped (string-right-trim *whitespace-chars*
                                      text))
         (parts (str:rsplit #\Space stripped :limit 2)))
    (car (last parts))))


(defun read-locative (text)
  (let* ((package (find-package "40ANTS-DOC/LOCATIVES"))
         (text (string-right-trim ".,!?" text ))
         (found (40ants-doc/swank::read-locative-from-string
                 text
                 :package package))
         (symbol
           ;; If FOUND is a locative name like a FUNCTION
           ;; or a list like (METHOD () (T T T))
           ;; then symbol will be bound to FUNCTION or METHOD.
           (typecase found
             (symbol found)
             (cons (typecase (car found)
                     (symbol (car found))
                     (t nil))))))
    (when symbol
      (multiple-value-bind (present-symbol status)
          (find-symbol (symbol-name symbol) package)
        (when (and (eq symbol present-symbol)
                   (eql status :external))
          found)))))
