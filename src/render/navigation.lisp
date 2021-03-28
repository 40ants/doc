(defpackage #:40ants-doc/render/navigation
  (:use #:cl)
  (:import-from #:40ants-doc/page)
  (:import-from #:40ants-doc/reference-api)
  (:import-from #:40ants-doc/builder/heading)
  (:import-from #:40ants-doc/render/navigation-api
                #:navigation-link))
(in-package 40ants-doc/render/navigation)


(defun write-navigation-link (heading stream)
  (let ((link-id (40ants-doc/page::link-to-reference
                  (40ants-doc/reference-api::canonical-reference (40ants-doc/builder/heading::heading-object heading)))))
    (format stream "[~A][~A]" (40ants-doc/builder/heading::heading-title heading) link-id)))


(defmethod navigation-link (object stream)
  (when (and 40ants-doc/link::*document-link-sections*
             40ants-doc/builder/vars::*document-text-navigation*)
    (let* ((position (position object 40ants-doc/builder/heading::*headings*
                               :key #'40ants-doc/builder/heading::heading-object))
           (level (40ants-doc/builder/heading::heading-level
                   (elt 40ants-doc/builder/heading::*headings*
                        position)))
           (n (length 40ants-doc/builder/heading::*headings*))
           (writtenp nil))
      (when (< position (1- n))
        (format stream "Next: ")
        (write-navigation-link (elt 40ants-doc/builder/heading::*headings*
                                    (1+ position))
                               stream)
        (setq writtenp t))
      (when (plusp position)
        (when writtenp
          (format stream " "))
        (format stream "Prev: ")
        (write-navigation-link (elt 40ants-doc/builder/heading::*headings*
                                    (1- position))
                               stream)
        (setq writtenp t))
      (when (plusp level)
        (when writtenp
          (format stream " "))
        (let ((parent (find (1- level) (subseq 40ants-doc/builder/heading::*headings*
                                               0 position)
                            :from-end t :key #'40ants-doc/builder/heading::heading-level)))
          (format stream "Up: ")
          (write-navigation-link parent stream))
        (setq writtenp t))
      (when writtenp
        (format stream "~%~%")))))

