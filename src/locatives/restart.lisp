(uiop:define-package #:40ants-doc/locatives/restart
  (:use #:cl)
  (:import-from #:40ants-doc/locatives/base
                #:locate-error
                #:locate-object
                #:define-locative-type)
  (:import-from #:40ants-doc/document
                #:document-object)
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
  (:import-from #:swank-backend)
  (:import-from #:swank-mop)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader)
  (:import-from #:40ants-doc/locatives/definers
                ;; #:define-definer-for-symbol-locative-type
                #:define-symbol-locative-type))
(in-package 40ants-doc/locatives/restart)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(define-symbol-locative-type restart ())


;; (define-definer-for-symbol-locative-type define-restart restart
;;   """A definer macro to hang the documentation of a restart on a
;;   symbol.

;;   ```
;;   (define-restart my-ignore-error ()
;;     "Available when MY-ERROR is signalled, MY-IGNORE-ERROR unsafely continues.")
;;   ```

;;   Note that while there is a CL:RESTART class, there is no
;;   corresponding source location or docstring like for CONDITIONs.
;;   """)
