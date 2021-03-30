(defpackage #:40ants-doc/source-api
  (:use #:cl)
  (:import-from #:named-readtables)
  (:import-from #:pythonic-string-reader))
(in-package 40ants-doc/source-api)

(named-readtables:in-readtable pythonic-string-reader:pythonic-string-syntax)


(defgeneric find-source (object)
  (:documentation """Like SWANK:FIND-DEFINITION-FOR-THING, but this
  one is a generic function to be extensible. In fact, the default
  implementation simply defers to SWANK:FIND-DEFINITION-FOR-THING.
  This function is called by LOCATE-DEFINITION-FOR-EMACS which lies
  behind the `M-.` extension (see @MGL-PAX-EMACS-INTEGRATION).

  If successful, the return value looks like this:

  ```commonlisp
  (:location (:file "/home/mega/own/mgl/pax/test/test.lisp")
             (:position 24) nil)
  ```

  The NIL is the source snippet which is optional. Note that position
  1 is the first character. If unsuccessful, the return values is
  like:

  ```commonlisp
  (:error "Unknown source location for SOMETHING")
  ```"""))