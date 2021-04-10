(uiop:define-package #:40ants-doc-test/test
  (:use #:cl
        #:40ants-doc/locatives)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:40ants-doc/doc)
  (:import-from #:rove
                #:ok
                #:deftest
                #:testing)
  (:import-from #:40ants-doc/utils)
  (:import-from #:40ants-doc-test/utils
                #:get-files-diff))
(in-package 40ants-doc-test/test)


(defsection @test (:export nil)
  "[*TEST-VARIABLE*][]
  [`*TEST-VARIABLE*`][]
  [*test-variable*][]
  [`*test-variable*`][]
  [40ants-doc-test/test:*test-variable*][]
  FOO function, function FOO,
  `FOO` function, function `FOO`,
  FOO `function`, `function` FOO,
  `FOO` `function`, `function` `FOO`,
  [foo][function],
  [foo][FUNCTION],
  [FOO][function],
  [FOO][FUNCTION],
  [`foo`][function],
  [`foo`][FUNCTION],
  [`FOO`][function],
  [`FOO`][FUNCTION],

  FOO-A `(accessor foo)`, `(accessor foo)` FOO-A,
  `FOO-A` `(accessor foo)`, `(accessor foo)` `FOO-A`,
  [foo-a][(accessor foo)],
  [foo-a][(ACCESSOR FOO)],
  [FOO-A][(accessor foo)],
  [FOO-A][(ACCESSOR FOO)],
  [`foo-a`][(accessor foo)],
  [`foo-a`][(ACCESSOR FOO)],
  [`FOO-A`][(accessor foo)],
  [`FOO-A`][(ACCESSOR FOO)]

  ->MAX

  Escaped: \\FOO [`FOO`][dislocated] *\\NAVIGATION-TEST-CASES*
  Non escaped: FOO *TEST-VARIABLE*
  @TEST-OTHER

  This should be no link because the page of @TEST-EXAMPLES
  has :URI-FRAGMENT NIL.

  This is code: T

  Plural uppercase ambiguous symbol: see FOOs

  Plural uppercase symbol: TEST-GFs

  Plural uppercase dislocated symbol: ->MAXs
  
  See
  FOO compiler-macro

  See FOO
  compiler-macro

  See
  compiler-macro FOO

  See compiler-macro
  FOO

  See
  compiler-macro 
  FOO

  See
  FOO

  ```cl-transcript
  (values (print (1+ 2)) :aaa)
  ..
  .. 3 
  => 3
  => :AAA
  ```

  ```cl-transcript
  (values '(1 2) '(3 4))
  ;=> (1 2)
  ;=> (3
  ;->  4)
  ```

  ```cl-transcript
  (make-array 12 :initial-element 0d0)
  => #(0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0
       0.0d0)
  ```

  In documentation, when the only ambiguity is between a generic
  function and its methods, it's resolved in favor if the gf:
  TEST-GF."
  (foo function)
  (foo compiler-macro)
  (foo-a (accessor foo))
  (*test-variable* variable)
  (@test-examples section)
  (@test-other section)
  (test-gf generic-function)
  (test-gf (method () (number)))
  (test-gf (method () ((eql 7))))
  (@test-section-with-link-to-other-page-in-title section)
  (@test-section-with-link-to-same-page-in-title section)
  (@test-tricky-title section))

(defsection @test-examples (:export nil)
  "example section")

(defsection @test-other (:export nil :title "test other title")
  "backlink @TEST")

(defsection @test-section-with-link-to-other-page-in-title
    (:title "Link to @TEST-OTHER"
     :link-title-to (@test-other section))
  "Same link in docstring to @TEST-OTHER.")

(defsection @test-section-with-link-to-same-page-in-title
    (:title "Link to @TEST" :link-title-to (@test section))
  "Same link in docstring to @TEST.")

(defsection @test-tricky-title
    (:export nil :title "`CODE` *italic* _italic2_ *bold* [link][sdf] <thing>")
  "backlink @TEST")

(defun foo ())
(define-compiler-macro foo ())
(defclass foo ()
  ((a :accessor foo-a)
   (r :reader foo-r)
   (w :writer foo-w)))
(defvar foo-a)
(defvar foo-b)
(defvar foo-c)

(defparameter *test-variable*
  '(xxx 34))

(defmacro bar ())
(deftype bar () 'null)
(defconstant bar 2)

(defgeneric baz ())
(defvar baz)
(defstruct baz
  aaa)

(defgeneric test-gf (x))
(defmethod test-gf ((x number)))
(defmethod test-gf ((x (eql 7))))

(defun ->max ())

(defparameter *navigation-test-cases*
  ;; (symbol locative prefix &optional alternative-prefix)
  '((foo function (defun foo))
    (foo type (defclass foo))
    (foo class (defclass foo))
    (foo compiler-macro (define-compiler-macro foo))
    (foo-a (accessor foo) (defclass foo) (a :accessor foo-a))
    (foo-r (reader foo) (defclass foo) (r :reader foo-r))
    (foo-w (writer foo) (defclass foo) (w :writer foo-w))
    (foo-a variable (defvar foo-a))
    (foo-b variable (defvar foo-b))
    (foo-c variable (defvar foo-c))
    (bar macro (defmacro bar))
    (bar type (deftype bar))
    (bar constant (defconstant bar))
    (baz generic-function (defgeneric baz))
    (baz variable (defvar baz))
    (40ants-doc/doc::@index section (defsection @index))
    (baz-aaa structure-accessor (defstruct baz))
    (40ants-doc package
     (cl:defpackage)
     (uiop:define-package))
    (40ants-doc system ())
    ;; Allegro has the location off by one form.
    #-allegro
    (test-gf generic-function (defgeneric test-gf))
    (test-gf (method () (number)) (defmethod test-gf))))


(defun working-locative-p (locative)
  (declare (ignorable locative))
  ;; AllegroCL doesn't store source location for DEFPACKAGE.
  #+allegro (not (eq locative 'package))
  #-allegro t)


(deftest test-navigation
  (dolist (test-case *navigation-test-cases*)
    (destructuring-bind
        (symbol locative prefix &optional alternative-prefix) test-case
      (testing (format nil "(~S ~S)"
                       symbol locative)
        (when (working-locative-p locative)
          (let ((location (40ants-doc/source-api::find-source
                           (40ants-doc/locatives/base::locate symbol locative))))
            (ok (not (eq :error (first location)))
                (format nil "Could not find source location for (~S ~S)"
                        symbol locative))
            (let* ((file (second (second location)))
                   (position (1- (second (third location))))
                   (form (let ((*package* (find-package :40ants-doc-test/test)))
                           (read-form-from-file-position file position))))
              (ok
               (or (alexandria:starts-with-subseq prefix form
                                                  :test #'equal)
                   (and alternative-prefix
                        (alexandria:starts-with-subseq
                         alternative-prefix form :test #'equal)))
               (format nil "Could not find prefix ~S~@[ or ~S~] ~
                            at source location~%~S~%for reference (~S ~S).~%~
                            Form found was:~%~S."
                       prefix alternative-prefix
                       location symbol locative form)))))))))


(defun read-form-from-file-position (filename position)
  (with-open-file (stream filename :direction :input)
    (file-position stream position)
    (read stream)))


(defvar *muffle-warnings* t)


(deftest test-package-qualifed-replacer
  (loop with cases = '("FOO::*BAR*"
                       "FOO::+BAR+"
                       "FOO::BAR"
                       "BLAH-ME/MINOR::BAR-ME"
                       "BLAH-ME/MINOR:BAR-ME"
                       "`BLAH-ME/MINOR:BAR-ME`")
        for case in cases
        for input = (format nil "before ~A after" case)
        for expected = (format nil "before `~A` after" case)
        do (testing case
             (ok (equal (40ants-doc/markdown/transform::replace-upcased-package-qualified-names input)
                        expected)))))


(deftest test-replace-known-references
  (let ((40ants-doc/reference::*reference-being-documented*
          (40ants-doc/reference::make-reference @test '40ants-doc::section)))
    (testing "Unbound symbol should not be marked as a code block, and we should issue a warning"
      (let (warning-caught)
        (handler-bind ((warning (lambda (c)
                                  (setf warning-caught t)
                                  (when *muffle-warnings*
                                    (muffle-warning c)))))
          (ok (string= "BLAH"
                       (40ants-doc/markdown/transform::replace-known-references
                        "BLAH"
                        :known-references
                        (list (40ants-doc/reference::make-reference 'bar 'macro)))))
          (ok warning-caught))))
    (testing "Transforming into a code block"
      (ok (string= "`FOO`"
                   (40ants-doc/markdown/transform::replace-known-references
                    "FOO"
                    :known-references ())))
      (ok (string= "`40ANTS-DOC/BUILDER/PRINTER::*DOCUMENT-NORMALIZE-PACKAGES*`"
                   (40ants-doc/markdown/transform::replace-known-references
                    "`40ANTS-DOC/BUILDER/PRINTER::*DOCUMENT-NORMALIZE-PACKAGES*`"
                    :known-references ())))
      (ok (string= "`40ANTS-DOC/BUILDER/PRINTER::*DOCUMENT-NORMALIZE-PACKAGES*`"
                   (40ants-doc/markdown/transform::replace-known-references
                    "40ANTS-DOC/BUILDER/PRINTER::*DOCUMENT-NORMALIZE-PACKAGES*"
                    :known-references ())))))
  (testing "Without references"
    (ok (string= "`FOO`"
                 (40ants-doc/markdown/transform::replace-known-references
                  "`FOO`"
                  :known-references ()))))
  (testing "With single locative 1"
    (ok (string= "[`FOO`][]"
                 (40ants-doc/markdown/transform::replace-known-references
                  "`FOO`"
                  :known-references
                  (list (40ants-doc/reference::make-reference 'foo 'function))))))
  (testing "With single locative 2"
    (ok (string= "[`FOO`][]"
                 (40ants-doc/markdown/transform::replace-known-references
                  "FOO"
                  :known-references
                  (list (40ants-doc/reference::make-reference 'foo 'function))))))
  (testing "With single locative 3"
    (ok (string= "[`40ANTS-DOC/SOURCE-API::FIND-SOURCE`][]"
                 (40ants-doc/markdown/transform::replace-known-references
                  "40ANTS-DOC/SOURCE-API::FIND-SOURCE"
                  :known-references
                  (list (40ants-doc/reference::make-reference '40ANTS-DOC/SOURCE-API::FIND-SOURCE
                                                              'generic-function))))))
  (testing "With multiple locatives"
    (ok (string= "`FOO`([`0`][] [`1`][])"
                 (40ants-doc/markdown/transform::replace-known-references
                  "`FOO`"
                  :known-references
                  (list (40ants-doc/reference::make-reference 'foo 'function)
                        (40ants-doc/reference::make-reference 'foo 'class)
                        ;; This reference should be ignored
                        (40ants-doc/reference::make-reference 'bar 'function)))))))


(deftest test-transform-tree
  (ok (equal '(1)
             (40ants-doc/utils::transform-tree
              (lambda (parent a)
                (declare (ignore parent))
                (values a (listp a) nil))
              '(1))))

  (ok (equal '(2 (3 (4 5)))
             (40ants-doc/utils::transform-tree
              (lambda (parent a)
                (declare (ignore parent))
                (values (if (listp a) a (1+ a))
                        (listp a)
                        nil))
              '(1 (2 (3 4))))))
  
  (ok (equal '(1 2 (2 3 (3 4 4 5)))
             (40ants-doc/utils::transform-tree
              (lambda (parent a)
                (declare (ignore parent))
                (values (if (listp a)
                            a
                            (list a (1+ a)))
                        (listp a)
                        (not (listp a))))
              '(1 (2 (3 4)))))))


(deftest test-macro-arg-names
  (ok (equal '(x a b c)
             (40ants-doc/args::macro-arg-names
              '((&key (x y)) (a b) &key (c d))))))


(defun test-document (format)
  (let ((outputs (write-test-document-files
                  (asdf:system-relative-pathname :40ants-doc "test/data/tmp/")
                  format)))
    (ok (= 4 (length outputs)))
    ;; the default page corresponding to :STREAM is empty
    (ok (string= "" (first outputs)))
    (ok (= 2 (count-if #'pathnamep outputs)))
    
    (dolist (output outputs)
      (when (pathnamep output)
        (let ((baseline (make-pathname
                         :directory (substitute "baseline" "tmp"
                                                (pathname-directory output)
                                                :test #'equal)
                         :defaults output)))
          (unless (string= (alexandria:read-file-into-string baseline)
                           (alexandria:read-file-into-string output))
            (cerror "Update output file."
                    "~@<Output ~S ~_differs from baseline ~S:~2%~A~@:>"
                    output baseline
                    (get-files-diff baseline
                                    output))
            (update-test-document-baseline format)))))))


(deftest test-markdown-document
  (test-document :markdown))


(deftest test-html-document
  (test-document :html))


(defun write-test-document-files (basedir format)
  (flet ((rebase (pathname)
           (merge-pathnames pathname
                            (make-pathname
                             :type (if (eq format :markdown) "md" "html")
                             :directory (pathname-directory basedir)))))
    (let ((open-args '(:if-exists :supersede :ensure-directories-exist t))
          (40ants-doc/builder/printer::*document-downcase-uppercase-code* (eq format :html)))
      (40ants-doc/document::document
       @test
       :pages `((:objects
                 ,(list @test-examples)
                 :output (nil))
                (:objects
                 ,(list @test-other)
                 :output (,(rebase "other/test-other") ,@open-args))
                (:objects
                 ,(list @test)
                 :output (,(rebase "test") ,@open-args)))
       :format format))))

(defun update-test-document-baseline (format)
  (write-test-document-files
   (asdf:system-relative-pathname :40ants-doc "test/data/baseline/")
   format))


(deftest test-core-dependencies
  (ok (equal (40ants-doc/utils::external-dependencies :40ants-doc)
             (list "asdf"
                   "named-readtables"
                   "pythonic-string-reader"))))
