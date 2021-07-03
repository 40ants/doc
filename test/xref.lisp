(uiop:define-package #:40ants-doc-test/xref
  (:use #:cl)
  (:import-from #:rove
                #:ok
                #:testing
                #:deftest)
  (:import-from #:40ants-doc/commondoc/builder))
(in-package 40ants-doc-test/xref)


(defun bar ()
  "Returns the answer."
  42)

(defun foo ()
  "This function just calls [BAR][function]."
  (values))


(40ants-doc:defsection @foo-n-bar (:title "Test section")
  (foo function)
  (bar function))


(deftest test-reference-collection
  (testing "Checking if this section includes two references"
    (let* ((doc (40ants-doc/commondoc/builder::to-commondoc @foo-n-bar))
           (references (40ants-doc/commondoc/xref::collect-references doc)))
      (ok (= (length references)
             2))
      (let ((objects (sort (mapcar #'40ants-doc/reference::reference-object references)
                           #'string<)))
        (ok (equal objects
                   '(bar foo)))))))


(deftest test-reference-replacing
  (testing "Simple case"
    (let* ((reference (40ants-doc/reference::make-reference 'foo 'function))
           (doc (40ants-doc/commondoc/markdown::parse-markdown "[FOO][function]")))
      
      (flet ((first-child ()
               (first (common-doc:children doc))))
        (testing "Before replacing we should have a paragraph with internal link"
          (ok (typep doc 'common-doc:paragraph))
          (ok (typep (first-child) '40ants-doc/commondoc/xref:xref)))
      
        (let ((result (40ants-doc/commondoc/xref::replace-references doc (list reference))))
          (testing "Resulting document should remain the same, because only paragraph's child should be changed"
            (ok (eql doc result)))

          (testing "But it's child should be changed to a real web link"
            (ok (typep (first-child) 'common-doc:document-link))))))))
