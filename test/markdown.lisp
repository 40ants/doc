(defpackage #:40ants-doc-test/markdown
  (:use #:cl)
  (:import-from #:rove
                #:ok
                #:deftest))
(in-package 40ants-doc-test/markdown)


(deftest test-fully-qualified-symbols-shouldnt-be-splitted-to-italic
  (rove:testing "The SOME-VAR:*BLAH* should become a single common-doc:text-node"
    (let* ((paragraph (40ants-doc/commondoc/markdown:parse-markdown "SOME-VAR:*BLAH*"))
           (children (common-doc:children paragraph)))
      (ok (= (length children)
             1))
      (ok (typep (first children)
                 'common-doc:text-node)))))


(deftest test-simple-markdown-text
  (rove:testing "Simple text should be parsed like a paragraph of a text"
    (let* ((paragraph (40ants-doc/commondoc/markdown:parse-markdown "Just a text"))
           (children (common-doc:children paragraph)))
      (ok (= (length children)
             1))
      (ok (typep (first children)
                 'common-doc:text-node)))))
