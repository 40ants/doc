(defpackage #:40ants-doc-test/locatives
  (:use #:cl)
  (:import-from #:40ants-doc)
  (:import-from #:40ants-doc/swank)
  (:import-from #:rove
                #:ok
                #:testing
                #:deftest))
(in-package 40ants-doc-test/locatives)


(deftest test-reading-locative
  (testing "Usual symbols should read without problems"
    (ok (eql (40ants-doc/swank::read-locative-from-string "FUNCTION")
             'function)))
  
  (testing "Locative with arguments can be parsed as well"
    (ok (equal (40ants-doc/swank::read-locative-from-string "(METHOD () (STRING))")
               '(method () (string))))))
