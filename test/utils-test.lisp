(defpackage #:40ants-doc-test/utils-test
  (:use #:cl)
  (:import-from #:rove
                #:ok
                #:deftest))
(in-package 40ants-doc-test/utils-test)


(deftest test-relative-paths
  (ok (string= (40ants-doc/utils:make-relative-path "foo"
                                                    "bar")
               "bar"))
  (ok (string= (40ants-doc/utils:make-relative-path "blah/minor"
                                                    "bar")
               "../bar"))
  (ok (string= (40ants-doc/utils:make-relative-path "foo/blah/minor"
                                                    "bar")
               "../../bar"))
  (ok (string= (40ants-doc/utils:make-relative-path "bar"
                                                    "foo/blah/minor")
               "foo/blah/minor"))

  (ok (string= (40ants-doc/utils:make-relative-path "foo/blah/minor"
                                                    "foo/blah/bar")
               "bar"))
  (ok (string= (40ants-doc/utils:make-relative-path "foo/blah/minor"
                                                    "foo/bar")
               "../bar"))
  (ok (string= (40ants-doc/utils:make-relative-path "foo/blah/minor"
                                                    "some/bar")
               "../../some/bar")))
