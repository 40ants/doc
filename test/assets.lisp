(uiop:define-package #:40ants-doc-test/assets
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:40ants-doc-full/assets
                #:defasset)
  (:import-from #:40ants-doc-full/builder
                #:render-to-files)
  (:import-from #:rove
                #:deftest
                #:ok
                #:signals
                #:testing)
  (:import-from #:alexandria
                #:read-file-into-string))
(in-package #:40ants-doc-test/assets)


(defasset @test-asset.png
  "static/rendering.png"
  :target-filename "assets/test-rendering.png")

(defasset @missing-asset.png
  "test/data/does-not-exist.png"
  :target-filename "assets/missing.png")

(defasset @unsafe-asset.png
  #.(asdf:system-relative-pathname :40ants-doc "static/rendering.png")
  :target-filename "../outside.png")

(defasset @colliding-asset-a.png
  #.(asdf:system-relative-pathname :40ants-doc "static/rendering.png")
  :target-filename "assets/collision.png")

(defasset @colliding-asset-b.png
  #.(asdf:system-relative-pathname :40ants-doc "static/rendering.png")
  :target-filename "assets/collision.png")


(defsection @asset-page (:export nil)
  "@TEST-ASSET.PNG")


(defsection @repeated-asset-page (:export nil)
  "@TEST-ASSET.PNG and @TEST-ASSET.PNG")


(defsection @missing-asset-page (:export nil)
  "@MISSING-ASSET.PNG")


(defsection @unsafe-asset-page (:export nil)
  "@UNSAFE-ASSET.PNG")


(defsection @colliding-asset-page (:export nil)
  "@COLLIDING-ASSET-A.PNG")


(defun make-test-output-directory ()
  (uiop:ensure-directory-pathname
   (merge-pathnames (format nil "40ants-doc-assets-~A/" (gensym))
                    (uiop:temporary-directory))))


(defun render-asset-page (format base-filename)
  (render-to-files
   (40ants-doc-full/page:make-page @asset-page :base-filename base-filename)
   :base-dir (make-test-output-directory)
   :format format))


(defun count-occurrences (needle text)
  (loop with start = 0
        for position = (search needle text :start2 start)
        while position
        count 1
        do (setf start (+ position (length needle)))))


(deftest test-asset-renders-to-html
  (multiple-value-bind (output-dir output-path)
      (render-asset-page :html "index")
    (testing "The source image is copied before HTML pages are emitted"
      (ok (probe-file (merge-pathnames "assets/test-rendering.png" output-dir))))
    (testing "The asset symbol is emitted as an image"
      (ok (search "<img src=\"assets/test-rendering.png\""
                  (read-file-into-string output-path))))))


(deftest test-asset-renders-to-markdown
  (multiple-value-bind (output-dir output-path)
      (render-asset-page :markdown "nested/page")
    (testing "The source image is copied for Markdown output"
      (ok (probe-file (merge-pathnames "assets/test-rendering.png" output-dir))))
    (testing "The asset has a page-relative Markdown image source"
      (ok (search "![@TEST-ASSET.PNG](../assets/test-rendering.png)"
                  (read-file-into-string output-path))))))


(deftest test-repeated-asset-is-copied-to-one-target
  (multiple-value-bind (output-dir output-path)
      (render-to-files @repeated-asset-page
                       :base-dir (make-test-output-directory)
                       :format :markdown)
    (testing "Each occurrence is rendered as an image"
      (ok (= 2 (count-occurrences "![@TEST-ASSET.PNG](assets/test-rendering.png)"
                                  (read-file-into-string output-path)))))
    (testing "Repeated occurrences share one output target"
      (ok (probe-file (merge-pathnames "assets/test-rendering.png" output-dir))))))


(deftest test-missing-asset-is-rejected
  (testing "A referenced declaration without a source file fails the build"
    (signals (render-to-files @missing-asset-page
                              :base-dir (make-test-output-directory)
                              :format :html)
             'error)))


(deftest test-unsafe-asset-target-is-rejected
  (testing "An asset target cannot escape the output directory"
    (signals (render-to-files @unsafe-asset-page
                              :base-dir (make-test-output-directory)
                              :format :html)
             'error)))


(deftest test-colliding-asset-target-is-rejected
  (testing "Two asset names cannot claim one output pathname"
    (signals (render-to-files @colliding-asset-page
                              :base-dir (make-test-output-directory)
                              :format :html)
             'error)))
