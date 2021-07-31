(defpackage #:40ants-doc/rewrite
  (:use #:cl))
(in-package 40ants-doc/rewrite)


(defun rewrite-url (url)
  "This function allows to have \"clean\" URLs on side. It is much
   nicer to see https://40ants.com/doc/some-lib/changelog/ than a
   https://40ants.com/doc/some-lib/changelog.html
"
  (cl-ppcre:regex-replace "index\\.html$" url ""))


(defun rewrite-file (file)
  "Takes a string with filename and transforms to make clean HTML urls.

   Here are a few input -> output examples:

   - index.html -> index.html
   - some/index.html -> some/index.html
   - some.html -> some/index.html
   - foo/bar.html -> foo/bar/index.html
"
  (cond
    ((and (str:ends-with-p ".html" file)
          (not (str:ends-with-p "index.html" file)))
     (cl-ppcre:regex-replace "\\.html"
                             file
                             "/index.html"))
    (t file)))
