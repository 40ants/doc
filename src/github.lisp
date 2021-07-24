(uiop:define-package #:40ants-doc/github
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection)
  (:import-from #:cl-fad)
  (:import-from #:40ants-doc/locatives/base)
  (:import-from #:40ants-doc/source)
  (:import-from #:40ants-doc/reference)
  (:import-from #:40ants-doc/source-api)
  (:export
   #:make-github-source-uri-fn))
(in-package 40ants-doc/github)


(defsection @github-workflow (:title "Github Workflow"
                              :ignore-words ("HTML"))
  "It is generally recommended to commit generated readmes (see
  40ANTS-DOC/BUILDER::UPDATE-ASDF-SYSTEM-README) so that users have something to read
  without reading the code and sites like github can display them.

  HTML documentation can also be committed, but there is an issue with
  that: when linking to the sources (see MAKE-GITHUB-SOURCE-URI-FN),
  the commit id is in the link. This means that code changes need to
  be committed first, then HTML documentation regenerated and
  committed in a followup commit.

  The second issue is that github is not very good at serving HTMLs
  files from the repository itself (and
  [http://htmlpreview.github.io](http://htmlpreview.github.io) chokes
  on links to the sources).

  The recommended workflow is to use
  [gh-pages](https://pages.github.com/), which can be made relatively
  painless with the `git workflow` command. The gist of it is to make
  the `doc/` directory a checkout of the branch named `gh-pages`. A
  good description of this process is
  [http://sangsoonam.github.io/2019/02/08/using-git-worktree-to-deploy-github-pages.html](http://sangsoonam.github.io/2019/02/08/using-git-worktree-to-deploy-github-pages.html).
  Two commits needed still, but it is somewhat less painful.

  This way the HTML documentation will be available at
  `http://<username>.github.io/<repo-name>`. It is probably a good
  idea to add section like the 40ANTS-DOC/DOC:@LINKS section to allow jumping
  between the repository and the gh-pages site."
  (make-github-source-uri-fn function))

(defun make-github-source-uri-fn (asdf-system github-uri &key git-version)
  "Return a function suitable as :SOURCE-URI-FN of a page spec (see
  the :PAGES argument of 40ANTS-DOC/DOCUMENT::DOCUMENT). The function looks the source
  location of the reference passed to it, and if the location is
  found, the path is made relative to the root directory of
  ASDF-SYSTEM and finally an URI pointing to github is returned. The
  URI looks like this:

      https://github.com/melisgl/mgl-pax/blob/master/src/pax-early.lisp#L12

  \"master\" in the above link comes from GIT-VERSION.

  If GIT-VERSION is NIL, then an attempt is made to determine to
  current commit id from the `.git` in the directory holding
  ASDF-SYSTEM. If no `.git` directory is found, then no links to
  github will be generated.

  A separate warning is signalled whenever source location lookup
  fails or if the source location points to a directory not below the
  directory of ASDF-SYSTEM."
  (let* ((git-version (or git-version (asdf-system-git-version asdf-system)))
         (system-dir (asdf:system-relative-pathname asdf-system "")))
    (if git-version
        (let ((line-file-position-cache (make-hash-table :test #'equal))
              (find-source-cache (make-hash-table :test #'equal)))
          (lambda (reference)
            (let ((40ants-doc/source::*find-source-cache* find-source-cache))
              (multiple-value-bind (relative-path line-number)
                  (convert-source-location (40ants-doc/source-api::find-source
                                            (40ants-doc/reference::resolve reference))
                                           system-dir reference
                                           line-file-position-cache)
                (when relative-path
                  (format nil "~A/blob/~A/~A#L~S" github-uri git-version
                          relative-path (1+ line-number)))))))
        (warn "No GIT-VERSION given and can't find .git directory ~
              for ASDF system~% ~A. Links to github will not be generated."
              (asdf:component-name (asdf:find-system asdf-system))))))

(defun asdf-system-git-version (system)
  (let ((git-dir
          (merge-pathnames (make-pathname :directory '(:relative ".git"))
                           (asdf:system-relative-pathname
                            (asdf:component-name (asdf:find-system system))
                            ""))))
    (if (probe-file git-dir)
        (git-version git-dir)
        nil)))

(defun git-version (git-dir)
  (let ((head-string (read-first-line
                      (merge-pathnames (make-pathname :name "HEAD") git-dir))))
    (if (alexandria:starts-with-subseq "ref: " head-string)
        (let ((ref (subseq head-string 5)))
          (values (read-first-line (merge-pathnames ref git-dir)) ref))
        head-string)))

(defun read-first-line (filename)
  (with-open-file (stream filename)
    (read-line stream)))

(defun convert-source-location (source-location system-dir reference
                                line-file-position-cache)
  (cond ((or
          ;; CCL
          (null source-location)
          ;; SBCL, AllegroCL
          (eq (first source-location) :error))
         (warn "~@<No source location found for reference ~:_~A: ~:_~A~%~@:>"
               reference (second source-location)))
        (t
         (assert (eq (first source-location) :location))
         (let* ((filename (second (assoc :file (rest source-location))))
                (position (second (assoc :position (rest source-location))))
                (relative-path (and filename
                                    (enough-namestring filename system-dir))))
           (if (and relative-path (cl-fad:pathname-relative-p relative-path))
               (values relative-path
                       (file-position-to-line-number filename position
                                                     line-file-position-cache))
               (warn "Source location for ~S is not below system ~
                     directory ~S.~%" reference system-dir))))))

(defun file-position-to-line-number (filename file-position cache)
  (if (null file-position)
      0
      (let ((line-file-positions (or (gethash filename cache)
                                     (setf (gethash filename cache)
                                           (line-file-positions filename)))))
        (loop for line-number upfrom 0
              for line-file-position in line-file-positions
              do (when (< file-position line-file-position)
                   (return line-number))))))

;;; This is cached because it is determining the line number for a
;;; given file position would need to traverse the file, which is
;;; extremely expesive. Note that position 0 is not included, but
;;; FILE-LENGTH is.
(defun line-file-positions (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
          for line-number upfrom 0
          while line
          collect (file-position stream))))
