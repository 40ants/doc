(uiop:define-package #:40ants-doc/ci
  (:use #:cl)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/jobs/run-tests)
  (:import-from #:40ants-ci/jobs/docs)
  (:import-from #:40ants-ci/jobs/autotag
                #:autotag))
(in-package #:40ants-doc/ci)


(defparameter *lisp-implementations*
  (list "sbcl-bin"
        ;; Some tests fail on CCL
        ;; "ccl-bin/1.12.1"
        "abcl-bin"
        ;; At 2023-04-22 tests started to fail on Allegro with error:
        ;; Allegro CL(pid 6257): System Error (gsgc) scavenge found ref to cons outside cons area in 0xffba645c
        ;; "allegro"
        "clasp"
        ;; This CL implementation does not work in any matrix combinations
        ;; "cmu-bin"
        "lispworks"
        "mkcl"
        ;; This fails to install under the Roswell on Ubuntu
        ;; "npt"
        "ecl") )


(defworkflow release
  :on-push-to "master"
  :jobs ((autotag)))


(defworkflow linter
  :on-push-to "master"
  :on-pull-request t
  :jobs ((40ants-ci/jobs/linter:linter
          :asdf-systems ("40ants-doc"
                         "40ants-doc-full"
                         "40ants-doc-test")
          :check-imports t)))


(defworkflow ci
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :jobs ((40ants-ci/jobs/run-tests:run-tests
          :asdf-system "40ants-doc-full"
          :quicklisp ("quicklisp"
                      "ultralisp")
          :lisp *lisp-implementations*
          :exclude (append
                    ;; All implementations except SBCL we'll check only on
                    ;; and Ultralisp dist.
                    (loop for lisp in *lisp-implementations*
                          unless (string-equal lisp "sbcl-bin")
                            append (list (list :quicklisp "quicklisp"
                                               :lisp lisp))))
          :coverage t)))


(defworkflow docs
  :on-push-to "master"
  :on-pull-request t
  :jobs ((40ants-ci/jobs/docs:build-docs
          :asdf-system "40ants-doc-full")))
