(defpackage #:40ants-doc/ci
  (:use #:cl)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/jobs/run-tests)
  (:import-from #:40ants-ci/jobs/docs))
(in-package 40ants-doc/ci)


(defworkflow linter
  :on-push-to "master"
  :on-pull-request t
  :jobs ((40ants-ci/jobs/linter:linter
          :asdf-systems ("40ants-doc/full"))))


(defworkflow ci
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :jobs ((40ants-ci/jobs/run-tests:run-tests
          :asdf-system "40ants-doc-full"
          :coverage t)))


(defworkflow docs
  :on-push-to "master"
  :on-pull-request t
  :jobs ((40ants-ci/jobs/docs:build-docs
          :asdf-system "40ants-doc/doc")))
