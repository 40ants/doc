;;;; -*- mode: Lisp -*-

(defsystem 40ants-doc
  :licence "MIT"
  :version "0.1.0"
  :author "Alexander Artemenko"
  :mailto "svetlyak.40wt@gmail.com"
  :homepage "https://40ants.com/doc"
  :bug-tracker "https://github.com/40ants/doc/issues"
  :source-control (:git "https://github.com/40ants/doc")
  :description "Allows to put documentation inside lisp files and cross-reference between different entities. Based on MGL-PAX."
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("uiop"
               "40ants-doc/core"
               "40ants-doc/restart"
               "40ants-doc/glossary"
               "40ants-doc/changelog"
               "40ants-doc/ignored-words")
  :in-order-to ((asdf:test-op (asdf:test-op "40ants-doc-test"))))


(asdf:register-system-packages "log4cl" '("LOG"))
