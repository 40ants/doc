;;;; -*- mode: Lisp -*-

(defsystem 40ants-doc-full
  :licence "MIT"
  :version "0.1.0"
  :author "Alexander Artemenko"
  :mailto "svetlyak.40wt@gmail.com"
  :homepage "http://40ants.com/doc"
  :bug-tracker "https://github.com/40ants/doc/issues"
  :source-control (:git "https://github.com/40ants/doc.git")
  :description "Documentation generator for 40ANTS-DOC (based on MGL-PAX). You will need to load this system, to build documentation for a library which uses 40ANTS-DOC system."
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("40ants-doc/full")
  :in-order-to ((asdf:test-op (asdf:test-op "40ants-doc-test"))))
