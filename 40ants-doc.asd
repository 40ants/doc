;;;; -*- mode: Lisp -*-

(defsystem 40ants-doc
  :licence "MIT"
  :version "0.1.0"
  :author "Alexander Artemenko"
  :mailto "svetlyak.40wt@gmail.com"
  :homepage "http://40ants.com/doc"
  :bug-tracker "https://github.com/40ants/doc/issues"
  :source-control (:git "https://github.com/40ants/doc.git")
  :description "Exploratory programming tool and documentation generator, based on MGL-PAX."
  :class :package-inferred-system
  :pathname "src"
  :depends-on (
               "40ants-doc/core"
               ;; "40ants-doc/small"
               )
  ;; :depends-on (:3bmd :3bmd-ext-code-blocks :alexandria :babel :cl-fad :colorize
  ;;              :ironclad :named-readtables :pythonic-string-reader :swank
  ;;              :mgl-pax-minimal)
  :in-order-to ((asdf:test-op (asdf:test-op "40ants-doc-test"))))


(asdf:register-system-packages "3bmd-ext-code-blocks" '("3BMD-CODE-BLOCKS"))
(asdf:register-system-packages "swank" '("SWANK-BACKEND"
                                         "SWANK-MOP"))
