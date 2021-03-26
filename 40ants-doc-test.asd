(defsystem 40ants-doc-test
  :licence "MIT"
  :author "Alexander Artemenko"
  :description "Test system for 40ants-doc."
  :depends-on ("40ants-doc")
  :class :package-inferred-system
  :pathname "t"
  :depends-on ("40ants-doc-test/test-transcribe"
               "40ants-doc-test/test")
  ;; :components ((:module "test"
  ;;               :serial t
  ;;               :components ((:file "package")
  ;;                            (:file "test-transcribe")
  ;;                            (:file "test"))))
  :perform (test-op (op c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))
