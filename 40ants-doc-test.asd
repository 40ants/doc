(defsystem 40ants-doc-test
  :licence "MIT"
  :author "Alexander Artemenko"
  :description "Test system for 40ANTS-DOC system."
  :class :package-inferred-system
  :pathname "test"
  :depends-on ("40ants-doc-test/test-transcribe"
               "40ants-doc-test/test")
  :perform (test-op (op c)
                    (unless (symbol-call :rove :run c)
                      (error "Tests failed"))))
