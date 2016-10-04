;;;; components-test.asd

(asdf:defsystem #:components-test
  :description "Test system for COMPONENTS."
  :depends-on (#:components #:prove)
  :defsystem-depends-on (#:prove-asdf)
  :components
  ((:test-file "tests"))
  :perform (test-op :after (op c)
                    (uiop:symbol-call '#:prove '#:run c)))
