;;;; components.asd

(asdf:defsystem #:components
  :description "Managed system components for Common Lisp."
  :author "Matthew Stickney"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "components"))
  :in-order-to ((test-op (test-op components-test))))
