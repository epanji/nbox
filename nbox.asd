;;;; nbox.asd

(defsystem #:nbox
  :version "0.0.1"
  :author "Panji Kusuma <epanji@gmail.com>"
  :description "NBOX is system to provide nested box with basic functionality"
  :license  "Public Domain"
  :serial t
  :components ((:file "package")
               (:file "nbox"))
  :in-order-to ((test-op (load-op "nbox/tests")))
  :perform (test-op (o c) (symbol-call :nbox/tests :suite-tests)))

(defsystem #:nbox/tests
  :depends-on ("fiveam" "nbox")
  :components ((:file "nbox-tests")))
