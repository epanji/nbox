;;;; nbox.asd
;;
;;;; Copyright (c) 2020 Panji Kusuma <epanji@gmail.com>

(asdf:defsystem #:nbox
  :version "0.0.1"
  :author "Panji Kusuma <epanji@gmail.com>"
  :description "Describe nested-box here"
  :license  "Specify license here"
  :serial t
  :components ((:file "package")
               (:file "nbox"))
  :in-order-to ((test-op (load-op "nbox/tests")))
  :perform (test-op (o c) (symbol-call :nbox/tests :suite-tests)))

(asdf:defsystem #:nbox/tests
  :depends-on ("fiveam" "nbox")
  :components ((:file "nbox-tests")))
