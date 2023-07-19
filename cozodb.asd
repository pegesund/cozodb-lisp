;;;; cozodb.asd

(asdf:defsystem #:cozodb
  :description "Binding to Cozodb"
  :author "Petter Egesund <petter.egesund@gmail.com>"
  :license  "BSD-license"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-json #:cffi #:alexandria #:fiveam #:cl-fad)
  :components ((:file "package")
               (:file "cozodb")
	       (:file "table")
	       (:file "test")
	       ))
