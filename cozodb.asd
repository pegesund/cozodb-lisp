;;;; cozodb.asd

(asdf:defsystem #:cozodb
  :description "Describe cozodb here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-json #:cffi)
  :components ((:file "package")
               (:file "cozodb")))
