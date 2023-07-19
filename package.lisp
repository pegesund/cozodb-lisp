;;;; package.lisp

(defpackage #:cozodb
  (:use #:cl #:cffi)
  (:export #:open-db
	   #:close-db
	   #:run-query
	   )
  )

(defpackage #:cozodb-test
  (:use #:cl #:cozodb #:fiveam))
