;;;; package.lisp

(defpackage #:cozodb
  (:use #:cl #:cffi)
  (:export #:open-db
	   #:close-db
	   #:run-query
	   #:query
	   #:backup
	   #:restore
	   #:show-as-table
	   )
  )

(defpackage #:cozodb-test
  (:use #:cl #:cozodb #:fiveam))
