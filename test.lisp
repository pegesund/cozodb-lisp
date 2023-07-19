(in-package #:cozodb-test)

(def-suite test-cozodb
  :description "Test the read-file-as-string function.")
(in-suite test-cozodb)

(test world-is-sane
  (is (= 1 1)))

(test create-database-and-close
  (fad:delete-directory-and-files "/tmp/cozo_test1" :if-does-not-exist :ignore)
  (let ((db-id (open-db "/tmp/cozo/cozo_test2")))
    (is (numberp db-id))
    (close-db db-id))
   (fad:delete-directory-and-files "/tmp/cozo_test1" :if-does-not-exist :ignore)
  )

(test raw-query
  (fad:delete-directory-and-files "/tmp/cozo_test2" :if-does-not-exist :ignore)
  (let* ((db-id (open-db "/tmp/cozo/cozo_test2"))
	 (res (progn
		(run-query db-id"?[l1, l2] <- [['a', 1], ['b',2]] :create stored {l1, l2}" "" nil)
		(run-query db-id "?[a, b] := *stored[a, b]")
		))
	 (rows (assoc :ROWS res))
	 )
    (is (equal (cdr (assoc :ROWS res)) '(("a" 1) ("b" 2))))
    (close-db db-id))
  (fad:delete-directory-and-files "/tmp/cozo_test2" :if-does-not-exist :ignore)
  )
