(in-package #:cozodb-test)

(def-suite test-cozodb
  :description "Test the read-file-as-string function. Run all test with:  (run! 'test-cozodb) ")
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


; Create database, insert some raw data, quere these and verify these are the same, close and clean
(test raw-query
  (fad:delete-directory-and-files "/tmp/cozo_test2" :if-does-not-exist :ignore)
  (let* ((db-id (open-db "/tmp/cozo/cozo_test2"))
	 (res (progn
		(run-query db-id"?[l1, l2] <- [['a', 1], ['b',2]] :create stored {l1, l2}")
		(run-query db-id "?[a, b] := *stored[a, b]")
		))
	 )
    (is (equal (cdr (assoc :ROWS res)) '(("a" 1) ("b" 2))))
    (close-db db-id))
  (fad:delete-directory-and-files "/tmp/cozo_test2" :if-does-not-exist :ignore)
  )

; Create database, insert some raw data, quere these and verify these are the same, close and clean
(test interpolated-query
  (fad:delete-directory-and-files "/tmp/cozo_test3" :if-does-not-exist :ignore)
  (let* ((db-id (open-db "/tmp/cozo/cozo_test3"))
	 (res (progn
		(query db-id "?[l1, l2] <- % :create stored {l1, l2}" '(("a" 1) ("b" 2)))
		(run-query db-id "?[a, b] := *stored[a, b]")
		))
	 )
    (is (equal (cdr (assoc :ROWS res)) '(("a" 1) ("b" 2))))
    (close-db db-id))
  (fad:delete-directory-and-files "/tmp/cozo_test3" :if-does-not-exist :ignore)
  )


; create a db and insert some values, backup and restore to another db
; make sure that vals are in the restored db
(test backup
  (fad:delete-directory-and-files "/tmp/cozo_test4" :if-does-not-exist :ignore)
  (fad:delete-directory-and-files "/tmp/cozo_test5" :if-does-not-exist :ignore)
  (when (probe-file "/tmp/backup") (delete-file "/tmp/backup"))
  (let ((db-id1 (open-db "/tmp/cozo_test4"))
	(db-id2 (open-db "/tmp/cozo_test5")))
    (query db-id1 "?[l1, l2] <- % :create stored {l1, l2}" '(("a" 1) ("b" 2)))
    (backup db-id1 "/tmp/cozo_backup")
    (restore db-id2 "/tmp/cozo_backup")
    (let ((res (run-query db-id2 "?[a, b] := *stored[a, b]")))
      (print res)
      (is (equal (cdr (assoc :ROWS res)) '(("a" 1) ("b" 2)))))
    (close-db db-id1)
    (close-db db-id2))
  (fad:delete-directory-and-files "/tmp/cozo_test4" :if-does-not-exist :ignore)
  (fad:delete-directory-and-files "/tmp/cozo_test5" :if-does-not-exist :ignore)
  (when (probe-file "/tmp/backup") (delete-file "/tmp/backup")))


	 

  
