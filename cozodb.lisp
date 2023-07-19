(in-package #:cozodb)

(load-foreign-library '(:default "libcozo_c"))

(defcfun "cozo_open_db" :string (engine :string) (path :string) (options :string) (db_id :pointer))
(defcfun "cozo_close_db" :boolean (id :int))
(defcfun "cozo_run_query" :pointer (db_id :int) (script_raw :string) (params_raw :string) (immutable_query :boolean))
(defcfun "cozo_free_str" :void (s :pointer))
(defcfun "cozo_export_relations" :pointer (db_id :int) (json_payload :string))
(defcfun "cozo_import_relations" :pointer (db_id :int) (json_payload :string))
(defcfun "cozo_backup" :pointer (db_id :int) (out_path :string))
(defcfun "cozo_restore" :pointer (db_id :int) (in_path :string))


; use like this: (export-relations 0 "{\"relations\":[\"rel\"]}")
; returns a json to be written

(defun export-relations(db-id json)
  (let* ((s (cozo-export-relations db-id json ))
	(ret (cffi:foreign-string-to-lisp s)))
    (cozo-free-str s)
    (with-input-from-string (s ret)
      (json:decode-json s)))) 

(defun import-relations(db-id json)
  (let* ((s (cozo-import-relations db-id json ))
	(ret (cffi:foreign-string-to-lisp s)))
    (cozo-free-str s)
    (with-input-from-string (s ret)
      (json:decode-json s))))

(defun backup(db-id out-path)
  (let* ((s (cozo-backup db-id out-path ))
	(ret (cffi:foreign-string-to-lisp s)))
    (cozo-free-str s)
    (with-input-from-string (s ret)
      (json:decode-json s))))

(defun restore(db-id in-path)
  (let* ((s (cozo-restore db-id in-path ))
	(ret (cffi:foreign-string-to-lisp s)))
    (cozo-free-str s)
    (with-input-from-string (s ret)
      (json:decode-json s))))

(defun open-db(db-dir)
  (let ((db-id-ptr (foreign-alloc :int :initial-element 0)))
    (cozo-open-db "rocksdb" db-dir "" db-id-ptr)
    (let ((db-id (mem-ref db-id-ptr :int)))
      (foreign-free db-id-ptr)
      db-id)))

(defun close-db(id)
  (cozo-close-db id))


(defun val-to-chars(v)
  (cond ((typep v 'number) (coerce (write-to-string v) 'list))
	((typep v 'string) (append (cons #\' (coerce v 'list)) (list #\')))
        ((typep v 'list) (concatenate 'list '(#\[)
				      (mapcar #'(lambda (x)
						(append (val-to-chars x) (list #\,)))
					      (butlast v))
					      (val-to-chars (car (last v)))
				      '(#\])))
  ))

(defun run-query(id sr &optional (pr "") (iq nil))
  (let* ((s (cozo-run-query id sr pr iq))
	(ret (cffi:foreign-string-to-lisp s)))
    (cozo-free-str s)
    (with-input-from-string (s ret)
      (json:decode-json s))))

(defun query-interpolate(raw-string in-quote interpolated-string &rest params)
  (if (null raw-string)
      (coerce (alexandria:flatten interpolated-string) 'string)
      (let ((c (car raw-string)))
	(cond ((equal c #\') (query-interpolate (cdr raw-string) (not in-quote) (cons c interpolated-string) (car params)))
	      (in-quote (query-interpolate (cdr raw-string) T (cons c interpolated-string) (car params)))
	      ((equal c #\%) (query-interpolate (cdr raw-string) nil (append (val-to-chars (caar params)) interpolated-string) (cdar params)))
	      (T (query-interpolate (cdr raw-string) nil (cons c interpolated-string) (car params)))))))

(defun query-interpolate-top(raw-string &rest params)
  (query-interpolate (reverse (coerce raw-string 'list)) nil '() (reverse params)))

(defun query(db-id query-string params &optional (extra-params "") (immutable nil))
  (let* ((qs (query-interpolate-top query-string params))
	 (res (run-query db-id qs extra-params immutable))
	 (rows (assoc :ROWS res)))
    (if rows
	(cdr rows)
	res)))
