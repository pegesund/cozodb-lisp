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

; code for formating output is borrowed from this url: https://gist.github.com/WetHat/a49e6f2140b401a190d45d31e052af8f

(defvar CELL-FORMATS '(:left   "~vA"
                              :center "~v:@<~A~>"
                              :right  "~v@A"))

(defun format-table (stream data &key (column-label (loop for i from 1 to (length (car data))
                                                          collect (format nil "COL~D" i)))
                                      (column-align (loop for i from 1 to (length (car data))
                                                          collect :left)))
    (let* ((col-count (length column-label))
           (strtable  (cons column-label ; table header
                          (loop for row in data ; table body with all cells as strings
                              collect (loop for cell in row
                                           collect (if (stringp cell)
                                                        cell
                                                    ;else
                                                        (format nil "~A" cell))))))
           (col-widths (loop with widths = (make-array col-count :initial-element 0)
                           for row in strtable
                           do (loop for cell in row
                                    for i from 0
                                  do (setf (aref widths i)
                                         (max (aref widths i) (length cell))))
                           finally (return widths))))
        ;------------------------------------------------------------------------------------
        ; splice in the header separator
        (setq strtable
              (nconc (list (car strtable) ; table header
                           (loop for align in column-align ; generate separator
                                 for width across col-widths
                               collect (case align
                                           (:left   (format nil ":~v@{~A~:*~}"
                                                        width  "-"))
                                           (:right  (format nil "~v@{~A~:*~}"
                                                        width  "-"))
                                           (:center (format nil ":~v@{~A~:*~}"
                                                        width "-")))))
                           (cdr strtable))) ; table body
        ;------------------------------------------------------------------------------------
        ; Generate the formatted table
        (let ((row-fmt (format nil "| ~{~A~^ | ~} |~~%" ; compile the row format
                           (loop for align in column-align
                               collect (getf CELL-FORMATS align))))
              (widths  (loop for w across col-widths collect w)))
            ; write each line to the given stream
            (dolist (row strtable)
                (apply #'format stream row-fmt (mapcan #'list widths row)))))
  )


(defun show-as-table(content)
  (if (assoc :CAUSES content)
      (format t "~a" content)
      (let* ((cl (cdr (assoc :HEADERS content)))
	     (ca (make-list (length cl) :initial-element :right))
	     (rows (cdr (assoc :ROWS content)))
	     (time-taken (cdr (assoc :TOOK content))))
	(format-table t rows :column-label cl :column-align ca)
	(format t "~% Time taken in seconds: ~5$ " time-taken)
	)))
  

; end def format table


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

(defun query(raw-string &rest params)
  (query-interpolate (reverse (coerce raw-string 'list)) nil '() (reverse params)))

