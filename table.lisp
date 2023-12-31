(in-package #:cozodb)

;code for formating output is borrowed from this url: https://gist.github.com/WetHat/a49e6f2140b401a190d45d31e052af8f

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
