;; Mirko Vukovic
;; Time-stamp: <2011-02-26 15:40:25 grid-io.lisp>
;; 
;; Copyright 2011 Mirko Vukovic
;; Distributed under the terms of the GNU General Public License
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :mv-grid)

(export '(read-grid))

(defgeneric read-grid (dimensions stream file-format
				  &key eof-error-p
				  eof-value type)
  (:documentation "read-grid returns a grid of values read from
`stream'.

Arguments and Values:

 - dimensions -- grid dimensions.  A list of numbers.  
   Some filetypes/methods permit the dimensions to be determined
   dynamically.  These dimensions are denoted by nil's.
   If the file contents do not conform to the dimensions
   specification, the consequences are unspecified.
 - file-format -- symbol (t or some other)
 - stream -- input stream 
 - eof-error-p -- boolean. Default is true.  Throw error if end of
   file before the whole grid is read.  If nil, return eof-value
 - eof-value -- value returned if at end-of-file before the whole grid is read
   and eof-error-p is nil
 - type -- default type of value stored in grid.  Read values are coerced to
   `type'

"))


(define-test read-grid
  (with-open-file (stream
		   #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data.txt"
		   #-cysshd1 "/home/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data.txt"
		   :direction :input) 
    (assert-grid-equal 
     (grid::make-grid `((,*array-type*) ,*float-type*)
		      :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0)))
     (read-grid '(2 3) stream t :eof-error-p t))))



(defmethod read-grid (dimensions (stream file-stream)
		      (file-format (eql 't))
		      &key (eof-error-p t) eof-value (type 'double-float))
  "Use `read' to read grid entries from stream.  The file need not be
in rows/cols format.  Values are read sequentially, coerced to `type'
and stored in grid.  Grid dimensions must be explicity specified.

Default type is 'double-float"
  (map-grid :source #'(lambda (&rest args)
			     (declare (ignore args))
			     (let ((value 
				    (read stream
					  eof-error-p eof-value)))
			       (when (eq value eof-value)
				 (return-from read-grid value))
			       (coerce value type)))
		 :source-dims dimensions
		 :destination-specification `((,*array-type* ,@dimensions)
					      ,type)))

(define-test read-csv-grid
  (with-open-file (stream
		   #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data.csv"
		   #-cysshd1 "/home/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data.csv"
		   :direction :input) 
    (assert-grid-equal 
     (grid::make-grid `((,*array-type*) ,*float-type*)
		      :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0)))
     (read-grid '(2 3) stream :csv :eof-error-p t :eof-value nil
		:key :read-from-string :type 'double-float)))
  (with-open-file (stream
		   #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data.csv"
		   #-cysshd1 "/home/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data.csv"
		   :direction :input) 
    (assert-grid-equal 
     (grid::make-grid `((,*array-type*) ,*float-type*)
		      :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0)))
     (read-grid '(nil 3) stream :csv :eof-error-p t :eof-value nil
		:key :read-from-string :type 'double-float)))
  (with-open-file (stream
		   #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data.csv"
		   #-cysshd1 "/home/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data.csv"
		   :direction :input) 
    (assert-grid-equal 
     (grid::make-grid `((,*array-type*) ,*float-type*)
		      :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0)))
     (read-grid '(2 nil) stream :csv :eof-error-p t :eof-value nil
		:key :read-from-string :type 'double-float)))
  (with-open-file (stream
		   #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data.csv"
		   #-cysshd1 "/home/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data.csv"
		   :direction :input) 
    (assert-grid-equal 
     (grid::make-grid `((,*array-type*) ,*float-type*)
		      :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0)))
     (read-grid '(nil nil) stream :csv :eof-error-p t :eof-value :eof
		:key :read-from-string :type 'double-float))))

(defmethod read-grid (dimensions stream 
		      (file-format (eql :csv))
		      &key (eof-error-p t) eof-value
		      (type t)
		      (key t))
  "Read a 2D grid from a csv file using `picard-csv:read-csv-line.

 - dimensions -two element list (rows cols)
 - rows -- number or nil.  If nil, determined from first row.
 - cols -- number or nil.  If nil, determined from whole file contents.

If rows are specified, and the file does not match specification,
return depends on eof-error-p and eof-value.

If cols are specified, and file does not contain enough cols,
consequences are unspecified.

Return values:

This method uses `next-csv-record' to read the next line in the file,
process and return its contents.  It passes the `key' and `type'
arguments to `next-csv-record'.  See the documentation on
`next-csv-record' on how to use `key' and `type' to controll the
parsing of csv records."
  ;;  (break)
  (symbol-macrolet
      ((next-record (next-csv-record stream key type)))
    (let ((record nil))
      (destructuring-bind (rows cols) dimensions
	(unless cols
	  (multiple-value-setq (record cols)
	    next-record))
	(if rows
	    (map-grid
	     :source #'(lambda (&rest args)
			 (declare (ignore args))
			 (or record
			     (setf record next-record))
			 (when (null record)
			   (if eof-error-p
			       (error #+clisp 'SYSTEM::SIMPLE-END-OF-FILE
				      "End of file")
			       (return-from read-grid eof-value)))
			 (pop record))
	     :source-dims `(,rows ,cols)
	     :destination-specification `((,*array-type* ,rows ,cols)
					  ,type))
	    (let ((data
		   ;; Here record is a list (a b c ...).  In order for
		   ;; push to work, I need to embed it in another
		   ;; list: ((a b c ...))
		   (if record (list record)
		       record)))
	      (loop for fields = next-record
		 ;;		   for i from 1
		 ;;		   do (print i)
		 while fields
		 do (push fields data))
	      (make-grid `((,*array-type* nil) ,type)
			 :initial-contents (nreverse data))))))))



(define-test next-csv-record
   (with-input-from-file (stream 
			  #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data.csv"
			  #-cysshd1 "/home/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data.csv")
     (multiple-value-bind (values count)
	      (next-csv-record stream t)
       (assert-equal
	'("1" "2" "3") values)
       (assert-numerical-equal 3 count)))
   (with-input-from-file (stream 
		   #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data.csv"
		   #-cysshd1 "/home/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data.csv")
     (multiple-value-bind (values count)
	      (next-csv-record stream :read-from-string)
       (assert-equal
	'(1 2 3) values)
       (assert-numerical-equal 3 count)))
   (with-input-from-file (stream
		   #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data.csv"
		   #-cysshd1 "/home/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data.csv"
)
     (multiple-value-bind (values count)
	 (next-csv-record stream :read-from-string
			  'double-float)
       (assert-equal
	'(1d0 2d0 3d0) values)
       (assert-numerical-equal 3 count)))
   (with-input-from-file (stream
		   #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data.csv"
		   #-cysshd1 "/home/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data.csv"
)
     (multiple-value-bind (values count)
	 (next-csv-record stream
			  #'(lambda (value column)
			      (expt (read-from-string
				     value)
				    column)))
       (assert-equal
	'(1 2 9) values)
       (assert-numerical-equal 3 count))))




(defgeneric next-csv-record (stream key &optional type)
  (:documentation
   "Read next record from csv-file bound to `stream' and return the
record contents as a list.

The second returned value is the number of found records.

We use csv-parser:read-csv-line from picard-csv to read csv records.
This function returns the record contents as a list of strings.

Parameters `key' and `type' can be used to convert the string to a
desired type.

Depending on the value of `key' the record strings can be extracted
and post-processed as follows:
 - key (eql t) returns the string
 - key (eql :read-from-string) applies `read-from-string' to the string
 - key can be a two-argument function (string and column number) to 
   process the string

In case of `key' being :read-from-string, type can be used to coerce
the result of read to a desired type
")
  (:method ((stream stream) (key (eql t)) &optional type)
    (declare (ignore type))
    (csv-parser:read-csv-line stream))
  (:method ((stream stream) (key (eql :read-from-string))
	    &optional (type t))
    (multiple-value-bind (record cols)
	(csv-parser:read-csv-line stream)
      (values
       (mapcar #'(lambda (string)
		   (coerce (read-from-string string)
			   type))
	       record)
       cols)))
  (:method ((stream stream) (key function)
	    &optional  (type t))
    (declare (ignore type))
    (multiple-value-bind (record cols)
	(csv-parser:read-csv-line stream)
      (values
       (loop for string in record
	  for column from 0
	  collect (funcall key string column))
       cols))))
