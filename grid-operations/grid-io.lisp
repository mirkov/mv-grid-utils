;; Mirko Vukovic
;; Time-stamp: <2011-02-28 10:22:04 grid-io.lisp>
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

Some read methods introduce additional error checking and action.
Consult their documentation for details.

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

(define-test read-incomplete-csv-grid
    (with-open-file (stream
		   #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data--incomplete.csv"
		   #-cysshd1 "/home/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data--incomplete.csv"
		   :direction :input) 
    (assert-grid-equal 
     (grid::make-grid `((,*array-type*) ,*float-type*)
		      :initial-contents '((1d0 2d0 3d0) (4d0 -1d0 -1d0)))
     (read-grid '(nil nil) stream :csv :eof-error-p t :eof-value :eof
		:key :read-from-string :type 'double-float
		:eor-error-p nil :eor-value -1d0)))
    (with-open-file (stream
		   #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data--incomplete.csv"
		   #-cysshd1 "/home/my-software-add-ons/my-lisp/mv-grid-utils/grid-operations/2d-grid-data--incomplete.csv"
		   :direction :input) 
    (assert-grid-equal 
     (grid::make-grid `((,*array-type*) ,*float-type*)
		      :initial-contents '((1d0 2d0 3d0) (4d0 2d0 3d0)))
     (read-grid '(nil nil) stream :csv :eof-error-p t :eof-value :eof
		:key :read-from-string :type 'double-float
		:eor-error-p nil :eor-value
		#'(lambda (col row)
		    (+ col row))))))



(defmethod read-grid (dimensions stream 
		      (file-format (eql :csv))
		      &key (eof-error-p t) eof-value
		      (eor-error-p t) (eor-value)
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

eor-error-p and eor-value control behavior if a record has fewer than
the required number of columns.

If eor-error-p is t (default), an error is signaled.  If it is nil,
`eor-value' is stored in that cell.  If eor-value is a function, the
function is called on the column index and the row index of the
missing cell, and its return value is stored.


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
	      (loop for (fields count) = (multiple-value-list next-record)
		 for row upfrom 0
		 while fields
		 do (print row)
		 do (push
		     (if (< count cols)
			 (if eor-error-p
			     (error "Row ~a is too short" row)
			     (append fields (record-fill eor-value cols count row)))
			 fields)
		     data))
	      (make-grid `((,*array-type* nil) ,type)
			 :initial-contents (nreverse data))))))))




