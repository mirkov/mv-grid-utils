;; Mirko Vukovic
;; Time-stamp: <2011-11-13 19:43:10 grid-iterating-functions.lisp>
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

;; Grid iterating functions modeled after `count', `find', `position',
;; `remove', `substitute' (PCL, Table 11.1) and their higher order
;; variants -if and -if-not.
;;
;; +----------+------+-----+-------+
;; |          |Simple| -if | if-not|
;; +----------+------+-----+-------+
;; |count     |      |     |       |
;; +----------+------+-----+-------+
;; |find      |  M   |  M  |       |
;; +----------+------+-----+-------+
;; |position  |  V   |  V  |       |
;; +----------+------+-----+-------+
;; |positions |  V   |     |       |
;; +----------+------+-----+-------+
;; |remove    |  M   | M   |       |
;; +----------+------+-----+-------+
;; |substitute|  M/V |     |       |
;; +----------+------+-----+-------+
;; 
;;
;; All operations are non-destructive -- the old grid is never
;; modified.
;;
;; There are additional functions such as reverse-vector
;;
;; 

(in-package :mv-grid)

(export '(grid-position position-nearest positions
	  remove-row remove-row-if
	  remove-column remove-column-if
	  grid-substitute
	  reverse-vector))


(defun test-grid-integer
    (type dimensions &optional (index-fill-function 'grid::index-fill-decadal))
  "Make a grid of the specified type and dimensions,
where the contents of type signed-byte 8 are computed by the
index-fill-function."
  (map-grid :source index-fill-function
	    :source-dims dimensions
	    :destination-specification
	    (grid::make-specification type dimensions '(signed-byte 8))))


(defun closest-element ()
  (error "Obsolete function.  Use `position-nearest"))


(define-test grid-position
  (let ((vector #+clisp #(0 1 2 3 4 5)
		#+sbcl #7m(0 1 2 3 4 5)))
    (assert-equal 3
     (grid-position 3 vector))))

(defgeneric grid-position (item grid &key key test)
  (:documentation "Return item index in grid.  Also return the found value

Default key is identity and default test is equal")
  (:method (item (vector #+sbcl grid::mvector
			 #+clisp vector)
	    &key (key #'identity) (test #'equal))
    "Return vector index that matches item.  Also return item value"
    (iter:iter (iter:for v :vector-element vector)
	       (iter:for i :vector-element-index vector)
	       (when (funcall test item (funcall key v))
		 (return-from grid-position (values i v)))))
  (:method (item (matrix #+sbcl matrix
			 #+clisp array)
	    &key (key #'identity) (test #'equal))
    "Return matrix indices (cons row col) of match"
    (declare (ignore matrix item key test))
    (error "(`grid-position' is not implemented on matrices")))


(define-test grid-positions
  (let ((vector #+clisp #(0 1 2 3 4 5)
		#+sbcl #7m(0 1 2 3 4 5)))
    (assert-equal '(3 4 5)
     (grid-positions 3 vector :test #'<=))))

(defgeneric grid-positions (item grid &key key test)
  (:documentation "Return item indices in grid.  Also return the found values

Default key is identity and default test is equal")
  (:method (item (vector #+sbcl grid::mvector
			 #+clisp vector)
	    &key (key #'identity) (test #'equal))
    "Return vector index that matches item.  Also return item value"
    (iter:iter (iter:for v :vector-element vector)
	       (iter:for i :vector-element-index vector)
	       (when (funcall test item (funcall key v))
		 (iter:collect v into vs)
		 (iter:collect i into is))
	       (iter:finally (return (values is vs)))))
  (:method (item (matrix #+sbcl matrix
			 #+clisp array)
	    &key (key #'identity) (test #'equal))
    "Return matrix indices (cons row col) of match"
    (declare (ignore matrix item key test))
    (error "(`grid-position' is not implemented on matrices")))


(define-test grid-position-if
  (let ((vector #+clisp #(0 1 2 3 4 5)
		#+sbcl #7m(0 1 2 3 4 5)))
    (assert-equal 3
     (grid-position-if #'(lambda (arg)
			   (= arg 3))
		       vector))))


(defgeneric grid-position-if (predicate grid &key key)
  (:documentation "Return index for item that satisfies predicate.
  Also return the found value

Default key is identity.")
  (:method (predicate (vector #+sbcl grid::mvector
			      #+clisp vector)
	    &key (key #'identity))
    "Return vector index that matches item.  Also return item value"
    (iter:iter (iter:for v :vector-element vector)
	       (iter:for i :vector-element-index vector)
	       (when (funcall predicate (funcall key v))
		 (return-from grid-position-if (values i v)))))
  (:method (predicate (matrix #+sbcl matrix
			      #+clisp array)
	    &key (key #'identity))
    "Return matrix indices (cons row col) of match"
    (declare (ignore predicate matrix key))
    (error "(`grid-position-if' is not implemented on matrices")))




(defun position-nearest (item vector &optional (distance #'(lambda (arg item)
							    (abs (- arg item)))))
  "Return the index, distance & value of the `vector' element with the
smallest `distance' from `item'.

`distance' is a designator for a function of two arguments that the
distance between them"
  (let* ((i-closest 0)
	 (closest-value (gref vector i-closest))
	 (min-distance (funcall distance closest-value item)))
    (iter:iter (iter:for V :vector-element vector)
	       (iter:for I from 0)
	       (let ((distance (funcall distance V item)))
		 (when (< distance min-distance)
		   (setf i-closest i
			 min-distance distance
			 closest-value V))))
    (values i-closest min-distance closest-value)))
  

(defun matching-indices ()
  (error "function `matching-indices' is retired.  Use `positions'"))
(defgeneric positions (grid predicate)
  (:documentation "Return `grid's indices that satisfy the predicate.
  Supports grid's vectors and matrices, and cl's simple-vectors of
  rank q")
  #+sbcl(:method ((vector vector-double-float) predicate)
    (iter:iter (iter:for v :vector-element vector)
	       (iter:for i upfrom 0)
	       (when (funcall predicate v)
		 (iter:collect i))))
  (:method ((vector #+sbcl simple-vector #+clisp vector) predicate)
    (iter:iter (iter:for v :vector-element vector)
	       (iter:for i upfrom 0)
	       (when (funcall predicate v)
		 (iter:collect i))))
  #+sbcl(:method ((matrix matrix-double-float) predicate)
    (iter:iter (iter:for m :matrix-element matrix)
	       (iter:for i upfrom 0)
	       (when (funcall predicate m)
		 (iter:collect i)))))

(defun match-vec-element (vector predicate)
  (declare (ignore vector predicate))
  (error "Obsoletet function - Use `position-element'"))


(define-test position-element
  (let ((vector (grid::test-grid-double-float
		 #+clisp 'array
		 #+sbcl 'foreign-array
		 '(5))))
    (assert-equal 3
		  (position-element #+sbcl 3d0 #+clisp 3 vector)))
  (let ((matrix (grid::test-grid-double-float
		 #+clisp 'array
		 #+sbcl 'foreign-array
		 '(5 6))))
    (assert-equal '(2 3)
		  (position-element #+sbcl 23d0 #+clisp 23 matrix))))

(defgeneric position-element (item grid &key key test)
  (:documentation "Return the index (or indices) of the first matching
grid element.  Also returns the value for which the `test' returns t.
Else return nil")
  #+sbcl
  (:method (item (vector mvector)
	    &key (key #'identity) (test #'equal))
    (iter:iter (iter:for v :vector-element vector)
	       (iter:for i :vector-element-index vector)
	       (when (funcall test item (funcall key v))
		 (return-from position-element (values i v))))
    nil)
  (:method (item (vector vector)
	    &key (key #'identity) (test #'equal))
    (iter:iter (iter:for v :vector-element vector)
	       (iter:for i :vector-element-index vector)
	       (when (funcall test item (funcall key v))
		 (return-from position-element (values i v))))
    nil)
  (:method (item (matrix #+clisp array
			 #+sbcl matrix)
	    &key (key #'identity) (test #'equal))
    "Return a list (row column) or nil"
    (iter:iter
      (iter:for C :matrix-column matrix)
      (iter:for iC :matrix-column-index matrix)
      (iter:iter
	(iter:for E :vector-element C)
	(iter:for iR :vector-element-index C)
	(when (funcall test item (funcall key E))
	  (return-from position-element (list iR iC)))))
    nil))


(define-test grid-substitute
  (let ((matrix (test-grid-integer
		 #+clisp 'array
		 #+sbcl 'foreign-array
		 '(5 6))))
    (assert-grid-equal
     #+clisp #2A((0 1 2 3 4 5)
		 (-10 11 12 13 14 15)
		 (20 21 22 23 24 25)
		 (30 31 32 33 34 35)
		 (40 41 42 43 44 45))
     #+sbcl #7m((0 1 2 3 4 5)
		(-10 11 12 13 14 15)
		(20 21 22 23 24 25)
		(30 31 32 33 34 35)
		(40 41 42 43 44 45))
     (grid-substitute -10 10 matrix))))


     

(defun grid-substitute (new old grid &key (test #'=) (key #'identity))
  "Return copy of sequence in which each element that
  satisfies the `test' is replaced by `new'"
    (map-grid :source grid
	    :element-function #'(lambda (element)
				  (if (funcall test
					       (funcall key element)
					       old)
				      new
				      element))))


(define-test remove-row
  (let ((matrix (test-grid-integer
		 #+clisp 'array
		 #+sbcl 'foreign-array
		 '(5 6))))
    (assert-grid-equal
     #+clisp #2A((10 11 12 13 14 15)
		 (20 21 22 23 24 25)
		 (30 31 32 33 34 35)
		 (40 41 42 43 44 45))
     #+sbcl #7m((10 11 12 13 14 15)
	       (20 21 22 23 24 25)
	       (30 31 32 33 34 35)
	       (40 41 42 43 44 45))
     (remove-row 0 matrix))
    (assert-grid-equal
     #+clisp #2A((0 1 2 3 4 5)
		 (10 11 12 13 14 15)
		 (20 21 22 23 24 25)
		 (30 31 32 33 34 35))
     #+sbcl #7m((0 1 2 3 4 5)
	       (10 11 12 13 14 15)
	       (20 21 22 23 24 25)
	       (30 31 32 33 34 35))
     (remove-row 4 matrix))
    (assert-grid-equal
     #+clisp #2A((0 1 2 3 4 5)
		 (10 11 12 13 14 15)
		 (30 31 32 33 34 35)
		 (40 41 42 43 44 45))
     #+sbcl #7m((0 1 2 3 4 5)
	       (10 11 12 13 14 15)
	       (30 31 32 33 34 35)
	       (40 41 42 43 44 45))
     (remove-row 2 matrix))))

(defun remove-row (row matrix)
  (destructuring-bind (rows cols) (dimensions matrix)
    (assert (< row rows)
	    (row)
	    "Row ~a is greater than number of rows ~a" row rows)
    (cond
      ((= row 0)
       (subgrid matrix (list (1- rows) cols) '(1 0)))
      ((= row (1- rows))
       (subgrid matrix (list (1- rows) cols) '(0 0)))
      (t
       (concatenate-grids
	(subgrid matrix (list row cols) '(0 0))
	(subgrid matrix (list (- rows row 1)
			      cols)
		 (list (1+ row) 0))
	:axis 0)))))


(define-test remove-col
  (let ((matrix (test-grid-integer #+clisp 'array #+sbcl 'foreign-array '(5 6))))
    (assert-grid-equal
     #+clisp #2A((1 2 3 4 5)
		 (11 12 13 14 15)
		 (21 22 23 24 25)
		 (31 32 33 34 35)
		 (41 42 43 44 45))
     #+sbcl #7m((1 2 3 4 5)
		(11 12 13 14 15)
		(21 22 23 24 25)
		(31 32 33 34 35)
		(41 42 43 44 45))
     (remove-col 0 matrix))
    (assert-grid-equal
     #+clisp #2A((0 1 2 3 4)
		 (10 11 12 13 14)
		 (20 21 22 23 24)
		 (30 31 32 33 34)
		 (40 41 42 43 44))
     #+sbcl #7m((0 1 2 3 4)
		(10 11 12 13 14)
		(20 21 22 23 24)
		(30 31 32 33 34)
		(40 41 42 43 44))
     (remove-col 5 matrix))
    (assert-grid-equal
     #+clisp #2A((0 1 3 4 5)
		 (10 11 13 14 15)
		 (20 21 23 24 25)
		 (30 31 33 34 35)
		 (40 41 43 44 45))
     #+sbcl #7m((0 1 3 4 5)
		(10 11 13 14 15)
		(20 21 23 24 25)
		(30 31 33 34 35)
		(40 41 43 44 45))
     (remove-col 2 matrix))))

(defun remove-col (col matrix)
  (destructuring-bind (rows cols) (dimensions matrix)
    (assert (< col cols)
	    (col)
	    "Col ~a is greater than number of cols ~a" col cols)
    (cond
      ((= col 0)
       (subgrid matrix (list rows (1- cols)) '(0 1)))
      ((= col (1- cols))
       (subgrid matrix (list rows (1- cols)) '(0 0)))
      (t
       (concatenate-grids
	(subgrid matrix (list rows col) '(0 0))
	(subgrid matrix (list rows
			      (- cols col 1))
		 (list 0 (1+ col)))
	:axis 1)))))

(define-test remove-row-if
  (let ((matrix (test-grid-integer
		 #+clisp 'array
		 #+sbcl 'foreign-array
		 '(5 6))))
    (assert-grid-equal
     #+clisp #2A((0 1 2 3 4 5)
		 (10 11 12 13 14 15)
		 (30 31 32 33 34 35)
		 (40 41 42 43 44 45))
     #+sbcl #7m((0 1 2 3 4 5)
		(10 11 12 13 14 15)
		(30 31 32 33 34 35)
		(40 41 42 43 44 45))
     (remove-row-if #'(lambda (row)
			(position-element 23 row))
		    matrix))))

(defun remove-row-if (test matrix)
  "Apply test to each row in turn.  Remove first row that satisfies
the test

This is a non-destructive operation.  If no match is found, a new grid
is returned"
  (let ((row
	 (iter:iter
	   (iter:for row :matrix-row matrix)
	   (iter:for index :matrix-row-index matrix)
	   (when (funcall test row)
	     (return index)))))
    (if row
	(remove-row row matrix)
	(map-grid :source matrix
		  :element-function #'identity))))


(define-test remove-col-if
  (let ((matrix (test-grid-integer
		 #+clisp 'array
		 #+sbcl 'foreign-array
		 '(5 6))))
    (assert-grid-equal
     #+clisp #2A(( 0  1  2  4  5)
	 (10 11 12 14 15)
	 (20 21 22 24 25)
	 (30 31 32 34 35)
	 (40 41 42 44 45))
     #+sbcl #7m(( 0  1  2  4  5)
	       (10 11 12 14 15)
	       (20 21 22 24 25)
	       (30 31 32 34 35)
	       (40 41 42 44 45))
     (remove-col-if #'(lambda (col)
			(position-element 23 col))
		    matrix))))

(defun remove-col-if (test matrix)
  "Apply test to each col in turn.  Remove first col that satisfies
the test

This is a non-destructive operation.  If no match is found, a matrix
copy is returned"
  (let ((col
	 (iter:iter
	   (iter:for col :matrix-column matrix)
	   (iter:for index :matrix-column-index matrix)
	   (when (funcall test col)
	     (return index)))))
    (if col
	(remove-col col matrix)
	(map-grid :source matrix
		  :element-function #'identity))))

;;; find

(define-test find-row
  (let ((matrix (test-grid-integer
		 #+clisp 'array
		 #+sbcl 'foreign-array
		 '(5 6))))
    (assert-grid-equal
     #+clisp #(20 21 22 23 24 25)
     #+sbcl #7m(20 21 22 23 24 25)
     (find-row 22 matrix))))


(defun find-row (item matrix &key (key #'identity) (test #'equal))
  (iter:iter
    (iter:for R :matrix-row matrix)
    (iter:for iR :matrix-row-index matrix)
    (when (grid-position item R :key key :test test)
      (return-from find-row (values R iR))))
  nil)


(define-test find-col
  (let ((matrix (test-grid-integer
		 #+clisp 'array
		 #+sbcl 'foreign-array
		 '(5 6))))
    (assert-grid-equal
     #+clisp #(2 12 22 32 42)
     #+sbcl #7m(2 12 22 32 42)
     (find-col 22 matrix))))

(defun find-col (item matrix &key (key #'identity) (test #'equal))
  (iter:iter
    (iter:for C :matrix-column matrix)
    (iter:for iC :matrix-column-index matrix)
    (when (grid-position item C :key key :test test)
      (return-from find-col (values C iC))))
  nil)

(define-test find-row-if
  (let ((matrix (test-grid-integer
		 #+clisp 'array
		 #+sbcl 'foreign-array
		 '(5 6))))
    (assert-grid-equal
     #(20 21 22 23 24 25)
     (find-row-if #'(lambda (arg)
		      (equal arg 22))
		  matrix))))


(defun find-row-if (predicate matrix &key (key #'identity))
  (iter:iter
    (iter:for R :matrix-row matrix)
    (iter:for iR :matrix-row-index matrix)
    (when (grid-position-if predicate R :key key)
      (return-from find-row-if (values R iR))))
  nil)


(define-test find-col-if
  (let ((matrix (test-grid-integer
		 #+clisp 'array
		 #+sbcl 'foreign-array
		 '(5 6))))
    (assert-grid-equal
     #(2 12 22 32 42)
     (find-col-if #'(lambda (arg)
		      (equal arg 22))
		  matrix))))


(defun find-col-if (predicate matrix &key (key #'identity))
  (iter:iter
    (iter:for C :matrix-column matrix)
    (iter:for iC :matrix-column-index matrix)
    (when (grid-position-if predicate C :key key)
      (return-from find-col-if (values C iC))))
  nil)


(defun reverse-vector (vector)
  "Create a new vector, reversing the order of elements"
  (assert (null (dim1 vector)) ()
	  "Argument must be a vector")
  (let* ((len (dim0 vector))
	 (affi (affi:make-affi (list len) (- len 1))))
    (setf (slot-value affi 'affi::coeff) #(-1))
    (map-grid :source vector
	      :destination-affi affi)))

(define-test reverse-vector
  (assert-grid-equal
   #(4 3 2 1 0)
   (reverse-vector (intgen 5))))