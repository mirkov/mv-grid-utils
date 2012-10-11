;; Mirko Vukovic
;; Time-stamp: <2012-10-11 10:27:30EDT grid-manipulations.lisp>
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

(export '(reform grid-coerce grid-bind))

(defun reform (vector rows columns)
  "Remap `vector' into a `rows'X`columns' matrix"
  (map-grid :source vector
		 :destination-specification
		 `((,*default-grid-type* ,rows ,columns)
		   ,*default-element-type*)))

(defun grid-coerce (grid type)
  "Return new grid, with all elements coerced to the new type

The array type is controlled by *default-grid-type*"
  (map-grid :source grid
	    :destination-specification
	    `((,*default-grid-type* ,@(dimensions grid))
	      ,type)))

(define-test grid-bind
  "Load a 2x2 grid with 2-element lists.  The grid is
#2A(((0 0) (0 2) (0 4)) ((1 0) (1 2) (1 4)))

GRID-BIND will return a list of two 2x2 grids.  The first will contain
the first list element #2A((0 0 0) (1 1 1)) and the second the second
list element #2A((0 2 4) (0 2 4))
"
  (let ((*default-element-type* 'integer))
    (let ((grid (map-grid :destination-specification
		(grid::make-specification 'array '(2 3) 'list)
		:source #'(lambda (i j)
			    (list i (* 2 j))))))
      (grid-bind (a b) grid
	(assert-grid-equal #2A((0 0 0) (1 1 1)) a)
	(assert-grid-equal #2A((0 2 4) (0 2 4)) b)))))

(defmacro grid-bind ((&rest vars) grid &body body)
  "Execute BODY inside a binding for grids whose each element is a list.

Bind each var in vars to its corresponding element in the grid element list.

Currently, the grid type (array or foreign-array) and the element type
are determined by *default-grid-type* and *default-element-type

See the macro documentation for implementation details"
  (alexandria:with-gensyms (dimensions affi walker tester specs)
    (alexandria:once-only (grid)
      `(let* (;; initialize new variables
	      ,@vars
	      ;; first extract grid dimensionality
	      (,dimensions (dimensions ,grid))
	      (,affi (grid::affi ,grid))
	      (,specs `((,*default-grid-type* ,@,dimensions)
			,*default-element-type*)))
	 ;; set new variables to grids
	 ,@(loop for var in vars
	      collect `(setf ,var (make-grid ,specs)))
	 ;; we use affi's walker.  It will walk through the grid and
	 ;; generate an addressing index
	 (multiple-value-bind (,walker ,tester) (affi:make-walker ,affi)
	   (declare (ignore ,tester))
	   ;; Walk through the grid
	   (do* ((i (funcall ,walker) (funcall ,walker)))
		((not i))
	     ;; load values into new variables
	     (when i
	       (let ((list (aref* ,grid i)))
		 ,@(loop for var in vars
		      collect `(setf (aref* ,var i) (pop list)))))))
	 ,@body))))