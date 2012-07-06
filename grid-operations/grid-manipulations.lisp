;; Mirko Vukovic
;; Time-stamp: <2012-07-05 23:14:51 grid-manipulations.lisp>
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
		 :destination-specification `((,*array-type* ,rows ,columns)
					      ,*float-type*)))

(defun grid-coerce (grid type)
  "Return new grid, with all elements coerced to the new type

The array type is controlled by *array-type*"
  (map-grid :source grid
	    :destination-specification
	    `((,*array-type* ,@(dimensions grid))
	      ,type)))

(define-test grid-bind
  (let ((grid (map-grid :destination-specification
		(grid::make-specification 'array '(2 3) 'list)
		:source #'(lambda (i j)
			    (list i (* 2 j))))))
    (print grid)
    (grid-bind (a b) grid
      (list a b))))

(defmacro grid-bind ((&rest vars) grid &body body)
  "Binding for grids whose each element is a list

Bind each var in vars to its corresponding element in the grid element list

Currently, the grid type (array or foreign-array) and the element type
are determined by *array-type* and *float-type "
  (alexandria:with-gensyms (dimensions affi walker tester specs)
    (alexandria:once-only (grid)
      `(let* (,@vars
	      (,dimensions (dimensions ,grid))
	      (,affi (grid::affi ,grid))
	      (,specs (list (append (list ',*array-type*) ,dimensions)
			    ',*float-type*)))
	 ,@(loop for var in vars
	      collect `(setf ,var (make-grid ,specs)))
	 
	 (multiple-value-bind (,walker ,tester) (affi:make-walker ,affi)
	   (do* ((i (funcall ,walker) (funcall ,walker)))
		((not i))
	     (when i
	       (let ((list (gref* ,grid i)))
		 ,@(loop for var in vars
		      collect `(setf (gref* ,var i) (pop list)))))))
	 ,@body))))