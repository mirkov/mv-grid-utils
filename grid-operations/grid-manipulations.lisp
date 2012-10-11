;; Mirko Vukovic
;; Time-stamp: <2012-10-10 20:49:04 grid-manipulations.lisp>
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
are determined by *default-grid-type* and *default-element-type "
  (alexandria:with-gensyms (dimensions affi walker tester specs)
    (alexandria:once-only (grid)
      `(let* (,@vars
	      (,dimensions (dimensions ,grid))
	      (,affi (grid::affi ,grid))
	      (,specs (list (append (list ',*default-grid-type*) ,dimensions)
			    ',*default-element-type*)))
	 ,@(loop for var in vars
	      collect `(setf ,var (make-grid ,specs)))
	 
	 (multiple-value-bind (,walker ,tester) (affi:make-walker ,affi)
	   (do* ((i (funcall ,walker) (funcall ,walker)))
		((not i))
	     (when i
	       (let ((list (aref* ,grid i)))
		 ,@(loop for var in vars
		      collect `(setf (aref* ,var i) (pop list)))))))
	 ,@body))))