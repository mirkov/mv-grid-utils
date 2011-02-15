;; Mirko Vukovic
;; Time-stamp: <2011-02-10 07:19:18 grid-manipulations.lisp>
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

(export '(matrify grid-coerce))

(defun matrify (vector rows columns)
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