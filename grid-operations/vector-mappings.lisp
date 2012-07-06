;; Mirko Vukovic
;; Time-stamp: <2012-07-05 22:56:54 vector-mappings.lisp>
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

(export '(gmapc gmap gsmap reduce-rows reduce-columns reduce-vector
	  map-rows))


(defun gmap (function grid)
  "Element-wise map FUNCTION over GRID, returning a grid

*array-type* and float-type* determine the result type

gmap specializes map-grid to use only the :element-function
keyword"
  (map-grid
   :source grid
   :element-function function))

(defgeneric gmapc (function grid &rest more-grids)
  (:documentation
   "Apply function to successive sets of arguments in which one
argument is obtained from each vector.  Return the first grid,
unmodified.

This map is used for the function's side effects.  It is modeled after
mapc")
  (:method ((function function) (vector #+clisp vector #+sbcl mvector)
	    &rest more-vectors)
    (let* ((vectors (cons vector more-vectors))
	   (affis (mapcar #'grid::affi vectors))
	   (index 2))
      (dolist (this-affi (rest affis))
	(unless (affi:check-conformability (first affis) this-affi)
	  (error "~s~:*~[nil~;st~;nd~;rd~:;th~] vectors's affi conflicts with the first vector's affi" index)))
      (map-n-grids :sources (mapcar #'list vectors affis)
		   :combination-function #'(lambda (&rest args)
					     (apply function args)
					     nil)
		   :destination-specification `((array ,@(dimensions vector))
						t))
      vector)))

(define-test gmapc
    (assert-numerical-equal
     (list 0 2 4) (let ((result (list)))
		    (gmapc #'(lambda (arg1 arg2)
			       (push (+ arg1 arg2) result))
			   *0-1-2* *0-1-2*)
		    (nreverse result)))
    (assert-grid-equal
     *0-2-4*
     (gsmap #'+ *0-1-2* *0-1-2*)))



(defun gsmap (function &rest grids)
  "Element-wise Map `function' over `grids'

*array-type* and float-type* determine the result type

gsmap specializes map-n-grids to use only
the :combination-function keyword"
  (let ((affis (mapcar #'grid::affi grids))
	(index 2))
    (dolist (this-affi (rest affis))
      (unless (affi:check-conformability (first affis) this-affi)
	(error "~s~:*~[nil~;st~;nd~;rd~:;th~] grids's affi conflicts with the first grid's affi" index)))
    (map-n-grids
     :sources  (mapcar #'list grids affis)
     :combination-function #'(lambda (&rest args)
			       (apply function args))
     :destination-specification `((,*array-type* ,@(dimensions (first grids)))
						 ,*float-type*))))




