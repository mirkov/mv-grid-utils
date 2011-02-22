;; Mirko Vukovic
;; Time-stamp: <2011-02-12 08:28:24 grid-mappers.lisp>
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

(export '(gmap gsmap reduce-rows reduce-columns))

(defgeneric gmapc (function grid &rest grids)
  (:documentation
   "Apply function to successive sets of arguments in which one argument is obtained from each vector.
  Return the first grid, unmodified.

This map is used for the function's side effects.  It is modeled after
mapc")
  (:method ((function function) (vector #+clisp vector #+sbcl mvector)
	    &rest vectors)
    (warn "untested")
    (map-n-grids :sources (cons vector vectors)
		 :combination-function #'(lambda (&rest args)
					   (declare (ignore args))
					   nil)
		 :destination-specification `((array ,@(dimensions (vector)))
					      t))
    vector))

(define-test gmap
    (assert-grid-equal
     *0-1-2* (gmap #'identity *0-1-2*))
    (assert-grid-equal
     *0-2-4*
     (gsmap #'+ *0-1-2* *0-1-2*)))


(defun gmap (function grid)
  "Element-wise Map `function' over `grids' *array-type* and
*float-type* determine the result type

gmap specializes map-grid to use only the :element-function
keyword"
  (map-grid
   :source grid
   :element-function function))
;;   :destination-specification `((,*array-type* ,@(dimensions grid))
;;						 ,*float-type*))


(defun gsmap (function &rest grids)
  "Element-wise Map `function' over `grids' *array-type* and
*float-type* determine the result type

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




(define-test reduce-rows/cols
  (assert-grid-equal 
   (grid::make-grid `((,*array-type*) ,*float-type*)
		    :initial-contents '(30d0 33d0 36d0 39d0))
   (reduce-columns *array-3-4-double-float*))
  (assert-grid-equal 
   (grid::make-grid `((,*array-type*) ,*float-type*)
		    :initial-contents '((6d0 46d0 86d0)))
   (reduce-rows *array-3-4-double-float*)))


(defun reduce-rows (matrix &optional (func #'+))
  "Return a column vector with each element a result reducing that row
using `func' (default #'+)"
  (make-grid `((,*array-type* 1 ,(first (dimensions matrix)))
		    double-float)
	     :initial-contents
	     ;; In order to create a column array, I have to return a
	     ;; nested list.  This could be a grid bug or a feature.
	     (list
	      (iter:iter
		(iter:for R :matrix-row matrix)
		(iter:collect
		    (iter:iter
		      (iter:for E :vector-element R)
		      (iter:reducing E by func initial-value 0d0)))))))


(defun reduce-columns (matrix &optional (func #'+))
  "Return a row vector with each element a result of reducing the
column using `func' (default #'+)"
  (make-grid `((,*array-type* ,(second (dimensions matrix)))
		    double-float)
	     :initial-contents
	     (iter:iter
	       (iter:for C :matrix-column matrix)
	       (iter:collect
		   (iter:iter
		     (iter:for E :vector-element C)
		     (iter:reducing E by func initial-value 0d0))))))