;; Mirko Vukovic
;; Time-stamp: <2012-07-03 06:45:50 grid-row-or-col-mappings.lisp>
;; 
;; Copyright 2012 Mirko Vukovic
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

;;; Routines that loop over rows or columns
(in-package :mv-grid)

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

(define-test reduce-vector
  (assert-equal 138d0
   (reduce-vector
    #'+
    (grid::make-grid `((,*array-type*) ,*float-type*)
		     :initial-contents '(6d0 46d0 86d0))))
  (assert-equal 148d0
   (reduce-vector
    #'+
    (grid::make-grid `((,*array-type*) ,*float-type*)
		     :initial-contents '(6d0 46d0 86d0))
    :initial-value 10d0)))

(defun reduce-vector (function vector &key (initial-value 0d0))
  "Return a vector using `function'.

- INITIAL-VALUE -- is it applied before the sequence, or at the end"
  (iter:iter
    (iter:for C :vector-element vector)
    (iter:reducing C by function :initial-value initial-value)))

(defun map-rows (function matrix)
  "Map FUNCTION over Matrix rows, returning results as a vector "
;;  (make-grid (make-grid `((,*array-type* ,(first (dimensions matrix)))
;;			  double-float)
;;	     :initial-contents
	     (iter:iter
	       (iter:for R :matrix-row matrix)
	       (iter:collect (funcall function R))))

