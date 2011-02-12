;; Mirko Vukovic
;; Time-stamp: <2011-02-12 08:26:12 make-grid-sequence.lisp>
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

;; functions for creating grids filled with specific patterns
(in-package :mv-grid)

(export '(intgen indgen natgen findgen cindgen))

(defparameter *array-3-4-double-float*
  (grid::test-grid-double-float *array-type* '(3 4)))

(defparameter *vector-4-double-float*
  (grid::test-grid-double-float *array-type* '(4)))

(defparameter *0-1-2* (grid::make-grid `((,*array-type*) ,*integer-type*)
			     :initial-contents '(0 1 2)))
(defparameter *0-2-4* (grid::make-grid `((,*array-type*) ,*integer-type*)
				      :initial-contents '(0 2 4)))



(defun intgen (count &optional (len 32))
  "Return vector of length `count', type signed-byte of `len' bytes
where the value of each element is its index.

Allowed values of `len' are 8, 16, 32, 64"
  (map-grid :source #'identity
		 :source-dims `(,count)
		 :destination-specification `((,*array-type* ,count)
					      (signed-byte ,len))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf mv-grid:*array-type* *array-type*))

(define-test indgen
  (assert-grid-equal (grid::make-grid `((,*array-type*) ,*integer-type*)
				      :initial-contents '(0 1 2))
		     (indgen 3)))



(defun indgen (count &optional (len 16))
  "Return vector of length `count', type signed-byte of `len' bytes
where the value of each element is its index.

Allowed values of `len' are 8, 16, 32, 64"
  (map-grid :source #'identity
		 :source-dims `(,count)
		 :destination-specification `((,*array-type* ,count) (unsigned-byte ,len))))


(defun natgen (count &optional (len 16))
  "Return vector of length `count' of natural numbers, starting at 1.

Allowed values of `len' are 8, 16, 32, 64"
  (map-grid :source #'(lambda (arg)
			     (1+ arg))
		 :source-dims `(,count)
		 :destination-specification `((,*array-type* ,count) (unsigned-byte ,len))))


(define-test findgen
  (assert-grid-equal
   (grid::make-grid `((,*array-type*) ,*float-type*)
		    :initial-contents '(0d0 1d0 2d0))
		     (findgen 3)))



(defun findgen (count &optional (type 'double-float))
  "Return floating vector of length `count', where the value of each
element is its index.

The floating type is either `single' or `double' (default), determined by `type'"
  (map-grid :source #'(lambda (i)
			     (coerce i 'float))
		 :source-dims `(,count)
		 :destination-specification `((,*array-type* ,count) ,type)))

(defun cindgen (count &optional (type 'double-float))
  "Return complex vector of length `count', where the real part of  each
element is its index.

The floating type is either `single' or `double' (default), determined by `type'"
  (map-grid :source #'(lambda (i)
			     (coerce i 'float))
		 :source-dims `(,count)
		 :destination-specification `((,*array-type* ,count) (complex ,type))))

(macrolet ((seq (fun)
	     `(grid::make-grid `((,*array-type*) ,*float-type*)
			       ;; make-foreign-array 'double-float :dimensions count
			       :initial-contents
			       (coerce (,fun (coerce begin 'double-float)
					     (coerce end 'double-float)
					     count) 'list))))
  (defun lseq (begin end &optional (count 51))
    "linear progression between two positive numbers `begin' and `end'
`begin' can be less than `end'"
    (declare (number begin end))
    (seq my-utils:rseq))


  (defun gseq (begin end &optional (count 51))
    "Geometric progression between two positive numbers `begin' and `end'
`begin' can be less than `end'"
    (declare (number begin end))
    (seq my-utils:xpseq)))