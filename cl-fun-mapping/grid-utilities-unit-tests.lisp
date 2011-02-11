;; Mirko Vukovic
;; Time-stamp: <2011-02-10 06:41:07 mv-gpl-header.txt>
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


(defparameter *array-3-4-double-float*
  (grid::test-grid-double-float *array-type* '(3 4)))

(defparameter *vector-4-double-float*
  (grid::test-grid-double-float *array-type* '(4)))

(defparameter *0-1-2* (grid::make-grid `((,*array-type*) ,*integer-type*)
			     :initial-contents '(0 1 2)))
(defparameter *0-2-4* (grid::make-grid `((,*array-type*) ,*integer-type*)
				      :initial-contents '(0 2 4)))

(defmacro assert-grid-equal (grid1 grid2)
  `(assert-numerical-equal (grid:copy-to ,grid1)
			   (grid:copy-to ,grid2)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf mv-grid:*array-type* *array-type*))

(define-test indgen
  (assert-grid-equal (grid::make-grid `((,*array-type*) ,*integer-type*)
				      :initial-contents '(0 1 2))
		     (indgen 3)))


(define-test findgen
  (assert-grid-equal
   (grid::make-grid `((,*array-type*) ,*float-type*)
		    :initial-contents '(0d0 1d0 2d0))
		     (findgen 3)))


(define-test gmap
    (assert-grid-equal
     *0-1-2* (gmap #'identity *0-1-2*))
    (assert-grid-equal
     *0-2-4*
     (gsmap #'+ *0-1-2* *0-1-2*)))

(define-test match-vec-element
  (let ((vec (findgen 12)))
    (assert-true
     (multiple-value-bind (index value)
	 (match-vec-element vec #'(lambda (arg)
				    (>= arg 5)))
       (and (assert-number-equal 5d0 value)
	    (assert-number-equal 5 index))))
    (assert-true
     (multiple-value-bind (index value)
	 (match-vec-element vec #'(lambda (arg)
				    (>= arg -2)))
       (and (assert-number-equal 0d0 value)
	    (assert-number-equal 0 index))))
    (assert-true
     (not (match-vec-element vec #'(lambda (arg)
				     (>= arg 15)))))))

(define-test matching-indices
  (assert-equal '(1)
		(matching-indices *0-1-2* #'(lambda (arg)
					      (equal arg 1)))))
(define-test matrify
  ;; convert vec-4 into 2x2 matrix
  (assert-grid-equal
   (grid::make-grid `((,*array-type*) ,*float-type*)
		    :initial-contents '((0d0 1d0) (2d0 3d0)))
   (matrify *vector-4-double-float* 2 2))
  ;; convert vec-4 into column vector of length 4
  (assert-grid-equal 
   (grid::make-grid `((,*array-type*) ,*float-type*)
		    :initial-contents '((0d0 1d0 2d0 3d0)))
   (matrify *vector-4-double-float* 1 4))
  ;; convert vec 4 into a row vector
  (assert-grid-equal
   (grid::make-grid `((,*array-type*) ,*float-type*)
		    :initial-contents '((0d0) (1d0) (2d0) (3d0)))
   (matrify *vector-4-double-float* 4 1)))


(define-test read-grid
  (with-open-file (stream
		   #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-gsll/mv-grid/2d-grid-data.txt"
		   #-cysshd1 "/home/my-software-add-ons/my-lisp/mv-gsll/mv-grid/2d-grid-data.txt"
		   :direction :input) 
    (assert-grid-equal 
     (grid::make-grid `((,*array-type*) ,*float-type*)
		      :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0)))
     (read-grid '(2 3) 't stream))))

(define-test read-csv-grid
  (with-open-file (stream
		   #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-gsll/mv-grid/2d-grid-data.txt"
		   #-cysshd1 "/home/my-software-add-ons/my-lisp/mv-gsll/mv-grid/2d-grid-data.csv"
		   :direction :input) 
    (assert-grid-equal 
     (grid::make-grid `((,*array-type*) ,*float-type*)
		      :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0)))
     (read-grid '(2 3) 'csv stream)))
  (with-open-file (stream
		   #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-gsll/mv-grid/2d-grid-data.txt"
		   #-cysshd1 "/home/my-software-add-ons/my-lisp/mv-gsll/mv-grid/2d-grid-data.csv"
		   :direction :input) 
    (assert-grid-equal 
     (grid::make-grid `((,*array-type*) ,*float-type*)
		      :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0)))
     (read-grid '(nil 3) 'csv stream)))
  (with-open-file (stream
		   #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-gsll/mv-grid/2d-grid-data.txt"
		   #-cysshd1 "/home/my-software-add-ons/my-lisp/mv-gsll/mv-grid/2d-grid-data.csv"
		   :direction :input) 
    (assert-grid-equal 
     (grid::make-grid `((,*array-type*) ,*float-type*)
		      :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0)))
     (read-grid '(2 nil) 'csv stream)))
  (with-open-file (stream
		   #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-gsll/mv-grid/2d-grid-data.txt"
		   #-cysshd1 "/home/my-software-add-ons/my-lisp/mv-gsll/mv-grid/2d-grid-data.csv"
		   :direction :input) 
    (assert-grid-equal 
     (grid::make-grid `((,*array-type*) ,*float-type*)
		      :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0)))
     (read-grid '(nil nil) 'csv stream))))



(defparameter *array-3-4-double-float*
  (grid::test-grid-double-float *array-type* '(3 4)))



(define-test reduce-rows/cols
  (assert-grid-equal 
   (grid::make-grid `((,*array-type*) ,*float-type*)
		    :initial-contents '(30d0 33d0 36d0 39d0))
   (reduce-columns *array-3-4-double-float*))
  (assert-grid-equal 
   (grid::make-grid `((,*array-type*) ,*float-type*)
		    :initial-contents '((6d0 46d0 86d0)))
   (reduce-rows *array-3-4-double-float*)))


;;;; Local variables: 
;;;; change-log-default-name: "~/my-software-add-ons/my-lisp/mv-gsll/ChangeLog"
;;;; End: