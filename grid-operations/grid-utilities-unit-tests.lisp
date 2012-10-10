;; Mirko Vukovic
;; Time-stamp: <2012-10-10 17:20:51EDT grid-utilities-unit-tests.lisp>
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
  (grid::test-grid-double-float *default-grid-type* '(3 4)))

(defparameter *vector-4-double-float*
  (grid::test-grid-double-float *default-grid-type* '(4)))

(defparameter *0-1-2* (grid::make-grid `((,*default-grid-type*) ,*integer-type*)
			     :initial-contents '(0 1 2)))
(defparameter *0-2-4* (grid::make-grid `((,*default-grid-type*) ,*integer-type*)
				      :initial-contents '(0 2 4)))

(defmacro assert-grid-equal (grid1 grid2)
  `(assert-numerical-equal (grid:copy-to ,grid1)
			   (grid:copy-to ,grid2)))

#+prune? (eval-when (:compile-toplevel :load-toplevel :execute)
  (setf mv-grid:*default-grid-type* *default-grid-type*))



(define-test read-grid
  (with-open-file (stream
		   #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-gsll/mv-grid/2d-grid-data.txt"
		   (merge-pathnames
		    "grid-operations/2d-grid-data.txt"
		    (asdf:system-source-directory "mv-grid-utils"))
		   :direction :input)
    (assert-grid-equal 
     (grid::make-grid `((,*default-grid-type*) ,*default-element-type*)
		      :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0)))
     (read-grid '(2 3) stream t ))))

(define-test read-csv-grid
  (with-open-file (stream
		   #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-gsll/mv-grid/2d-grid-data.txt"
		   #-cysshd1 "/home/my-software-add-ons/my-lisp/mv-gsll/mv-grid/2d-grid-data.csv"
		   :direction :input) 
    (assert-grid-equal 
     (grid::make-grid `((,*default-grid-type*) ,*default-element-type*)
		      :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0)))
     (read-grid '(2 3) 'csv stream)))
  (with-open-file (stream
		   #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-gsll/mv-grid/2d-grid-data.txt"
		   #-cysshd1 "/home/my-software-add-ons/my-lisp/mv-gsll/mv-grid/2d-grid-data.csv"
		   :direction :input) 
    (assert-grid-equal 
     (grid::make-grid `((,*default-grid-type*) ,*default-element-type*)
		      :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0)))
     (read-grid '(nil 3) 'csv stream)))
  (with-open-file (stream
		   #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-gsll/mv-grid/2d-grid-data.txt"
		   #-cysshd1 "/home/my-software-add-ons/my-lisp/mv-gsll/mv-grid/2d-grid-data.csv"
		   :direction :input) 
    (assert-grid-equal 
     (grid::make-grid `((,*default-grid-type*) ,*default-element-type*)
		      :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0)))
     (read-grid '(2 nil) 'csv stream)))
  (with-open-file (stream
		   #+cysshd1 "/home/mv/my-software-add-ons/my-lisp/mv-gsll/mv-grid/2d-grid-data.txt"
		   #-cysshd1 "/home/my-software-add-ons/my-lisp/mv-gsll/mv-grid/2d-grid-data.csv"
		   :direction :input) 
    (assert-grid-equal 
     (grid::make-grid `((,*default-grid-type*) ,*default-element-type*)
		      :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0)))
     (read-grid '(nil nil) 'csv stream))))



(defparameter *array-3-4-double-float*
  (grid::test-grid-double-float *default-grid-type* '(3 4)))



(define-test reduce-rows/cols
  (assert-grid-equal 
   (grid::make-grid `((,*default-grid-type*) ,*default-element-type*)
		    :initial-contents '(30d0 33d0 36d0 39d0))
   (reduce-columns *array-3-4-double-float*))
  (assert-grid-equal 
   (grid::make-grid `((,*default-grid-type*) ,*default-element-type*)
		    :initial-contents '((6d0 46d0 86d0)))
   (reduce-rows *array-3-4-double-float*)))


