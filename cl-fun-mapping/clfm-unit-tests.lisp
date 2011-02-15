;; Mirko Vukovic
;; Time-stamp: <2011-02-12 21:40:05 clfm-unit-tests.lisp>
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

;;; Special variables that will be used in the mapper tests.

(defparameter *0+-vector* (findgen 5)
  "Vector of zero on positive double floats")
(defparameter *+-vector* (lseq -2d0 2d0 5)
  "Vector of positive and negative double floats")
(defparameter *-1x-+1x-vector* (lseq 0.1d0 0.9d0 5)
  "Vector of values between +/-1, exclusive")
(defparameter *+1-vector* (lseq 1d0 6d0 5)
  "Vector of values above 1d0")
(defparameter *+2-vector* (lseq 2d0 7d0 5)
  "Vector of values above 1d0")
(defparameter *complex-vector*
  #+sbcl #2m(#c(1d0 2d0) #c(5d0 -3d0))
  #+clisp #2(#c(1d0 2d0) #c(5d0 -3d0)))
(defparameter *natural-vector* (natgen 5)
  "Vector of natural numbers 1, 2, ...")


;;; assert macros used in unit tests

(defmacro assert-grid-equal (grid1 grid2)
  `(assert-numerical-equal (grid:copy-to ,grid1)
                           (grid:copy-to ,grid2)))

(defmacro assert-gmap-equal (map-fun cl-fun arg)
  "Test correctness of a grid map function of one argument
 (grid vector).

Apply the grid-mapping function `map-fun' to `arg' (a grid)

Also map a corresponding `cl-fun' to `arg's contents"
  `(assert-numerical-equal
    (copy-to (,map-fun ,arg))
    (map-grid>cl #',cl-fun ,arg)))


(defun vecp (arg)
  "return t if arg is #+clisp vector #+sbcl mvector"
  #+clisp
  (if (equal (class-name (class-of (value arg)))
			'vector)
      t nil)
  #+sbcl
  (if (equal (find-class 'mvector)
	     (car
	      (sb-mop:class-direct-superclasses
	       (find-class (class-name (class-of arg))))))
      t nil))

(defmacro assert-gmap2-equal (grid-map-fun cl-fun
			      arg1 arg2)
  "Test correctness of a grid map function of two arguments.
Arguments can be scalars or grid vectors

Apply the grid-mapping function `map-fun' to `arg' (a grid)

Also map a corresponding `cl-fun' to `arg's contents"
  (labels ((value (arg) (if (symbolp arg)
			    (symbol-value arg)
			    arg)))
    (let ((vec1p (vecp (value arg1)))
	  (vec2p (vecp (value arg2))))
      (cond
	((and vec1p (not vec2p))
	 `(assert-numerical-equal
	   (copy-to (,grid-map-fun ,arg1 ,arg2))
	   (map-grid>cl #'(lambda (x)
			    (,cl-fun x ,arg2))
			,arg1)
	   'arg1-vec))
	((and (not vec1p) vec2p)
	 `(assert-numerical-equal
	   (copy-to (,grid-map-fun ,arg1 ,arg2))
	   (map-grid>cl #'(lambda (x)
			    (,cl-fun ,arg1 x))
			,arg2)
	   'arg2-vec))
	((and vec1p vec2p)
	 `(assert-numerical-equal
	   (copy-to (,grid-map-fun ,arg1 ,arg2))
	   (map-grid>cl #'(lambda (x y)
			    (,cl-fun x y))
			,arg1 ,arg2)
	   'arg&base-vec))
	(t (error "Neither argument was specified as vector, ~a, ~a" grid-map-fun cl-fun))))))



;;; Functions that map over their arguments.  Both cl and grid
;;; versions.  These functions are used to in the unit tests where the
;;; mapping of cl function will be compared to grid's mappers.


(defun cl-calc (fun arg)
  "Map cl function `fun' on a grid `arg' and return a cl array"
  (map 'vector fun (copy-to arg)))

(defun map-grid>cl (fun arg &optional arg2)
  "Map cl function `fun' on a grid `arg' and return a cl array"
  (if arg2
      (map 'vector fun (copy-to arg) (copy-to arg2))
      (map 'vector fun (copy-to arg))))


;;;; Rest of the file is commented out.  Not sure if it is necessary
;;;; for rank-based method testing.

#|



(defparameter *one-arg-func-test-def*
  `((sin . ,*+-vector*) (cos . ,*+-vector*) (tan . ,*+-vector*)
    (asin . ,*-1x-+1x-vector*) (acos . ,*-1x-+1x-vector*)
    (atan . ,*-1x-+1x-vector*)
    (sinh . ,*+-vector*) (cosh . ,*+-vector*) (tanh . ,*+-vector*)
    (asinh . ,*+-vector*) (acosh . ,*+1-vector*) (atanh . ,*-1x-+1x-vector*)
    (1+ . ,*+-vector*) (1- . ,*+-vector*)
    (abs . ,*+-vector*)
    (log . ,*+1-vector*) (exp . ,*+-vector*)
    (sqrt . ,*0+-vector*) (isqrt . ,*natural-vector*)
    (cis . ,*+1-vector*) (conjugate . ,*complex-vector*)
    (phase . ,*complex-vector*) (realpart . ,*complex-vector*)
    (imagpart . ,*complex-vector*)
    (signum . ,*complex-vector*))
  "A-list of functions and grids used for testing of functions of one
  argument")

(defparameter *predicate-func-test-def*
  `((minusp . ,*+-vector*)
    (plusp . ,*+-vector*)
    (zerop . ,*+-vector*))
  "A-list of functions and grids used for testing of predicate
  functions")

(defparameter *funcs-with-second-arg-optional-test-def*
  `((log . ,*+1-vector*)
    (floor . ,*natural-vector*)
    (ceiling . ,*natural-vector*)
    (truncate . ,*natural-vector*)
    (round . ,*natural-vector*)
    (ffloor . ,*+-vector*)
    (fceiling . ,*+-vector*)
    (ftruncate . ,*+-vector*)
    (fround . ,*+-vector*))
  "A-list of functions and grids used for testing of functions with
two arguments, the second of which is optional")


(defparameter *two-arg-test-def*
  `((exp . ((gs . (,*+1-vector* 2d0))
	    (sg . (2d0 ,*+1-vector*))
	    (gg . (,*+1-vector* ,*+1-vector*))))
    (mod . ((gs . (,*+1-vector* 2d0))
	    (sg . (2d0 ,*+1-vector*))
	    (gg . (,*+1-vector* ,*+1-vector*))))
    (rem . ((gs . (,*+1-vector* 2d0))
	    (sg . (2d0 ,*+1-vector*))
	    (gg . (,*+1-vector* ,*+1-vector*)))))
  "A-list of functions and grids used for testing two-argument
functions The car of each is the cl-function.  The cdr is another
a-list which contains the type of two-arg function (gs, sg, gg) and
the arguments used for testing that particular type.")


(defun fun-arg (fun &optional (a-list *one-arg-func-test-def*))
  "Retreive the argument to be used for testing `fun' from `a-list'"
  (cdr (assoc fun a-list)))


(defgeneric cl-calc-2 (fun arg1 arg2)
  (:documentation "Evaluate function of two arguments, looping over
  one or both as necessary.  Vector arguments are of grid type.  The
  return values is a native cl array.")
  (:method (fun (arg1 vector-double-float) (arg2 double-float))
    (map 'vector #'(lambda (vec-arg)
		     (funcall fun vec-arg arg2)) (copy-to arg1)))
  (:method (fun (arg1 vector-double-float) (arg2 vector-double-float))
    (map 'vector #'(lambda (vec-arg1 vec-arg2)
		     (funcall fun vec-arg1 vec-arg2))
	 (copy-to arg1) (copy-to arg2)))
  (:method (fun (arg1 double-float) (arg2 vector-double-float))
    (map 'vector #'(lambda (vec-arg)
		     (funcall fun arg1 vec-arg)) (copy-to arg2))))
  
(defun grid-calc (fun arg)
  "Invoke a grid function `fun' (such as grid-sin) on a grid `arg' and
return a cl array"
  (copy-to (funcall fun arg)))
  

(defgeneric grid-calc-2 (fun arg1 arg2)
  (:documentation "Evaluate grid-mapping function corresponding to cl
function `fun' of two arguments `arg1' and `arg2'.  The vector
arguments are of grid type.  The return values is a native cl array.")
  (:method (fun (arg1 vector-double-float) (arg2 double-float))
    (copy-to
     (funcall (grid-fun-name fun "GS") arg1 arg2)))
  (:method (fun (arg1 vector-double-float) (arg2 vector-double-float))
    (copy-to
     (funcall (grid-fun-name fun "GG") arg1 arg2)))
  (:method (fun (arg1 double-float) (arg2 vector-double-float))
    (copy-to
     (funcall (grid-fun-name fun "SG") arg1 arg2))))



(defmacro assert-grid-cl-equal-1 (cl-fun grid-fun)
  "Build an assert segment for testing results of mapping of single
argument functions `cl-fun' and `grid-fun' The arguments for the test
are obtained from the `*one-arg-func-test-def*'"
  `(let* ((arg (cdr (or (assoc ,cl-fun
			       *one-arg-func-test-def*)
			(assoc ,grid-fun
			       *one-arg-func-test-def*)))))
     (assert-numerical-equal
      (cl-calc ,cl-fun (copy-to arg))
      (grid-calc ,grid-fun arg)
      ',grid-fun )))


(define-test one-arg-func-test--single--cis
  ;; A simple test of one-argument functions using the sin function
  (assert-grid-cl-equal-1 'cis 'grid-cis ))

(defmacro assert-grid-cl-equal (cl-fun arg)
  (let ((grid-fun (grid-fun-name cl-fun)))
    `(assert-numerical-equal
      (cl-calc #',cl-fun (copy-to ,arg))
      (grid-calc #',grid-fun ,arg))))

(define-test one-arg-func-test--multiple--sin
  (assert-grid-cl-equal sin *+-vector* )
  (assert-grid-cl-equal sin *natural-vector*)
  (assert-grid-cl-equal sin *complex-vector*))






;;; this is a more explicit version of the next test
;; (define-test one-arg-func-test--all
;;   ;; test correctness of all one-argument functions
;;   (do-fundef (*one-arg-functions*)
;;     (let ((argument (fun-arg fun)))
;;       (print fun)
;;       (assert-numerical-equal
;;        (cl-calc  fun argument)
;;        (grid-calc (grid-fun-name fun) argument))
;;        fun)))


(define-test one-arg-func-test--all
  ;; test correctness of all one-argument functions
  (do-fundef (*one-arg-functions*)
    (assert-grid-cl-equal-1 fun (grid-fun-name fun))))


(define-test new-one-arg-funcs
  (assert-numerical-equal
   (cl-calc #'log *+1-vector*)
   (grid-calc #'grid-ln *+1-vector*))
  (assert-numerical-equal
   (cl-calc #'(lambda (arg)
		    (log arg 10d0))
	    *+1-vector*)
   (grid-calc #'grid-log10 *+1-vector*)))

(define-test predicate-funcs
  (do-fundef (*predicate-functions*)
    (let ((argument (fun-arg fun *predicate-func-test-def*)))
      (assert-equalp
       (cl-calc  fun argument)
       (grid-calc (grid-fun-name fun) argument))
       fun)))

(define-test one-arg-with-second-optional&omitted
  (do-fundef (*one-arg-with-second-optional-functions*)
    (let ((argument (fun-arg fun *funcs-with-second-arg-optional-test-def*)))
      (assert-numerical-equal
       (cl-calc  fun argument)
       (grid-calc (grid-fun-name fun)argument))
      fun)))
  
(define-test two-arg-functions
  (do-fundef (*two-arg-functions*)
    (let ((test-defs (cdr (assoc fun *two-arg-test-def*))))
      (dolist (test-def test-defs)
	(destructuring-bind (type arg1 arg2) test-def
	  (declare (ignore type))
	  (let ((res-cl (cl-calc-2 fun arg1 arg2))
		(res-grid (grid-calc-2 fun arg1 arg2)))
	    (assert-numerical-equal (coerce res-cl 'list)
				    (coerce res-grid 'list)
				    fun)))))))

|#