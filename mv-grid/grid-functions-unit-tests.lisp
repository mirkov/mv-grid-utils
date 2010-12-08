(in-package :mv-gsll)

;;; Define special variables that will be used in the tests.  These
;;; include the grids and test definitions.

(defparameter *0+-vector* (findgen 5)
  "Vector of zero on positive double floats")
(defparameter *+-vector* (lseq -2d0 2d0 5)
  "Vector of positive and negative double floats")
(defparameter *-1x-+1x-vector* (lseq 0.1d0 0.9d0 5)
  "Vector of values between +/-1, exclusive")
(defparameter *+1-vector* (lseq 1d0 6d0 5)
  "Vector of values above 1d0")
(defparameter *complex-vector*
  #2m(#c(1d0 2d0) #c(5d0 -3d0)))
(defparameter *natural-vector* (natgen 5)
  "Vector of natural numbers 1, 2, ...")

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

;;; Functions that map over their arguments.  Both cl and grid
;;; versions.  These functions are used to in the unit tests where the
;;; mapping of cl function will be compared to grid's mappers.

(defun cl-calc (fun arg)
  "Map cl function `fun' on a grid `arg' and return a cl array"
  (map 'vector fun (grid:copy-to arg)))

(defgeneric cl-calc-2 (fun arg1 arg2)
  (:documentation "Evaluate function of two arguments, looping over
  one or both as necessary.  Vector arguments are of grid type.  The
  return values is a native cl array.")
  (:method (fun (arg1 grid::vector-double-float) (arg2 double-float))
    (map 'vector #'(lambda (vec-arg)
		     (funcall fun vec-arg arg2)) (grid:copy-to arg1)))
  (:method (fun (arg1 grid::vector-double-float) (arg2 grid:vector-double-float))
    (map 'vector #'(lambda (vec-arg1 vec-arg2)
		     (funcall fun vec-arg1 vec-arg2))
	 (grid:copy-to arg1) (grid:copy-to arg2)))
  (:method (fun (arg1 double-float) (arg2 grid:vector-double-float))
    (map 'vector #'(lambda (vec-arg)
		     (funcall fun arg1 vec-arg)) (grid:copy-to arg2))))
  
(defun grid-calc (fun arg)
  "Invoke a grid function `fun' (such as grid-sin) on a grid `arg' and
return a cl array"
  (grid:copy-to (funcall fun arg)))
  

(defgeneric grid-calc-2 (fun arg1 arg2)
  (:documentation "Evaluate grid-mapping function corresponding to cl
function `fun' of two arguments `arg1' and `arg2'.  The vector
arguments are of grid type.  The return values is a native cl array.")
  (:method (fun (arg1 grid::vector-double-float) (arg2 double-float))
    (grid:copy-to
     (funcall (mv-gsll::grid-fun-name fun "GS") arg1 arg2)))
  (:method (fun (arg1 grid::vector-double-float) (arg2 grid:vector-double-float))
    (grid:copy-to
     (funcall (mv-gsll::grid-fun-name fun "GG") arg1 arg2)))
  (:method (fun (arg1 double-float) (arg2 grid:vector-double-float))
    (grid:copy-to
     (funcall (mv-gsll::grid-fun-name fun "SG") arg1 arg2))))



(defmacro assert-grid-cl-equal-1 (cl-fun grid-fun)
  "Build an assert segment for testing results of mapping of single
argument functions `cl-fun' and `grid-fun' The arguments for the test
are obtained from the `*one-arg-func-test-def*'"
  `(let* ((arg (cdr (or (assoc ,cl-fun
			       *one-arg-func-test-def*)
			(assoc ,grid-fun
			       *one-arg-func-test-def*)))))
     (assert-numerical-equal
      (cl-calc ,cl-fun (grid:copy-to arg))
      (grid-calc ,grid-fun arg)
      ',grid-fun )))


(define-test one-arg-func-test--simple
  ;; A simple test of one-argument functions using the sin function
  (assert-grid-cl-equal-1 'sin 'grid-sin ))





#|

;; this is a more explicit version of the next test
(define-test one-arg-func-test--all
  ;; test correctness of all one-argument functions
  (do-fundef (mv-gsll::*one-arg-functions*)
    (let ((argument (fun-arg fun)))
      (print fun)
      (assert-numerical-equal
       (cl-calc  fun argument)
       (grid-calc (mv-gsll::grid-fun-name fun) argument))
       fun)))
|#

(define-test one-arg-func-test--all
  ;; test correctness of all one-argument functions
  (do-fundef (mv-gsll::*one-arg-functions*)
    (assert-grid-cl-equal-1 fun (mv-gsll::grid-fun-name fun))))


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
  (do-fundef (mv-gsll::*predicate-functions*)
    (let ((argument (fun-arg fun *predicate-func-test-def*)))
      (assert-equalp
       (cl-calc  fun argument)
       (grid-calc (mv-gsll::grid-fun-name fun) argument))
       fun)))

(define-test one-arg-with-second-optional&omitted
  (do-fundef (mv-gsll::*one-arg-with-second-optional-functions*)
    (let ((argument (fun-arg fun *funcs-with-second-arg-optional-test-def*)))
      (assert-numerical-equal
       (cl-calc  fun argument)
       (grid-calc (mv-gsll::grid-fun-name fun)argument))
      fun)))
  
(define-test two-arg-functions
  (do-fundef (mv-gsll::*two-arg-functions*)
    (let ((test-defs (cdr (assoc fun *two-arg-test-def*))))
      (dolist (test-def test-defs)
	(destructuring-bind (type arg1 arg2) test-def
	  (declare (ignore type))
	  (let ((res-cl (cl-calc-2 fun arg1 arg2))
		(res-grid (grid-calc-2 fun arg1 arg2)))
	    (assert-numerical-equal (coerce res-cl 'list)
				    (coerce res-grid 'list)
				    fun)))))))