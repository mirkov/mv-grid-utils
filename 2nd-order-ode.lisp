;;;; Utilities for solution of second order ODE's

(in-package :mv-gsll)


(defmacro setup-vector (N first last bulk)
  "Return a vector of length `N' that will be used in a triangular
matrix setup:
 - Execute `first' and store it in element 0
 - Execute `last' and store it in element `N-1'
 - Execute `bulk' for each of indices I = 1,...,N-2 and store in that location.
   The bulk loop has access to the index `I'

Any preparatory stuff needs to be done outside of the body of `setup-vector'"
  (cl-utilities:with-gensyms (v)
    `(let ((,v (make-marray 'double-float :dimensions ,N)))
       (setf (maref ,v 0) ,first
	     (maref ,v (1- ,N)) ,last)
       (iter:iter (iter:for i from 1 below (1- ,N))
		  (setf (maref ,v i) ,bulk))
       ,v)))


(defun setup-tridiag-11 (delta N PSI_0 PSI_1 &key alpha beta)
  "Create the vectors for the tridiagonal matrix using a uniform grid
of `N' xi values stored in `xi-table' with spacing `delta' and the
values at the lower and upper limits, `PSI_0' and `PSI_1'

 d^2 PSI
------- + PSI.alpha + beta = 0
 d xi^2

The variables alpha and beta are either vectors of dimension `N' constants or
nil.


The `11' suffix denotes first-order boundary conditions
"
  (let* ((DELTA^2 (expt DELTA 2))
	 (-1/DELTA^2 (- (/ DELTA^2)))
	 (2/DELTA^2 (/ 2d0 DELTA^2)))
    (macrolet ((x-val (var)
		 `(if ,var (case (type-of ,var)
			     (vector-double-float (maref ,var i))
			     (double-float ,var)
			     (t (error "var must be of type vector-double-float or double float.  It is of type ~a" (type-of ,var))))
		      0d0)))
      (let ((E (setup-vector (1- N) 0d0 -1/DELTA^2 -1/DELTA^2))
	    (F (setup-vector (1- N) -1/DELTA^2 0d0 -1/DELTA^2))
	    (D (setup-vector N 1d0 1d0
			     (+ (x-val alpha)
				2/DELTA^2)))
	    (B (setup-vector N PSI_0 PSI_1 (- (x-val beta)))))
	(values D E F B)))))

