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
    `(let ((,v (make-array ,N :element-type 'double-float)))
       (setf (grid:gref ,v 0) ,first
	     (grid:gref ,v (1- ,N)) ,last)
       (iter:iter (iter:for i from 1 below (1- ,N))
		  (setf (grid:gref ,v i) ,bulk))
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
;;	 (-1/DELTA^2 (- (/ DELTA^2)))
	 (1/DELTA^2 (/ DELTA^2))
	 (2/DELTA^2 (/ 2d0 DELTA^2)))
    (macrolet ((x-val (var)
		 `(if ,var (case (type-of ,var)
			     (grid:vector-double-float (grid:gref ,var i))
			     (double-float ,var)
			     (t (error "var must be of type vector-double-float or double float.  It is of type ~a" (type-of ,var))))
		      0d0)))
      ;; (let ((E (setup-vector (1- N) 0d0 -1/DELTA^2 -1/DELTA^2))
      ;; 	    (F (setup-vector (1- N) -1/DELTA^2 0d0 -1/DELTA^2))
      ;; 	    (D (setup-vector N 1d0 1d0
      ;; 			     (+ (x-val alpha)
      ;; 				2/DELTA^2)))
      (let ((E (setup-vector (1- N) 0d0 1/DELTA^2 1/DELTA^2))
	    (F (setup-vector (1- N) 1/DELTA^2 0d0 1/DELTA^2))
	    (D (setup-vector N 1d0 1d0
			     (- (x-val alpha)
				2/DELTA^2)))
	    (B (setup-vector N PSI_0 PSI_1 (- (x-val beta)))))
	(values D E F B)))))


(defun 2nd-order-coeffs (f_0 f_1 xi_0 xi_1 PSI_0 PSI_1)
  "Given two basis functions `f_0', `f_1' and the solution values
`PSI_0' and `PSI_1' at the interval endpoints `xi_0' and `xi_1',
return the A & B coefficients so that A f_0 + B f_1 satisfy the
boundary conditions."
  (let* ((matrix (grid:make-foreign-array
		  'double-float :dimensions '(2 2)
		  :initial-contents
		  `((,(funcall f_0 xi_0) ,(funcall f_1 xi_0))
		    (,(funcall f_0 xi_1) ,(funcall f_1 xi_1)))))
    	 (rhs (grid:make-foreign-array 'double-float :dimensions '(2)
			   :initial-contents
			   `(,PSI_0 ,PSI_1))))
    (multiple-value-bind (upper permutation signum)
	(lu-decomposition matrix)
      (declare (ignore signum))
      (lu-solve upper rhs permutation t))))