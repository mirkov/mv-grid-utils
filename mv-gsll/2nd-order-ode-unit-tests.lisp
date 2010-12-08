(in-package :mv-gsll-unit-tests)

(defmacro 2nd-order-coeffs-test-body ((a-coeff b-coeff)
				      (PSI_0-value PSI_1-value))
  `(destructuring-bind (a b)
       (coerce (grid:copy-to (2nd-order-coeffs f_0 f_1 xi_0 xi_1 PSI_0 PSI_1))
	       'list)
     (assert-numerical-equal
      '(,a-coeff ,b-coeff)  (list a b))
     (assert-numerical-equal
      '(,PSI_0-value ,PSI_1-value)
      (labels ((f (xi)
		 (+ (* a (funcall f_0 xi))
		    (* b (funcall f_1 xi)))))
	(mapcar #'f (list xi_0 xi_1))))))


(define-test 2nd-order-coeffs
  (let ((f_0 #'sin)
	(f_1 #'cos))
    (let ((xi_0 0d0)
	  (xi_1 (/ pi 2d0))
	  (PSI_0 1d0)
	  (PSI_1 0d0))
      (2nd-order-coeffs-test-body (0d0 1d0) (1d0 0d0)))
    (let ((xi_0 0d0)
	  (xi_1 (/ pi 2d0))
	  (PSI_0 0d0)
	  (PSI_1 1d0))
      (2nd-order-coeffs-test-body (1d0 0d0) (0d0 1d0)))
    (let ((xi_0 0d0)
	  (xi_1 (/ pi 2d0))
	  (PSI_0 1.5d0)
	  (PSI_1 1.5d0))
      (2nd-order-coeffs-test-body (1.5d0 1.5d0) (1.5d0 1.5d0)))
    (let ((xi_0 -5d0)
	  (xi_1 6d0)
	  (PSI_0 3.2d0)
	  (PSI_1 2.5d0))
      (2nd-order-coeffs-test-body (1.5d0 1.5d0) (3.2d0 2.5d0)))
    ))

(define-test setup-vector
  (let ((N 4))
    (assert-numerical-equal #(1d0 0d0 0d0 2d0)
			    (setup-vector N 1d0 2d0 0d0))))

(defmacro assert-tridiag-eq ((d e f b) &body body)
  `(multiple-value-bind (d-res e-res f-res b-res) ,@body
;;     (assert-true
 ;;     (and
       (assert-numerical-equal ,d d-res "D comparison")
       (assert-numerical-equal ,e e-res "E comparison")
       (assert-numerical-equal ,f f-res "F comparison")
       (assert-numerical-equal ,b b-res "B comparison")));))


(define-test setup-tridiag-11/f=0/g=0
  ;;  Most basic test with values 1 at left and 0 at right
  (let ((N 5))
    ;; delta = 1.0
    (assert-tridiag-eq
	((setup-vector N 1d0 1d0 -2d0 )
	 (setup-vector (1- N) 0d0 1d0 1d0 )
	 (setup-vector (1- N) 1d0 0d0 1d0 )
	 (setup-vector N 1d0 0d0 0d0 ))
      (setup-tridiag-11 1d0 5 1d0 0d0))
    ;; delta = 2.0 to verify the diagonal element
    (assert-tridiag-eq
	((setup-vector N 1d0 1d0 -0.5d0)
	 (setup-vector (1- N) 0d0 0.25d0 0.25d0)
	 (setup-vector (1- N) 0.25d0 0d0 0.25d0)
	 (setup-vector N 0d0 1d0 0d0))
      (setup-tridiag-11 2d0 5 0d0 1d0))
    ))

(define-test setup-tridiag-11/f=1/g=1
  ;; Test with non-zero f & g - both constant
  (let ((N 5))
    (assert-tridiag-eq
	((setup-vector N 1d0 1d0 -1d0)
	 (setup-vector (1- N) 0d0 1d0 1d0)
	 (setup-vector (1- N) 1d0 0d0 1d0)
	 (setup-vector N 1d0 0d0 -1d0))
      (setup-tridiag-11 1d0 5 1d0 0d0 :alpha 1d0 :beta 1d0))))

(define-test setup-tridiag-11/f=var/g=var
  ;; Test with variable f & g
  (let ((N 5)
	(alpha #m(0d0 1d0 2d0 3d0 4d0))
	(beta #m(4d0 3d0 2d0 1d0 0d0)))
    (assert-tridiag-eq
	((setup-vector N 1d0 1d0 (- (grid:gref alpha mv-gsll::i) 2d0))
	 (setup-vector (1- N) 0d0 1d0 1d0)
	 (setup-vector (1- N) 1d0 0d0 1d0)
	 (setup-vector N 1d0 0d0 (- (grid:gref beta mv-gsll::i))))
      (setup-tridiag-11 1d0 5 1d0 0d0 :alpha alpha :beta beta))))


;;;; Tests for setup-tridiag-11



(defmacro define-solver-11-test (name SIGMA &key alpha beta (n 6)) 
  "Build a 2nd-order BVP ODE test `name'.  `SIGMA' is the function
that generates the solution, and `alpha' and `beta' generate the
coefficients.  The solution is an vector of length `n' (default 6).

If `alpha' and/or `beta' are not provided, the coefficients are
assumed zero (This is the behavior of the setup-tridiag-11 function
that is used here.

The expanded form first sets up the domain via `xi_0' and `xi_1' and
boundary conditions via `PSI_0' and `PSI_1', and also other
problem coefficients `k' and `k^2'.

Next, the expanded form calculates the grid spacing DELTA, the xi,
alpha and beta arrays.  Using the alpha-array & beta-array it then
builds the matrix vectors.

The matrix is solved using gsll's solve-tridiagonal, and the solution
is compared with the solution vector calculated by the SIGMA-function

mv-gnuplot is used to show the plots of the expected and actual solution"
  `(define-test ,name
     (let* ((xi_0 0d0)
	    (xi_1 5d0)
	    (k 2d0)
	    (k^2 (expt k 2))
	    (PSI_0 0d0)
	    (PSI_1 5d0))
       (let* ((DELTA (/ (- xi_1 xi_0)
			(1- ,n)))
	      (xi-arr (lseq xi_0 xi_1 ,n))
	      (alpha-arr ,(if alpha `(gmap ,alpha xi-arr)))
	      (beta-arr ,(if beta `(gmap ,beta xi-arr)))
	      (SIGMA-arr (gmap ,SIGMA xi-arr))
	      (solution
	       (multiple-value-bind (d e f b)
		   (setup-tridiag-11 DELTA ,n PSI_0 PSI_1
				     :alpha (when alpha-arr
					      (grid:copy-to alpha-arr 'grid:foreign-array))
				     :beta (when beta-arr
					     (grid:copy-to beta-arr 'grid:foreign-array)))
		 (SOLVE-TRIDIAGONAL 
		  (grid:copy-to D 'grid:foreign-array) 
		  (grid:copy-to E 'grid:foreign-array) 
		  (grid:copy-to F 'grid:foreign-array) 
		  (grid:copy-to B 'grid:foreign-array) 
		  (grid:COPY-to XI-ARR 'grid:foreign-array)))))
	 (set-to ((title (format nil "~a" ',name))
		  (xlabel "x")
		  (ylabel "y"))
	   (PLOT-XYS XI-ARR
		     `((,(grid:copy-to SIGMA-ARR) :LINEWIDTH 4 :TITLE "Expected")
		       (,(grid:copy-to SOLUTION) :LINEWIDTH 2 :TITLE "Numeric"))))
	 (ASSERT-NUMERICAL-EQUAL (grid:copy-to SIGMA-ARR) (grid:copy-to SOLUTION))))))

(define-solver-11-test alpha=0/beta=0
    #'(lambda (xi) xi))
(define-solver-11-test alpha=0/beta=1 #'(lambda (xi)
					  (+ (* 0.5 (expt xi 2))
					     (- (* 1.5 xi))))
  :beta #'(lambda (xi)
	     (declare (ignore xi))
	     1d0))
(define-solver-11-test alpha=0/beta=x^2
    #'(lambda (xi)
	(let ((power 2))
	  (labels ((first-term (x)
		     (/ (expt x (+ 2 power))
			(* (1+ power) (+ 2 power)))))
	    (+ (first-term xi)
	       (* (/ (- PSI_1
			(first-term xi_1))
		     xi_1)
		  xi)))))
  :beta #'(lambda (xi) (expt xi 2)))
(define-solver-11-test alpha=0/beta=coskx
    #'(lambda (xi)
	(+ (- (/ (- (cos (* k xi)) 1)
		 k^2))
	   (* (+ PSI_1 (/ (- (cos (* k xi_1)) 1)
			  k^2))
	      (/ xi xi_1))))
  :beta #'(lambda (xi) (cos (* k xi))))
(define-solver-11-test alpha=k^2/beta=0
    #'(lambda (xi)
	(let* ((k_0 (* k xi_0))
	       (k_1 (* k xi_1))
	       (cosh_0 (cosh k_0))
	       (cosh_1 (cosh k_1))
	       (sinh_0 (sinh k_0))
	       (sinh_1 (sinh k_1))
	       (DELTA (- (* cosh_0 sinh_1)
			 (* cosh_1 sinh_0))))
	  (let ((A (/ (- (* PSI_0 sinh_1) (* PSI_1 sinh_0))
		      DELTA))
		(B (/ (- (* PSI_1 cosh_0) (* PSI_0 sinh_1))
		      DELTA)))
	    (+ (* A (cosh (* k xi))) (* B (sinh (* k xi)))))))
  :n 16
  :alpha #'(lambda (xi)
	     (declare (ignore xi))
	     k^2))

(define-solver-11-test alpha=-k^2/beta=0 
    #'(lambda (xi)
	(let* ((k_0 (* k xi_0))
	       (k_1 (* k xi_1))
	       (cos_0 (cos k_0))
	       (cos_1 (cos k_1))
	       (sin_0 (sin k_0))
	       (sin_1 (sin k_1))
	       (DELTA (- (* cos_0 sin_1)
			 (* cos_1 sin_0))))
	  (let ((A (/ (- (* PSI_0 sin_1) (* PSI_1 sin_0))
		      DELTA))
		(B (/ (- (* PSI_1 cos_0) (* PSI_0 sin_1))
		      DELTA)))
	    (+ (* A (cos (* k xi))) (* B (sin (* k xi)))))))
  :n 16
  :alpha #'(lambda (xi)
	    (declare (ignore xi))
	    (- k^2)))

(defmacro define-solver-11-test-1 (name SIGMA &key alpha beta (n 6)) 
  "Build a 2nd-order BVP ODE test `name'.  `SIGMA' is the function
that generates the solution, and `alpha' and `beta' generate the
coefficients.  The solution is an vector of length `n' (default 6).

If `alpha' and/or `beta' are not provided, the coefficients are
assumed zero (This is the behavior of the setup-tridiag-11 function
that is used here.

The expanded form first sets up the domain via `xi_0' and `xi_1' and
boundary conditions via `PSI_0' and `PSI_1', and also other
problem coefficients `k' and `k^2'.

Next, the expanded form calculates the grid spacing DELTA, the xi,
alpha and beta arrays.  Using the alpha-array & beta-array it then
builds the matrix vectors.

The matrix is solved using gsll's solve-tridiagonal, and the solution
is compared with the solution vector calculated by the SIGMA-function

mv-gnuplot is used to show the plots of the expected and actual solution"
  `(define-test ,name
     (let* ((xi_0 -5d0)
	    (xi_1 5d0)
	    (k 2d0)
	    (k^2 (expt k 2))
	    (PSI_0 3.2d0)
	    (PSI_1 -0.1d0))
       (let* ((DELTA (/ (- xi_1 xi_0)
			(1- ,n)))
	      (xi-arr (lseq xi_0 xi_1 ,n))
	      (alpha-arr ,(if alpha `(gmap ,alpha xi-arr)))
	      (beta-arr ,(if beta `(gmap ,beta xi-arr)))
	      (SIGMA-arr (gmap ,SIGMA xi-arr))
	      (solution
	       (multiple-value-bind (d e f b)
		   (setup-tridiag-11 DELTA ,n PSI_0 PSI_1 
				     :alpha (when alpha-arr
					      (grid:copy-to alpha-arr 'grid:foreign-array))
				     :beta (when beta-arr
					     (grid:copy-to beta-arr 'grid:foreign-array)))
		 (SOLVE-TRIDIAGONAL 
		  (grid:copy-to D 'grid:foreign-array) 
		  (grid:copy-to E 'grid:foreign-array) 
		  (grid:copy-to F 'grid:foreign-array) 
		  (grid:copy-to B 'grid:foreign-array) 
		  (grid:COPY-to XI-ARR 'grid:foreign-array)))))
	 (set-to ((title (format nil "~a" ',name))
		  (xlabel "x")
		  (ylabel "y"))
	   (PLOT-XYS XI-ARR
		     `((,(grid:copy-to SIGMA-ARR) :LINEWIDTH 4 :TITLE "Expected")
		       (,(grid:copy-to SOLUTION) :LINEWIDTH 2 :TITLE "Numeric"))))
	 (ASSERT-NUMERICAL-EQUAL (grid:copy-to SIGMA-ARR) (grid:copy-to SOLUTION))))))

(define-solver-11-test-1 alpha=k^2/beta=0-1
    #'(lambda (xi)
	(labels ((f_0 (xi)
		   (cosh (* k xi)))
		 (f_1 (xi)
		   (sinh (* k xi))))
	  (destructuring-bind (A B)
	      (coerce
	       (grid:copy-to (2nd-order-coeffs #'f_0 #'f_1
					   xi_0 xi_1 PSI_0 PSI_1))
	       'list)
	    (+ (* A (funcall #'f_0 xi)) (* B (funcall #'f_1 xi))))))
  :n 160
  :alpha #'(lambda (xi)
	     (declare (ignore xi))
	     k^2))

(define-solver-11-test-1 alpha=-k^2/beta=0-1 
    #'(lambda (xi)
	(labels ((f_0 (xi)
		   (cos (* k xi)))
		 (f_1 (xi)
		   (sin (* k xi))))
	  (destructuring-bind (A B)
	      (coerce
	       (2nd-order-coeffs #'f_0 #'f_1
					   xi_0 xi_1 PSI_0 PSI_1)
	       'list)
	    (+ (* A (funcall #'f_0 xi)) (* B (funcall #'f_1 xi))))))
  :n 160
  :alpha #'(lambda (xi)
	    (declare (ignore xi))
	    (- k^2)))

