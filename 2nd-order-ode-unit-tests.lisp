(in-package :mv-gsll-unit-tests)



(define-test setup-vector
  (let ((N 4))
    (assert-numerical-equal #(1d0 0d0 0d0 2d0)
			    (cl-array (setup-vector N 1d0 2d0 0d0)))))

(defmacro assert-tridiag-eq ((d e f b) &body body)
  `(multiple-value-bind (d-res e-res f-res b-res) ,@body
;;     (assert-true
 ;;     (and
       (assert-numerical-equal (cl-array ,d) (cl-array d-res) "D comparison")
       (assert-numerical-equal (cl-array ,e) (cl-array e-res) "E comparison")
       (assert-numerical-equal (cl-array ,f) (cl-array f-res) "F comparison")
       (assert-numerical-equal (cl-array ,b) (cl-array b-res) "B comparison")));))


(define-test setup-tridiag-11/f=0/g=0
  ;;  Most basic test with values 1 at left and 0 at right
  (let ((N 5))
    ;; delta = 1.0
    (assert-tridiag-eq
	((setup-vector N 1d0 1d0 2d0 )
	 (setup-vector (1- N) 0d0 -1d0 -1d0 )
	 (setup-vector (1- N) -1d0 0d0 -1d0 )
	 (setup-vector N 1d0 0d0 0d0 ))
      (setup-tridiag-11 1d0 5 1d0 0d0))
    ;; delta = 2.0 to verify the diagonal element
    (assert-tridiag-eq
	((setup-vector N 1d0 1d0 0.5d0)
	 (setup-vector (1- N) 0d0 -0.25d0 -0.25d0)
	 (setup-vector (1- N) -0.25d0 0d0 -0.25d0)
	 (setup-vector N 0d0 1d0 0d0))
      (setup-tridiag-11 2d0 5 0d0 1d0))
    ))

(define-test setup-tridiag-11/f=1/g=1
  ;; Test with non-zero f & g - both constant
  (let ((N 5))
    (assert-tridiag-eq
	((setup-vector N 1d0 1d0 3d0)
	 (setup-vector (1- N) 0d0 -1d0 -1d0)
	 (setup-vector (1- N) -1d0 0d0 -1d0)
	 (setup-vector N 1d0 0d0 1d0))
      (setup-tridiag-11 1d0 5 1d0 0d0 :alpha 1d0 :beta 1d0))))

(define-test setup-tridiag-11/f=var/g=var
  ;; Test with variable f & g
    (let ((N 5)
	  (alpha #m(0d0 1d0 2d0 3d0 4d0))
	  (beta #m(4d0 3d0 2d0 1d0 0d0)))
      (assert-tridiag-eq
	  ((setup-vector N 1d0 1d0 (+ 2d0 (maref alpha mv-gsll::i)))
	   (setup-vector (1- N) 0d0 -1d0 -1d0)
	   (setup-vector (1- N) -1d0 0d0 -1d0)
	   (setup-vector N 1d0 0d0 (maref beta mv-gsll::i )))
	(setup-tridiag-11 1d0 5 1d0 0d0 :alpha alpha :beta beta))))


;;;; Tests for setup-tridiag-11
;;;;
;;;; I set the problem between xi=0 and xi=5 and assign the values of
;;;; PSI_0=0 and PSI_1=5.  On top of that I build a suite of tests.
(progn
  (defparameter +xi_0+ 0d0)
  (defparameter +xi_1+ 5d0)
  (defparameter +k+ 2d0)
  (defparameter +k^2+ (expt +k+ 2))
  (defparameter +PSI_0+ 0d0)
  (defparameter +PSI_1+ 5d0))
(defparameter +functional-tests+
  `((alpha=0/beta=0 ,#'(lambda (xi)
		  (declare (ignore xi))
		  0d0)
	     ,#'(lambda (xi) xi))
    (alpha=0/beta=1 ,#'(lambda (xi)
		  (declare (ignore xi))
		  1d0)
	     ,#'(lambda (xi)
		  (+ (* 0.5 (expt xi 2))
		     (- (* 1.5 xi)))))
    (alpha=0/beta=x^2 ,#'(lambda (xi) (expt xi 2))
	       ,#'(lambda (xi) (let ((power 2))
			   (labels ((first-term (x)
				      (/ (expt x (+ 2 power))
					 (* (1+ power) (+ 2 power)))))
			     (+ (first-term xi)
				(* (/ (- +PSI_1+
					 (first-term +xi_1+))
				      +xi_1+)
				   xi))))))
    (alpha=0/beta=coskx ,#'(lambda (xi) (cos (* +k+ xi)))
		 ,#'(lambda (xi)
		      (+ (- (/ (- (cos (* +k+ xi)) 1)
			       +k^2+))
			 (* (+ +PSI_1+ (/ (- (cos (* +k+ +xi_1+)) 1)
					+k^2+))
			    (/ xi +xi_1+)))))
    (alpha=k^2/beta=0 nil
	       ,#'(lambda (xi)
		    (let* ((k_0 (* +k+ +xi_0+))
			   (k_1 (* +k+ +xi_1+))
			   (cosh_0 (cosh k_0))
			   (cosh_1 (cosh k_1))
			   (sinh_0 (sinh k_0))
			   (sinh_1 (sinh k_1))
			   (DELTA (- (* cosh_0 sinh_1)
				 (* cosh_1 sinh_0))))
		      (let ((A (/ (- (* +PSI_0+ sinh_1) (* +PSI_1+ sinh_0))
				  DELTA))
			    (B (/ (- (* +PSI_1+ cosh_0) (* +PSI_0+ sinh_1))
				  DELTA)))
			(+ (* A (cosh (* +k+ xi))) (* B (sinh (* +k+ xi)))))))
	       ,#'(lambda (xi)
		    (declare (ignore xi))
		    +k^2+))
    (alpha=-k^2/beta=0 nil
	       ,#'(lambda (xi)
		    (let* ((k_0 (* +k+ +xi_0+))
			   (k_1 (* +k+ +xi_1+))
			   (cos_0 (cos k_0))
			   (cos_1 (cos k_1))
			   (sin_0 (sin k_0))
			   (sin_1 (sin k_1))
			   (DELTA (- (* cos_0 sin_1)
				 (* cos_1 sin_0))))
		      (let ((A (/ (- (* +PSI_0+ sin_1) (* +PSI_1+ sin_0))
				  DELTA))
			    (B (/ (- (* +PSI_1+ cos_0) (* +PSI_0+ sin_1))
				  DELTA)))
			(+ (* A (cos (* +k+ xi))) (* B (sin (* +k+ xi)))))))
	       ,#'(lambda (xi)
		    (declare (ignore xi))
		    (- +k^2+))))
  "For each test store the name, the beta function and the exact solution
  function.  Eventually, also store the alpha function.

This is an alist.  It is a list of sublists.  The first element of
each sublist is the test name, on which the alist is searched.  The
other sublist elements are the functions beta SIGMA and optionally alpha")

(defmacro defsolvertest (name &optional (n 6))
  "Build a 2nd-order BVP ODE test.  `name' refers to one of the cases
stored in +functional-tests+.  `n' is the number of grid points.

The expanded form calculates the grid spacing DELTA, the xi and beta arrays.
Using the beta-array it then builds the matrix vectors.  

The matrix is solved using gsll's solve-tridiagonal, and the solution
is compared with the solution vector calculated by the SIGMA-function

mv-gnuplot is used to show the plots of the expected and actual solution"
  `(define-test ,name
     (let* ((beta-fun (second (assoc  ',name +functional-tests+)))
	    (SIGMA-fun (third (assoc ',name +functional-tests+)))
	    (alpha-fun (fourth (assoc ',name +functional-tests+)))
	    (DELTA (/ (- +xi_1+ +xi_0+)
		  (1- ,n)))
	    (xi-arr (useq +xi_0+ +xi_1+ ,n))
	    (beta-arr (when beta-fun (gmap #'(lambda (xi)
					 (funcall beta-fun xi))
				     xi-arr)))
	    (alpha-arr (when alpha-fun (gmap #'(lambda (xi)
					 (funcall alpha-fun xi))
				     xi-arr)))
	    (SIGMA-arr (cl-array (gmap SIGMA-fun xi-arr)))
	    (solution
	     (multiple-value-bind (d e f b)
		 (setup-tridiag-11 DELTA ,n +PSI_0+ +PSI_1+ :alpha alpha-arr :beta beta-arr)
	       (cl-array (solve-tridiagonal d e f b (copy xi-arr))))))
       (set-to ((title (symbol-name ',name))
		(xlabel "x")
		(ylabel "y"))
	 (mv-gnuplot:plot-xys xi-arr
			      `((,SIGMA-arr :linewidth 4 :title "Expected")
				(,solution :linewidth 2 :title "Numeric"))))
       (assert-numerical-equal SIGMA-arr solution))))


(defsolvertest alpha=0/beta=0 )
(defsolvertest alpha=0/beta=1 )
(defsolvertest alpha=0/beta=x^2 16)
(defsolvertest alpha=0/beta=coskx 160 )
(defsolvertest alpha=k^2/beta=0 160 )
(defsolvertest alpha=-k^2/beta=0 160 )
