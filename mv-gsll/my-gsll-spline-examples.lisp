(in-package :gsl)


(defun spline-integration-example ()
  "Integral of  pi * sin (pi x) between 0 and 0.5.  Should come close to 1.0"
  (let* ((acc (make-acceleration))
	 (xarr (mv-gsll:lseq 0d0 0.5d0 6))
	 (yarr
	  (grid:map-grid :source xarr
			 :element-function  #'(lambda (x)
						(* x pi))))
	 (spline (make-spline gsll:+cubic-spline-interpolation+ xarr yarr)))
    (* pi (evaluate-integral spline 0d0 0.5d0 :acceleration acc))))

(defun spline-integration-periodic_example ()
  "Integral of  sin (pi x) between 0 and 2.  Should come close to 0.0"
  (let* ((acc (make-acceleration))
	 (xarr (mv-gsll:lseq 0d0 2d0 9))
	 (yarr
	  (grid:map-grid :source xarr
			 :element-function  #'(lambda (x)
						(* x pi))))
	 (spline (make-spline +periodic-cubic-spline-interpolation+ xarr yarr)))
    (evaluate-integral spline 0d0 2.0d0 :acceleration acc)))


(defun spline-exact-integral-test ()
  "This routine shows the limitation of the natural (zero second
derivative) boundary on splines.  The integrals of second and third
order polynomials are incorrect"
  (let ((xarr (mv-gsll:lseq 0d0 2d0 9)))
    (dotimes (n 4)
      (let* ((acc (make-acceleration))
	     (yarr (grid:map-grid :source xarr
				  :element-function  #'(lambda (x)
							 (expt x n)))))
	(format t "Exact: ~a Numeric: ~a~%"
		(/ (expt 2.0 (1+ n))
		   (1+ n))
		(evaluate-integral
		 (make-spline +cubic-spline-interpolation+ xarr yarr)
		 0d0 2d0 :acceleration acc))))))


