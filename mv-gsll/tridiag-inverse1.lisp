(defpackage :tridiag-inverse1
  (:use :cl :gsll :array-indexing1 :lisp-unit :anaphora :grid)
  (:import-from :iterate
		:iter :for))

(in-package :tridiag-inverse1)
(defun vector-product (vector &optional start stop)
  "Calculate product of `vector' elements from `start' (default 0)
to `stop' (default (dim0 vector)"
  (let ((res 1d0))
    (iter
      (for i from (if start start 0)
	   to (if stop stop (dim0 vector)))
      (setf res (* res (gref vector i))))
    res))

(defun create-vector (function n)
  "Create vector of length `n', with contents based on the `function' of
the element index i"
  (gsll::create-matrix #'(lambda (i)
			   (funcall function j))
		       n nil))

(define-test vector-product
  (let ((vec (gsll::create-matrix #'identity 5 nil)))
    (assert-equal 24d0 (vector-product vec 1 4))))

(defun identity-matrix-p (matrix &optional (equality-test #'=))
  "Return t for a square `matrix' with diagonal elements 1d0 and
off-diagnoal lelements 0d0"
;;  (print matrix)
  (let ((n (dim0 matrix)))
    (unless (= n (dim1 matrix))
      (return-from identity-matrix-p nil))
    (iter
      (for i from 0 below n)
      (iter
	(for j from 0 below n)
	(with-_-indexing1 (() gref)
	  ;; (format t "~a,~a,~a~%" i j matrix_i_j)
	  ;; (format t "zero test ~a~%" (funcall equality-test 0d0 matrix_i_j))
	  (if (= i j)
	      (awhen (not (funcall equality-test 1d0 matrix_i_j))
		;; (format t "i,j= ~a, ~a~% m_i_j=~a ~%" i j matrix_i_j)
		;; (format t "diagonal test results ~a~%" it)
		(return-from identity-matrix-p nil))
	      (awhen (not (funcall equality-test 0d0 matrix_i_j))
		;; (format t "i,j= ~a, ~a~% m_i_j=~a ~%" i j matrix_i_j)
		;; (format t "off-diagonal test results ~a~%" it)
		(return-from identity-matrix-p nil))))))
    t))

(define-test identity-matrix-p
  (assert-true (identity-matrix-p
		(make-marray 'double-float
			     :initial-contents '((1d0 0d0 0d0) 
						 (0d0 1d0 0d0) 
						 (0d0 0d0 1d0)))
		#'number-equal)
	       "testing against identity")
  (assert-true (not
		(identity-matrix-p
		 (make-marray 'double-float
			      :initial-contents '((1d0 0d0 0d0) 
						  (0d0 1d0 0d0)))
		 #'number-equal))
	       "testing against non-square")
  (assert-true (not
		(identity-matrix-p
		 (make-marray 'double-float
			      :initial-contents '((0d0 0d0 0d0) 
						  (0d0 1d0 0d0) 
						  (0d0 0d0 1d0)))
		 #'number-equal))
	       "testing against zero on diagonal")
  (assert-true (not
		(identity-matrix-p
		 (make-marray 'double-float
			      :initial-contents '((1d0 0d0 0d0) 
						  (0d0 1d0 0d0) 
						  (1d0 0d0 1d0)))
		#'number-equal))
	       "testing against non-zero off-diagonal"))



(defun theta (n a b c)
  "Calculate theta_i=a_i-1 theta_i-1 - b_i-2 c_i-2 theta_i-2 for i=2,...n
with theta_0 = 1, theta_1 = a_0"
  (let ((theta (gsll:make-marray 'double-float :dimensions (1+ N))))
    (with-_-indexing1 (((theta 0) (a 1) (b 1) (c 1)) gref)
      (setf theta_0 1d0
	    theta_1 a_1)
      (iter (for i from 2 to n)
	    (setf theta_i
		  (- (* a_i theta_i-1)
		     (* b_i-1 c_i-1 theta_i-2)))))
    theta))

(define-test theta
  (assert-numerical-equal
   #(1d0 1d0 -3d0 -7d0)
   (let ((N 3))
     (let ((a (make-marray 'double-float :initial-contents '(1d0 1d0 1d0)))
	   (b (make-marray 'double-float :initial-contents '(2d0 2d0)))
	   (c (make-marray 'double-float :initial-contents '(2d0 2d0))))
       (cl-array (theta n a b c)))) "theta"))
  

(defun phi (n a b c)
  "Calculate phi_i=a_i phi_i+1 - b_i c_i phi_i+2 for i=n-1,...,1
with phi_n+1 = 1, phi_n = a_n-1"
  (let ((phi (gsll:make-marray 'double-float :dimensions (1+ N))))

    (with-_-indexing1 (((i (- i 1))
			(i+1 i)
			(i+2 (+ i 1))
			(n (- n 1))
			(n+1 n))
		       gref)
      (setf phi_n+1 1d0
	    phi_n a_n)
      (iter (for i from (1- n) downto 1)
	    (setf phi_i
		  (- (* a_i phi_i+1)
		     (* b_i c_i phi_i+2)))))
    phi))

(define-test phi
  (let ((N 3))
    (let ((a (make-marray 'double-float :initial-contents '(1d0 1d0 1d0)))
	  (b (make-marray 'double-float :initial-contents '(2d0 2d0)))
	  (c (make-marray 'double-float :initial-contents '(2d0 2d0))))
      (assert-numerical-equal #(-7d0 -3d0 1d0 1d0)
		    (cl-array (phi n a b c) "phi")))))


(defun tridiag-inverse (n a b c)
  "Implement (1.1) of Da Fonseca's article"
  (let ((theta (theta n a b c))
	(phi (phi n a b c))
    	(inverse (make-marray 'double-float :dimensions (list N N))))
    (with-_-indexing3 ((inverse
			((i (- i 1))
			 (j (- j 1)))
			gref)
		       (theta
			((j-1 (- j 1))
			 (i-1 (- i 1)))
			gref)
		       (phi
			((i+1 i)
			 (j+1 j))
			gref))
      (iter
	(for i from 1 to N)
	(iter
	  (for j from 1 to N)
	  (if (> i j)
	      (setf inverse_i_j
		    (* (expt -1d0 (+ i j))
		       (vector-product c (- j 1) (- i 2))
		       theta_j-1 phi_i+1 (/ theta_n)))
	      (setf inverse_i_j
		    (* (expt -1d0 (+ i j))
		       (vector-product b (- i 1) (- j 2))
		       theta_i-1 phi_j+1 (/ theta_n)))))))
    inverse))




(defun test (n)
  (let ((E (create-vector #'(lambda (index)
				    (declare (ignore index))
				    2d0)
				(1- N)))
	(D (create-vector #'(lambda (index)
				    (declare (ignore index))
				    1d0)
				N))
	(F (create-vector #'(lambda (index)
				    (declare (ignore index))
				    0d0)
				(1- N))))
    (tridiag-inverse N D E F)))

(defun tridiag->d/e/f (matrix)
  (values (diagonal (cl-array matrix))
	  (diagonal (cl-array matrix) :offset 1)
	  (diagonal (cl-array matrix) :offset -1)))

(defun d/e/f->tridiag (d e f)
  (let ((n (dim0 d)))
    (when (not (= (1- n) (dim0 e)))
      (error "Vector E length ~a is incorrect" e))
    (when (not (= (1- n) (dim0 f)))
      (error "Vector F length ~a is incorrect" f))
    (let ((matrix (make-array 'double-float :dimensions (list n n)
			       :initial-element 0d0)))
      (setf ())
      (with-_-indexing1 (((i-1 (- i 1))
			  (i+1 (+ i 1)))
			 gref)
	(iter
	  (for i from 0 below n)
	  (format t "row: ~a~%" i)
	  (setf matrix_i_i d_i)
	  (unless (= i (1- n)) (setf matrix_i_i+1 e_i))
	  (unless (zerop i) (setf matrix_i_i-1 f_i-1))))
      matrix)))

(defun diag&sup-matrix (&optional (n 5))
  "Crete test matrix"
  (gsll::create-matrix
		  #'(lambda (i j)
		      (cond
			((= i (1- j)) (coerce (1+ i) 'double-float))
			((= i j) (coerce (1+ j) 'double-float))
			(t 0d0)))
		  n))

(defun diag&sub-matrix (&optional (n 5))
  "Crete test matrix"
  (gsll::create-matrix
		  #'(lambda (i j)
		      (cond
			((= i (1+ j)) (coerce (1+ i) 'double-float))
			((= i j) (coerce (1+ j) 'double-float))
			(t 0d0)))
		  n))

(define-test full-matrix-inverse
  "Create a square matrix, its inverse, and compute their product"
  (labels ((crunch (test-matrix)
	     (let* ((work-copy (copy test-matrix))
		    (inverse (invert-matrix work-copy)))
	       (matrix-product test-matrix inverse))))
    (assert-true (identity-matrix-p (crunch (diag&sub-matrix)) #'number-equal)
		 "diag&sub")
    (assert-true (identity-matrix-p (crunch (diag&sup-matrix)) #'number-equal)
		 "diag&sup")))

(define-test tridiag-matrix-inverse-test
  (labels ((crunch (test-matrix)
	     (multiple-value-bind (d e f) (tridiag->d/e/f test-matrix)
	       (let ((inverse (tridiag-inverse (dim0 d) d e f)))
		 (matrix-product test-matrix inverse)))))
    (assert-true (identity-matrix-p (crunch (diag&sub-matrix))
				    #'number-equal)
		 "diag&sub")))
    ;; (assert-true (identity-matrix-p (crunch (diag&sup-matrix))
    ;; 				    #'number-equal)
    ;; 		 "diag&sup")))

