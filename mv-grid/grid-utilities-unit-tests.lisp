(in-package :mv-grid-unit-tests)


(defparameter +array-3-4-double-float+
  (grid::test-grid-double-float 'grid:foreign-array '(3 4)))

(defparameter +vector-4-double-float+
  (grid::test-grid-double-float 'grid:foreign-array '(4)))

(defmacro assert-grid-equal (grid1 grid2)
  `(assert-numerical-equal (grid:copy-to ,grid1)
			   (grid:copy-to ,grid2)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf mv-grid:*array-type* 'grid:foreign-array))

(define-test indgen
  (assert-grid-equal #15m(0 1 2)
		     (indgen 3)))


(define-test findgen
  (assert-grid-equal #1m(0d0 1d0 2d0)
		     (findgen 3)))


(define-test gmap
  (let ((v1 #15m(0 1 2))
	(v2 #15m(0 1 2))
	(v3 #15m(0 2 4))
	#|(v4 #(0 1))|#)
    (assert-grid-equal
     v2 (gmap #'identity v1))
    (assert-grid-equal
     v3
     (gsmap #'+ v1 v2))
#|    (assert-grid-equal
     v3
     (gmap #'+ v1 v4))|#))

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

(define-test matrify
  ;; convert vec-4 into 2x2 matrix
  (assert-grid-equal #1m((0d0 1d0) (2d0 3d0))
		     (matrify +vector-4-double-float+ 2 2))
  ;; convert vec-4 into column vector of length 4
  (assert-grid-equal #1m((0d0 1d0 2d0 3d0))
		     (matrify +vector-4-double-float+ 1 4))
  ;; convert vec 4 into a row vector
  (assert-grid-equal #1m((0d0) (1d0) (2d0) (3d0))
		     (matrify +vector-4-double-float+ 4 1)))


(define-test read-grid
  (with-open-file (stream
		   #P"/home/mv/my-software-add-ons/my-lisp/mv-gsll/mv-grid/2d-grid-data.txt"
		   :direction :input) 
    (assert-grid-equal #1m((1d0 2d0 3d0) (4d0 5d0 6d0))
			    (read-grid '(2 3) stream))))

(defparameter +array-3-4-double-float+
  (grid::test-grid-double-float 'grid:foreign-array '(3 4)))



(define-test reduce-rows/cols
  (assert-grid-equal #1m(30d0 33d0 36d0 39d0)
			  (reduce-columns +array-3-4-double-float+))
  (assert-grid-equal #1m((6d0 46d0 86d0))
			  (reduce-rows +array-3-4-double-float+)))


;;;; Local variables: 
;;;; change-log-default-name: "~/my-software-add-ons/my-lisp/mv-gsll/ChangeLog"
;;;; End: