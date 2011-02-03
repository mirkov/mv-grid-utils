(in-package :mv-grid)


;;; Tests of creation of grid mapping functions.  This tests whether
;;; the grid-functions are defined and if they run without errors.
;;; They do not check numerical accuracy.  That is done by tests in
;;; another file.

;; functions of one argument (isqrt)
(define-test def-cl-grid-maps
  (assert-true
   (progn 
     (def-cl-grid-maps (assoc 'isqrt *one-arg-functions*)
	 '(arg))
     (grid-isqrt (indgen 5)))
   "One-argument-function creation and execution"))

;; functions of two arguments (mod)
(define-test def-cl-grid-maps-2
  (progn
    (def-cl-grid-maps-2 (second *two-arg-functions*)
	'(arg1 arg2))
    (assert-true (grid-mod (findgen 10) 3d0) "Two arg, vector/scalar")
    (assert-true (grid-mod 10d0 (grid-1+ (findgen 5))) "Two arg, scalar/vector")
    (assert-true (grid-mod *-1x-+1x-vector*
			 (grid-1+ *-1x-+1x-vector*))
	       "Two arg, vector/vector")))

;; functions of of one or two arguments (log)
(define-test def-cl-grid-maps-2
  (progn
    (def-cl-grid-maps-2 (assoc *one&optional-second-arg-functions*)
	'(arg1 arg2))
    (assert-true (grid-log *+1-vector* 3d0) "Two arg, vector/scalar")
    (assert-true (grid-log *+1-vector*) "One vector arg")
    (assert-true (grid-log 10d0 (grid-1+ *+1-vector*)) "Two arg, scalar/vector")
    (assert-true (grid-log (grid-1+ *+1-vector*)
			 (grid-1+ *+1-vector*))
	       "Two arg, vector/vector")))

