;; Grid manipulation package:
;;
;; By default the array types and float
;; types are set to foreign-arrays and double floats, as needed by
;; GSL.  These can be overridden by specifying the *array-type* and
;; *float-type* to array and single-float

(in-package :mv-gsll)

(export '(
	 #:*array-type* #:*float-type*
	 #:indgen #:natgen #:findgen #:dindgen #:lseq #:gseq #:gmap #:gsmap
	 #:closest-element #:matrify
	 #:match-vec-element #:matching-indices
	 #:read-grid
	 #:reduce-rows #:reduce-columns))

(defparameter *array-type* 'grid::foreign-array
  "Default array type, either foreign-array (default for GSLL use) or
  array (native CL) ")

(defparameter *float-type* 'grid::double-float
  "Default float type, either single-float or double-float (default
  for GSLL use)")

(defparameter *integer-type* '(unsigned-byte 32)
  "Default integer byte length")

(defun indgen (count &optional (len 16))
  "Return vector of length `count', type signed-byte of `len' bytes
where the value of each element is its index.

Allowed values of `len' are 8, 16, 32, 64"
  (grid:map-grid :source #'identity
		 :source-dims `(,count)
		 :destination-specification `((,*array-type* ,count) (unsigned-byte ,len))))


(defun natgen (count &optional (len 16))
  "Return vector of length `count' of natural numbers, starting at 1.

Allowed values of `len' are 8, 16, 32, 64"
  (grid:map-grid :source #'(lambda (arg)
			     (1+ arg))
		 :source-dims `(,count)
		 :destination-specification `((,*array-type* ,count) (unsigned-byte ,len))))


(defun findgen (count &optional (type 'double-float))
  "Return floating vector of length `count', where the value of each
element is its index.

The floating type is either `single' or `double' (default), determined by `type'"
  (grid:map-grid :source #'(lambda (i)
			     (coerce i 'float))
		 :source-dims `(,count)
		 :destination-specification `((,*array-type* ,count) ,type)))

(defun cindgen (count &optional (type 'double-float))
  "Return complex vector of length `count', where the real part of  each
element is its index.

The floating type is either `single' or `double' (default), determined by `type'"
  (grid:map-grid :source #'(lambda (i)
			     (coerce i 'float))
		 :source-dims `(,count)
		 :destination-specification `((,*array-type* ,count) (complex ,type))))



(macrolet ((seq (fun)
	     `(grid:make-foreign-array 'double-float :dimensions count
			  :initial-contents
			  (coerce (,fun (coerce begin 'double-float)
					(coerce end 'double-float)
					count) 'list))))
  (defun lseq (begin end &optional (count 51))
    "linear progression between two positive numbers `begin' and `end'
`begin' can be less than `end'"
    (declare (number begin end))
    (seq my-utils:rseq))


  (defun gseq (begin end &optional (count 51))
    "Geometric progression between two positive numbers `begin' and `end'
`begin' can be less than `end'"
    (declare (number begin end))
    (seq my-utils:xpseq)))

(defun gmap (function grid)
  "Element-wise Map `function' over `grids' *array-type* and
*float-type* determine the result type

gmap specializes grid:map-grid to use only the :element-function
keyword"
  (print *float-type*)
  (grid:map-grid
   :source grid
   :element-function function
   :destination-specification `((,*array-type* ,@(dimensions grid))
						 ,*float-type*)))


(defun gsmap (function &rest grids)
  "Element-wise Map `function' over `grids' *array-type* and
*float-type* determine the result type

gsmap specializes grid:map-n-grids to use only
the :combination-function keyword"
  (let ((affis (mapcar #'grid::affi grids))
	(index 2))
    (dolist (this-affi (rest affis))
      (unless (affi:check-conformability (first affis) this-affi)
	(error "~s~:*~[nil~;st~;nd~;rd~:;th~] grids's affi conflicts with the first grid's affi" index)))
    (grid:map-n-grids
     :sources  (mapcar #'list grids affis)
     :combination-function #'(lambda (&rest args)
			       (apply function args))
     :destination-specification `((,*array-type* ,@(dimensions (first grids)))
						 ,*float-type*))))

(defun closest-element (item vector &optional (distance #'(lambda (arg item)
							    (abs (- arg item)))))
  "Return the index, distance & value of the `vector' element with the
smallest `distance' from `item'.

`distance' is a designator for a function of two arguments that the
distance between them"
  (let* ((i-closest 0)
	 (closest-value (grid::gref vector i-closest))
	 (min-distance (funcall distance closest-value item)))
    (iter:iter (iter:for V :vector-element vector)
	       (iter:for I from 0)
	       (let ((distance (funcall distance V item)))
		 (when (< distance min-distance)
		   (setf i-closest i
			 min-distance distance
			 closest-value V))))
    (values i-closest min-distance closest-value)))

(defun matrify (vector rows columns)
  "Remap `vector' into a `rows'X`columns' matrix"
  (grid:map-grid :source vector
		 :destination-specification `((,*array-type* ,rows ,columns)
					      ,*float-type*)))


(defun match-vec-element (vector predicate)
  "Return the first `vector' index and value for which the `predicate'
returns t.  Else return nil"
  (iter:iter (iter:for v :vector-element vector)
	     (iter:for i :vector-element-index vector)
	     (when (funcall predicate v)
	       (return-from match-vec-element (values i v))))
  nil)

(defgeneric matching-indices (grid predicate)
  (:documentation "Return `grid's indices that satisfy the predicate.
  Supports gsll's vectors and matrices, and cl's simple-vectors of
  rank q")
  (:method ((vector grid:vector-double-float) predicate)
    (iter:iter (iter:for v :vector-element vector)
	       (iter:for i upfrom 0)
	       (when (funcall predicate v)
		 (iter:collect i))))
  (:method ((vector #+sbcl simple-vector #+clisp vector) predicate)
    (iter:iter (iter:for v :vector-element vector)
	       (iter:for i upfrom 0)
	       (when (funcall predicate v)
		 (iter:collect i))))
  (:method ((matrix grid:matrix-double-float) predicate)
    (iter:iter (iter:for m :matrix-element matrix)
	       (iter:for i upfrom 0)
	       (when (funcall predicate m)
		 (iter:collect i)))))

	       
(defun read-grid (dimensions stream &optional (type 'double-float))
  "Read grid of `dimensions' from `stream', converting all elements
into `type' (default: double-float)

For a matrix, the dimensions are specified as (rows columns)"
  (grid:map-grid :source #'(lambda (&rest args)
			     (declare (ignore args))
			     (coerce (read stream) type))
		 :source-dims dimensions
		 :destination-specification `((,*array-type* ,@dimensions)
					      ,*float-type*)))

(defun reduce-rows (matrix &optional (func #'+))
  "Return a column vector with each element a result reducing that row
using `func' (default #'+)"
  (grid:make-grid `((,*array-type* 1 ,(first (grid:dimensions matrix)))
		    double-float)
	     :initial-contents
	     ;; In order to create a column array, I have to return a
	     ;; nested list.  This could be a gsll bug or a feature.
	     (list
	      (iter:iter
		(iter:for R :matrix-row matrix)
		(iter:collect
		    (iter:iter
		      (iter:for E :vector-element R)
		      (iter:reducing E by func initial-value 0d0)))))))


(defun reduce-columns (matrix &optional (func #'+))
  "Return a row vector with each element a result of reducing the
column using `func' (default #'+)"
  (grid:make-grid `((,*array-type* ,(second (grid:dimensions matrix)))
		    double-float)
	     :initial-contents
	     (iter:iter
	       (iter:for C :matrix-column matrix)
	       (iter:collect
		   (iter:iter
		     (iter:for E :vector-element C)
		     (iter:reducing E by func initial-value 0d0))))))

  

;;;; Local variables: 
;;;; change-log-default-name: "~/my-software-add-ons/my-lisp/mv-gsll/ChangeLog"
;;;; End: