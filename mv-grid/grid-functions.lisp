;;;; Define mathematical functions that operate on grids.  These
;;;; functions are either cl math functions, or small deviations from
;;;; them.  The functions are defined programatically based on the
;;;; contents of several parameters.  The routines that define the
;;;; functions also generate the documentation and export the symbols
;;;;
;;;; The defined functions are named grid-foo where foo is the
;;;; descriptive part, such as `sin'

(in-package :mv-gsll)

;;; Grid function definitions

(defparameter *one-arg-functions*
  '(sin cos tan asin acos atan
    sinh cosh tanh asinh acosh atanh
    1+ 1-
    abs
    log exp
    sqrt (isqrt (unsigned-byte 32))
    (cis (complex double-float)) conjugate
    (phase double-float) (realpart double-float) (imagpart double-float)
    signum)
  "Functions that are applied to each element in turn and return a
  congruent foreign-array of results")

(defparameter *one-arg-with-second-optional-functions*
  '(log
    (floor (signed-byte 32)) ffloor
    (ceiling (signed-byte 32)) fceiling
    (truncate (signed-byte 32)) ftruncate
    (round (signed-byte 32)) fround)
  "Functions of two arguments, where the second one is optional")

(defparameter *two-arg-functions*
  '(expt mod rem)
  "Two argument functions that are applied to each argument in turn
  and return a congruent foreign-array of results")

(defparameter *new-one-arg-functions*
  '((grid-ln (log) "Natural logarithm")
    (grid-log10 (log 10d0) "Base 10 logarithm"))
  "One-argument functions that do not exist in cl.  These are
  specialization of cl's function")

(defparameter *predicate-functions*
  '(minusp plusp zerop
    ;;evenp oddp ; these work only on integer arguments.
    )
  "Functions that test each element of grid and return a congruent
  cl-array of booleans (t or nil)")

;;; function generation code


(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun grid-fun-name (cl-fun-name &optional (suffix ""))
  "Intern a symbol of a function name operating on a grid.  The
function name is derived from the cl-function name.  For example given
`cl-fun-name' of `foo' the equivalent grid function name is
`grid-foo'"
  (intern (format nil "GRID-~a~a"
		  (string-upcase (symbol-name cl-fun-name))
		  (string-upcase suffix))))

  (defun defgridop1 (cl-function &optional result-type)
    "Return a grid-mapping function that applies the `cl-function' of
one argument, to a foreign-array grid.

Result grid type is unspecified and is typically the type of the input grid

`result-type' can be used to override the default behavior by
specifying for example (such as 'double-float or '(complex
double-float)
"
    (let ((grid-fun-name (grid-fun-name cl-function))
	  (documentation (format nil "Apply CL function ~a to grid" cl-function))
	  (func (if result-type
		    (lambda (grid)
		      (grid:map-grid :source grid
				     :element-function cl-function
				     :destination-specification
				     (list (append '(grid::foreign-array)
						   (grid:dimensions grid))
					   result-type)))
		    (lambda (grid)
		      (grid:map-grid :source grid
				     :element-function cl-function)))))
      (setf (symbol-function grid-fun-name) func
	    (documentation grid-fun-name 'function) documentation)
      (export (list grid-fun-name))))

  (defun defgridop-p (cl-predicate-function)
    "Return a grid-mapping function that applies the `cl-predicate-function' of
one argument to a foreign-array grid.

The result type is a native cl-array congruent with the input grid,
but of type `t'"
    (let ((grid-fun-name (grid-fun-name cl-predicate-function))
	  (documentation (format nil "Apply CL function ~a to grid"
				 cl-predicate-function))
	  (func (lambda (grid)
		  (grid:map-grid :source grid
				 :element-function cl-predicate-function
				 :destination-specification
				 (list (append '(grid::array)
					       (grid:dimensions grid))
				       t)))))
      (setf (symbol-function grid-fun-name) func
	    (documentation grid-fun-name 'function) documentation)
      (export (list grid-fun-name))))

  (defun defgridop2 (cl-function &optional result-type)
    "Define two argument functions that operate on grids.  The following
cases are handled:

 - (grid-fun grid scalar) maps (fun grid-element scalar) to each grid element
 - (grid-fun grid1 grid2) maps (fun grid1-element grid2-element) to two
   congruent grids
 - (grid-fun scalar grid) maps (fun scalar grid-element) to each grid element
 - (grid-fun grid) maps (fun grid-element) where the second argument is the
   default argument of the cl function fun"
    (let ((grid-fun-name-vv (grid-fun-name cl-function "GG"))
	  (grid-fun-name-sv (grid-fun-name cl-function "SG"))
	  (grid-fun-name-vs (grid-fun-name cl-function "GS"))
	  (func-vv
	   (if result-type
	       (lambda (grid1 grid2)
		 (grid:map-n-grids
		  :sources (list grid1 grid2)
		  :combination-function cl-function
		  :destination-specification
		  (list (append '(grid::foreign-array)
				(grid:dimensions grid1))
			result-type)))
	       (lambda (grid1 grid2)
		 (grid:map-n-grids
		  :sources (list grid1 grid2)
		  :combination-function cl-function))))
	  (func-sv
	   (if result-type
	       (lambda (scalar grid)
		 (grid:map-grid
		  :source grid
		  :combination-function
		  #'(lambda (grid-element)
		      (funcall cl-function scalar grid-element))
		  :destination-specification
		  (list (append '(grid::foreign-array)
				(grid:dimensions grid))
			result-type)))
	       (lambda (scalar grid)
		 (grid:map-grid
		  :source grid
		  :combination-function
		  #'(lambda (grid-element)
		      (funcall cl-function scalar grid-element))))))
	  (func-vs
	   (if result-type
	       (lambda (grid scalar)
		 (grid:map-grid
		  :source grid
		  :combination-function
		  #'(lambda (grid-element)
		      (funcall cl-function grid-element scalar))
		  :destination-specification
		  (list (append '(grid::foreign-array)
				(grid:dimensions grid))
			result-type)))
	       (lambda (scalar grid)
		 (grid:map-grid
		  :source grid
		  :combination-function
		  #'(lambda (grid-element)
		      (funcall cl-function grid-element scalar)))))))
      (setf (symbol-function grid-fun-name-vv) func-vv
	    (symbol-function grid-fun-name-sv) func-sv
	    (symbol-function grid-fun-name-vs) func-vs)
      (export (list grid-fun-name-vv grid-fun-name-sv grid-fun-name-vs))))

  (defun defgridop-new (function-def)
    "Define a new function from `function-def'
 (fun-name (cl-fun-name &optional default-arg) &optional documentation)"
    (destructuring-bind (grid-fun-name
			 (cl-function &optional default-2nd-arg)
			 &optional documentation)
	function-def
      (let ((func (if default-2nd-arg
		      (lambda (grid)
			(grid:map-grid :source grid
				       :element-function
				       #'(lambda (arg)
					   (funcall cl-function arg default-2nd-arg))))
		      (lambda (grid)
			(grid:map-grid :source grid
				       :element-function cl-function)))))
	(setf (symbol-function grid-fun-name) func
	      (documentation grid-fun-name 'function) documentation)
	(export (list grid-fun-name))))))

;;; Definition of function happens when the file is loaded, compiled
;;; or executed.
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (dolist (fun-def *one-arg-functions*)
      (if (consp fun-def)
	  (destructuring-bind (cl-fun-name result-type) fun-def
	    (defgridop1 cl-fun-name result-type))
	  (defgridop1 fun-def)))
    (dolist (fun-def *predicate-functions*)
      (defgridop-p fun-def))
    (dolist (fun-def *new-one-arg-functions*)
      (defgridop-new fun-def))
    (dolist (fun-def *two-arg-functions*)
      (defgridop2 fun-def)))

