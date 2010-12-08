(in-package :mv-gsll)

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
	     (lambda (grid scalar)
	       (grid:map-grid
		:source grid
		:combination-function
		#'(lambda (grid-element)
		    (funcall cl-function grid-element scalar))))))
	(func-vv
	 (if result-type
	     (lambda (grid1 grid2)
	       (grid:map-n-grids
		:sources (list (list grid1 nil)
			       (list grid2 nil))
		:combination-function cl-function
		:destination-specification
		(list (append '(grid::foreign-array)
			      (grid:dimensions grid1))
		      result-type)))
	     (lambda (grid1 grid2)
	       (grid:map-n-grids
		:sources (list (list grid1 nil)
			       (list grid2 nil))
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
		    (funcall cl-function scalar grid-element)))))))
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
      (export (list grid-fun-name)))))