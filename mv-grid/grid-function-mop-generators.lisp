;; Mirko Vukovic
;; Time-stamp: <2011-02-10 07:20:03 grid-function-generators.lisp>
;; 
;; Copyright 2011 Mirko Vukovic
;; Distributed under the terms of the GNU General Public License
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :mv-grid)

(defparameter *grid-fun-prefix* "GRID-"
  "Prefix added to mappers derived from CL-functions")

(defun grid-fun-name (cl-fun-name &optional (suffix ""))
  "Intern a symbol of a function name operating on a grid.  The
function name is derived from the cl-function name and the
*grid-fun-prefix*."
  (intern (format nil "~a~a~a" *grid-fun-prefix*
		  (string-upcase (symbol-name cl-fun-name))
		  (string-upcase suffix))))


(defun grid-gf (lambda-list documentation)
  "Create a generalized function with a `lambda-list' and
`documentation'"
  (make-instance #+sbcl 'sb-mop::standard-generic-function
		 #-sbcl 'closer-mop:standard-generic-function
		 :lambda-list lambda-list
		 :documentation documentation))

(defun add-grid-method (gf lambda-list specializers body)
  "Add a method to generalized function `gf'.  The method is defined
by its `lambda-list', a congruent list of `specializers' and its
`body'.  `body' is spliced into the lambda list using ,@

The specializers is a list of specializers.  Thus even for a function
of one argument, this must be a list"
  (let ((method-class
	 (#+sbcl sb-mop:generic-function-method-class
		 #-sbcl closer-mop:generic-function-method-class
		 gf)))
    (multiple-value-bind (lambda initargs)
	(#+sbcl sb-mop:make-method-lambda
	 #-sbcl closer-mop:make-method-lambda
	 gf
	 (#+sbcl sb-mop:class-prototype
		 #-sbcl closer-mop:class-prototype
		 method-class)
	 `(lambda ,lambda-list
	   ,@body)
	 nil)
      (add-method gf
		  (apply #'make-instance method-class
			 :function (compile nil lambda)
			 :specializers (mapcar #'find-class specializers)
			 :qualifiers nil
			 :lambda-list lambda-list
			 initargs)))))


(defparameter *type-dictionary*
  '((!C .
     #+sbcl(complex-double-float (complex double-float) complex)
     #+clisp(complex complex complex))
    (!F .
     #+sbcl (double-float double-float double-float)
     #+clisp(float float float))
    (!I .
     #+sbcl (signed-byte-32 (signed-byte 32) signed-byte)
     #+clisp(integer integer integer))
    (!U .
     #+sbcl (unsigned-byte-16 (unsigned-byte 16) unsigned-byte)
     #+clisp(integer integer integer)))
  "Translations from the mnemonic !I,U,F,C to the grid and CL types
types necessary for building the method specializations and grid
declarations.

The mnemoninics correspond to three translations:

- 1. base foreign-array type, e.g., `complex-double-float' used for
  defining grid types, `vector-complex-double-float' and
  `matrix-complex-double-float' (These are eventually used as method
  specializers).
  
- 2. The type of elements stored in the grid ,e.g. complex
  double-float.  This correspondes to the element type column in
  section 2.2 of gsd/grid documentation. (These are used to define the
  type of elements stored in the grid when the grid is constructed in
  map-grid)

- 3. Supertype of the grid data type. (These are used as method
  specializers in two-argument methods, where one argument is a grid
  and another is a scalar).

Lisp implementations that use foreign-array (such as sbcl), the
vectors and arrays can be specialized fairly narrowly, such as
double-float, or signed-byte-32.

Lisp implementations that use the native CL arrays (such as clisp)
often cannot specialize an array as narrowly.  These then use more
generic declarations such as float or integer.


")


(defun foreign-array-vector-typedef (type)
  "Make a foreign array vector type definition by returning a symbol
with VECTOR- prepended to `type'"
  (let ((match (assoc type *type-dictionary*)))
    (assert match (type) "Type ~a was not found in dictionary" type)
    (intern (format nil "VECTOR-~a"
		    (first (cdr match))))))

(defun typedef (type &optional (parent nil))
  "Given one of the generic types !F,C,... return the cl type, or its
parent type (if `parent' is set to t)"
  (let ((match (assoc type *type-dictionary*)))
    (assert match (type) "Type ~a was not found in dictionary" type)
    (if parent (third (cdr match))
	(second (cdr match)))))



(defun cl-fun-doc (fun)
  "Create documentation for mapping function"
  (format nil "Map CL function ~a over a grid" fun))

(defun map-grid-call (function arg-name result-type)
  "Build the call to map grid, and embed into list (the latter part because of
the splicing in `add-grid-method'

These calls to map-grid use the same argument as the source
specification and the dimensions of the result specification."
  `((map-grid :source ,arg-name
	      :element-function #',function
	      :destination-specification
	      (list (append '(#+sbcl foreign-array
			      #+clisp array)
			    (dimensions ,arg-name))
		    ',(typedef result-type)))))

(defun map-n-grids-call (function source-def dimensions-ref result-type)
  "Build the call to map-n-grids, and embed into list (the latter part because of
the splicing in `add-grid-method'"
  `((map-n-grids :sources ,source-def
		 :combination-function #',function
		 :destination-specification
		 (list (append '(#+sbcl foreign-array
				 #+clisp array)
			       (dimensions ,dimensions-ref))
		       ',(typedef result-type)))))

(defun def-cl-grid-maps (declaration)
  "Use declaration to define the generalized function and associated
methods for a function of one argument."
  (let* ((fun (first declaration))
	 (lambda-list '(arg))
	 (gf (grid-gf lambda-list (cl-fun-doc fun)))
	 (i/o-decl (second declaration)))
    (labels ((add-this-grid-method (i/o-decl)
	       ;; Parse `i/o-decl' to create the specilizer, the
	       ;; `map-grid' call and then call `add-grid-method'
	       (destructuring-bind (arg-type &key return) i/o-decl
		 #+clisp (declare (ignore arg-type))
		 (let ((specializers (list #+sbcl(foreign-array-vector-typedef arg-type)
					   #+clisp 'array))
		       (body (map-grid-call fun 'arg return)))
		   (add-grid-method gf lambda-list specializers body)))))
      (if (consp (first i/o-decl))
	  (mapcar #'add-this-grid-method i/o-decl)
	  (add-this-grid-method i/o-decl)))
    (let ((grid-fun-name (grid-fun-name fun)))
      (setf (symbol-function grid-fun-name) gf))))


(defun def-cl-grid-maps-2 (declaration)
  "Use declaration to define the generalized function and associated
methods for functions of two arguments."
  (let* ((fun (first declaration))
	 (lambda-list '(arg1 arg2))
	 (gf (grid-gf lambda-list (cl-fun-doc fun)))
	 (i/o-decl (second declaration)))
    (labels
	((add-this-grid-method (i/o-decl)
	   (destructuring-bind (arg1 arg2 &key return) i/o-decl
	     ;; vector & scalar
	     (let ((specializers (list #+sbcl(foreign-array-vector-typedef arg1)
				       #+clisp 'array
				       (typedef arg2 t)))
		   (body (map-grid-call `(lambda (arg)
					   (,fun arg arg2))
					'arg1 return)))
	       (add-grid-method gf lambda-list specializers body))
	     ;; scalar & vector
	     (let ((specializers (list (typedef arg1 t) 
				       #+sbcl(foreign-array-vector-typedef arg2)
				       #+clisp 'array))
		   (body (map-grid-call `(lambda (arg)
					   (,fun arg1 arg))
					'arg2 return)))
	       (add-grid-method gf lambda-list specializers body))
	     ;; vector & vector
	     (let ((specializers (list #+sbcl(foreign-array-vector-typedef arg1)
				       #+clisp 'array
				       #+sbcl(foreign-array-vector-typedef arg2)
				       #+clisp 'array))
		   (body (map-n-grids-call `(lambda (arg1 arg2)
					      (,fun arg1 arg2))
					   '(list (list arg1 nil)
					     (list arg2 nil)) 'arg1 return)))
	       (add-grid-method gf lambda-list specializers body)))))
      (if (consp (first i/o-decl))
	  (mapcar #'add-this-grid-method i/o-decl)
	  (add-this-grid-method i/o-decl)))
    (let ((grid-fun-name (grid-fun-name fun)))
      (setf (symbol-function grid-fun-name) gf))))



(defun def-cl-grid-maps-2o (declaration)
  "Use declaration to define the generalized function and associated
methods for functions of two arguments, where the second argument is
optional.

According to hyperspec section 7.6.3, the method specialization is
done only on required parameters and not on optional ones.  sbcl seems
to extend the specialization to optional ones as well.

The code below conforms to hyperspec - the specialization is done only
on the first argument (vector or scalar).  Some of the subcases will
test the type of the optional parameter, if provided.  See code
comments."
  (let* ((fun (first declaration))
	 (lambda-list '(arg1 &optional arg2))
	 (gf (grid-gf lambda-list (cl-fun-doc fun)))
	 (i/o-decl (second declaration)))
    (labels
	((add-this-grid-method (i/o-decl)
	   ;; the only place where the optional second argument has
	   ;; effect is for vector & scalar or vector & vector
	   ;; combination.  See discussion below.
	   (destructuring-bind (arg1-decl arg2-decl &key return) i/o-decl
	     ;; vector & (scalar | vector)
	     #+clisp (declare (ignore arg1-decl))
	     (let ((specializers (list #+sbcl(foreign-array-vector-typedef arg1-decl)
				       #+clisp 'array))
		   (body
		    `(if arg2
			 (case (type-of arg2 ,(typedef arg2-decl))
			   (#+sbcl double-float #+clisp float
				   (map-grid :source arg1
					     :element-function #',fun
					     :destination-specification
					     (list (append '(#+sbcl foreign-array
							     #+clisp array)
							   (dimensions arg1))
						   ',(typedef return))))
			   (#+sbcl foreign-array #+clisp array
				   (map-n-grids :sources (list (list arg1 nil) (list arg2 nil))
						:combination-function #',fun
						:destination-specification
						(list (append '(#+sbcl foreign-array
								#+clisp array)
							      (dimensions arg1))
						      ',(typedef return))))
			   (t (error #+clisp "Second argument must be either a scalar or an array")))
			 (map-grid-call `(lambda (arg)
					   (,fun arg))
					'arg1 return))))
	       (add-grid-method gf lambda-list specializers body))
	     (let ((specializers (list #+sbcl(foreign-array-vector-typedef arg1-decl)
				       #+clisp 'array
				       #+sbcl(foreign-array-vector-typedef arg2)
				       #+clisp 'array))
		   (body (map-n-grids-call `(lambda (arg1 arg2)
					      (,fun arg1 arg2))
					   '(list (list arg1 nil)
					     (list arg2 nil)) 'arg1 return)))
	       (add-grid-method gf lambda-list specializers body)))))
      (if (consp (first i/o-decl))
	  (mapcar #'add-this-grid-method i/o-decl)
	  (add-this-grid-method i/o-decl)))
    (let ((grid-fun-name (grid-fun-name fun)))
      (setf (symbol-function grid-fun-name) gf))))





#|

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
      (export (list grid-fun-name)))))

|#
