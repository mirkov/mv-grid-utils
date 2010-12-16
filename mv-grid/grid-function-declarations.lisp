;;;; Declarations of common lisp functions and utility functions to
;;;; access the declarations.
;;;;
;;;; The declarations are used to generate the grid-mapping functions
;;;; and also the testing code.

(in-package :mv-grid)

(defparameter *one-arg-functions*
  '(sin cos tan asin acos atan
    sinh cosh tanh asinh acosh atanh
    1+ 1-
    abs
    log exp
    sqrt (isqrt (unsigned-byte 32))
    (cis (complex double-float)) conjugate
    (phase double-float) (realpart double-float) (imagpart double-float)
    (signum (complex double-float)))
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


;;; Code used to extract function names and argument types.  This code
;;; should be used instead of raw access.  So if I ever change the
;;; format of the variables, I will only have to change this code.
(defun cl-fun-name (arg)
  (car arg))
(defun cl-fun-arg-type (arg)
  (cadr arg))

(defmacro do-fundef ((fundefs) &body body)
  "Loop over function definitions, and bind cl function name to
`fun' and result type to `result-type'

The function definitions are typically global variables from the
grid-function-declarations.lisp file.  They consist of the cl function
name and the expected argument type.
"
  (let ((fun-def (gensym "FUN-DEF")))
    `(let (fun result-type)
     (dolist (,fun-def ,fundefs)
       (if (consp ,fun-def)
	   (setf fun (cl-fun-name ,fun-def)
		 result-type (cl-fun-arg-type ,fun-def))
	   (setf fun ,fun-def))
	 ,@body))))