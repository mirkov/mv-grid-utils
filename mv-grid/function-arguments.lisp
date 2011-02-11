(in-package :mv-grid)

;; This file contains definitions for several types of grid mapping
;; functions:
;; - Mapping of CL functions on grids to produce new grids
;; - Reduce like functions (min, max) that operate on a grid to
;;   produce a result grid
;; - New convenience functions.
;;
;; Since grids are declared to store numbers of a specified type only,
;; the mapping functions operate only of grids of a predefined type.
;;
;; Many of CL's functions can accept floats, integers, or complex's.
;; We use methods with specializers to build the mapping functions.
;;

;; The function definitions are stored in parameters.  

;; The symbol macros are used to simplify the notation in the
;; definitions below
(define-symbol-macro !C '(complex double-float))
(define-symbol-macro !F 'double-float)
(define-symbol-macro !I '(signed-byte 32))



;;; Mapping of CL functions on grids

;; There are three types of function definitions.
;; - functions of one argument
;; - functions of two arguments
;; - functions of two arguments, with the second one optional
;;
;; All of these functions are mapped onto one or two grids and produce
;; a result grid.  In case of two argument functions, at least one of
;; the arguments must be a grid, while the other one can be a grid or
;; a scalar.
;;
;; (There is a class of of arbitrary number of arguments.  These are
;; discussed separately.)
;;
;;
;; We use methods with specializers to build mapping for functions of
;; two arguments
;;
;; The function definitions are of following types:
;;
;; - cl-function-name (like 1+): Build mappers for floats, integers and
;; complex and make result of the same type
;;
;; - (cl-function-name . (i/o decl)): Build mapper method(s) using i/o
;; declarations
;;
;; The i/o-decl is used to declare the input and output types and is
;; defined as follows:
;;
;; i/o-decl -> symbol | i/o-decl-list
;; symbol -> !I | !F | !C
;; i/o-decl-list -> (symbol) |
;;                  (symbol :return symbol) |
;;                  (i/o-decl-list i/o-decl-list ...)
;; 
;;
;; For two argument functions, the i/o-decl-list is similar, except 
;; i/o-decl-list -> (symbol) |
;;                  (symbol symbol :return symbol) | ...
;;
;; Some cl functions (sqrt, log, asin, acos) can return a complex
;; number for a floating or integer argument (negative for sqrt, log,
;; |arg|>1 for asin, acos).  For these, I define corresponding
;; convenience functions that always return complexes, irrespective or
;; the argument type.  See the appropriate file section.



(defparameter *one-arg-functions-float-args-only*
  '((cis !C))
  "Functions of one argument of real type.  Double float argument
  types are assumed unless specified otherwise")

(defparameter *one-arg-functions-other*
  '((sin . ((!I :return !F)
	   (!F :return !F)
	   (!C :return !C)))
    (cos . ((!I :return !F)
	    (!F :return !F)
	    (!C :return !C)))
    (tan . ((!I :return !F)
	    (!F :return !F)
	    (!C :return !C)))
    (asin . ((!I :return !F)
	 (!F :return !F)
	 (!C :return !C)))
    (acos . ((!I :return !F)
	 (!F :return !F)
	 (!C :return !C)))
    (sinh . ((!I :return !F)
	 (!F :return !F)
	 (!C :return !C)))
    (cosh . ((!I :return !F)
	 (!F :return !F)
	 (!C :return !C)))
    (tanh . ((!I :return !F)
	 (!F :return !F)
	 (!C :return !C)))
    (asinh . ((!I :return !F)
	 (!F :return !F)
	 (!C :return !C)))
    (acosh . ((!I :return !F)
	 (!F :return !F)
	 (!C :return !C)))
    (atanh . ((!I :return !F)
	 (!F :return !F)
	 (!C :return !C)))
    1+ 1-
    (abs . (:return !F))
    exp
    sqrt ;; sqrt of neg arguments are handled by a convenience function
    conjugate
    (isqrt . !I)
    (phase . ((!C :return !F)
	      (!F :return !F)))
    (realpart . ((!C :return !F)
		  (!F :return !F)))
    (imagpart . ((!C :return !F)
		 (!F :return !F)))
    (signum . ((!C :return !F)
	       (!F :return !F))))
  "Functions of one argument which may be of float or complex type.
The results are assumed of type of argument unless specified
otherwise")))

(defparameter *one&optional-second-arg-functions*
  '((atan . ((!F !F :return !F)
	     (!C !C :return !C)
	     (!F !C :return !C)
	     (!C !F :return !C)))
    (log . ((!F !F :return !F)
	    (!C !C :return !C)
	    (!F !C :return !C)
	    (!C !F :return !C)))
    (expt . ((!F !F :return !F)
	    (!C !C :return !C)
	    (!F !C :return !C)
	    (!C !F :return !C)))
    (floor . !I) (ffloor . !F)
    (ceiling . !I) (fceiling . !F)
    (truncate . !I) (ftruncate . !F)
    (round . !I) (fround . !F))
  "Functions of two arguments, where the second argument is optional.
The list specifies the argument types and result type.")

(defparameter *two-arg-functions*
  '((expt . ((!F !F :return !F)
	     (!C !C :return !C)
	     (!F !C :return !C)
	     (!C !F :return !C)))
    (mod . ((!F !F :return !F)))
    (rem . ((!F !F :return !F))))
  "Functions of two arguments, where the second argument is optional.
The list specifies the argument types and result type.")


;;; CL functions that operate on an arbitrary number of arguments

;; The following functions operate on an arbitrary number of grids

(defparameter *comparisons*
  '(= /=
    (> . ((!F)
	  (!I)))
    (>= . ((!F)
	   (!I)))
    (< . ((!F)
	  (!I)))
    (<= . ((!F)
	   (!I))))
  "Comparison functions that produce a cl-array of nils or t's.
Unless specified, the functions operate on integers, floats, or complexes.")

(defparameter *predicate-functions*
  '((minusp . ((!I) (!F)))
    (plusp . ((!I) (!F)))
    zerop
    (evenp . !I) (oddp . !I))
  "Functions that test each element of grid and return a congruent
  cl-array of booleans (t or nil).  Unless specified, the functions
  operate on integers, floats, or complexes.")

(defparameter *arithemtic-functions*
  '(+ - *
    (/ . ((!F :result !F)
	  (!I :result !F)
	  (!C :result !C))))
  "Operations of one or more arguments of one type, that produces a
  result of the same type (unless over-ridden)")



;;; Definitions of reduce-like functions

;; These functions operate on one or more grids at a time, and produce
;; scalar scalar of the reduced value.  They correspond strictly to
;; the spirit of the reduce operator.
(defparameter *reduce-one-grid*
  '(min max)
  "Apply CL function to a grid and return a single or multiple
  values")

;;; Convenience fuctions
;;
;; They are defined as an alist whose contents are
;; (grid-function-name.(function-definition))
;;
;; The function definition is:
;; function-definition -> (fun-def & rest)
;; fun-def -> (documentation-string & rest fun-forms)
;; documentation-string -> documents the function
;; fun-forms -> forms that make up the will be executed for each grid
;;              element.  The final form has to produce the
;;              computation result.
;; rest -> i/o-definition , i/o-definition, ...
;; i/o-definition -> (symbol :return symbol)
;; symbol -> !I | !F | !C

(defparameter *new-one-arg-functions* '
  ((grid-sqrt-fc . (("Calculates square roots of real numbers,
including negative ones, returning a complex number."
		     (sqrt arg))
		    (!F :return !C)))
   (grid-log-fc . (("Calculates natural logarithm of positive and
negative real numbers, returning a complex in all instances"
		    (log arg))
		   ((!F :return !C)
		    (!I :return !C))))
   (grid-asin-fc . (("Calculates inverse sin of real numbers and
return a complex result.  Accepts arguments whose magnitude exceeds
unity."
		     (asin arg))
		    (!F :return !C)
		    (!I :return !C)))
   (grid-acos-fc . (("Calculates inverse cos of real numbers and
return a complex result.  Accepts arguments whose magnitude exceeds
unity."
		     (acos arg))
		    (!F :return !C)
		    (!I :return !C)))
   (grid-log10 . (("Calculates logarithm to the base 10 of numbers.
For integer and float arguments, return a float, for complex
arguments, return a complex"
		   (log arg 10d0))
		  (!F :return !F)
		  (!I :return !F)
		  (!C :return !C)))
   (grid-log10-fc . (("Return logarithm to the base 10 of numbers and
return a complex result.  Accepts positive and negative integers and
floats."
		      (log arg 10d0))
		     (!F :return !C)
		     (!I :return !C)))
   (grid-log2 . (("Calculates logarithm to the base 2 of numbers.
For integer and float arguments, return a float, for complex
arguments, return a complex"
		  (log arg 2d0))
		 (!F :return !F)
		 (!I :return !F)
		 (!C :return !C)))
   (grid-log2-fc . (("Return logarithm to the base 2 of numbers
and return a complex result.  Accepts positive and negative integers
and floats."
		     (log arg 2d0))
		    (!F :return !C)
		    (!I :return !C))))
  "One-argument functions that do not exist in cl.  These are
  specialization or generalizations of cl's functions.")