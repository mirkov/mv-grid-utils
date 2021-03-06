;; Mirko Vukovic
;; Time-stamp: <2011-02-10 06:41:07 mv-gpl-header.txt>
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


;;;; Declarations of common lisp functions and utility functions to
;;;; access the declarations.
;;;;
;;;; The declarations are used to generate the grid-mapping functions
;;;; They can also be used for testing these functions.

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
;; We use methods with specializers to build mapping for functions of
;; two arguments
;;
;; The function definitions syntax is as follows:
;;
;; - (cl-function-name i/o-decl i/o-decl ... )
;; - i/o-decl -> symbol | (symbol symbol) | ((symbol symbol) symbol)
;; - symbol -> !I | !F | !C
;; 
;; Syntax examples:
;;
;; - (1+ !F !C !I) : The single argument function `1+' accepts floats,
;;                   complexes and integers, and returns the same type
;; - (realpart     : Single argument function `realpart'
;;      (!F  !F)     for a float returns a float and for a complex returns
;;      (!C  !F))    a float
;; - (expt     	     Two argument function `expt' accepts four combinations
;;     ((!F !F) !F)  of floats and complexes, and returns floats only if 	;;     ((!C !C) !C)  both arguments are floats.  Otherwise it returns   	;;     ((!F !C) !C)  a complex	       	       	       	       	       	     
;;     ((!C !F) !C))





(defparameter *one-arg-functions*
  '((cis  (!F  !C))
    (sin ;;(!U  !F)
	  ;;(!I  !F)
	  (!F  !F)
	  (!C  !C))
    (cos  ;;(!I  !F)
	  (!F  !F)
	  (!C  !C))
    (tan  ;;(!I  !F)
	  (!F  !F)
	  (!C  !C))
    (asin  ;;(!I  !F)
	   (!F  !F)
	   (!C  !C))
    (acos  ;;(!I  !F)
	   (!F  !F)
	   (!C  !C))
    (sinh  ;;(!I  !F)
	   (!F  !F)
	   (!C  !C))
    (cosh  ;;(!I  !F)
	   (!F  !F)
	   (!C  !C))
    (tanh  ;;(!I  !F)
	   (!F  !F)
	   (!C  !C))
    (asinh  ;;(!I  !F)
	    (!F  !F)
	    (!C  !C))
    (acosh  ;;(!I  !F)
	    (!F  !F)
	    (!C  !C))
    (atanh  ;;(!I  !F)
	    (!F  !F)
	    (!C  !C))
    (1+ !F !C !I) (1- !F !I !C)
    (abs (!F  !F)
	  (!C  !F))
    (exp (!F  !F)
	  (!C  !C))
    ;; sqrt of neg arguments is handled by a convenience function
    (sqrt (!F  !F)
	   (!C  !C))
    (conjugate (!F  !F)
		(!C  !F))
    (isqrt !U  !U)
    (phase (!C  !F)
	    (!F  !F))
    (realpart (!C  !F)
	       (!F  !F))
    (imagpart (!C  !F)
	       (!F  !F))
    (signum (!C  !F)
	     (!F  !F)))
  "Type declarations for functions of one argument")



(defparameter *two-arg-functions*
  '((expt
     ((!F !F) !F)
     ((!C !C) !C)
     ((!F !C) !C)
     ((!C !F) !C))
    (mod  ((!F !F) !F))
    (rem  ((!F !F) !F)))
  "Type declarations for functions of two arguments")




(defparameter *one&optional-second-arg-functions*
  '((atan
     (!F !F)
     (!C !C)
     ((!F !F) !F)
     ((!C !C) !C)
     ((!F !C) !C)
     ((!C !F) !C))
    (log
     (!F !F)
     (!C !C)
     ((!F !F) !F)
     ((!C !C) !C)
     ((!F !C) !C)
     ((!C !F) !C))

    #|(floor . ((!I !I :return !I)
    (!I !F :return !I)
    (!F !I :return !I)
    (!F !F :return !I)))
    (ceiling . ((!I !I :return !I)
		(!I !F :return !I)
		(!F !I :return !I)
		(!F !F :return !I)))
    (truncate . ((!I !I :return !I)
		 (!I !F :return !I)
		 (!F !I :return !I)
		 (!F !F :return !I)))
    (round . ((!I !I :return !I)
	      (!I !F :return !I)
	      (!F !I :return !I)
	      (!F !F :return !I)))
    (ffloor . ((!I !I :return !F)
	       (!I !F :return !F)
	       (!F !I :return !F)
	       (!F !F :return !F)))
    (fceiling . ((!I !I :return !F)
		 (!I !F :return !F)
		 (!F !I :return !F)
		 (!F !F :return !F)))
    (ftruncate . ((!I !I :return !F)
		  (!I !F :return !F)
		  (!F !I :return !F)
		  (!F !F :return !F)))
    (fround . ((!I !I :return !F)
	       (!I !F :return !F)
	       (!F !I :return !F)
	       (!F !F :return !F)))|#)

  
  "Type declarations for functions of one required and one optional
  argument")


;;; The rest of the file consists of declarations that are not
;;; currently implemented.  They will be removed from the comment
;;; block as they are implemented.  The syntax has not been fixed.
#|

(defparameter *comparisons*
  '(= /=
    (> . ((!F)))
	  ;;(!I)))
    (>= . ((!F)))
	   ;;(!I)))
    (< . ((!F)))
	  ;;(!I)))
    (<= . ((!F))))
	   ;;(!I))))
  "Comparison functions that produce a cl-array of nils or t's.
Unless specified, the functions operate on integers, floats, or complexes.")

(defparameter *predicate-functions*
  '((minusp . (;;(!I)
	       (!F)))
    (plusp . (;;(!I)
	      (!F)))
    zerop
    (evenp . !I) (oddp . !I))
  "Functions that test each element of grid and return a congruent
  cl-array of booleans (t or nil).  Unless specified, the functions
  operate on integers, floats, or complexes.")

(defparameter *arithemtic-functions*
  '(+ - *
    (/ . ((!F :result !F)
	  ;;(!I :result !F)
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

(defparameter *new-one-arg-functions*
  '((grid-sqrt-fc . (("Calculates square roots of real numbers,
including negative ones, returning a complex number."
		     (sqrt arg))
		    (!F :return !C)))
    (grid-log-fc . (("Calculates natural logarithm of positive and
negative real numbers, returning a complex in all instances"
		    (log arg))
		    ((!F :return !C))))
		    ;;;;(!I :return !C))))
    (grid-asin-fc . (("Calculates inverse sin of real numbers and
return a complex result.  Accepts arguments whose magnitude exceeds
unity."
		     (asin arg))
		    (!F :return !C)))
		    ;;;;(!I :return !C)))
   (grid-acos-fc . (("Calculates inverse cos of real numbers and
return a complex result.  Accepts arguments whose magnitude exceeds
unity."
		     (acos arg))
		    (!F :return !C)))
		    ;;;;(!I :return !C)))
   (grid-log10 . (("Calculates logarithm to the base 10 of numbers.
For integer and float arguments, return a float, for complex
arguments, return a complex"
		   (log arg 10d0))
		  (!F :return !F)
		  ;;;;(!I :return !F)
		  (!C :return !C)))
   (grid-log10-fc . (("Return logarithm to the base 10 of numbers and
return a complex result.  Accepts positive and negative integers and
floats."
		      (log arg 10d0))
		     (!F :return !C)))
		     ;;;;(!I :return !C)))
   (grid-log2 . (("Calculates logarithm to the base 2 of numbers.
For integer and float arguments, return a float, for complex
arguments, return a complex"
		  (log arg 2d0))
		 (!F :return !F)
		 ;;;;(!I :return !F)
		 (!C :return !C)))
   (grid-log2-fc . (("Return logarithm to the base 2 of numbers
and return a complex result.  Accepts positive and negative integers
and floats."
		     (log arg 2d0))
		    (!F :return !C))))
		    ;;;;(!I :return !C))))
  "One-argument functions that do not exist in cl.  These are
  specialization or generalizations of cl's functions.")


|#

;;; Code used to extract function names and argument types.  This code
;;; should be used instead of raw access.  So if I ever change the
;;; format of the variables, I will only have to change this code.

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun decl>cl-fun (arg)
    (car arg))
  (defun decl>i/o-decls (arg)
    (rest arg)))

(defmacro do-fundef ((fun i/o-decl fundefs) &body body)
  "Loop over function definitions, and bind cl function name to
`fun' and result type to `result-type'

The function definitions are typically global variables from the
grid-function-declarations.lisp file.  They consist of the cl function
name and the expected argument type.
"
  (let ((fun-def (gensym "FUN-DEF")))
    `(let (fun result-type)
     (dolist (,fun-def ,fundefs)
       (setf ,fun (decl>cl-fun ,fun-def)
	     ,i/o-decl (decl>i/o/decls ,fun-def))
       ,@body))))

;;; Utility functions

(defun fun-decl (fun dictionary)
  "Retreive the function's declaration from dictionary

Signal error if function not found"
  (let ((decl (assoc fun dictionary)))
    (or decl
	(error "Function ~a not found in dictionary ~a" fun dictionary))
    (rest decl)))

(defgeneric fun/x/-result-type (fun arg &optional dictionary)
  (:documentation "Return `fun'ction's result type for `arg' using
  `dictionary' if necessary (some methods may not require the
  dictionary)")
  (:method ((decl list) (arg symbol) &optional dictionary)
    (declare (ignore dictionary))
    ;; Fine the matching i/o declaration in the dictionary
    (let ((matching-declaration
	   (find arg decl :test
		 #'(lambda (arg item)
		     (equal arg (if (consp item)
				    (car item)
				    item))))))
      (or matching-declaration
	  (error "Argument type ~a not found in declaration ~a"
		 arg decl))
      ;; extract result type
      (if (consp matching-declaration)
	  (second matching-declaration)
	  matching-declaration)))
  (:method ((fun symbol) (arg symbol)
	    &optional (dictionary *one-arg-functions*))
    (fun/x/-result-type (fun-decl fun dictionary) arg
			dictionary)))



(define-test fun/x/result-type
  (assert-equal '!F (fun/x/-result-type '(!F !C !I) '!F))
  (assert-equal '!C (fun/x/-result-type '1+ '!C))
  (assert-equal
   '!F (fun/x/-result-type '((!F !F) (!C !C))
			   '!F))
  (assert-equal
   '!C (fun/x/-result-type '((!F !F) (!C !C))
			   '!C))
  (assert-equal '!F (fun/x/-result-type 'sin '!F))
  (assert-equal '!F (fun/x/-result-type 'log '!F
					*one&optional-second-arg-functions*)))

(defgeneric fun/xy/-result-type (fun arg1 arg2 &optional dictionary)
  (:documentation "Return `fun'ction's result type for `arg1' and
 `arg2' using `dictionary' if necessary (some methods may not require
 the dictionary)")
  (:method ((decl list) (arg1 symbol) (arg2 symbol) &optional dictionary)
    (declare (ignore dictionary))
    (second (find (list arg1 arg2) decl :key #'car :test #'equal)))
  (:method ((fun symbol) (arg1 symbol) (arg2 symbol)
	    &optional (dictionary *one-arg-functions*))
    (fun/xy/-result-type (fun-decl fun dictionary) arg1 arg2 dictionary)))



(define-test fun/xy/result-type
  (assert-equal
   '!F (fun/xy/-result-type '(((!F !F) !F)
			      ((!C !C) !C))
			    '!F '!F))
  (assert-equal '!F (fun/xy/-result-type 'log '!F '!F
					 *one&optional-second-arg-functions*)))

 
