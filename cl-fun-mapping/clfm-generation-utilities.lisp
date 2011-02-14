;; Mirko Vukovic
;; Time-stamp: <2011-02-14 16:58:22 clfm-generation-utilities.lisp>
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

- 1. base for foreign-array type, e.g., `complex-double-float' is a
  base for `vector-complex-double-float' vector class and
  `matrix-complex-double-float' matrix class (The latter two are
  eventually used as method specializers).
  
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
often cannot specialize an array as narrowly. clisp's arrays can store
any numeric type, no matter how they are declared.
")

(defun typedef (type &key vector parent)
  "Return CL number type corresponding to the mnemonic.  If `vector'
is `t' return the FFA grid type.  If `parent' is `t' return the parent
  type of the CL number type"
  (let ((match (assoc type *type-dictionary*)))
    (assert match (type) "Type ~a was not found in dictionary" type)
    (cond
      (vector (intern (format nil "VECTOR-~a"
		    (first (cdr match)))))
      (parent (third (cdr match)))
      (t (second (cdr match))))))

(define-test typedef
  (assert-equal
   #+sbcl 'vector-complex-double-float
   #+clisp 'vector-complex
   (typedef '!C :vector t))
  (assert-equal 'complex (typedef '!C :parent t))
  (assert-equal #+sbcl '(complex double-float)
		#+clisp 'complex (typedef '!C)))

(defun type-mnemonic (argument)
  "Return the mnemonic that describes the type of data
  in this argument"
  (typecase argument
    #+sbcl (vector-double-float '!F)
    #+clisp (vector '!F)
    #+sbcl (vector-complex-double-float '!C)
    #+sbcl (double-float '!F)
    #+clisp (float '!F)
    (complex '!C)
    (t (error "Argument type ~a is not supported" (type-of argument)))))


(defgeneric one-arg-gmap-ds (function arg &optional dictionary)
  (:documentation "Return the list that is keyword argument to
grid-map's destination-specification.

`function' describes the function that will be mapped
`arg' is the argument itself
`dictionary' points to a dictionary where necessary information may be found
")

  (:method ((function symbol) (arg #+ffa mvector #-ffa vector)
	    &optional (dictionary *one-arg-functions*))

    "Use the `function' symbol (such as `sin') and the argument type (such as '!F) to build the keyword argument :destination-specification"
    (let* ((arg-type (type-mnemonic arg))
	   (result-type (fun/x/-result-type function arg-type dictionary)))
      (list (append #+sbcl '(foreign-array) #+clisp '(array)
		    (dimensions arg))
	    #+sbcl
	    (typedef result-type)
	    #+clisp t))))

(define-test one-arg-gmap-ds
  (assert-equal #+sbcl '((foreign-array 3) double-float)
		#+clisp '((array 3) t)
		(one-arg-gmap-ds 'sin
				  #+sbcl #m(1d0 2d0 3d0)
				  #+clisp #(1d0 2d0 3d0)))
  (assert-equal #+sbcl '((foreign-array 2) (complex double-float))
		#+clisp '((array 2) t)
		  (one-arg-gmap-ds 'log
				   #+sbcl #2m(#C(1d0 2d0) #C(3d0 4d0))
				   #+clisp #2(#C(1d0 2d0) #C(3d0 4d0))
				   *one&optional-second-arg-functions*)))




(defgeneric two-arg-gmap-ds (function arg1 arg2 &optional dictionary)
  (:documentation "Return list that is keyword argument to a grid map
  function (either map-grid or map-n-grids) for a cl function of two
  arguments")
  (:method ((function symbol) (arg1 #-clisp mvector #+clisp vector) arg2 &optional
	    (dictionary *two-arg-functions*))
    (let* ((arg1-type (type-mnemonic arg1))
	   (arg2-type (type-mnemonic arg2))
	   (result-type (fun/xy/-result-type function arg1-type arg2-type
					     dictionary)))
      (list (append #+sbcl '(foreign-array) #+clisp '(array)
		    (dimensions arg1))
	    #+sbcl
	    (typedef result-type)
	    #+clisp t)))
  (:method ((function symbol) arg1 (arg2 #-clisp mvector #+clisp vector) &optional
	    (dictionary *two-arg-functions*))
    (let* ((arg1-type (type-mnemonic arg1))
	   (arg2-type (type-mnemonic arg2))
	   (result-type (fun/xy/-result-type function arg1-type arg2-type
					     dictionary)))
      (list (append #+sbcl '(foreign-array) #+clisp '(array)
		    (dimensions arg2))
	    #+sbcl
	    (typedef result-type)
	    #+clisp t)))
  (:method ((function symbol) (arg1 #-clisp mvector #+clisp vector) (arg2 #-clisp mvector #+clisp vector) &optional
	    (dictionary *two-arg-functions*))
    (let* ((arg1-type (type-mnemonic arg1))
	   (arg2-type (type-mnemonic arg2))
	   (result-type (fun/xy/-result-type function arg1-type arg2-type
					     dictionary)))
      (list (append #+sbcl '(foreign-array) #+clisp '(array)
		    (dimensions arg1))
	    #+sbcl
	    (typedef result-type)
	    #+clisp t))))

(define-test two-arg-gmap-ds
  (assert-equal #+sbcl '((foreign-array 3) double-float)
		#+clisp '((array 3) t)
		(two-arg-gmap-ds 'expt
				 #+sbcl #m(1d0 2d0 3d0)
				 #+clisp #(1d0 2d0 3d0)
				 2d0))
  (assert-equal #+sbcl '((foreign-array 3) double-float)
		#+clisp '((array 3) t)
		(two-arg-gmap-ds 'expt 2d0
				 #+sbcl #m(1d0 2d0 3d0)
				 #+clisp #(1d0 2d0 3d0)))
  (assert-equal #+sbcl '((foreign-array 3) (complex double-float))
		#+clisp '((array 3) t)
		(two-arg-gmap-ds 'expt
				  #+sbcl #2m(#C(1d0 1d0) #C(1d0 1d0) #C(1d0 1d0))
				  #+clisp #(#C(1d0 1d0) #C(1d0 1d0) #C(1d0 1d0))
				  #+sbcl #m(1d0 2d0 3d0)
				  #+clisp #(1d0 2d0 3d0)))
  (assert-equal #+sbcl '((foreign-array 2) (complex double-float))
		#+clisp '((array 2) t)
		  (two-arg-gmap-ds 'log
				   #+sbcl #2m(#C(1d0 2d0) #C(3d0 4d0))
				   #+clisp #(#C(1d0 2d0) #C(3d0 4d0))
				   3d0
		   *one&optional-second-arg-functions*)))


(defmacro one-arg-map-call (fun arg &optional dictionary)
  "Generate a call to `map-grid' applying `fun' on `arg'.

If `dictionary' is specified, it is included in the call to
`one-arg-gmap-ds'"
  `(map-grid :source ,arg
	      :element-function #',fun
	      :destination-specification
	      (one-arg-gmap-ds ',fun ,arg ,@(if dictionary `(,dictionary)))))

(define-test one-arg-map-call
  (assert-expands
   '(MAP-GRID :SOURCE ARG :ELEMENT-FUNCTION #'SIN
	     :DESTINATION-SPECIFICATION (ONE-ARG-GMAP-DS 'SIN ARG))
   (one-arg-map-call sin arg))
  (assert-expands
   '(MAP-GRID :SOURCE ARG :ELEMENT-FUNCTION #'SIN
     :DESTINATION-SPECIFICATION (ONE-ARG-GMAP-DS 'SIN ARG *foo*))
   (one-arg-map-call sin arg *foo*)))

(defmacro two-arg-map-call (fun (arg1 &optional vec1p) (arg2 &optional vec2p)
				 &optional dictionary)
  "Generate a call to `map-grid' applying `fun' on `arg'.

If `dictionary' is specified, it is included in the call to
`one-arg-gmap-ds'"
  (cond
    ((and vec1p (not vec2p))
     `(map-grid :source ,arg1
		:element-function #'(lambda (,arg1)
				      (,fun ,arg1 ,arg2))
		:destination-specification
		(two-arg-gmap-ds
		 ',fun ,arg1 ,arg2 ,@(if dictionary `(,dictionary)))))
    ((and (not vec1p) vec2p)
     `(map-grid :source ,arg2
		:element-function #'(lambda (,arg2)
				      (,fun ,arg1 ,arg2))
		:destination-specification
		(two-arg-gmap-ds
		 ',fun ,arg1 ,arg2 ,@(if dictionary `(,dictionary)))))
    ((and vec1p vec2p)
     `(map-n-grids :sources (list (list ,arg1 nil) (list ,arg2 nil))
		 :combination-function #'(lambda (,arg1 ,arg2)
					   (,fun ,arg1 ,arg2))
		 :destination-specification
		 (two-arg-gmap-ds
		  ',fun ,arg1 ,arg2
		  *one&optional-second-arg-functions*)))
    (t (error "Neither argument was specified as vector"))))

(define-test two-arg-map-call
  (assert-expands
      '(MAP-GRID :SOURCE ARG
	:ELEMENT-FUNCTION #'(LAMBDA (ARG)
			      (LOG ARG BASE))
	:DESTINATION-SPECIFICATION
	(TWO-ARG-GMAP-DS
	 'LOG ARG BASE
	 *ONE&OPTIONAL-SECOND-ARG-FUNCTIONS*))
      (two-arg-map-call log (arg t) (base)
			*one&optional-second-arg-functions* ))
  (assert-expands
    '(MAP-N-GRIDS :SOURCES (LIST (LIST ARG NIL) (LIST BASE NIL))
		 :COMBINATION-FUNCTION #'(LAMBDA (ARG BASE)
					   (LOG ARG BASE))
		 :DESTINATION-SPECIFICATION
		 (TWO-ARG-GMAP-DS
		  'LOG ARG BASE
		  *ONE&OPTIONAL-SECOND-ARG-FUNCTIONS*))
          (two-arg-map-call log (arg t) (base t)
			*one&optional-second-arg-functions*))
  (assert-expands
    '(MAP-GRID :SOURCE BASE
      :ELEMENT-FUNCTION #'(LAMBDA (BASE)
			    (LOG ARG BASE))
      :DESTINATION-SPECIFICATION
      (TWO-ARG-GMAP-DS
       'LOG ARG BASE
       *ONE&OPTIONAL-SECOND-ARG-FUNCTIONS*))
    (two-arg-map-call log (arg) (base t)
			*one&optional-second-arg-functions*)))

(defmacro def-one-arg-gmap-fun (fun-name fun arg &optional dictionary)
  `(defgeneric ,fun-name (,arg)
     (:documentation
      ,(format nil "~(~a~) function that accepts scalars and vectors" fun))
     (:method (,arg)
       "Fallback, unspecialized method"
       (,fun ,arg))
     (:method ((,arg #-clisp mvector #+clisp vector))
       ,(format nil "map `~(~a~)' over a vector grid" fun)
       (one-arg-map-call ,fun ,arg ,@(if dictionary `(,dictionary))))))

(define-test def-one-arg-gmap-fun
  (assert-expands
   '(defgeneric log%-one-arg (arg)
     (:documentation "log function that accepts scalars and vectors")
     (:method (arg) "Fallback, unspecialized method"
       (log arg))
     (:method ((arg #-clisp mvector #+clisp vector))
       "map `log' over a vector grid"
       (one-arg-map-call log arg *one&optional-second-arg-functions*)))
   (def-one-arg-gmap-fun log%-one-arg log arg
			 *one&optional-second-arg-functions* )))


(defmacro def-two-arg-gmap-fun (fun-name fun arg1 arg2 &optional dictionary)
  `(defgeneric ,fun-name (,arg1, arg2)
     (:documentation
      ,(format nil "Apply the `~(~a~)' function to ~(~a~) and ~(~a~), either or both of which can be vectors" fun arg1 arg2))
     (:method (,arg1 ,arg2)
       ,(format nil "(~(~a~) ~(~a~) ~(~a~)) where ~(~a~) and ~(~a~) are unspecialized"
		fun arg1 arg2 arg1 arg2)
       (,fun ,arg1 ,arg2))
     (:method ((,arg1 #-clisp mvector #+clisp vector) (,arg2 #+sbcl double-float #+clisp float))
       ,(format nil
		"(~(~a~) ~(~a~) ~(~a~)) where ~(~a~) is a vector and ~(~a~) a scalar"
		fun arg1 arg2 arg1 arg2)
       (two-arg-map-call ,fun (,arg1 t) (,arg2)
			 ,@(if dictionary `(,dictionary))))
     (:method ((,arg1 #-clisp mvector #+clisp vector) (,arg2 #-clisp mvector #+clisp vector))
       ,(format nil "(~(~a~) ~(~a~) ~(~a~)) where ~(~a~) and ~(~a~) are vectors"
		fun arg1 arg2  arg1 arg2)
       (two-arg-map-call ,fun (,arg1 t) (,arg2 t)
			 ,@(if dictionary `(,dictionary))))
     (:method ((,arg1 #+sbcl double-float #+clisp float) (,arg2 #-clisp mvector #+clisp vector))
       ,(format nil "(~(~a~) ~(~a~) ~(~a~)) where ~(~a~) is scalar and ~(~a~) is a vector"
		fun arg1 arg2  arg1 arg2)
       (two-arg-map-call ,fun (,arg1) (,arg2 t)
			 ,@(if dictionary `(,dictionary))))))

(define-test def-two-arg-gmap-fun
  (assert-expands
   '(defgeneric log%-two-arg (arg  base)
    (:documentation "Apply the `log' function to arg and base, either or both of which can be vectors")
    (:method (arg base)
      "(log arg base) where arg and base are unspecialized"
      (log arg base))
    (:method ((arg #-clisp mvector #+clisp vector) (base #+sbcl double-float #+clisp float))
      "(log arg base) where arg is a vector and base a scalar"
      (two-arg-map-call log (arg t) (base) *one&optional-second-arg-functions*))
    (:method ((arg #-clisp mvector #+clisp vector) (base #-clisp mvector #+clisp vector))
      "(log arg base) where arg and base are vectors"
      (two-arg-map-call log (arg t) (base t) *one&optional-second-arg-functions*))
    (:method ((arg #+sbcl double-float #+clisp float) (base #-clisp mvector #+clisp vector))
      "(log arg base) where arg is scalar and base is a vector"
      (two-arg-map-call log (arg) (base t) *one&optional-second-arg-functions*)))
   (def-two-arg-gmap-fun log%-two-arg log arg base
			 *one&optional-second-arg-functions*)))

(defmacro def-one-req-one-opt-args-gmap-fun
    (fun-name fun req-arg opt-arg &optional dictionary)
  "Define the interface function and the two generic functions (and
their methods) for mapping cl-functions of one required and one
optional argument over grids"
  (let ((fun-name-one-arg (intern (format nil "~a-ONE-ARG"
					  (symbol-name fun-name))))
	(fun-name-two-arg (intern (format nil "~a-TWO-ARG"
					  (symbol-name fun-name)))))
    `(progn
       (defun ,fun-name (,req-arg &optional ,opt-arg)
	 ,(format nil "Depending whether `~(~a~)' is provided, call the one or two
argument ~(~a~) function"
		  opt-arg fun-name)
	 (if ,opt-arg (,fun-name-two-arg ,req-arg ,opt-arg)
	     (,fun-name-one-arg ,req-arg)))
       (def-one-arg-gmap-fun ,fun-name-one-arg
	   ,fun ,req-arg ,@(if dictionary `(,dictionary)))
       (def-two-arg-gmap-fun
	   ,fun-name-two-arg ,fun ,req-arg  ,opt-arg
	   ,@(if dictionary `(,dictionary))))))


(define-test def-one-req-one-opt-args-gmap-fun
  (assert-expands
   '(progn
     (defun log% (arg &optional base)
       "Depending whether `base' is provided, call the one or two
argument log% function"
       (if base (log%-two-arg arg base) (log%-one-arg arg)))
     (def-one-arg-gmap-fun log%-one-arg
	 log arg *one&optional-second-arg-functions*)
     (def-two-arg-gmap-fun
	 log%-two-arg log arg  base
	 *one&optional-second-arg-functions*))
   (def-one-req-one-opt-args-gmap-fun
       log% log arg base *one&optional-second-arg-functions*)))

#|

|#
;;;; Local variables: 
;;;; change-log-default-name: "~/my-software-add-ons/my-lisp/mv-grid-utils/ChangeLog"
;;;; End: