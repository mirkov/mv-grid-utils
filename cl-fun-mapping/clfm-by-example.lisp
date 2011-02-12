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

;;;; This file explains and documents the functions and macros used to
;;;; define grid-mapping methods on CL numeric functions.
;; For the user, the most important macros demonstrated here are:
;;  - def-one-arg-gmap-fun
;;  - def-two-arg-gmap-fun
;;  - def-one-req-one-opt-args-gamp-fun
(in-package :mv-grid)
;; We consider functions of one and two arguments, including
;; functions of one required and one optional argument.
;;
;; We belive there is not a convincing need to specialize on the exact
;; variable type, and that instead, specialization on the rank is
;; sufficient.  This should help expand the code to non-ffi platforms,
;; such as clisp.
;;
;; We start by explicity defining a mapping method for a function of
;; one argument, using a full-blown (grid-map ... ) call.  We then
;; introduce several helper functions that help us reduce the size of
;; this definition from 20+ to 10 or so lines.  The use of each
;; function is demostrated by an example, and each example includes
;; unit-tests.  The final example demonstrates the use of the public
;; interface.
;;
;; After finishing a one-argument mapping, using `sin', we turn to a
;; mapping of a function of one required and one optional argument,
;; `log'.  As for `sin', we start with a fully explicit definition,
;; and then again introduce, by examples, various helper functions
;; that help us condense the code considerably.  Each example is
;; followed by a unit-test.  Again, the final example demonstrates the
;; use of the public interface.
;;
;; The naming convention uses `%'.  The least concise examples have
;; most %, while the final one has only one %.
;;
;; 



;;; Function of one argument, using `sin'

;; An explicit definition
(defgeneric sin%%%%% (arg)
  (:documentation "sin function that accepts float, integer or complex
  grids")
  (:method (arg)
    "Apply sin to any argument"
    (sin arg))
  (:method ((arg vector-double-float))
    "apply `sin' to grid of double floats and return double-float"
    (map-grid :source arg
	      :element-function #'sin
	      :destination-specification (list (append '(foreign-array)
						     (dimensions arg))
					       'double-float)))
  (:method ((arg vector-complex-double-float))
    "apply `sin' to grid of complex double-floats and return grid of
complex double-floats"
    (map-grid :source arg
	      :element-function #'sin
	      :destination-specification (list (append '(foreign-array)
						     (dimensions arg))
					       '(complex double-float)))))

(define-test sin%%%%%
  (assert-numerical-equal
   (sin 2d0) (sin%%%%% 2d0))
  (assert-numerical-equal
   (copy-to (sin%%%%% *+1-vector*))
   (map-grid>cl #'sin *+1-vector*))
  (assert-numerical-equal
   (copy-to (sin%%%%% *complex-vector*))
   (map-grid>cl #'sin *complex-vector*)))



(define-test sin%%%%%
  (assert-numerical-equal
   (sin 2d0) (sin%%%%% 2d0))
  (assert-gmap-equal sin%%%%% sin *+1-vector*)
  (assert-gmap-equal sin%%%%% sin *complex-vector*))


;; We simplify the definition a bit by using the `one-arg-gmap-gs' to
;; define map-grid's  :destination-specification keyword:

(defgeneric sin%%%% (arg)
  (:documentation "sin function that accepts float, integer or complex
  grids")
  (:method (arg)
    "Apply sin to any argument"
    (sin arg))
  (:method ((arg vector-double-float))
    "apply `sin' to grid of double floats and return double-float"
    (map-grid :source arg
	      :element-function #'sin
	      :destination-specification
	      (one-arg-gmap-ds 'sin arg)))
  (:method ((arg vector-complex-double-float))
    "apply `sin' to grid of complex double-floats and return grid of
complex double-floats"
    (map-grid :source arg
	      :element-function #'sin
	      :destination-specification
	      (one-arg-gmap-ds 'sin arg))))

(define-test sin%%%%
  (assert-numerical-equal (sin 2d0) (sin%%%% 2d0))
  (assert-gmap-equal sin%%%% sin *+1-vector*)
  (assert-gmap-equal sin%%%% sin *complex-vector*))


;; Note that the two methods are identical.  Thus, there is no point
;; in specializing on vector-double-float or
;; vector-complex-double-float.  Instead I can specialize on the rank.
;; 
;; In case of foreign arrays, we use grid's superclasses `mvector' and
;; `matrix'.
(defgeneric sin%%% (arg)
  (:documentation "sin function that accepts float, complex
  grids.")
  (:method (arg)
    "Apply sin to any argument"
    (sin arg))
  (:method ((arg mvector))
    "apply `sin' to grid of double floats and return double-float"
    (map-grid :source arg
	      :element-function #'sin
	      :destination-specification
	      (one-arg-gmap-ds 'sin arg))))

(define-test sin%%%
  (assert-numerical-equal (sin 2d0) (sin%%% 2d0))
  (assert-gmap-equal sin%%% sin *+1-vector*)
  (assert-gmap-equal sin%%% sin *complex-vector*))

;; Finally, we may simplify this further by defining a macro that will
;; expand into map-grid

(defgeneric sin%% (arg)
  (:documentation "sin function that accepts float, complex
  grids.")
  (:method (arg)
    "Apply sin to any argument"
    (sin arg))
  (:method ((arg mvector))
    "apply `sin' to grid of double floats and return double-float"
    (one-arg-map-call sin arg)))


(define-test sin%%
  (assert-numerical-equal (sin 2d0) (sin%% 2d0))
  (assert-gmap-equal sin%% sin *+1-vector*)
  (assert-gmap-equal sin%% sin *complex-vector*))


;; This can be condensed by using a macro that defines mapping over
;; one-argument functions.  This macro is part of the user interface -
;; this is how users define mappings over functions of one argument
(def-one-arg-gmap-fun sin% sin arg)

(define-test sin%
  (assert-numerical-equal (sin 2d0) (sin% 2d0))
  (assert-gmap-equal sin% sin *+1-vector*)
  (assert-gmap-equal sin% sin *complex-vector*))

;;; Example on functions with one required and one optional argument
;;; such as log or atan.  This example will also demonstrate how to
;;; build mappings for functions of two arguments.

;; The CL standard does not allow specialization on optional
;; arguments.  For these functions I cannot write front-end methods.
;; Instead I use a front-end function with one optional and one
;; required argument.  Depending on the number of passed arguments, it
;; will call apropriate methods the specialize on one or two
;; arguments.
;;
;; Since we have demonstrated the use of `one-arg-map-call' for `sin'
;; mapping, we use it and the `two-arg-map-call' in the code below.

(defun log%%% (arg &optional base)
  "Depending whether `base' is is provided, call the one or two
argument log-grid function"
  (if base (log%%%-two-arg arg base) (log%%%-one-arg arg)))

(defgeneric log%%%-one-arg (arg)
  (:documentation "log function that accepts float, integer or complex
  grids")
  (:method (arg)
    "Unspecialized method that can accept scalars"
    (log arg))
  (:method ((arg mvector))
    "apply `log' to grid of double floats and return double-float"
    (one-arg-map-call log arg *one&optional-second-arg-functions*)))

;; I specialize on various compbinations of scalars & vectors
(defgeneric log%%%-two-arg (arg  base)
  (:documentation "log function of log argument and base.  The
  argument and base can be either scalars or grid vector
  double-floats.")
  (:method (arg base)
    (log arg base))
  (:method ((arg mvector) (base double-float))
    "apply `log' to grid of double floats and return double-float"
    (two-arg-map-call log (arg t) (base) *one&optional-second-arg-functions*))
  (:method ((arg mvector) (base mvector))
    "apply `log' to grid of complex double-floats and return grid of
complex double-floats"
    (two-arg-map-call log (arg t) (base t) *one&optional-second-arg-functions*))
  (:method ((arg double-float) (base mvector))
    (two-arg-map-call log (arg) (base t) *one&optional-second-arg-functions*)))



	  
	  

(define-test log%%%
  (assert-gmap-equal log%%% log *+1-vector*)
  (assert-gmap-equal log%%% log *complex-vector*)
  (assert-numerical-equal (log%%% 2d0) (log 2d0))
  (assert-gmap2-equal log%%% log *+2-vector* 2d0)
  (assert-gmap2-equal log%%% log 2d0 *+2-vector*)
  (assert-gmap2-equal log%%% log *+2-vector* *+2-vector*))



;; We condense the definitions by using macros that expand into the
;; defgeneric form.

(defun log%% (arg &optional base)
  "Depending whether `base' is is provided, call the one or two
argument log-grid function"
  (if base (log%%-two-arg arg base) (log%%-one-arg arg)))

(def-one-arg-gmap-fun log%%-one-arg
    log arg *one&optional-second-arg-functions*)

(def-two-arg-gmap-fun
    log%%-two-arg log arg base
    *one&optional-second-arg-functions*)

(define-test log%%
  (assert-gmap-equal log%% log *+1-vector*)
  (assert-gmap-equal log%% log *complex-vector*)
  (assert-numerical-equal (log%% 2d0) (log 2d0))
  (assert-gmap2-equal log%% log *+2-vector* 2d0)
  (assert-gmap2-equal log%% log 2d0 *+2-vector*)
  (assert-gmap2-equal log%% log *+2-vector* *+2-vector*))


;; We finally demonstrate the way for users to define mappings for
;; functions of one required and one optional argument:

(def-one-req-one-opt-args-gmap-fun
    log% log arg base
    *one&optional-second-arg-functions* )

(define-test log%
  (assert-gmap-equal log% log *+1-vector*)
  (assert-gmap-equal log% log *complex-vector*)
  (assert-numerical-equal (log% 2d0) (log 2d0))
  (assert-gmap2-equal log% log *+2-vector* 2d0)
  (assert-gmap2-equal log% log 2d0 *+2-vector*)
  (assert-gmap2-equal log% log *+2-vector* *+2-vector*))


