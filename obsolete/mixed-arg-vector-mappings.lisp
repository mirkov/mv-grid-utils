;; Mirko Vukovic
;; Time-stamp: <2012-07-06 14:30:09 mixed-arg-vector-mappings.lisp>
;; 
;; Copyright 2012 Mirko Vukovic
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


;;; Utilities for mapping functions with mixed arguments: some
;;; arguments are scalars, others are vectors
(in-package :mv-grid)


(export '(gcmap gpmap gmap2d gmap-vl grid-bind))


(define-test mcurry
 (assert-expands
  '(lambda (arg-x) (pow arg-x arg-y arg-z))
  (mcurry pow @!arg-x arg-y arg-z))
 (assert-expands
  '(lambda (arg-y) (pow arg-x arg-y arg-z))
  (mcurry pow arg-x @!arg-y arg-z))
 (assert-expands
  '(lambda (arg-z) (pow arg-x arg-y arg-z))
  (mcurry pow arg-x arg-y @!arg-z)))
                   
(defmacro mcurry (fun &rest args)
  "Create lambda expression of one argument.  The argument is derived
from the only marked arg.  The function calls fun on all the args.

 (mcurry pow x @!y z)  expands into
                |
          +-----+---+
          |         |
 (lambda (y) (pow x y z))"
  (let (clean-args marked-arg)
    (mapc #'(lambda (arg)
	      (push (if (@!-symbol-p arg)
			(progn
			  (and marked-arg
			       (error "Found more than one marked symbol"))
			  (let ((cleaned-sym
				 (intern (subseq (symbol-name arg) 2))))
			    (setf marked-arg cleaned-sym)
			    cleaned-sym))
			arg)
		    clean-args))
	  args)
    `(lambda (,marked-arg)
       (,fun ,@(nreverse clean-args)))))


(define-test gcmap
    (assert-expands 
     '(gmap (mcurry pow @!x y) vector)
     (gcmap (pow @!x y) vector)))

(defmacro gcmap ((function &rest args) vector)
  "Map a function over arguments, all scalar, except one that is a
  vector.  Return a vector

FUNCTION - a function
ARGS - symbol list of arguments. One of the symbols must start with @!
VECTOR - an expression that evaluates into a vector

During the macro expansion, the @!-tagged argument will be
mapped-over.  For example, a call (gcmap (fun x y @!q z) vector)
expands into

 (gmap #'(lambda (q) (fun x y q z)) vector)
"
  `(gmap (mcurry ,function ,@args) ,vector))





(defun pow (x y)
  (expt x y))

(define-test pow-curry
  ;; Loop over first variable
  (let ((y 2))
    ;; Sanity check for explicit mapping
    (assert-numerical-equal
     #(0 1 4)
     (copy-to (gmap #'(lambda (x)
			(pow x y)) (indgen 3))))
    ;; Explicit vs. curry notation
    (assert-numerical-equal
     (copy-to (gmap #'(lambda (x)
			(pow x y)) (indgen 3)))
     (copy-to (gcmap (pow @!x y) (indgen 3)))))
  ;; Loop over second variable
  (let ((x 2))
    ;; Sanity check for explicit mapping
    (assert-numerical-equal
     #(1 2 4 8)
     (copy-to (gmap #'(lambda (y)
			(pow x y)) (indgen 4))))
    ;; Explicit vs. curry notation
    (assert-numerical-equal
     (copy-to (gmap #'(lambda (y)
			(pow x y)) (indgen 4)))
     (copy-to (gcmap (pow x @!y) (indgen 4))))))


(define-test gpmap
  (assert-expands
   '(MAP-N-GRIDS :SOURCES (LIST (LIST X NIL) (LIST Y NIL)) :COMBINATION-FUNCTION
 (LAMBDA (X Y) (FUNCALL #'EXPT X Y)))
   (gpmap (expt @!x @!y) x y))
  (assert-expands
   '(MAP-N-GRIDS :SOURCES (LIST (LIST (lseq 1 2 2) NIL) (LIST (lseq 1 3 2) NIL))
     :COMBINATION-FUNCTION (LAMBDA (X Y) (FUNCALL #'EXPT X Y)))
   (gpmap (expt @!x @!y) (lseq 1 2 2) (lseq 1 3 2)))
  (assert-expands
   '(MAP-N-GRIDS :SOURCES (LIST (LIST (lseq 1 2 2) NIL) (LIST (lseq 1 3 2) NIL))
     :COMBINATION-FUNCTION (LAMBDA (X Y) (FUNCALL #'FOO X Y Z)))
   (gpmap (foo @!x @!y z) (lseq 1 2 2) (lseq 1 3 2)))
  (assert-expands
   '(MAP-N-GRIDS :SOURCES (LIST (LIST (lseq 1 2 2) NIL) (LIST (lseq 1 3 2) NIL))
     :COMBINATION-FUNCTION (LAMBDA (X Y) (FUNCALL #'FOO Z X Y)))
   (gpmap (foo z @!x @!y) (lseq 1 2 2) (lseq 1 3 2)))
  (assert-expands
   '(MAP-N-GRIDS :SOURCES (LIST (LIST (lseq 1 2 2) NIL) (LIST (lseq 1 3 2) NIL))
     :COMBINATION-FUNCTION (LAMBDA (X Y) (FUNCALL #'FOO X Z Y)))
   (gpmap (foo @!x z @!y) (lseq 1 2 2) (lseq 1 3 2))))

(defmacro gpmap ((fun &rest args) &rest vectors)
  "Partial/Parallel map function over arguments marked with @! with
values of `vectors' keeping all other args constant.

The vectors can be CL or grid vectors

A call to (gpmap (fun @!x y @!z) vx vz)

expands into
 (map-n-grids :sources (list (list vx nil) (list vz nil))
              :combination-function #'(lambda (x z)
                                           (fun x y z)

"
  (let (#+needed clean-args
		 vector-args
		 cleaned-arg-list)
    (mapc #'(lambda (arg)
	      (if (or (@0-symbol-p arg)
		      (@1-symbol-p arg))
		  (let ((cleaned-sym
			 (intern (subseq (symbol-name arg) 2))))
		    (push cleaned-sym vector-args)
		    (push cleaned-sym cleaned-arg-list))
		  (progn
		    #+needed (push arg clean-args)
		    (push arg cleaned-arg-list))))
	  args)
    (let ((c-vector-args (length vector-args)))
      (assert (= c-vector-args (length vectors))
	      () "Mismatch between number of vector arguments ~a 
and marked arguments ~a" vectors vector-args)
      (let ((sources `(list ,@(mapcar (lambda (vector)
					`(list ,vector
					#|  ,(if (atom vector)
					       vector
					       (eval vector))|#
					  nil))
					vectors)))
	    (combination-function
	     `(lambda (,@(nreverse vector-args))
		(funcall #',fun ,@(nreverse cleaned-arg-list)))))
	`(map-n-grids :sources ,sources 
		      :combination-function ,combination-function)))))



