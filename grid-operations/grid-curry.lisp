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

;; currying utilities to enable mapping of functions of several
;; variables over a single variable.
;;
;; To map (fun x y) to vec-x keeping y constant, use the following:
;;
;; (let ((y ...)
;;       (vec-x ...))
;;   (gcmap (fun @!x y) vec-x))

(in-package :mv-grid)

(export '(gcmap))

(define-test gcmap
    (assert-expands 
     '(gmap (mcurry pow @x y) vector)
     (gcmap (pow @x y) vector)))

(defmacro gcmap ((function &rest args) vector)
  "Map function over argument marked with @! with values of `vector'
keeping all other args constant.

A call (gcmap (fun x y @!q z) vector)
expands into
(gmap #'(lambda (q) (fun x y q z)) vector)"
  `(gmap (mcurry ,function ,@args) ,vector))


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
	      (push (if (my-utils:?!-symbol-p "@" arg)
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
  
