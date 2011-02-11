;; Mirko Vukovic
;; Time-stamp: <2011-02-10 07:20:14 grid-function-generators-unit-tests.lisp>
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

;;; Tests of declaring the generic functions and adding methods

(define-test make-gf
  (let ((gf (grid-gf '(arg) "foo")))
    (assert-equal 'closer-mop:standard-generic-function
		  (type-of gf))))


(define-test add-grid-method
  (assert-equal 2d0
		(let ((gf (grid-gf '(x) "foo")))
		  (add-grid-method gf '(x) #+sbcl '(double-float) #+clisp '(float)
				   '((sqrt x)))
		  (funcall gf 4d0))))



;;; Tests of creation of grid mapping functions.  This tests whether
;;; the grid-functions are defined and if they run without errors.
;;; They do not check numerical accuracy.  That is done by tests in
;;; another file (grid-functions-unit-tests)

;; functions of one argument (isqrt)
(define-test def-cl-grid-maps
  (assert-true
   (progn 
     (def-cl-grid-maps (assoc 'sin *one-arg-functions*))
     (grid-sin (findgen 5)))
   "One-argument-function creation and execution"))

;; functions of two arguments (mod)
(define-test def-cl-grid-maps-2
  (progn
    (def-cl-grid-maps-2 (second *two-arg-functions*))
    (assert-true (grid-mod (findgen 10) 3d0) "Two arg, vector/scalar")
    (assert-true (grid-mod 10d0 (grid-1+ (findgen 5))) "Two arg, scalar/vector")
    (assert-true (grid-mod *-1x-+1x-vector*
			 (grid-1+ *-1x-+1x-vector*))
	       "Two arg, vector/vector")))

