;; Mirko Vukovic
;; Time-stamp: <2011-03-05 19:57:12 grid-2d-mappers.lisp>
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

(export '(gcmap gmap2d gmap-vl))

(define-test gcmap
    (assert-expands 
     '(gmap (mcurry pow @!x y) vector)
     (gcmap (pow @!x y) vector)))

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
                   

(defun ?!-symbol-p (? s)
  "Does the symbol start with `?!' where `?' stands for a single
character?"
  (check-type ? string)
  (check-type s symbol)
  (and (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
		(format nil "~a!" ?)
		:start1 0
		:end1 2)))

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
	      (push (if (?!-symbol-p "@" arg)
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
  

;;;; 2d mapping over either two vectors or a vector and a list
;;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun @0-symbol-p (s)
    "Does the symbol start with `@0'.  Return nil if s is not a symbol"
    (typecase s
      (symbol
       (and (> (length (symbol-name s)) 2)
	    (string= (symbol-name s)
		     "@0"
		     :start1 0
		     :end1 2)))
      (t nil))))

(define-test @0-symbol-p
  (assert-true (@0-symbol-p '@0foo))
  (assert-true (not (@0-symbol-p '@1foo)))
  (assert-true (not (@0-symbol-p 3))))
  


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun @1-symbol-p (s)
    "Does the symbol start with `@1'.  Return nil if s is not a symbol"
    (typecase s
      (symbol
       (and (> (length (symbol-name s)) 2)
	    (string= (symbol-name s)
		     "@1"
		     :start1 0
		     :end1 2)))
      (t nil))))

(define-test @1-symbol-p
  (assert-true (@1-symbol-p '@1foo))
  (assert-true (not (@1-symbol-p '@2foo)))
  (assert-true (not (@1-symbol-p 3))))


(defmacro gmap2d ((fun &rest args) vec0 vec1)
  "Return a matrix of dimensions (dim0 vec0) (dim0 vec1) filled by
applying `fun' to `args'.

`args' must have two arguments whose names start by @0 and @1.  In the
calls to `fun' the elements of arguments @0 and @1 are accessed using
the row and column indices respectively

The other arguments are passed unevaluated"
  (let (funcall-args arg0 arg1)
    (mapc
     ;; loop over args and create a cleaned up argument list by
     ;; stripping the @ tags.  At the same time, process the @-tagged
     ;; args as follows:
     ;; - create a (gref ...) snippet that will be used to access
     ;;   their elements
     ;; - Make sure there is only one of @0 and @1 tags present.  This
     ;;   is done by examining contents of arg0 and arg1 which store
     ;;   the cleaned up @0 and @1 args.
     #'(lambda (arg)
	 (cond
	   ((@0-symbol-p arg)
	    (and arg0
		 (error "Found more than one marked symbol for row scan"))
	    (let ((cleaned-sym
		   (intern (subseq (symbol-name arg) 2))))
	      (setf arg0 cleaned-sym)
	      (push `(gref ,cleaned-sym row) funcall-args)))
	   ((@1-symbol-p arg)
	    (and arg1
		 (error "Found more than one marked symbol for column scan"))
	    (let ((cleaned-sym
		   (intern (subseq (symbol-name arg) 2))))
	      (setf arg1 cleaned-sym)
	      (push `(gref ,cleaned-sym column) funcall-args)))
	   (t (push arg funcall-args))))
	  args)
    (assert arg0 () "Vector 0 is undefined")
    (assert arg1 () "Vector 1 is undefined")
    `(let ((,arg0 ,vec0)
	   (,arg1 ,vec1))
       (let ((d0 (dim0 ,arg0))
	     (d1 (dim0 ,arg1)))
	 (let ((res (make-grid (list (list *array-type* d0 d1)
				     *float-type*))))
	   (iter:iter
	    (iter:for row :matrix-row-index res)
	    (iter:iter
	     (iter:for column  :matrix-column-index res)
	       (setf (gref res row column)
		     (,fun ,@(nreverse funcall-args)))))
	   res)))))

(define-test gmap2d
  (assert-expands
   '(let ((x x)
	  (y y))
     (let ((d0 (dim0 x))
	   (d1 (dim0 y)))
       (let ((res (make-grid (list (list *array-type* d0 d1)
				   *float-type*))))
	 (iter:iter
	   (iter:for row :matrix-row-index res)
	   (iter:iter
	     (iter:for column  :matrix-column-index res)
	     (setf (gref  res row column)
		   (fun
		    (gref x row)
		    (gref y column)
		    z))))
	 res)))
   (gmap2d (fun @0x @1y z) x y))
  (assert-numerical-equal
   #2A((1 1 1) (2 4 8))
   (gmap2d (expt @0b @1x) #(1 2) #(1 2 3)))
  (labels ((fun (x y z)
	     (+ z (expt x y))))
    (assert-numerical-equal
     #2A((0 0 0) (1 3 7))
     (gmap2d (fun @0b @1x -1) #(1 2) #(1 2 3)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun @.-symbol-p (s)
    "Does the symbol start with `@.'.  Return nil if s is not a symbol"
    (typecase s
      (symbol
       (and (> (length (symbol-name s)) 2)
	    (string= (symbol-name s)
		     "@."
		     :start1 0
		     :end1 2)))
      (t nil)))

    (defun @#-symbol-p (s)
    "Does the symbol start with `@#'.  Return nil if s is not a symbol"
    (typecase s
      (symbol
       (and (> (length (symbol-name s)) 2)
	    (string= (symbol-name s)
		     "@#"
		     :start1 0
		     :end1 2)))
      (t nil))))


(define-test @#-symbol-p
  (assert-true (@#-symbol-p '@#foo))
  (assert-true (not (@#-symbol-p '@2foo)))
  (assert-true (not (@#-symbol-p 3))))

(defmacro gmap-vl ((fun &rest args) vector list)
  "Return a matrix of dimensions (dim0 vec0) (dim0 vec1) filled by
applying `fun' to `args'.

`args' must have two arguments whose names start by @0 and @1.  In the
calls to `fun' the elements of arguments @0 and @1 are accessed using
the row and column indices respectively

The other arguments are passed unevaluated"
  (let (cleaned-args arg0 arg1)
    (mapc
     ;; loop over args and create a cleaned up argument list by
     ;; stripping the @ tags.  At the same time, process the @-tagged
     ;; args as follows:
     ;; - create a (gref ...) snippet that will be used to access
     ;;   their elements
     ;; - Make sure there is only one of @0 and @1 tags present.  This
     ;;   is done by examining contents of arg0 and arg1 which store
     ;;   the cleaned up @0 and @1 args.
     #'(lambda (arg)
	 (cond
	   ((@.-symbol-p arg)
	    (and arg0
		 (error "Found more than one marked symbol for list scan"))
	    (let ((cleaned-sym
		   (intern (subseq (symbol-name arg) 2))))
	      (setf arg0 cleaned-sym)
	      (push cleaned-sym cleaned-args)))
	   ((@#-symbol-p arg)
	    (and arg1
		 (error "Found more than one marked symbol for vector scan"))
	    (let ((cleaned-sym
		   (intern (subseq (symbol-name arg) 2))))
	      (setf arg1 cleaned-sym)
	      (push cleaned-sym cleaned-args)))
	   (t (push arg cleaned-args))))
	  args)
    (assert arg0 () "List is undefined")
    (assert arg1 () "Vector is undefined")
    (print vector)
    `(let ((e-list ,list)
	   (e-vector ,vector))
       (mapcar
	#'(lambda (,arg0)
	    (gmap #'(lambda (,arg1)
		      (,fun ,@(nreverse cleaned-args)))
		  e-vector))
	e-list))))


(define-test gmap-vl
  (assert-expands
   '(let ((e-list list)
	  (e-vector vector))
     (mapcar
     #'(lambda (x)
	 (gmap #'(lambda (y)
		   (fun y x z))
	       e-vector))
      e-list))
   (gmap-vl (fun @#y @.x z) vector list))
  (assert-numerical-equal
   '(#(1 2) #(1 4) #(1 8))
   (gmap-vl (expt @#b @.x) #(1 2) '(1 2 3)))
  (assert-numerical-equal
   '(#(1 1) #(2 4) #(3 9))
   (gmap-vl (expt @.b @#v) #(1 2) '(1 2 3)))
  (labels ((fun (x y z)
	     (+ z (expt x y))))
    (assert-numerical-equal
     '(#(0 1) #(0 3) (0 7))
     (gmap-vl (fun @#b @.x -1) #(1 2) '(1 2 3)))))

#|
;;; This is an attempt to unify 2D mapping along the following lines:
;;
;; Use a single macro (gmap2d (fun args) arg0 arg1)
;; where arg0 is a vector (or 1D grid) and arg1 is a vector or list
;;
;; The idea was to use one of the two make-2d-map methods, that would
be chosen
;; depending whether arg1 is a vector or list.
;;
;; But for that to work I need to understand better how macros interpret
;; arguments.
;; 

(defgeneric make-2d-map (arg1 arg2 fun &rest fun-args)
  (:documentation "Return a form that when compiled will loop `fun'
  over `arg1' and `arg2' on arguments labeled by @1 and @2 in
  `fun-args'

arg1 must be a vector, while arg2 can be a vector or a list")
  (:method ((vec0 vector) (vec1 vector) fun &rest fun-args)
    (let (funcall-args arg0 arg1)
      (mapc
       ;; loop over args and create a cleaned up argument list by
       ;; stripping the @ tags.  At the same time, process the @-tagged
       ;; args as follows:
       ;; - create a (gref ...) snippet that will be used to access
       ;;   their elements
       ;; - Make sure there is only one of @0 and @1 tags present.  This
       ;;   is done by examining contents of arg0 and arg1 which store
       ;;   the cleaned up @0 and @1 args.
       #'(lambda (arg)
	   (cond
	     ((@0-symbol-p arg)
	      (and arg0
		   (error "Found more than one marked symbol for row scan"))
	      (let ((cleaned-sym
		     (intern (subseq (symbol-name arg) 2))))
		(setf arg0 cleaned-sym)
		(push `(gref ,cleaned-sym row) funcall-args)))
	     ((@1-symbol-p arg)
	      (and arg1
		   (error "Found more than one marked symbol for column scan"))
	      (let ((cleaned-sym
		     (intern (subseq (symbol-name arg) 2))))
		(setf arg1 cleaned-sym)
		(push `(gref ,cleaned-sym column) funcall-args)))
	     (t (push arg funcall-args))))
       fun-args)
      (assert arg0 () "Vector 0 is undefined")
      (assert arg1 () "Vector 1 is undefined")
      `(let ((,arg0 ,vec0)
	     (,arg1 ,vec1))
	 (let ((d0 (dim0 ,arg0))
	       (d1 (dim0 ,arg1)))
	   (let ((res (make-grid `((,*array-type* ,d0 ,d1)
				   ,*float-type*))))
	     (iter:iter
	       (iter:for row :matrix-row-index res)
	       (iter:iter
		 (iter:for column  :matrix-column-index res)
		 (setf (gref res row column)
		       (,fun ,@(nreverse funcall-args)))))
	     res)))))
  (:method ((vector vector) (list list) fun &rest fun-args)
    (let (cleaned-args arg0 arg1)
      (mapc
       ;; loop over args and create a cleaned up argument list by
       ;; stripping the @ tags.  At the same time, process the @-tagged
       ;; args as follows:
       ;; - create a (gref ...) snippet that will be used to access
       ;;   their elements
       ;; - Make sure there is only one of @0 and @1 tags present.  This
       ;;   is done by examining contents of arg0 and arg1 which store
       ;;   the cleaned up @0 and @1 args.
       #'(lambda (arg)
	   (cond
	     ((@.-symbol-p arg)
	      (and arg0
		   (error "Found more than one marked symbol for list scan"))
	      (let ((cleaned-sym
		     (intern (subseq (symbol-name arg) 2))))
		(setf arg0 cleaned-sym)
		(push cleaned-sym cleaned-args)))
	     ((@#-symbol-p arg)
	      (and arg1
		   (error "Found more than one marked symbol for vector scan"))
	      (let ((cleaned-sym
		     (intern (subseq (symbol-name arg) 2))))
		(setf arg1 cleaned-sym)
		(push cleaned-sym cleaned-args)))
	     (t (push arg cleaned-args))))
       fun-args)
      (assert arg0 () "List is undefined")
      (assert arg1 () "Vector is undefined")
      (print vector)
      `(let ((e-list ,list)
	     (e-vector ,vector))
	 (mapcar
	  #'(lambda (,arg0)
	      (gmap #'(lambda (,arg1)
			(,fun ,@(nreverse cleaned-args)))
		    e-vector))
	  e-list)))))

|#

