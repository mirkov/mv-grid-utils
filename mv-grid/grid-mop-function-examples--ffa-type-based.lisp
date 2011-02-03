;;;; This file contains MOP code that hightlights how to define
;;;; generic functions and their methods that specialize on rank.
;;;; This code is guide to the dynamically generated code.  The
;;;; methods themselves further test for argument types in order to
;;;; build the grids correctly (necessary for installations that use
;;;; FFAs)
;;
;; This file is meant to be studied, not loaded by asdf.


(in-package :mv-grid)

;; the following is a show-it all version.  Further versions down the
;; line use more and more of local utilities
(setf (symbol-function 'sin%mx)
      (let* ((gf (make-instance 'sb-mop::standard-generic-function
				:lambda-list '(x)))
	     (method-class (sb-mop:generic-function-method-class gf)))
	(multiple-value-bind (lambda initargs)
	    (sb-mop:make-method-lambda
	     gf
	     (sb-mop:class-prototype method-class)
	     '(lambda (x)
	       (sin x))
	     nil)
	  (add-method gf
		      (apply #'make-instance method-class
			     :function (compile nil lambda)
			     :specializers (list (find-class t))
			     :qualifiers nil
			     :lambda-list '(x)
			     initargs)))
	(multiple-value-bind (lambda initargs)
	    (sb-mop:make-method-lambda
	     gf
	     (sb-mop:class-prototype method-class)
	     '(lambda (arg)
	       (let ((destination-specification
		      (list (append '(foreign-array)
				    (dimensions arg))
			    'double-float))
		     (map-function #'(lambda (x)
				       (declare (double-float x))
				       (sin x))))
		 (map-grid :source arg
			   :element-function map-function
			   :destination-specification destination-specification)))
	     nil)
	  (add-method gf
		      (apply #'make-instance method-class
			     :function (compile nil lambda)
			     :specializers (list (find-class 'vector-double-float))
			     :qualifiers nil
			     :lambda-list '(x)
			     initargs)))
	(multiple-value-bind (lambda initargs)
	    (sb-mop:make-method-lambda
	     gf
	     (sb-mop:class-prototype method-class)
	     '(lambda (arg)
	       (let ((destination-specification
		      (list (append '(foreign-array)
				    (dimensions arg))
			    '(complex double-float)))
		     (map-function #'(lambda (x)
				       (declare ((complex double-float) x))
				       (sin x))))
		 (map-grid :source arg
			   :element-function map-function
			   :destination-specification destination-specification)))
	     nil)
	  (add-method gf
		      (apply #'make-instance method-class
			     :function (compile nil lambda)
			     :specializers (list (find-class 'vector-complex-double-float))
			     :qualifiers nil
			     :lambda-list '(x)
			     initargs)))))

(define-test sin%mx
  (assert-numerical-equal
   (sin 2d0) (sin%mx 2d0))
  (assert-numerical-equal
   (copy-to (sin%mx *+1-vector*))
   (map-grid>cl #'sin *+1-vector*))
  (assert-numerical-equal
   (copy-to (sin%mx *complex-vector*))
   (map-grid>cl #'sin *complex-vector*)))
  
;; this version uses typedef to simplify the above example slightly

(setf (symbol-function 'sin%mx2)
      (let* ((gf (make-instance 'sb-mop::standard-generic-function
				:lambda-list '(x)))
	     (method-class (sb-mop:generic-function-method-class gf)))
	(multiple-value-bind (lambda initargs)
	    (sb-mop:make-method-lambda
	     gf
	     (sb-mop:class-prototype method-class)
	     '(lambda (x)
	       (sin x))
	     nil)
	  (add-method gf
		      (apply #'make-instance method-class
			     :function (compile nil lambda)
			     :specializers (list (find-class t))
			     :qualifiers nil
			     :lambda-list '(x)
			     initargs)))
	(multiple-value-bind (lambda initargs)
	    (sb-mop:make-method-lambda
	     gf
	     (sb-mop:class-prototype method-class)
	     '(lambda (arg)
	       (let ((destination-specification
		      (list (append '(foreign-array)
				    (dimensions arg))
			    (typedef '!F)))
		     (map-function #'(lambda (x)
				       (declare (double-float x))
				       (sin x))))
		 (map-grid :source arg
			   :element-function map-function
			   :destination-specification destination-specification)))
	     nil)
	  (add-method gf
		      (apply #'make-instance method-class
			     :function (compile nil lambda)
			     :specializers (list (find-class (typedef '!F :vector t)))
			     :qualifiers nil
			     :lambda-list '(x)
			     initargs)))
	(multiple-value-bind (lambda initargs)
	    (sb-mop:make-method-lambda
	     gf
	     (sb-mop:class-prototype method-class)
	     '(lambda (arg)
	       (let ((destination-specification
		      (list (append '(foreign-array)
				    (dimensions arg))
			    (typedef '!C)))
		     (map-function #'(lambda (x)
				       (declare ((complex double-float) x))
				       (sin x))))
		 (map-grid :source arg
			   :element-function map-function
			   :destination-specification destination-specification)))
	     nil)
	  (add-method gf
		      (apply #'make-instance method-class
			     :function (compile nil lambda)
			     :specializers (list (find-class (typedef '!C :vector t)))
			     :qualifiers nil
			     :lambda-list '(x)
			     initargs)))))

(define-test sin%mx2
  (assert-numerical-equal
   (sin 2d0) (sin%mx2 2d0))
  (assert-numerical-equal
   (copy-to (sin%mx2 *+1-vector*))
   (map-grid>cl #'sin *+1-vector*))
  (assert-numerical-equal
   (copy-to (sin%mx2 *complex-vector*))
   (map-grid>cl #'sin *complex-vector*)))


;; the next simplification is to automate the declaration in the
;; labmda form.  I cannot use a function to do that, instead I have to
;; use a macro to expand into the whole lambda form

(setf (symbol-function 'sin%mx3)
      (let* ((gf (make-instance 'sb-mop::standard-generic-function
				:lambda-list '(x)))
	     (method-class (sb-mop:generic-function-method-class gf)))
	(multiple-value-bind (lambda initargs)
	    (sb-mop:make-method-lambda
	     gf
	     (sb-mop:class-prototype method-class)
	     '(lambda (x)
	       (sin x))
	     nil)
	  (add-method gf
		      (apply #'make-instance method-class
			     :function (compile nil lambda)
			     :specializers (list (find-class t))
			     :qualifiers nil
			     :lambda-list '(x)
			     initargs)))
	(let ((xx `(lambda (arg)
		(let ((destination-specification
		       (list (append '(foreign-array)
				     (dimensions arg))
			     (typedef '!F)))
		      (map-function ,(typed-lambda1 !F (sin x))))
		  (map-grid :source arg
			    :element-function map-function
			    :destination-specification destination-specification)))))
	  (print xx))))
	  (multiple-value-bind (lambda initargs)
	    (sb-mop:make-method-lambda
	     gf
	     (sb-mop:class-prototype method-class)
	     xx
	     nil)
	  (add-method gf
		      (apply #'make-instance method-class
			     :function (compile nil lambda)
			     :specializers (list (find-class (typedef '!F :vector t)))
			     :qualifiers nil
			     :lambda-list '(x)
			     initargs)))))

  

(setf (symbol-function 'log!m1)
      (let* ((gf (make-instance 'sb-mop::standard-generic-function
				:lambda-list '(x)))
	     (method-class (sb-mop:generic-function-method-class gf)))
	(multiple-value-bind (lambda initargs)
	    (sb-mop:make-method-lambda
	     gf
	     (sb-mop:class-prototype method-class)
	     '(lambda (x)
	       (log x))
	     nil)
	  (add-method gf
		      (apply #'make-instance method-class
			     :function (compile nil lambda)
			     :specializers (list (find-class t))
			     :qualifiers nil
			     :lambda-list '(x)
			     initargs)))
	(multiple-value-bind (lambda initargs)
	    (sb-mop:make-method-lambda
	     gf
	     (sb-mop:class-prototype method-class)
	     `(lambda (arg)
		(let ((destination-specification
		       ,(one-arg-fun-map-destination-specification1
			 'log *one&optional-second-arg-functions*))
		      (map-function
		       #+sbcl #'log
		       #+clisp #'log))
		  (map-grid :source arg
			    :element-function map-function
			    :destination-specification destination-specification)))
	     nil)
	  (add-method gf
		      (apply #'make-instance method-class
			     :function (compile nil lambda)
			     :specializers (list (find-class #+sbcl 'mvector
							     #+clisp 'array))
			     :qualifiers nil
			     :lambda-list '(x)
			     initargs)))))

(define-test log!m1
  (assert-numerical-equal
   (log!m1 2d0) (log 2d0))
  (assert-numerical-equal
   (copy-to (log!m1 *+1-vector*))
   (map-grid>cl #'log *+1-vector*))
  (assert-numerical-equal
   (copy-to (log!m1 *complex-vector*))
   (map-grid>cl #'log *complex-vector*)))

(setf (symbol-function 'log!m2)
      (let* ((gf (make-instance 'sb-mop::standard-generic-function
				:lambda-list '(arg base)))
	     (method-class (sb-mop:generic-function-method-class gf)))
	(multiple-value-bind (lambda initargs)
	    (sb-mop:make-method-lambda
	     gf
	     (sb-mop:class-prototype method-class)
	     '(lambda (arg base)
	       (log arg base))
	     nil)
	  (add-method gf
		      (apply #'make-instance method-class
			     :function (compile nil lambda)
			     :specializers (list (find-class t)
						 (find-class t))
			     :qualifiers nil
			     :lambda-list '(arg base)
			     initargs)))
	(multiple-value-bind (lambda initargs)
	    (sb-mop:make-method-lambda
	     gf
	     (sb-mop:class-prototype method-class)
	     `(lambda (arg base)
		(let ((destination-specification
		       ,(two-arg-fun-map-destination-specification1
			 'log 'arg 'base t nil
			 *one&optional-second-arg-functions*))
		      (map-function
		       #'(lambda (arg)
			   (log arg base))))
		  (map-grid :source arg
			    :element-function map-function
			    :destination-specification destination-specification)))
	     nil)
	  (add-method gf
		      (apply #'make-instance method-class
			     :function (compile nil lambda)
			     :specializers (list (find-class #+sbcl 'mvector
							     #+clisp 'array)
						 (find-class t))
			     :qualifiers nil
			     :lambda-list '(arg base)
			     initargs)))
	(multiple-value-bind (lambda initargs)
	    (sb-mop:make-method-lambda
	     gf
	     (sb-mop:class-prototype method-class)
	     `(lambda (arg base)
		(let ((destination-specification
		       ,(two-arg-fun-map-destination-specification1
			 'log 'arg 'base nil t
			 *one&optional-second-arg-functions*))
		      (map-function
		       #'(lambda (base)
			   (log arg base))))
		  (map-grid :source base
			    :element-function map-function
			    :destination-specification destination-specification)))
	     nil)
	  (add-method gf
		      (apply #'make-instance method-class
			     :function (compile nil lambda)
			     :specializers (list (find-class t)
						 (find-class #+sbcl 'mvector
							     #+clisp 'array))
			     :qualifiers nil
			     :lambda-list '(arg base)
			     initargs)))
	(multiple-value-bind (lambda initargs)
	    (sb-mop:make-method-lambda
	     gf
	     (sb-mop:class-prototype method-class)
	     `(lambda (arg base)
		(let ((destination-specification
		       ,(two-arg-fun-map-destination-specification1
			 'log 'arg 'base t t
			 *one&optional-second-arg-functions*))
		      (map-function
		       #'(lambda (arg base)
			   (log arg base))))
		  (map-n-grids :sources (list (list arg nil) (list base nil))
			       :combination-function map-function
			       :destination-specification destination-specification)))
	     nil)
	  (add-method gf
		      (apply #'make-instance method-class
			     :function (compile nil lambda)
			     :specializers (list (find-class #+sbcl 'mvector
							     #+clisp 'array)
						 (find-class #+sbcl 'mvector
							     #+clisp 'array))
			     :qualifiers nil
			     :lambda-list '(arg base)
			     initargs)))))

(define-test log!m2arg
  (assert-numerical-equal (log!x 2d0 2d0) 1d0)
  (assert-numerical-equal
   (copy-to (log!m2 *+1-vector* 2d0))
   (map-grid>cl #'(lambda (x)
		    (log x 2d0))
		*+1-vector*))
  (assert-numerical-equal
   (copy-to (log!m2 2d0 *+2-vector*))
   (map-grid>cl #'(lambda (x)
		    (log 2d0 x))
		*+2-vector*))
  (assert-numerical-equal
   (copy-to (log!m2 *+2-vector* *+2-vector*))
   (map-grid>cl #'(lambda (x)
		    (log x x))
		*+2-vector*)))


;; example of defining a function using utilities that accomplish all
;; of the above
(let ((function 'sin!m2)
      (dictionary *one-arg-functions*)
      (arg-list '(arg)))
  (setf (symbol-function 'foo)
	(destructuring-bind (gf class)
	    (grid-gf&method-class arg-list)
	  (destructuring-bind (lambda-form specializers)
	      (grid-method-one-arg-rank-lambda&specializers
	       function dictionary)
	    (add-grid-method gf class arg-list lambda-form specializers))
	  gf)))

|#
