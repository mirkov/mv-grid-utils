(in-package :cl-user)

;; file for building prototype code for defining generic functions and
;; adding methods using the MOP.  We use sbcl's native mop in sbcl and
;; closer-mop in clisp.  The code here is mostly independent of grid,
;; and thus we use the `cl-user' package.

(defvar *f1* nil)

(setf (symbol-function '*f1*)
      (let* ((gf (make-instance 'sb-mop::standard-generic-function
				:lambda-list '(x y)))
	     (method-class (sb-mop:generic-function-method-class gf)))
	(multiple-value-bind (lambda initargs)
	    (sb-mop:make-method-lambda
	     gf
	     (sb-mop:class-prototype method-class)
	     '(lambda (x y)
	       (expt x y))
	     nil)
	  (add-method gf
		      (apply #'make-instance method-class
			     :function (compile nil lambda)
			     :specializers (list (find-class 'double-float)
						 nil)
						 ;;(find-class 'nil))
			     :qualifiers nil
			     :lambda-list '(x y)
			     initargs)))))


(let (gsin)
  (setf (symbol-function 'gsin)
	(let* ((gf (make-instance #+sbcl 'sb-mop::standard-generic-function
				  #+clisp 'closer-mop::standard-generic-function
				  :lambda-list '(x)))
	       (method-class (#+sbcl sb-mop:generic-function-method-class
				     #+clisp closer-mop::generic-function-method-class gf)))
	  (multiple-value-bind (lambda initargs)
	      (#+sbcl sb-mop:make-method-lambda
		      #+clisp closer-mop:make-method-lambda
		      gf
		      (#+sbcl sb-mop:class-prototype
			      #+clisp closer-mop:class-prototype
			      method-class)
		      '(lambda (x)
			(sin x))
		      nil)
	    (add-method gf
			(apply #'make-instance method-class
			       :function (compile nil lambda)
			       :specializers (list (find-class 'complex))
			       :qualifiers nil
			       :lambda-list '(x)
			       initargs))
	    (add-method gf
			(apply #'make-instance method-class
			       :function (compile nil lambda)
			       :specializers
			       (list (find-class #+sbcl 'double-float
						 #+clisp 'float))
			       :qualifiers nil
			       :lambda-list '(x)
			       initargs))))))



;; definition of a generic function with optional arguments
(setf (symbol-function 'glog)
      (let* ((gf (make-instance #+sbcl 'sb-mop::standard-generic-function
				#+clisp 'closer-mop::standard-generic-function
				:lambda-list '(x &optional y)
				:documentation "Grid exp function"))
	     (method-class (#+sbcl sb-mop:generic-function-method-class
				   #+clisp closer-mop:generic-function-method-class
				   gf)))
	(multiple-value-bind (lambda initargs)
	    (#+sbcl sb-mop:make-method-lambda
		    #+clisp closer-mop:make-method-lambda
		    gf
		    (#+sbcl sb-mop:class-prototype 
			    #+clisp closer-mop:class-prototype method-class)
		    '(lambda (x &optional y)
		      (if y (log x y)
			  (log x)))
		    nil)
	  (add-method gf
		      (apply #'make-instance method-class
			     :function (compile nil lambda)
			     :specializers
			     (list (find-class #+sbcl 'double-float
					       #+clisp 'float))
;;				   (find-class t))
			     :qualifiers nil
			     :lambda-list '(x &optional y)
			     initargs)))))


