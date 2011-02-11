(in-package :cl-user)

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
			       :specializers (list (find-class 'complex))
			       :qualifiers nil
			       :lambda-list '(x)
			       initargs))
	    (add-method gf
			(apply #'make-instance method-class
			       :function (compile nil lambda)
			       :specializers (list (find-class 'double-float))
			       :qualifiers nil
			       :lambda-list '(x)
			       initargs))))))


(setf (symbol-function 'gexpt)
      (let* ((gf (make-instance 'sb-mop::standard-generic-function
				:lambda-list '(x &optional y)
				:documentation "Grid exp function"))
	     (method-class (sb-mop:generic-function-method-class gf)))
	(multiple-value-bind (lambda initargs)
	    (sb-mop:make-method-lambda
	     gf
	     (sb-mop:class-prototype method-class)
	     '(lambda (x &optional y)
	       (if y (expt x y)
		   (expt x)))
	     nil)
	  (add-method gf
		      (apply #'make-instance method-class
			     :function (compile nil lambda)
			     :specializers (list (find-class 'double-float)
						 (find-class t))
			     :qualifiers nil
			     :lambda-list '(x &optional y)
			     initargs)))))
	  ;; (add-method gf
	  ;; 	      (apply #'make-instance method-class
	  ;; 		     :function (compile nil lambda)
	  ;; 		     :specializers (list (find-class 'double-float)
	  ;; 					 (find-class t))
	  ;; 		     :qualifiers nil
	  ;; 		     :lambda-list '(x y)
	  ;; 		     initargs)))))