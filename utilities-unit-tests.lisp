(in-package :mv-gsll-unit-tests)


(define-test match-vec-element
  (let ((vec (useq 0 11 12)))
    (assert-true
     (multiple-value-bind (index value)
	 (match-vec-element vec #'(lambda (arg)
				    (>= arg 5)))
       (and (assert-number-equal 5d0 value)
	    (assert-number-equal 5 index))))
    (assert-true
     (multiple-value-bind (index value)
	 (match-vec-element vec #'(lambda (arg)
				    (>= arg -2)))
       (and (assert-number-equal 0d0 value)
	    (assert-number-equal 0 index))))
    (assert-true
     (not (match-vec-element vec #'(lambda (arg)
				     (>= arg 15)))))))


;;;; Local variables: 
;;;; change-log-default-name: "~/my-software-add-ons/my-lisp/mv-gsll/ChangeLog"
;;;; End: