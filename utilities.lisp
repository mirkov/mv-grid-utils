(in-package :mv-gsll)


(defmacro seq (fun)
  `(progn
;;     (declare (number lo hi))
     (coerce lo 'double-float)
     (coerce hi 'double-float)
     (make-marray 'double-float
		  :initial-contents
		  (coerce (,fun lo hi count) 'list))))
  
(defun useq (lo hi &optional (count 51))
  (seq my-utils:rseq ))


(defun xpseq (lo hi &optional (count 51))
  (seq my-utils:xpseq))

(defun gmap (function &rest grids)
  "Element-wise Map `function' over `grids'"
  (grid:map-n-grids
   :sources  (loop for grid in grids
		  collect `(,grid ,(grid:affi grid)))
   :combination-function #'(lambda (dummy &rest args)
			     (declare (ignore dummy))
			     (apply function args))))

(defun match-vec-element (vector predicate)
  "Return the first `vector' index and value for which the `predicate'
returns t.  Else return nil"
  (iter:iter (iter:for v :vector-element vector)
	     (iter:for i :vector-element-index vector)
	     (when (funcall predicate v)
	       (return-from match-vec-element (values i v))))
  nil)

	       
  

;;;; Local variables: 
;;;; change-log-default-name: "~/my-software-add-ons/my-lisp/mv-gsll/ChangeLog"
;;;; End: