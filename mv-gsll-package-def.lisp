(defpackage :mv-gsll
  (:use :cl :gsll);; :grid)
  (:export :useq :xpseq :gmap :match-vec-element
	   :setup-vector :setup-tridiag-11))


(defpackage :mv-gsll-unit-tests
  (:use :cl :lisp-unit :gsll :mv-gsll :mv-gnuplot))

;;;; Local variables: 
;;;; change-log-default-name: "~/my-software-add-ons/my-lisp/mv-gsll/ChangeLog"
;;;; End: