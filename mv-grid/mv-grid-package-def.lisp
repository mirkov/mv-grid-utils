(defpackage :mv-grid
  (:use :cl :lisp-unit :grid :anaphora);; :affi)
  (:shadow :lisp-unit :norm)
  (:documentation "Package of utilities for generating grids,
operating on them with mathematical functions, and (not yet 
implemented), operating on a mix of scalars and grids with 
custom functions."))



(defpackage :mv-grid-unit-tests
  (:use :cl :lisp-unit  :mv-grid :mv-gnuplot))

;;;; Local variables: 
;;;; change-log-default-name: "~/my-software-add-ons/my-lisp/mv-gsll/ChangeLog"
;;;; End: