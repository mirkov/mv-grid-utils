;;;; Define mathematical functions that operate on grids.  These
;;;; functions are either cl math functions, or small deviations from
;;;; them.  The functions are defined programatically based on the
;;;; contents of several parameters.  The routines that define the
;;;; functions also generate the documentation and export the symbols
;;;;
;;;; The defined functions are named grid-foo where foo is the
;;;; descriptive part, such as `sin'

(in-package :mv-grid)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-fundef (*one-arg-functions*)
    (if result-type
	(defgridop1 fun result-type)
	(defgridop1 fun)))
  (do-fundef (*predicate-functions*)
    (defgridop-p fun))
  (dolist (fun-def *new-one-arg-functions*)
    (defgridop-new fun-def))
  (do-fundef (*one-arg-with-second-optional-functions*)
    (if result-type
	(defgridop1 fun result-type))
	(defgridop1 fun))
  (do-fundef (*two-arg-functions*)
    (defgridop2 fun result-type)))

