;;;; Define default grid-mapping functions.  
;;
;; This file is a placeholder.  It can be used for code to loop over
;; function declarations in grid-mapping-declarations.lisp and declare
;; grid mappers for all the supported lisp built-ins

(in-package :mv-grid)

(defun generate-one-arg-grid-maps (&optional (dictionary *one-arg-functions*))
  (declare (ignore dictionary)))
(defun generate-two-arg-grid-maps (&optional (dictionary *two-arg-functions*))
  (declare (ignore dictionary)))
(defun generate-one&optional-second-arg-grid-maps
    (&optional (dictionary
		*one&optional-second-arg-functions*))
  (declare (ignore dictionary)))

#|
(eval-when (:compile-toplevel :load-toplevel :execute)
  (generate-one-arg-grid-maps)
  (generate-two-arg-grid-maps)
  (generate-one&optional-second-arg-grid-maps)
|#