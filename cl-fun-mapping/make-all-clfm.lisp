;; Mirko Vukovic
;; Time-stamp: <2011-02-10 06:41:07 mv-gpl-header.txt>
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