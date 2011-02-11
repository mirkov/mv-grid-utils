;; Mirko Vukovic
;; Time-stamp: <2011-02-10 07:20:22 grid-function-instantiations.lisp>
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

;;;; Define mathematical functions that operate on grids.  These
;;;; functions are either cl math functions, or small deviations from
;;;; them.  The functions are defined programatically based on the
;;;; contents of several parameters.  The routines that define the
;;;; functions also generate the documentation and export the symbols
;;;;
;;;; The defined functions are named grid-foo where foo is the
;;;; descriptive part, such as `sin'

(in-package :mv-grid)


(defun generate-one-arg-grid-fun-1 (declaration)
  "Generate grid functions based on declaration"
  (def-cl-grid-maps (if (= 1 (length declaration))
			`(,(car declaration) ((!I :return !I)
					      (!F :return !F)
					      (!C :return !C)))
			declaration)))

(defun generate-one-arg-grid-fun (fun)
  "Generate grid functions for fun based on declarations in
*one-arg-functions*"
  (let ((declaration (assoc fun *one-arg-functions*)))
    (generate-one-arg-grid-fun-1 declaration)))

(defun generate-one-arg-grid-funs ()
  "Generate grid functions for all functions in *one-arg-functions*"
  (dolist (declaration *one-arg-functions*)
    (generate-one-arg-grid-fun-1 declaration)))



(defun generate-two-arg-grid-fun-1 (declaration)
  "Generate grid functions based on declaration"
  (def-cl-grid-maps-2 declaration))

(defun generate-two-arg-grid-fun (fun)
  "Generate grid functions for fun based on declarations in
*two-arg-functions*"
  (let ((declaration (assoc fun *two-arg-functions*)))
    (generate-two-arg-grid-fun-1 declaration)))

(defun generate-two-arg-grid-funs ()
  "Generate grid functions for all functions in *two-arg-functions*"
  (dolist (declaration *two-arg-functions*)
    (generate-two-arg-grid-fun-1 declaration)))

(defun generate-one/opt-two-arg-grid-fun-1 (declaration)
  "Generate grid functions based on declaration"
  (def-cl-grid-maps-2 declaration))

(defun generate-one/opt-two-arg-grid-fun (fun)
  "Generate grid functions for fun based on declarations in
*one&optional-second-arg-functions*"
  (let ((declaration (assoc fun *one&optional-second-arg-functions*)))
    (generate-one/opt-two-arg-grid-fun-1 declaration)))

(defun generate-one/opt-two-arg-grid-funs ()
  "Generate grid functions for all functions in *one&optional-second-arg-functions*"
  (dolist (declaration *one&optional-second-arg-functions*)
    (generate-one/opt-two-arg-grid-fun-1 declaration)))


#|
(eval-when (:compile-toplevel :load-toplevel :execute)
  (generate-one-arg-grid-funs)
  (generate-two-arg-grid-funs)
  (generate-one/opt-two-arg-grid-funs)
|#