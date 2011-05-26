;; Mirko Vukovic
;; Time-stamp: <2011-03-08 13:15:47 grid-utilities-setup.lisp>
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

;; Grid manipulation package:
;;
;; By default the array types and float
;; types are set to foreign-arrays and double floats, as needed by
;; GSL.  These can be overridden by specifying the *array-type* and
;; *float-type* to array and single-float

(in-package :mv-grid)

(export '(*array-type* *float-type* *integer-type* *complex-type*))
#|(export '(
	 *array-type* *float-type* *integer-type* *complelx-type*
	 indgen natgen findgen dindgen lseq gseq gmap gsmap
	 closest-element matrify
	 match-vec-element matching-indices
	 read-grid
	 reduce-rows reduce-columns))|#

(defparameter *array-type* #+sbcl 'foreign-array
	      #+clisp 'array
  "Default array type, either foreign-array (default for GSLL use) or
  array (native CL) ")

(defparameter *float-type* 'double-float
  "Default float type, either single-float or double-float (default
  for GSLL use)")

(defparameter *integer-type* '(unsigned-byte 32)
  "Default integer byte length")

(defparameter *complex-type* #+sbcl '(complex double-float)
	      #+clisp 'complex
	      "Default complex type")




(defparameter *array-3-4-double-float*
  (grid::test-grid-double-float *array-type* '(3 4)))



(defparameter *array-3-4-double-float*
  (grid::test-grid-double-float *array-type* '(3 4)))

(defparameter *vector-4-double-float*
  (grid::test-grid-double-float *array-type* '(4)))

(defparameter *0-1-2* (grid::make-grid `((,*array-type*) ,*integer-type*)
			     :initial-contents '(0 1 2)))
(defparameter *0-2-4* (grid::make-grid `((,*array-type*) ,*integer-type*)
				      :initial-contents '(0 2 4)))



;; disable the #m reader macro until it gets ported to clisp
#+clisp nil
#|
(set-dispatch-macro-character
 #\# #\m
 (lambda (stream subchar arg)
   (declare (ignore arg))
   (declare (ignore subchar))
   (read-char stream)
   (let ((list (read-delimited-list #\) stream)))
     (declare (ignore list))
     nil)))|#
     ;; `(make-grid
     ;;   ;; Specification without dimensions, they will be filled in
     ;;   ;; with initial-contents in make-grid.
     ;;   '((foreign-array) ,(hashm-eltype arg))
     ;;   :initial-contents
     ;;   ,(if (find *row-separator* list)
     ;; 	    (conslist
     ;; 	     (mapcar 'conslist
     ;; 		     (split-sequence:split-sequence *row-separator* list)))
     ;; 	    `(list ,@(mapcar 'quote-numeric-list list)))))))


  

;;;; Local variables: 
;;;; change-log-default-name: "~/my-software-add-ons/my-lisp/mv-grid-utils/ChangeLog"
;;;; End: