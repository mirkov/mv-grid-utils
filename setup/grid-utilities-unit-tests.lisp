;; Mirko Vukovic
;; Time-stamp: <2012-10-10 21:46:32 grid-utilities-unit-tests.lisp>
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

(in-package :mv-grid)


(defparameter *array-3-4-double-float*
  (grid::test-grid-double-float *default-grid-type* '(3 4)))

(defparameter *0-1-2* (grid::make-grid
		       `((,*default-grid-type*) ,*default-integer-type*)
		       :initial-contents '(0 1 2)))
(defparameter *0-2-4* (grid::make-grid
		       `((,*default-grid-type*) ,*default-integer-type*)
		       :initial-contents '(0 2 4)))
(defparameter *vector-4-double-float*
  (grid::test-grid-double-float *default-grid-type* '(4)))

(defparameter *array-3-4-double-float*
  (grid::test-grid-double-float *default-grid-type* '(3 4)))

(defmacro assert-grid-equal (grid1 grid2)
  "This macro converts grids to native arrays before using lisp-unit's
ASSERT-NUMERICAL-EQUAL"
  `(assert-numerical-equal (grid:copy-to ,grid1)
			   (grid:copy-to ,grid2)))












