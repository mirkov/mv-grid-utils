;; Mirko Vukovic
;; Time-stamp: <2012-10-10 16:56:38EDT mv-grid-package-def.lisp>
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

(defpackage :mv-grid
  (:use :cl :lisp-unit
	:csv-parser
	#+prune? :symbol-name-queries)
  (:shadow :lisp-unit :norm)
  (:import-from :alexandria :with-input-from-file)
  (:documentation "Package of utilities for grids.  Currently contains
grid input only."))

(antik:make-user-package :mv-grid)

#|
(defpackage :mv-grid-unit-tests
  (:use :cl :lisp-unit  :mv-grid :mv-gnuplot))
|#
;;;; Local variables: 
;;;; change-log-default-name: "~/my-software-add-ons/my-lisp/mv-gsll/ChangeLog"
;;;; End: