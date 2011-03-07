;; Mirko Vukovic
;; Time-stamp: <2011-03-06 16:30:57 mv-grid-package-def.lisp>
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
  (:use :cl :lisp-unit :grid :csv-parser);; :affi)
  (:shadow :lisp-unit :norm)
  (:import-from :alexandria :with-input-from-file)
  (:documentation "Package of utilities for generating grids,
operating on them with mathematical functions, and (not yet 
implemented), operating on a mix of scalars and grids with 
custom functions."))


#|
(defpackage :mv-grid-unit-tests
  (:use :cl :lisp-unit  :mv-grid :mv-gnuplot))
|#
;;;; Local variables: 
;;;; change-log-default-name: "~/my-software-add-ons/my-lisp/mv-gsll/ChangeLog"
;;;; End: