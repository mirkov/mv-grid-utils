;; Mirko Vukovic
;; Time-stamp: <2012-10-10 21:49:42 mv-grid-utils.asd>
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

(asdf:defsystem mv-grid-utils
  :name "mv-grid"
  :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
  :version "0.1"
  :description "utilities built on top of grid"
  :components
  ((:module "setup"
	    :serial t
	    :components ((:file "mv-grid-package-def")
			 (:file "grid-utilities-setup")
			 (:file "grid-utilities-unit-tests")))
   (:module "grid-io"
	    :serial t
	    :depends-on ("setup")
	    :components ((:file "grid-io")
			 ))
   (:module "grid-operations"
	    :depends-on ("setup")
	    :components ((:file "grid-iterating-functions")
			 (:file "grid-manipulations")
			 (:file "matrix-row-or-col-mappings")))
   #+prune(:module "cl-fun-mapping"
	    :depends-on ("package-def")
	    :components ((:file "clfm-declarations")
			 (:file "clfm-unit-tests"
				:depends-on ("clfm-declarations"))
			 (:file "clfm-generation-utilities"
				:depends-on ("clfm-declarations"))
			 (:file "clfm-by-example"
				:depends-on ("clfm-declarations"
					     "clfm-generation-utilities"))
			 (:file "make-all-clfm"
				:depends-on ("clfm-declarations"
					     "clfm-generation-utilities")))))
  :depends-on ("iterate"
	       "antik"
	       "lisp-unit"
	       "picard-csv"))



