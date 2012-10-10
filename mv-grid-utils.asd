;; Mirko Vukovic
;; Time-stamp: <2012-10-10 17:13:11EDT mv-grid-utils.asd>
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
  ((:module "package-def"
	    :pathname #P"./"
	    :serial t
	    :components ((:file "mv-grid-package-def")
			 (:file "grid-utilities-setup")))
   (:module "grid-io"
	    :pathname #P"./grid-operations/"
	    :serial t
	    :depends-on ("package-def")
	    :components ((:file "grid-io")
			 (:file "grid-utilities-unit-tests")))
   #+prune(:module "grid-operations"
	    ;; :pathname #P"./"
	    :depends-on ("package-def")
	    :components ((:file "make-grid-sequence")
			 (:file "grid-iterating-functions")
			 (:file "grid-manipulations")
			 (:file "vector-mappings")
			 (:file "matrix-row-or-col-mappings")
			 (:file "mixed-arg-vector-mappings")
			 (:file "mixed-arg-2vector-ortho-mappings")
			 (:file "grid-io")))
   #+prune(:module "cl-fun-mapping"
	    ;; :pathname #P"./"
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
  :depends-on (#+needed? :cl-utilities
	       #+needed? :anaphora
	       #+needed? :alexandria
	       #+needed? :split-sequence
	       #+part-of-antik "foreign-array"
	       "antik"
	       #+needed? :iterate
	       "lisp-unit"
	       "picard-csv"
	       #+needed? :symbol-name-queries))



