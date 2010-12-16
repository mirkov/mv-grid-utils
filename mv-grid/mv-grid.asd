(asdf:defsystem mv-grid
  :name "mv-grid"
  :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
  :version "0.1"
  :description "utilities built on top of grid"
  :components
  ((:module "base"
	    :pathname #P"./"
	    :components ((:file "mv-grid-package-def")
			 (:file "grid-utilities"
				:depends-on ("mv-grid-package-def"))
			 (:file "grid-utilities-unit-tests"
				:depends-on ("mv-grid-package-def"
					     "grid-utilities"))))
   (:module "math-functions"
	    :pathname #P"./"
	    :depends-on ("base")
	    :components ((:file "grid-function-declarations")
			 (:file "grid-function-generators")
			 (:file "grid-function-instantiations"
				:depends-on ("grid-function-declarations"
					     "grid-function-generators"))
			 (:file "grid-functions-unit-tests"
				:depends-on ("grid-function-instantiations"))))
    :depends-on (:cl-utilities
		 :grid
		 :iterate
		 :lisp-unit
		 :my-utils
		 :mv-gnuplot))

