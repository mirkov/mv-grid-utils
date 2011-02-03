(asdf:defsystem mv-grid
  :name "mv-grid"
  :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
  :version "0.1"
  :description "utilities built on top of grid"
  :components
  ((:module "package-def"
	    :pathname #P"./"

	    :components ((:file "mv-grid-package-def")))
   (:module "utilities"
	    :pathname #P"./"
	    :depends-on ("package-def")
	    :components ((:file "grid-utilities")
			 (:file "grid-utilities-unit-tests"
				:depends-on ("grid-utilities"))))
   (:module "grid-mapping"
	    :pathname #P"./"
	    :depends-on ("package-def")
	    :components ((:file "grid-mapping-declarations")
			 (:file "grid-mapping-unit-tests"
				:depends-on ("grid-mapping-declarations"))
			 (:file "grid-mapping-generators"
				:depends-on ("grid-mapping-declarations"))
			 (:file "grid-generic-functions&methods-examples"
				:depends-on ("grid-mapping-declarations"
					     "grid-mapping-generators"))
			 (:file "grid-default-mappings"
				:depends-on ("grid-mapping-declarations"
					     "grid-mapping-generators")))))
  :depends-on (:cl-utilities
	       :anaphora
	       :grid
	       :iterate
	       :lisp-unit
	       :my-utils
	       :mv-gnuplot))

