(asdf:defsystem mv-grid
  :name "mv-grid"
  :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
  :version "0.1"
  :description "utilities built on top of grid"
  :components
  ((:module "package-def"
	    :pathname #P"./"

	    :components ((:file "mv-grid-package-def")
			 (:file "grid-utilities-setup"
				:depends-on ("mv-grid-package-def"))))
   (:module "grid-operations"
	   ;; :pathname #P"./"
	    :depends-on ("package-def")
	    :components ((:file "make-grid-sequence")
			 (:file "grid-iterating-functions")
			 (:file "grid-manipulations")
			 (:file "grid-mappers")
			 (:file "grid-io")
			 (:file "grid-curry")))
   (:module "cl-fun-mapping"
	   ;; :pathname #P"./"
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
	       :mv-gnuplot
	       :csv-parser))

