(asdf:defsystem mv-gsll
  :name "mv-gsll"
  :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
  :version "0.1"
  :description "utilities built on top of gsll & grid"
  :components
  ((:module "base"
	    :pathname #P"./"
	    :components ((:file "mv-gsll-package-def")
			 (:file "grid-utilities"
				:depends-on ("mv-gsll-package-def"))
			 #|(:file "grid-utilities-unit-tests"
				:depends-on ("mv-gsll-package-def"
					     "grid-utilities"))|#))
   (:module "grid-functions"
	    :pathname #P"./"
	    :depends-on ("base")
	    :components ((:file "grid-function-declarations")
			 (:file "grid-function-generators")
			 (:file "grid-function-instantiations"
				:depends-on ("grid-function-declarations"
					     "grid-function-generators"))
			 (:file "grid-functions-unit-tests"
				:depends-on ("grid-function-instantiations")))))
   #|(:module "ode"
   :pathname #P"./"
   :depends-on ("base")
   :components
   ((:file "2nd-order-ode")
   (:file "2nd-order-ode-unit-tests"
   :depends-on ("2nd-order-ode"))))|#
    :depends-on (:cl-utilities
		 ;;:gsll
		 :grid
		 :iterate
		 :lisp-unit
		 :my-utils
		 :mv-gnuplot))

