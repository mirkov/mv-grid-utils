(defpackage :asds-mv-gsll (:use :asdf :cl))
(in-package :asds-mv-gsll)

(defsystem mv-gsll
    :name "mv-gsll"
    :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
    :version "0.1"
    :description "utilities built on top of gsll"
    :components
    ((:module "base"
	      :pathname #P"./"
	      :components ((:file "mv-gsll-package-def")
			   (:file "utilities"
			    :depends-on ("mv-gsll-package-def"))
			   (:file "utilities-unit-tests"
			    :depends-on ("mv-gsll-package-def"
					 "utilities"))))
     (:module "ode"
	      :pathname #P"./"
	      :depends-on ("base")
	      :components
	      ((:file "2nd-order-ode"))))
    :depends-on (:cl-utilities
		 :gsll
		 :grid
		 :iterate
		 :lisp-unit
		 :my-utils
		 :mv-gnuplot))

