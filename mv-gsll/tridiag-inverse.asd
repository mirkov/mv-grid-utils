(defpackage :asd-tridiag-inverse
  (:use :cl :asdf))


(in-package :asd-tridiag-inverse)

(defsystem tridiag-inverse
  :name "tridiag-inverse"
  :author "Mirko Vukovic"
  :version "0.1"
  :maintainer "Mirko Vukovic"
  :license "Tokyo Electron"
  :description "Facilitates writing of array-indexed variables"
  :components (;;(:file "tridiag-inverse")
	       ;;(:file "tridiag-inverse1")
	       (:file "tridiag-inverse2")
	       )
  :depends-on (:gsll
	       :array-indexing-notation
	       :lisp-unit
	       :iterate))
