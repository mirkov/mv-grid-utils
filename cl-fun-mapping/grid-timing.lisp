(defpackage mv-grid-timing
  (:use :cl :mv-grid)
  (:documentation "Workspace for implementing and testing the
  efficiency of grid map routines.")

(in-package :grid)

(defun optimized (arg)
  (grid:map-grid :source arg
	    :element-function 
	    #'(lambda (arg)
		(declare
		 (optimize (speed 3) (safety 0) (debug 0))
		 (double-float arg))
		(the double-float (sin arg))))
  (values))

(defun unoptimized (arg)
  (grid:map-grid :source arg
	    :element-function 
	    #'(lambda (arg) 
		(sin arg)))
  (values))



;;;; Timing results show very little effect on optimization (SBCL)
#|

MV-GRID@SBCL>
(progn
  (defvar *arg* nil)
  (setf *arg* (grid:make-grid '((foreign-array 300000) double-float)
			      :initial-element 1d0))
  nil)

MV-GRID@SBCL> (time (unoptimized *arg*))
Evaluation took:
  0.582 seconds of real time
  0.581911 seconds of total run time (0.572913 user, 0.008998 system)
  [ Run times consist of 0.032 seconds GC time, and 0.550 seconds non-GC time. ]
  100.00% CPU
  1,741,971,960 processor cycles
  96,001,688 bytes consed
  
; No value
MV-GRID@SBCL> (time (unoptimized *arg*))
Evaluation took:
  0.585 seconds of real time
  0.584911 seconds of total run time (0.577912 user, 0.006999 system)
  [ Run times consist of 0.040 seconds GC time, and 0.545 seconds non-GC time. ]
  100.00% CPU
  1,749,124,323 processor cycles
  96,006,056 bytes consed
|#

