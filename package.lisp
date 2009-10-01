(defpackage :cl-mesh
  (:use :common-lisp :alexandria
	:iterate :split-sequence)
  (:export
   #:parse-wavefront-obj
   #:mesh-properties))

(in-package :cl-mesh)
