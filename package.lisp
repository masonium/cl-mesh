(defpackage :cl-mesh
  (:use :common-lisp :alexandria
	:iterate :split-sequence :metabang-bind)
  (:export
   #:make-mesh-from-parametric
   #:make-mesh-from-graph
   #:parse-wavefront-obj
   #:mesh-properties))

(in-package :cl-mesh)
