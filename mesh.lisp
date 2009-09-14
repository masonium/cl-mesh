(in-package :cl-mesh)

(defclass mesh ()
  ())

(defclass geometric-mesh (mesh)
  ((vertices :type (or (simple-array (vector float 3)) null)
             :initarg :vertices
	     :initform nil)
   (indices :initarg :indices
	    :initform nil)
   (normals :type (simple-array (vector float 3))
            :initarg :normals
	    :initform nil)
   (tangents :type (simple-array (vector float 3))
	     :initarg :normals
	     :initform nil)))


	     