(in-package :cl-mesh)

(defclass mesh ()
  ())

(defgeneric compute-normals (mesh &optional method)
  (:documentation "Compute the normals of the mesh using the face normals"))

(defclass geometric-mesh (mesh)
  ((vertices :type (or (simple-array (vector float 3)) null)
             :initarg :vertices
	     :initform nil)
   (indices  :initarg :indices
	     :initform nil)
   (normals :type (simple-array (vector float 3))
            :initarg :normals
	    :initform nil)
   (tangents :type (simple-array (vector float 3))
	     :initarg :normals
	     :initform nil)))

(defun make-mesh (vertices indices &optional normals tangents)
  (make-instance 'geometric-mesh
		 :vertices vertices
		 :indices indices
		 :normals normals
		 :tangents tangents))

(defmethod compute-normals ((mesh geometric-mesh) &optional method))
