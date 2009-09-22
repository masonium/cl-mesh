(in-package :cl-mesh)

;;;; Simple examples to demonstrate the use of the cl-mesh

;;; 1. Loading mesh data from an obj and give basic information about
;;; the mesh
(defun mesh-properties (filename)
  (let ((mesh-ht (parse-wavefront-obj filename)))
    (format t "Mesh has ~D vertices and ~D faces.~%" 
	    (array-dimension (gethash "vertices" mesh-ht) 0)
	    (array-dimension (gethash "indices" mesh-ht) 0))))