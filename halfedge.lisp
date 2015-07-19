(in-package :cl-mesh)

(defstruct half-edge
  opposite
  next
  prev
  face
  v1
  v2)

(defstruct face
  half-edges)

(defclass vertex ()
  ((pos :initarg :position)))

(defstruct half-edge-mesh
  half-edges
  faces
  vertexes)

(defun make-he-from-indicies (faces)
  "A face is a "
  (let ((hemap (make-hash)))
    (iterate
      (for face in faces))))


(defun make-cube (&optional (vertex-class 'vertex))
  "constructu a half-edge-mesh representation of a cube"
  (let ((vpos (map-product #'vector '(-1 1) '(-1 1) '(-1 1)))
        (indices '((0 1 2) (2 1 3)
                   (4 5 0) (0 5 1)
                   (6 7 4) (4 7 5)
                   (2 3 6) (6 3 7)
                   (1 5 3) (3 5 7)
                   (4 0 6) (0 6 2)))
        (mesh (make-instance 'half-edge-mesh)))
    vpos))
