(in-package :cl-mesh)

(defun mesh-dual (he-mesh)
  (with-slots (half-edges faces vertexes) he-mesh
    (let ((vert-to-dual-face (make-hash-table :size (length vertexes)))
          (face-to-dual-vert (make-hash-table :size (length faces))))
      (labels ((get-dual-face (vert)
                 (aif (gethash vert vert-to-dual-face)
                      it
                      (setf (gethash vert vert-to-dual-face)
                            (make-face)))))
        (iterate
          (for vertex in (slot-value he-mesh 'vertexes))
          (collect
              (iterate
                (for he in (slot-value vertex 'half-edges))
                ))

          )))))
