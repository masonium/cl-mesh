(in-package :cl-mesh)

(defmacro with-dual-vert (fn-list &body body)
  "Create helper functions, which map existing edges or faces vertices in the
dual of the new objects."
  (let ((dual-vert-map (gensym))
        fns)
    (when (member :face fn-list)
      (push `(face-to-dual-vert (face)
                                (aif (gethash face ,dual-vert-map)
                                     it
                                     (setf (gethash face ,dual-vert-map)
                                           (hash-table-count ,dual-vert-map))))
            fns))
    (when (member :edge fn-list)
      (push `(edge-to-dual-vert (edge)
                  (acond
                    ((gethash edge ,dual-vert-map) it)
                    ((gethash (he-opposite edge) ,dual-vert-map) it)
                    (t
                     (setf (gethash edge ,dual-vert-map)
                           (hash-table-count ,dual-vert-map)))))
       fns))
    `(let ((,dual-vert-map (make-hash-table)))
       (labels ,fns
         ,@body))))

(defun conway-dual (he-mesh)
  (with-slots (half-edges faces vertexes) he-mesh
    (with-dual-vert (:face)
      (make-he-from-indices
       (iterate
         (for vertex in (slot-value he-mesh 'vertexes))
         (collect
             (iterate
               (for-face face in-vertex vertex)
               (collect (face-to-dual-vert face)))))))))

(defun conway-ambo (he-mesh)
  (with-slots (faces vertexes) he-mesh
    (with-dual-vert (:edge)
      (make-he-from-indices
       (append
        (iterate (for face in faces)
          (collect
              (iterate
                (for-edge edge in-face face)
                (collect (edge-to-dual-vert edge)))))
        (iterate (for vertex in vertexes)
          (collect
              (iterate
                (for-edge edge in-vertex vertex)
                (collect (edge-to-dual-vert edge))))))))))
