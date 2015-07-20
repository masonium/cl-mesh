(in-package :cl-mesh)

(defstruct (half-edge (:conc-name he-)
                      (:constructor))
  opposite
  next
  prev
  face
  v1
  v2)



(defstruct (face (:conc-name face-)
                 (:constructor))
  half-edge)

(defclass vertex ()
  ((pos :initarg :position)
   (half-edge :initarg :half-edge
               :initform nil)))

(defstruct (half-edge-mesh (:constructor))
  half-edges
  faces
  vertexes)

(defmethod print-object ((he half-edge) stream)
  (format stream "#<HALF-EDGE ~A ~A>" (he-v1 he) (he-v2 he)))

(defmethod print-object ((obj half-edge-mesh) stream)
  (with-slots (half-edges faces vertexes) obj
    (format stream "#<HEM V=~A E=~A F=~A>"
            (length vertexes) (/ (length half-edges) 2) (length faces))))

(defun make-he-from-indices (faces &optional (vertex-class 'vertex))
  "A face is a "
  (let ((he-map (make-hash-table :test 'equal))
        (vert-map (make-hash-table))
        hem-faces)
    (labels ((get-vert (i)
               (aif (gethash i vert-map)
                    it
                    (setf (gethash i vert-map) (make-instance vertex-class))))
             (get-he (i1 i2)
               (let ((key (cons i1 i2)))
                 (aif (gethash key he-map)
                      it
                      (setf (gethash key he-map) (make-half-edge))))))

      (iterate
        (for face in faces)
        (for hem-face = (make-face))
        (push hem-face hem-faces)
        ;; For each, pair of vertices, create an edge
        (for half-edges =
             (iterate
               (for i below (length face))
               (for v1 = (elt face i))
               (for v2 = (elt face (mod (1+ i) (length face))))
               (for he = (get-he v1 v2))
               (setf (he-v1 he) (get-vert v1)
                     (he-v2 he) (get-vert v2)
                     (he-opposite he) (get-he v2 v1)
                     (he-face he) hem-face
                     (slot-value (get-vert v1) 'half-edge) he
                     )

               (collect he)))

        (for last-edge =
             (iterate
               (for e2 in (cdr half-edges))
               (for e1 in half-edges)
               (setf (he-next e1) e2
                     (he-prev e2) e1)
               (finally (return e2))))
        (print last-edge)
        (setf (he-prev (first half-edges)) last-edge
              (he-next last-edge) (first half-edges)
              (face-half-edge hem-face) last-edge))
      (make-half-edge-mesh :half-edges (hash-table-values he-map)
                           :vertexes (hash-table-values vert-map)
                           :faces hem-faces))))

(defmacro-driver (FOR-EDGE edge IN-FACE face)
  (let ((f (gensym))
        (first-he (gensym))
        (i (gensym))
        (kwd (if generate 'generate 'for)))
    `(progn
       (with ,f = ,face)
       (with ,first-he = (face-half-edge ,f))
       (for ,i = 0 )
       (,kwd ,edge next (if (null ,edge)
                            ,first-he
                            (progn
                              (setf ,edge (he-next ,edge))
                              (when (eq ,edge ,first-he)
                                (terminate))
                              ,edge))))))

(defmacro-driver (FOR-FACE other-face IN-FACE face)
  (let ((edge (gensym))
        (f (gensym))
        (kwd (if generate 'generate 'for)))
    `(progn
       (with ,f = ,face)
       (for-edge ,edge in-face ,f)
       (,kwd ,other-face next (he-face (he-opposite ,edge))))))

(defmacro-driver (FOR-FACE face IN-VERTEX vert)
  (let ((v (gensym))
        (f (gensym))
        (kwd (if generate 'generate 'for)))
    `(progn
       (with ,v = ,vert)
       (with ,he = (slot-value ,v 'half-edge))
       (,kwd ,face next (if (null face)
                            (he-face ,he)
                            (progn
                              (setf ,he (he-opposite (he-prev he)))
                              (when (eq ,he ,first-he)
                                (terminate))
                              (he-face ,he)))))))

(defun make-cube (&optional (vertex-class 'vertex))
  "constructu a half-edge-mesh representation of a cube"
  (let ((vpos (map-product #'vector '(-1 1) '(-1 1) '(-1 1)))
        (indices '((0 1 2 3)
                   (4 5 0 1)
                   (6 7 4 5)
                   (2 3 6 7)
                   (1 5 3 7)
                   (4 0 6 2))))
    (make-he-from-indices indices)))
