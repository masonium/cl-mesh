(in-package :cl-mesh)

;;; A canonical polyhedron has three properites:
;;;
;;; 1. The centroid of the vertices is the origin.
;;; 2. Each face is planar.
;;; 3. Each edge is tangent to the unit sphere, with its midpoint intersecting the sphere.
;;;
;;; Canonicalizations are unique up to isometry.


(defun canonicalize-hem (mesh)
  "Transform the vertices of the input mesh so that the resulting polyhedron
  is in canonical form."
  (let ((pos (map 'vector (lambda (x) (slot-value x 'pos)) (slot-value mesh 'vertexes))))
    (labels ((update-pos (pos mesh)
               (%canonicalize-edges
                (%canonicalize-faces
                 (%canonicalize-center pos) mesh) mesh))
             (movement (pos new-pos) (reduce #'+ (map 'vector #'v-dist2 pos new-pos))))
      (iterate (for c upfrom 0)
        (with old-pos = pos)
        (for new-pos = (update-pos old-pos mesh))
        (for m = (movement old-pos new-pos))
        (until (< m 1e-7))
        (finally
         (return
           (progn
             (iterate (for vertex in (slot-value mesh 'vertexes))
               (setf (slot-value vertex 'pos) (aref new-pos (slot-value vertex 'index))))
             (format *standard-output* "Canonicalized after ~A iterations" c)
             (print m)
             mesh)))
        (setf old-pos new-pos)))))

(defun %canonicalize-center (pos)
  "Center the positions by computing the centroid and translating all position
by the negative centroid."
  (let ((center (v*f (reduce #'v+ pos) (/ (length pos)))))
    (map 'vector #'(lambda (x) (v- x center)) pos)))

(defun %face-center-pos (face pos)
  (iterate (for-vertex v in-face face)
    (summing 1 into c)
    (reducing (aref pos (slot-value v 'index)) by #'v+ into r)
    (finally (return (prog1 (v*f r (/ c)) c)))))

(defun %face-average-normal (face center pos)
  (v-normal
   (iterate (for-edge edge in-face face)
     (for v1 = (aref pos (slot-value (he-v1 edge) 'index)))
     (for v2 = (aref pos (slot-value (he-v2 edge) 'index)))
     (reducing (v-cross (v- v1 center) (v- v2 center )) by #'v+))))

(defun %canonicalize-faces (pos mesh &optional (step 1))
  "Return a new position array for the mesh, that brings each face closer to being planar.

For each face, project the vertexes on to a plane representative of the face."
  (let ((new-pos (make-array (length pos) :adjustable nil :initial-contents pos)))
    (iterate (for face in (slot-value mesh 'faces))
      (for center = (%face-center-pos face new-pos))
      ;; Compute the plane on which the vertexes will be projected, defined by
      ;; avg-norm and dist.
      (for avg-norm = (%face-average-normal face center new-pos))
      (for dist = (v-dot center avg-norm))
      (iterate (for-vertex v in-face face)
        (for i = (slot-value v 'index))
        (for old = (aref new-pos i))
        ;; Project the vertexes onto the plane.
        (for f = (* step (- (v-dot avg-norm old) dist)))
        (setf (aref new-pos i) (v- old (v*f avg-norm f)))))
    new-pos))

(defun %canonicalize-edges (pos mesh)
  "Return a new position array for the mesh that tries to canonicalize the edges."
  (let ((new-pos (make-array (length pos) :adjustable nil :initial-contents pos)))
    (labels ((update-vertex (i cosa)
               (let ((old (aref new-pos i)))
                 (setf (aref new-pos i) (v*f old (sqrt (/ 2 (* (v-norm old) (1+ cosa)))) )))))
      (iterate (for edge in (slot-value mesh 'half-edges))
        (for v1 = (he-v1 edge))
        (for v2 = (he-v2 edge))
        (for ca = (v-cos-angle (aref new-pos (vert-index v1)) (aref new-pos (vert-index v2))))
        (update-vertex (vert-index v1) ca)
        (update-vertex (vert-index v2) ca)))
    new-pos))
