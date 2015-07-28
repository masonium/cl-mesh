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

(defun make-hem (faces &optional positions (vertex-class 'vertex))
  "A face is a list of vertex indices in counter-clockwise order"
  (let ((he-map (make-hash-table :test 'equal))
        (vert-map (make-hash-table :test 'equal))
        hem-faces)
    ;; We construct edges and vertices on demand, basd on
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
                     (slot-value (get-vert v1) 'half-edge) he)
               (collect he)))
        (for last-edge =
             (iterate
               (for e2 in (cdr half-edges))
               (for e1 in half-edges)
               (setf (he-next e1) e2
                     (he-prev e2) e1)
               (finally (return e2))))
        (setf (he-prev (first half-edges)) last-edge
              (he-next last-edge) (first half-edges)
              (face-half-edge hem-face) last-edge))
      (when positions
        (maphash #'(lambda (k v) (setf (slot-value v 'pos) (elt positions k)))
                 vert-map))
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

(defmacro-driver (FOR-VERTEX vertex IN-FACE face)
  (let ((edge (gensym))
        (f (gensym))
        (kwd (if generate 'generate 'for)))
    `(progn
       (with ,f = ,face)
       (for-edge ,edge in-face ,f)
       (,kwd ,vertex next (he-v1 ,edge)))))

(defmacro-driver (FOR-EDGE edge IN-VERTEX vert)
  (let ((v (gensym))
        (first-he (gensym))
        (kwd (if generate 'generate 'for)))
    `(progn
       (with ,v = ,vert)
       (with ,first-he = (slot-value ,v 'half-edge))
       (,kwd ,edge next (if (null ,edge)
                            (setf ,edge ,first-he)
                            (prog1
                                (setf ,edge (he-opposite (he-prev ,edge)))
                              (when (eq ,edge ,first-he)
                                (terminate))))))))

(defmacro-driver (FOR-FACE face IN-VERTEX vert)
  (let ((edge (gensym))
        (v (gensym))
        (kwd (if generate 'generate 'for)))
    `(progn
       (with ,v = ,vert)
       (for-edge ,edge in-vertex ,v)
       (,kwd ,face next (he-face ,edge)))))

(defun make-pyramid (&optional (n 3))
  "Construct a half-edge mesh representing a pyramid, where the base has
n-sides."
  (make-hem
   (cons (nreverse (iota n))
         (mapcar #'(lambda (i) (list i (mod (1+ i) n) n))
                 (iota n)))
   (let ((am (/ (* 2 pi) n)))
     (append
      (mapcar #'(lambda (i) (vector (cos (* am i)) (sin (* am i)) -1.0)) (iota n))
      (list (vector 0 0 1))))))

(defun make-prism (&optional (n 3))
  "Construct a half-edge mesh of a prism, with a base of n sides."
  (make-hem
   (cons (nreverse (iota n))
         (cons (iota n :start n)
               (mapcar
                #'(lambda (i)
                    (list i (mod (1+ i) n) (+ n (mod (1+ i) n)) (+ n i)))
                (iota n))))
   (let ((am (/ (* 2 pi) n)))
     (append
      (mapcar #'(lambda (i) (vector (cos (* am i)) (sin (* am i)) -1.0)) (iota n))
      (mapcar #'(lambda (i) (vector (cos (* am i)) (sin (* am i)) 1.0)) (iota n))))))

(defun make-cube ()
  (make-prism 4))
