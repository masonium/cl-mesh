(in-package :cl-mesh)

(defmacro %with-dual-vert (fn-list &body body)
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
    (when (member :half-edge fn-list)
      (push `(half-edge-to-dual-vert (edge)
				     (aif (gethash edge ,dual-vert-map)
					  it
					  (setf (gethash edge ,dual-vert-map)
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
    (when (member :vert fn-list)
      (push `(vert-to-dual-vert (vert)
                                (aif (gethash vert ,dual-vert-map)
                                     it
                                     (setf (gethash vert ,dual-vert-map)
                                           (hash-table-count ,dual-vert-map))))
            fns))
    `(let ((,dual-vert-map (make-hash-table)))
       (labels ,fns
         ,@body))))

(defvar *conway-operators* (make-hash-table :test 'equal))

(defmacro defconway (name op-name num-vertexes fn-list &body body)
  "Helper macro for defining conway methods and registering them as operators."
  (with-gensyms
   (param o!-op-name pos-array)
   (let ((func-name (intern (concatenate 'string "CONWAY-" (string name)))))
     `(progn
	(defun ,func-name (,param)
	  (with-slots (half-edges faces vertexes) ,param
	    (let ((,pos-array (make-array
			       ;; evaluate the form for num-vertexes in an environment where
			       ;; f, e, and v are properly defined as necessary
			       (let ((e (/ (length half-edges) 2))
				     (f (length faces))
				     (v (length vertexes)))
				 (declare (ignore ,@(set-difference '(e f v)
								    (flatten num-vertexes))))
				 ,num-vertexes))))
	      (labels ((set-pos (i v) (setf (aref ,pos-array i) v))
		       (positions () ,pos-array))
		(%with-dual-vert ,fn-list
				 ,@body)))))
	(let ((,o!-op-name (string-downcase,op-name)))
	  (setf (gethash ,o!-op-name *conway-operators*) (function ,func-name)))))))

(defun lerp-vector (v1 v2 x)
  "Return the linear interpolation of two vectors at x"
  (apply #'vector
	 (map 'list (lambda (a b) (+ a (* x (- b a)))) v1 v2)))

(defun mean-vector (vec-list)
  "Return the arithmetic mean of a sequence of vectors"
  (apply #'vector
	 (mapcar #'mean
		 (apply #'map 'list #'list vec-list))))

(defun face-center (face)
  "Return the centroid of the vertexes of the face"
  (mean-vector
   (iterate (for-vertex vertex in-face face)
	    (collect (slot-value vertex 'pos))))  )

(defun lerp-edge (edge x)
  "Return the linear interpolation of the two vertices of the edge, at x"
  (lerp-vector (slot-value (he-v1 edge) 'pos)
	       (slot-value (he-v2 edge) 'pos)
	       x))

(defconway dual "d" f (:face)
  (iterate
   (for face in faces)
   (set-pos (face-to-dual-vert face)
	    (face-center face)))
  (make-hem
   (iterate
    (for vertex in vertexes)
    (collect
     (iterate
      (for-face face in-vertex vertex)
      (collect (face-to-dual-vert face)))))
   (positions)))

(defconway ambo "a" e (:edge)
  (iterate
   (for edge in half-edges)
   (set-pos (edge-to-dual-vert edge)
	    (lerp-edge edge 1/2)))
  (make-hem
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
	       (collect (edge-to-dual-vert edge))))))
   (positions)))

(defconway truncate "t" (* 2 e) (:half-edge)
  (iterate
   (for edge in half-edges)
   (set-pos (half-edge-to-dual-vert edge)
	    (lerp-vector (slot-value (he-v1 edge) 'pos)
			 (slot-value (he-v2 edge) 'pos) 1/3)))
  (make-hem
   (nconc
    (iterate (for vertex in vertexes)
	     (collect
	      (iterate (for-edge e in-vertex vertex)
		       (collect (half-edge-to-dual-vert e)))))
    (iterate (for face in faces)
	     (collect
	      (iterate (for-edge e in-face face)
		       (appending (list (half-edge-to-dual-vert e)
					(half-edge-to-dual-vert (he-opposite e))))))))
   (positions)))


(defconway gyro "g" (+ 2 (* 3 e)) (:half-edge :vert :face)
  (iterate
   (for edge in half-edges)
   (set-pos (half-edge-to-dual-vert edge)
	    (lerp-edge edge 1/3)))
  (iterate
   (for face in faces)
   (set-pos (face-to-dual-vert face)
	    (face-center face)))

  (iterate
   (for vertex in vertexes)
   (set-pos (vert-to-dual-vert vertex)
	    (vert-pos vertex)))
  (make-hem
   (iterate
    (for edge in half-edges)
    (collect (list (half-edge-to-dual-vert edge)
		   (half-edge-to-dual-vert (he-opposite edge))
		   (vert-to-dual-vert (he-v2 edge))
		   (half-edge-to-dual-vert (he-next edge))
		   (face-to-dual-vert (he-face edge)))))
   (positions)))

(defconway dual-chamfer "x" (+ e f) (:edge :face)
  (iterate
   (for face in faces)
   (set-pos (face-to-dual-vert face)
	    (face-center face)))
  (iterate
   (for edge in half-edges)
   (set-pos (edge-to-dual-vert edge)
	    (lerp-edge edge 1/2)))
  (make-hem
   (nconc
    (iterate
     (for face in faces)
     (appending
      (iterate (for-edge edge in-face face)
	       (collect (list (face-to-dual-vert face)
			      (edge-to-dual-vert edge)
			      (edge-to-dual-vert (he-next edge)))))))
    (iterate
     (for vert in vertexes)
     (collect (iterate (for-edge edge in-vertex vert)
		       (collect (edge-to-dual-vert edge))))))
   (positions)))

(defconway propellor "p" (+ (* 2 e) f v) (:half-edge :vert)
  (iterate
   (for edge in half-edges)
   (set-pos (half-edge-to-dual-vert edge)
	    (lerp-edge edge 2/3)))
  (iterate
   (for vertex in vertexes)
   (set-pos (vert-to-dual-vert vertex)
	    (slot-value vertex 'pos)))

  (make-hem
   (nconc
    (iterate
     (for edge in half-edges)
     (collect (list (half-edge-to-dual-vert edge)
		    (vert-to-dual-vert (he-v2 edge))
		    (half-edge-to-dual-vert (he-opposite (he-next edge)))
		    (half-edge-to-dual-vert (he-next edge)))))
    (iterate
     (for face in faces)
     (collect (iterate (for-edge edge in-face face)
		       (collect (half-edge-to-dual-vert edge))))))
   (positions)))

(defvar *translation-rules*
  '(("k" "dtd")
    ("i" "dk")
    ("s" "dg")
    ("c" "dx")
    ("j" "da")
    ("b" "ta")
    ("m" "kj")
    ("o" "ja")
    ("e" "aa")
    ("n" "kd")
    ("dd" "")))

(defun conway (ident mesh)
  (funcall (apply #'compose (map 'list
                                 #'(lambda (x) (gethash (string-downcase (string x))
                                                        *conway-operators*
                                                        #'identity))
                                 ident))
           mesh))
