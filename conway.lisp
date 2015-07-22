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

(defmacro defconway (name op-name fn-list &body body)
  "Helper macro for defining conway methods and registering them as operators."
  (with-gensyms (param o!-op-name)
    (let ((func-name (intern (concatenate 'string "CONWAY-" (string name)))))
      `(progn
         (defun ,func-name (,param)
           (with-slots (half-edges faces vertexes) ,param
             (with-dual-vert ,fn-list
               ,@body)))
         (let ((,o!-op-name (string-downcase,op-name)))
           (setf (gethash ,o!-op-name *conway-operators*) (function ,func-name)))))))

(defconway dual "d" (:face)
  (make-he-from-indices
   (iterate
     (for vertex in vertexes)
     (collect
         (iterate
           (for-face face in-vertex vertex)
           (collect (face-to-dual-vert face)))))))

(defconway ambo "a" (:edge)
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
            (collect (edge-to-dual-vert edge))))))))

(defconway kis "k" (:face :vert)
  (make-he-from-indices
   (iterate (for face in faces)
     (appending
      (iterate (for-edge edge in-face face)
        (collect (list (face-to-dual-vert face)
                       (vert-to-dual-vert (he-v1 edge))
                       (vert-to-dual-vert (he-v2 edge)))))))))

(defconway bitruncate "i" (:half-edge)
  (make-he-from-indices
   (nconc
    (iterate (for vertex in vertexes)
      (collect
          (iterate (for-edge e in-vertex vertex)
            (appending (list (half-edge-to-dual-vert e)
                             (half-edge-to-dual-vert (he-prev e)))))))
    (iterate (for face in faces)
      (collect (iterate (for-edge e in-face face)
                 (collect (half-edge-to-dual-vert e))))))))

(defconway truncate "t" (:half-edge)
  (make-he-from-indices
   (nconc
    (iterate (for vertex in vertexes)
      (collect
          (iterate (for-edge e in-vertex vertex)
            (collect (half-edge-to-dual-vert e)))))
    (iterate (for face in faces)
      (collect
          (iterate (for-edge e in-face face)
            (appending (list (half-edge-to-dual-vert e)
                             (half-edge-to-dual-vert (he-opposite e))))))))))

(defun conway-translation (from to)
  (setf (gethash (string-downcase from) *conway-operators*)
        (apply #'compose (map 'list (lambda (c) (gethash (string (string-downcase c))
                                                         *conway-operators*))
                              to))))

(mapcar #'(lambda (x) (apply #'conway-translation x))
	'(("j" "da")
	  ("b" "ta")
	  ("m" "kj")
	  ("o" "ja")
	  ("e" "aa")
	  ("n" "kd")))

(defun conway (ident mesh)
  (funcall (apply #'compose (map 'list
                                 #'(lambda (x) (gethash (string-downcase (string x))
                                                        *conway-operators*
                                                        #'identity))
                                 ident))
           mesh))
