(in-package :cl-mesh)

(define-string-lexer wavefront-obj-lexer
  ("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?" (return (values 'float
							     (read-from-string $@))))
  ("v" (return (values 'v $@)))
  ("n" (return (values 'n $@)))
  ("f" (return (values 'f $@))))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun rcons (a b) (cons b a))
(defun wop-make-float-triple (v a b c)
  (declare (ignore v))
  (vector a b c))
(defun wop-make-face (f a b c)
  (declare (ignore f))
  (list a b c))
(defun wop-make-dbl (db)
  (let ((ht (make-hash-table)))
    (setf (gethash (car db) ht) (cdr db))
    ht))
(defun wop-add-db (ht db)
  (setf (gethash (car db) ht) (cdr db))
  ht))

(define-parser *wavefront-obj-parser*
  (:start-symbol wavefront-obj)
  (:terminals (float int v n f))
  (:precedence ((:right data-block-list) (:left vertex-list normal-list face-list) (data-block) (vertex normal face)))

  (wavefront-obj
   data-block-list)
  
  (data-block-list
   (data-block #'wop-make-dbl)
   (data-block-list data-block #'wop-add-db))

  ;; data blocks are cons pairs
  ;; the car is a symbol identifier
  ;; the cdr is the data
  (data-block
   (vertex-list #'(lambda (vl) (cons 'points (make-array (length vl) :initial-contents vl))))
   (normal-list #'(lambda (nl) (cons 'normals (make-array (length nl) :initial-contents nl))))
   (face-list #'(lambda (fl) (cons 'faces (make-array (list (length fl) 3) :initial-contents fl)))))

  (vertex-list
   (vertex #'list)
   (vertex-list vertex #'rcons))

  (vertex 
   (v float float float #'wop-make-float-triple))

  (normal-list
   (normal #'list)
   (normal normal-list #'cons))
  (normal 
   (n float float float #'wop-make-float-triple))
  
  (face-list
   (face #'list)
   (face face-list #'cons))
  
  ;; for now, the grammar only supports triangles
  (face
   (f float float float #'wop-make-face)))

(defun parse-wavefront-obj (filename)
  (with-open-file (str filename :direction :input)
    (parse-with-lexer 
     (stream-lexer (rcurry #'read-line nil t) #'wavefront-obj-lexer (constantly t) (constantly nil) :stream str)
     *wavefront-obj-parser*)))
