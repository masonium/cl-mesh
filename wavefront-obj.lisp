(in-package :cl-mesh)

(define-string-lexer wavefront-obj-lexer
  ("([0-9]+)\\s" (return (values 'int (parse-integer $1))))
  ("([0-9]+)$" (return (values 'int (parse-integer $1))))
  ("([-+]?[0-9]*)\\.?([0-9]+)" (return (values 'float 
					       (float (/ (parse-integer (concatenate 'string $1 $2))
							 (if (= (length $1) 0)
							     1
							     (expt 10 (length $2))))))))
  ("v" (return (values 'v $@)))
  ("n" (return (values 'n $@)))
  ("f" (return (values 'f $@))))

(defun add-vertex (v vl)
  (cons v vl))

(defun make-vertex (v a b c)
  (declare (ignore v))
  (vector a b c))
(defun make-face (f a b c)
  (declare (ignore f))
  (list a b c))

(defun make-final-obj (vertex-list face-list))

(define-parser *wavefront-obj-grammar*
  (:start-symbol wavefront-obj)
  (:terminals (float int v n f))
  
  (wavefront-obj
   (vertex-list face-list #'list))

  (vertex-list
   (vertex #'list)
   (vertex vertex-list #'cons))
  (vertex 
   (v float float float #'make-vertex))
  
  (face-list
   (face #'list)
   (face face-list #'cons))
  
  (face
   (f int int int #'make-face)))
  
(defun parse-wavefront-obj (str)
  (parse-with-lexer 
   (wavefront-obj-lexer str)
   *wavefront-obj-grammar*))
