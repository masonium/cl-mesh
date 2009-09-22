(in-package :cl-mesh)

(define-string-lexer wavefront-obj-lexer
  ("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?" 
   (return (values 'float
		   (read-from-string $@))))
  ("v" (return (values 'v $@)))
  ("vn" (return (values 'n $@)))
  ("f" (return (values 'f $@))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun rcons (a b) (cons b a))
  (defun wop-make-float-triple (v a b c)
    (declare (ignore v))
    (vector (float a) (float b) (float c)))
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
  (:start-symbol wavefront-entries)
  (:terminals (float v n f))
  
  (wavefront-entries
   (entry-list #'nreverse))
  
  (entry-list
   (entry #'list)
   (entry-list entry #'rcons))

  (entry
   vertex
   normal
   face)

  (vertex 
   (v float float float))
  (normal 
   (n float float float))
  ;; for now, the grammar only supports triangles
  (face
   (f float float float)))

(defun my-stream-lexer (read-source-line string-lexer &key (stream *standard-input*))
  "Note: modified version of stream-lexer from
cl-lex/lex.lisp. Returns a closure that takes no arguments and will
return each token from stream when called.  read-source-line is a
function that takes an input stream and returns a non-empty line of
the source."
  (let (eof line-lexer (update t))
    (labels ((next-token ()
	       (multiple-value-bind (token value)
		   (funcall line-lexer)
		 (values token value)))
	     (update-line-lexer ()
	       (let ((line (funcall read-source-line stream)))
		 (if (null line)
		     (setf eof t))
		 (setf line-lexer (funcall string-lexer line)))))
      (lambda ()
	(when update
	  (update-line-lexer)
	  (setf update nil))
	(multiple-value-bind (token value)
	    (next-token)
	  (if token
	      (values token value)
	      (if eof
		  (error 'end-of-file :stream stream)
		  
		  (progn
		    (update-line-lexer)
		    (next-token)))))))))

(defun parse-wavefront-obj (filename)
  (let* ((raw-data
	  (with-open-file (str filename :direction :input)
	    (parse-with-lexer 
	     (my-stream-lexer 
	      ;; returns the next non-empty line, or NIL at EOF
	      #'(lambda (str)
		  (do ((line (read-line str nil nil) (read-line str nil nil)))
		      ((not (string-equal line "")) line)))
					;(rcurry #'read-line nil nil)
	      #'wavefront-obj-lexer 
	      :stream str)
	     *wavefront-obj-parser*)))
	 (vertex-data (mapcar #'(lambda (x) (apply #'vector 
						   (mapcar #'float (cdr x))))
			      (remove-if-not 
			       (rcurry #'string-equal "v")
			       raw-data :key #'car)))
	 (normal-data (mapcar #'(lambda (x) (apply #'vector 
						   (mapcar #'float (cdr x))))
			      (remove-if-not
			       (rcurry #'string-equal "vn")
			       raw-data :key #'car)))
	 (index-data (mapcar #'(lambda (x) (mapcar #'1- (cdr x)))
			     (remove-if-not
			      (rcurry #'string-equal "f")
			      raw-data :key #'car)))
	 (ht (make-hash-table :test 'equal)))
    (setf (gethash "vertices" ht) 
	  (make-array (length vertex-data) :initial-contents vertex-data))
    (setf (gethash "indices" ht) 
	  (make-array (list 
		       (length index-data) 3) :initial-contents index-data))
    (setf (gethash "normals" ht) 
	  (make-array (length normal-data) :initial-contents normal-data))
    ht))


