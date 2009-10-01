(in-package :cl-mesh)

(defun parse-wavefront-obj (filename)
  "Returns a hashtable with entries \"vertices\", \"normals\", and \"indices\" containing data"
  (let* ((lines
	  (iterate (for line in-file filename using #'read-line)
		   (let ((stripped-line (string-trim " \\t" line)))
		     (when (not (emptyp stripped-line))
		       (collect (split-sequence #\Space stripped-line)))))))
    (let* ((vertex-data
	    (mapcar #'(lambda (x)
			(apply #'vector (mapcar
					 #'(lambda (x) (float (read-from-string x)))
					 (cdr x))))
		    (remove-if-not
		     (rcurry #'string-equal "v")
		     lines :key #'car)))
	   (normal-data
	    (mapcar #'(lambda (x)
			(apply #'vector (mapcar
					 #'(lambda (x) (float (read-from-string x)))
					 (cdr x))))
		    (remove-if-not
		     (rcurry #'string-equal "vn")
		     lines :key #'car)))
	   (index-data
	    (mapcar #'(lambda (x) (mapcar
				   #'(lambda (x) (1- (the fixnum (read-from-string x))))
				   (cdr x)))
		    (remove-if-not
		     (rcurry #'string-equal "f")
		     lines :key #'car)))
	   (ht (make-hash-table :test 'equal)))
      (setf (gethash "vertices" ht)
	    (make-array (length vertex-data) :initial-contents vertex-data))
      (setf (gethash "indices" ht)
	    (make-array (list
			 (length index-data) 3) :initial-contents index-data))
      (setf (gethash "normals" ht)
	    (make-array (length normal-data) :initial-contents normal-data))
      ht)))
