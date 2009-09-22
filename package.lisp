(defpackage :cl-mesh
  (:use :common-lisp :alexandria
	:iterate :cl-lex :yacc)
  (:export
   #:parse-wavefront-obj))

(in-package :cl-mesh)
