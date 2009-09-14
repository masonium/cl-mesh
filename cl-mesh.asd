(in-package :cl-mesh)

(defpackage #:cl-mesh-system (:use #:cl #:asdf))
(in-package #:cl-mesh-system)

(unless (find-system 'asdf-system-connections nil)
  (warn "cl-mesh-system would enjoy having asdf-system-connections
around. See http://www.cliki.net/asdf-system-connections for details
and download instructions."))
(when (find-system 'asdf-system-connections nil)
  (operate 'load-op 'asdf-system-connections))

(defsystem cl-mesh
    :description "Library for loading and manipulating 3D meshes"
    :version "0.0.0"
    :author "Mason Smith <masonium@gmail.com>"
    :maintainer "Mason Smith <masonium@gmail.com>"
    :license "GPL v3"
    :serial t
    :components 
    ((:file "package"))
    
    :depends-on (:alexandria :metabang-bind :iterate :yacc :cl-lex))

