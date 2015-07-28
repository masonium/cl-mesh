(in-package :cl-user)

(asdf:defsystem :cl-mesh
    :description "Library for loading and manipulating 3D meshes"
    :version "0.0.0"
    :author "Mason Smith <masonium@gmail.com>"
    :maintainer "Mason Smith <masonium@gmail.com>"
    :license "GPL v3"
    :serial t
    :components 
    ((:file "package")
     (:file "wavefront-obj")
     (:file "surface")
     (:file "vec")
     (:file "mesh")
     (:file "halfedge")
     (:file "conway"))
  :depends-on (:alexandria :iterate :split-sequence :metabang-bind
                           :anaphora))
