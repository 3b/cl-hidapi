(defsystem cl-hidapi
  :description "Common Lisp (cffi) bindings to libhidapi"
  :depends-on (cffi)
  :serial t
  :components ((:file "package")
               (:file "library")
               (:file "bindings")
               (:file "hidapi")))