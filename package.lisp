(cl:defpackage :cxx
  (:use :cl :cffi :trivial-garbage)
  ;; Basic types and constructors.
  (:export
   #:add-package
   #:remove-package
   #:number-of-allocated-bytes
   #:destruct-string
   #:init)
  )
