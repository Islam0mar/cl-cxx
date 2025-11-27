;;;; load-lib.lisp

(in-package :my-cl-lib)

;;; change this to the load path of libClCxx
(pushnew (pathname "/usr/local/lib/")
         cffi:*foreign-library-directories*
         :test #'equal)

(cffi:define-foreign-library clcxx
    (t (:default "libClCxx")))

(pushnew (asdf:system-relative-pathname :my-cl-lib "custom-lib/lib/")
         cffi:*foreign-library-directories*
         :test #'equal)

(cffi:define-foreign-library my-lib
  (t (:default "libMyLib")))

(cffi:use-foreign-library clcxx)
(cffi:use-foreign-library my-lib)

(cxx:init)

(cxx:add-package "CL-TEST" "TEST")

;;; optionally import the symbols of cl-test into my-cl-lib
;;; (use-package :cl-test)

