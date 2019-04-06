(defpackage cxx/test
  (:use :cl
        :prove
        ))
(in-package :cxx/test)

(cffi:define-foreign-library libclcxx
  (:darwin "libclcxx.dylib")
  (:unix (:or "~/common-lisp/programs/hack/clcxx/build/lib/libclcxx.so" "libclcxx.so"))
  (t (:default "libclcxx")))

(cffi:use-foreign-library libclcxx)

(plan 1)

;; start here
(ok (cxx:init))

(defun test ()
  (cxx:add-package "test" "TEST")
  (cxx:remove-package "test")
  (delete-package "test")
  )

(ok (test))

(cffi:close-foreign-library 'libclcxx)

(finalize)


