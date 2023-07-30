;;; 
;;; scratch.lisp
;;;

(in-package :my-cl-lib)

(cl-test:greet)
;;; -> "Hello, World"

(defparameter *testclass* (cl-test:create-cl-xx2 10 20))
;;; -> *TESTCLASS*

(cl-test:foo *testclass*)
;;; -> "Hello, World"

(cl-test:foo.x *testclass*)
;;; -> 10

(cl-test:y.get *testclass*)
;;; -> 20

(cl-test:y.set *testclass* 22)
;;; ; No value



;;; import the symbols to avoid the leading cl-test: in the function
;;; calls

(use-package :cl-test)

(greet)
;;; -> "Hello, World"



