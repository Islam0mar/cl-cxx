;;; 
;;; scratch.lisp
;;;

(in-package :my-cl-lib)

(cl-test:greet) -> "Hello, World"

(defparameter *testclass* (cl-test:create-xx 10 20))

(cl-test:foo *testclass*)

(cl-test:foo.x *testclass*)

(cl-test:y.get *testclass*)

(cl-test:y.set *testclass* 22)

;;; avoiding the leading cl-test:

(use-package :cl-test)

(greet)
