#include <string>
#include "clcxx/clcxx.hpp"

// standard C function definitions:

std::string greet() { return "Hello, World"; }
int Int(int x) { return x + 100; }
float Float(float y) { return y + 100.34; }
auto gr(std::complex<float> x) { return x; }
std::string hi(char* s) { return std::string("hi, " + std::string(s)); }
void ref_int(int& x) { x += 30; }

// C++ class definition with members and method: class xx { public:

class xx {
 public:
  xx(int xx, int yy) : y(yy), x(xx) {}
  std::string greet() { return "Hello, World"; }
  int y;
  int x;
};
void ref_class(xx& x) { x.y = 1000000; }

// definitions of the API exposure to Common Lisp:
//
// CLCXX_PACKAGE defines a Common Lisp package which can be loaded by
// Common Lisp with the command "(add-package "TEST" "CL-TEST")"
//
// The first argument of add-package is the name of the Package
// defined with CLCXX_PACKAGE in the cpp file.
//
// The second argument of add-package is the name of the common-lisp
// package to define.
//
// pack is the variable name referencing the package.
//
// The methods are:
//
// .defun (std::string "lisp-fn-name", fn-pointer c-fn)
//
// defines the Common Lisp function #'cl-test:lisp-fn-name to call
// "c-fn" from the code above.
//
// .defclass<cname, false> ("lisp-class-name")
//
// defines a class accessor for Common Lisp
//
// .member("cl-name", class-pointer c-name)
//
// define a member of the class
//
// .defmethod("cl-name", fn-pointer c-method)
//
// the class's setter and getter functions are also defined this way:
//
// .defmethod("foo.x", F_PTR([](xx x){return x.x;}))
//
// defines the methods #'cl-test:foo.x as getter and
// #'cl-test:foo.x.set as setter for the member x of class xx.
//
// .constructor defines a constructor function for Common-lisp
//
// .constructor<int, int>("create-xx"); defines a Common Lisp
// constructor function #'cl-test:create-xx with two integer
// arguments.

CLCXX_PACKAGE TEST(clcxx::Package& pack) {
  pack.defun("hi", F_PTR(&hi));
  pack.defun("test-int", F_PTR(&Int));
  pack.defun("greet", F_PTR(&greet));
  pack.defun("test-float", F_PTR(&Float));
  pack.defun("test-complex", F_PTR(&gr));
  pack.defun("ref-int", F_PTR(&ref_int));
  pack.defun("ref-class", F_PTR(&ref_class));
  pack.defclass<xx, false>("xx")
    .member("y", &xx::y)
    .defmethod("foo", F_PTR(&xx::greet))
    .defmethod("foo.x", F_PTR([](xx x){return x.x;}))
    .constructor<int, int>("create-xx");
}
