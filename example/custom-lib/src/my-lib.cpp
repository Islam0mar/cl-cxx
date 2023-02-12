#include <string>
#include "clcxx/clcxx.hpp"

// standard C function definitions

std::string greet() { return "Hello, World"; }
int Int(int x) { return x + 100; }
float Float(float y) { return y + 100.34; }
auto gr(std::complex<float> x) { return x; }
std::string hi(char* s) { return std::string("hi, " + std::string(s)); }
void ref_int(int& x) { x += 30; }

// standard C++ class definition with members and method

class xx {
 public:
  xx(int xx, int yy) : y(yy), x(xx) {}
  std::string greet() { return "Hello, World"; }
  int y;
  int x;
};
void ref_class(xx& x) { x.y = 1000000; }

// definitions of the API exposure to Common Lisp

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
