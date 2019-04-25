# CL-CXX - Common Lisp and CXX interoperation 

This is a C++ library to be used with COMMON-LISP such as boost.python, PYBIND11, ... 

## Prerequisites

- [CLCXX](https://github.com/Islam0mar/CLCXX) is installed

## Installation

Clone into home/common-lisp directory. Then `asdf:test-system "cxx"`

## Supported Types

* Fundamental types
* Pointers
* Classes can add public members and functions
* POD
* functions, std::function and lambda

### example

```c++
#include <string>
#include "clcxx/clcxx.hpp"

std::string greet() { return "Hello, World"; }
int Int(int x) { return x + 100; }
float Float(float y) { return y + 100.34; }
auto gr(std::complex<float> x) { return x; }
std::string hi(char* s) { return std::string("hi, " + std::string(s)); }
void ref_int(int& x) { x += 30; }
void ref_class(xx& x) { x.y = 1000000; }
class xx {
 public:
  xx(int xx, int yy) : y(yy), x(xx) {}
  std::string greet() { return "Hello, World"; }
  int y;
  int x;
};

CLCXX_PACKAGE TEST(clcxx::Package& pack) {
  pack.defun("hi", &hi);
  pack.defun("test-int", &Int);
  pack.defun("greet", &greet);
  pack.defun("test-float", &Float);
  pack.defun("test-complex", &gr);
  pack.defun("ref-int", &ref_int);
  pack.defun("ref-class", &ref_class);
  pack.defclass<xx, false>("xx")
      .member("y", &xx::y)
      .defmethod("foo", &xx::greet)
      .constructor<int, int>();
}
```
compiled as shared lib. libtest

in lisp

```common lisp
(defpackage cxx/test
  (:use :cl
        ))
(in-package :cxx/test)

(pushnew (merge-pathnames #p"ros/lisp-demo/lib/" (user-homedir-pathname))
         cffi:*foreign-library-directories*
         :test #'equal)

(cffi:define-foreign-library my-lib
  (t (:default "libtest")))


(cffi:use-foreign-library my-lib)

(cxx:init)

(cxx:add-package "TEST" "TEST")

(test:greet)

(test:hi "Cxx")
```

## Usage

See test files

### NOTE

Tested on SBCL 1.4.5.debian

## TODO

- [x] test functions
- [x] classes
- [x] references
- [ ] Smart pointers
- [ ] Tuple
- [ ] benchmark

## Copyright

Copyright (c) 2018 Islam Omar (io1131@fayoum.edu.eg)

## License

Licensed under the MIT License.
