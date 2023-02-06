# CL-CXX - Common Lisp wrappers for C++

This library provides an interface to C++ from lisp. It was inspired by Julia's [libcxxwrap](https://github.com/JuliaInterop/libcxxwrap-julia).

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

## Type Conversion

|------------------|------------------|-------------------|
| C++ type         | Lisp cffi type   | Allocation/copy   |
|------------------|------------------|-------------------|
| fundamental      | same             | copy value        |
| POD struct       | same             | copy value        |
| std::string      | :string          | allocate c string |
| reference        | :pointer         | allocate refernced object |
| class            | :pointer         | :pointer          |
|------------------|------------------|-------------------|

## A Small Example
First you will need to create a C++ glue library that CFF will use when calling C++ functions from Lisp. Compile the code below as a shared library say, libtest.so or libtest.dylib. The commands to do this will vary by compiler and operating system. For testing, it is always good to put the library in the same directory as the lisp code, and ensure that you set the current working directory in your Lisp image. CFFI can take some fiddling to find all the dependencies. This is not unique to this project, all Lisp C/C++ bindings with CFFI have the same requirements.

### The C++ Side of the Equation
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

### The Lisp Side of Things
You should walk through the commands below in the REPL to get an idea of how the bindings work. Before you start, ensure you have the dependencies loaded:

```common lisp
(ql:quickload "cffi")
(ql:quickload "cxx")
```

```common lisp
(defpackage cxx/test
  (:use :cl))
(in-package :cxx/test)

;;; Change the pathname to match that of your system.
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

## Loading C++ Class

To create a C++ class you should have the following:

- C++
```C++
class A {
 public:
  A(int A, int yy) : y(yy), x(A) {}
  int y;
  int x;
};
CLCXX_PACKAGE class(clcxx::Package& pack) {
pack.defclass<A, false>("A")
      .constructor<int, int>()
      .member("y", &A::y)
      .member("x", &A::x);
}
```
- lisp
```common lisp
(class:create-A2)
```
## Usage

See test files or the [cl-cxx-eigen](https://github.com/Islam0mar/cl-cxx-eigen) project for further examples.

## Architecture
- [CLCXX](https://github.com/Islam0mar/CLCXX) is used to expose `C++` function/lambda/member_function/overloaded_function to `C` function pointer that lisp cffi would call.`C++` code is compiled into a shared library --for internals see [CLCXX](https://github.com/Islam0mar/CLCXX)--.

The following steps are expected by this package:
 - load `C++` shared library `(cffi:use-foreign-library my-lib)`.
 - call `(cxx:init)`: this function calls `clcxx_init(void (*error_handler)(char *), void (*reg_data_callback)(clcxx::MetaData *, uint8_t))`. 
     + `error_handler` is used to make `C++` call lisp `error` function.
     + `reg_data_callback` is used to pass `C` function pointer, function paramters types and return type to lisp for registering lisp `cffi` function.
 - call `(cxx:add-package "TEST" "TEST")`: this function calls `register_package(const char *cl_pack,void (*regfunc)(clcxx::Package &))`.
     + `cl_pack` is the lisp package name.
     + `regfunc` is the function name in the compiled `C++` shared library `CLCXX_PACKAGE regfunc(clcxx::Package& pack)`.
 - `C++` package functions are avialable.

### NOTE

Tested on:
* SBCL 1.4.5 on debian
* CCL  1.12  on MacOS 10.13.6

## TODO

- [x] test functions
- [x] classes
- [x] references
- [ ] Smart pointers
- [ ] Tuple
- [ ] benchmark

## Alternative
 - [CL-CXX-JIT](https://github.com/Islam0mar/CL-CXX-JIT) is a new  project to make the process easier example: `(from '("<cmath>") 'import '("static_cast<double(*)(double)>(std::sin)" . "cpp-sin")),` then can be called with`(cpp-sin 0d0)`. [CL-CXX-JIT](https://github.com/Islam0mar/CL-CXX-JIT) package recompiles c++ files every time, so can be used for quick experimentations, then use `cl-cxx`. 

## Copyright

Copyright (c) 2018 Islam Omar (io1131@fayoum.edu.eg)

## License

Licensed under the MIT License.
