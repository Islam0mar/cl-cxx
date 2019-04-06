# CL-CXX - Common Lisp and CXX interoperation 

This is a C++ library to be used with COMMON-LISP such as boost.python, PYBIND11, ... 

## Prerequisites

- [CLCXX](https://github.com/Islam0mar/CLCXX) is installed

## Installation

Clone into home/common-lisp directory. Then `asdf:test-system "cxx"`

## Supported Types

* Fundamental types
* Pointers

### example

```
(cffi:define-foreign-library libclcxx
  (:darwin "libclcxx.dylib")
  (:unix (:or "~/common-lisp/programs/hack/clcxx/build/lib/libclcxx.so" "libclcxx.so"))
  (t (:default "libclcxx")))

(cffi:use-foreign-library libclcxx)
(cxx:add-package "TTT" "TEST")

(TTT:greet)

```



## Usage

See test files

### NOTE

Tested on SBCL 1.4.5.debian

## TODO

- [x] test functions
- [ ] classes [c++ side is done]
- [ ] Smart pointers
- [ ] Tuple
- [ ] benchmark

## Copyright

Copyright (c) 2018 Islam Omar (io1131@fayoum.edu.eg)

## License

Licensed under the MIT License.
