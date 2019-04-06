<!-- # Lcm - Lightweight Communications and Marshalling

This is a Common Lisp wrapper of [LCM](https://github.com/lcm-proj/lcm)-C project for message passing and data marshalling.

Most of the code is ported from [LCMCore](https://github.com/JuliaRobotics/LCMCore.jl) julia repo. with the following differences:

 - hash-table used for storing dimensions, types, and, fingerprints.
 - callback pass function name and message type as strings.

## Prerequisites

- [LCM](https://github.com/lcm-proj/lcm) is installed

## Installation

Clone into home/common-lisp directory. Then `asdf:test-system "lcm"`

- lcm.lisp file was generated using [siwg](http://www.swig.org/).

## Supported Types

* :boolean
* :uint8
* :sint8
* :sint16
* :sint32
* :sint64
* :float32
* :float64
* :string

Then `:array (size)` keyword would be added for arrays

### example

```
(deflcmstruct  example_t
    (timestamp :sint64)
  (position :float64 :array (3)) ;; '(size) or (size)
  (orientation :float64 :array '(4))
  (num_ranges :sint32)
  (ranges :float64 :array (num_ranges))
  (name :string)
  (enabled :boolean))
```

## Usage

See test files

### NOTE

Tested on SBCL 1.4.10

## TODO

- [x] test publish
- [ ] test subscribe
- [ ] parse .lcm file
- [ ] benchmark

## Copyright

Copyright (c) 2018 Islam Omar (catchmefastfat@gmail.com)

## License

Licensed under the MIT License. -->
