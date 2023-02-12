;;; 
;;; export-syms.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :my-cl-lib)

;;; optional exports from my-cl-lib to make the symbols of cl-test
;;; available. For this to work you have to uncomment (use-package
;;; "cl-test") in load-lisp.lib

(export
 '(y.set
   create-xx
   cxx-ptr
   destruct-xx
   foo
   foo.x
   greet
   hi
   ref-class
   ref-int
   test-complex
   test-float
   test-int
   y.get))
