;;;; cl-link.asd
;;
;;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(asdf:defsystem #:cl-link
  :description "Describe cl-link here"
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "gpl 2.0 or later"
  :version "0.0.1"
  :serial t
  :depends-on (:cffi :cxx)
  :components ((:file "package")
               (:file "load-lib")
               (:file "export-syms")))
