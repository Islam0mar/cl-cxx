;;;; cl-link.asd

(asdf:defsystem #:my-cl-lib
  :description "example for cl-cxx"
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "gpl 2.0 or later"
  :version "0.0.1"
  :serial t
  :depends-on (:cffi :cxx)
  :components ((:file "package")
               (:file "load-lib")
;;;               (:file "export-syms")
               ))
