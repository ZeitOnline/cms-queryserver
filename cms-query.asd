(defpackage #:cms-query-system (:use :cl :asdf))
(in-package #:cms-query-system)

(defsystem #:cms-query 
  :name       "cms-query"
  :version    "0.0.0"
  :maintainer "R. Mattes"
  :author     "R. Mattes"
  :licence    "GPL"

  :description "cms-query"
  :long-description "Lisp implementation of a query interface to the CMS facts DB"

  :serial t
  :depends-on (:clsql-postgresql)
  :components ((:file "defpackage")
	       (:file "cms-query")))