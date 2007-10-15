(defpackage #:cms-query-server-system (:use :cl :asdf))
(in-package #:cms-query-server-system)

(defsystem #:cms-query-server
  :name       "cms-query-server"
  :version    "0.0.0"
  :maintainer "R. Mattes"
  :author     "R. Mattes"
  :licence    "GPL"

  :description "cms-query-server: ..."
  :long-description "..."

  :serial t
  :depends-on (:hunchentoot :cl-webdav :cms-query :cl-who)
  :components ((:file "defpackage")
	       (:file "query-server")
               (:file "db-pool")
	       (:file "utils")))
