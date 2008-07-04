(defpackage #:cms-query 
  (:use :cl :s-sql :hunchentoot :cl-who)
  (:shadow :assert)
  (:export 
   #:compile-query
   #:compile-sql
   #:*ns-map*
   #:find-resources
   #:register-namespace
   #:run-query))

(defpackage #:cms-query-user 
  (:use :cl 
        :cms-query)
  (:nicknames :cms))
