(defpackage #:cms-query
  (:use :cl :s-sql :hunchentoot :cl-who)
  (:shadow :assert)
  (:export
   #:start-server
   #:stop-server
   #:compile-query
   #:compile-sql
   #:*ns-map*
   #:find-resources
   #:register-namespace
   #:run-query
   #:*address*
   #:*port*))

(defpackage #:cms-query-user
  (:use :cl
        :cms-query)
  (:nicknames :cms))
