(defpackage #:cms-query 
  (:use :cl)
  (:export 
   #:*ns-map*
   #:find-resources
   #:register-namespace
   #:run-query))

(defpackage #:cms-query-user 
  (:use :cl :cms-query :clsql)
  (:nicknames :cms))